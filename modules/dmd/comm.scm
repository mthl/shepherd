;; comm.scm -- Communication between processes and general output.
;; Copyright (C) 2013 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA  02111-1307, USA.

(define-module (dmd comm)
  #:use-module (oop goops)
  #:use-module (dmd support)
  #:use-module (srfi srfi-1)
  #:export (<sender>
            send-data
            <receiver>
            receive-data
            terminating-string

            original-output-port
            log-output-port
            start-logging
            stop-logging

            be-silent
            be-verbose

            open-extra-sender
            close-extra-sender
            without-extra-output
            dmd-output-port))

;; This file encapsulates the exact semantics of the communication
;; between deco and dmd.  It also does the other output stuff.

(define-class <sender> ()
  target-file
  socket)

;; Pass only a file name to the `make' procedure.
(define-method (initialize (obj <sender>) args)
  (assert (= (length args) 1))
  (slot-set! obj 'target-file (car args))
  (slot-set! obj 'socket (socket PF_UNIX SOCK_DGRAM 0)))

(define-method (send-data (obj <sender>) data)
  (define (send-packet data)
    (sendto (slot-ref obj 'socket)
	    data
	    AF_UNIX
	    (slot-ref obj 'target-file)))

  (send-packet (number->string (string-length data)))
  (send-packet data))



(define-class <receiver> ()
  socket)

;; Pass only a file name to the `make' procedure.
(define-method (initialize (obj <receiver>) args)
  (assert (= (length args) 1))
  (slot-set! obj 'socket (socket PF_UNIX SOCK_DGRAM 0))
  ;; Make the socket available to us.
  (catch-system-error (delete-file (car args)))
  (bind (slot-ref obj 'socket) AF_UNIX (car args)))

;; Get a message from the receiver.
(define-method (receive-data (obj <receiver>))
  (let ((sock (slot-ref obj 'socket))
	(buf (make-string 10))
	(len #f) (real-len #f))
    ;; We need to do it this way for getting data from the socket while
    ;; being able to process signals (in particular SIGCHLD in dmd and
    ;; SIGINT in both deco and dmd) immediatelly.  At least until the
    ;; implementation of `recv!' in Guile changes, this is necessary.
    (define (select+recv)
      (let retry ()
	(catch 'system-error
	  (lambda ()
	    (select (list sock) '() '())
	    (recv! sock buf))
	  (lambda (key . args)
	    ;; Usually this will be an interruption of select due to a
	    ;; signal, but in any case we cannot do much else (except
	    ;; for aborting maybe).
	    (retry)))))

    ;; Get length of message, then the message itself.
    (set! len (string->number (string-take buf (select+recv))))
    (or len ;; Not a valid number.
	(set! len 1024)) ;; The best we can do...
    (set! buf (make-string len))
    (set! real-len (select+recv))
    (or (= real-len len)
	(local-output "Invalid data received."))
    buf))



;; FIXME: That may be ``good enough'', but should be fixed anyway.
;; For communication between deco and dmd.
(define terminating-string "!§&§&§&§&!") ;; Unlikely to appear in output.

(begin-dmd
 ;; Create a `backup' of the original standard output.
 (define original-output-port (current-output-port))

 ;; Port for logging.  This must always be a valid port, never `#f'.
 (define log-output-port (%make-void-port "w"))
 (define (start-logging file)
   (set! log-output-port (open-file file "wl"))) ;; Line-buffered port.
 (define (stop-logging)
   (set! log-output-port (%make-void-port "w")))

 ;; Whether we should be silent.
 (define silent #f)
 (define (be-silent) (set! silent #t))
 (define (be-verbose) (set! silent #f))

 ;; Additional output destination, if any.
 (define extra-output-sender #f)
 (define (open-extra-sender file)
   (catch 'system-error
     (lambda ()
       (set! extra-output-sender (make <sender> file)))
     (lambda (key . args)
       (local-output "Failed to open ~a." file))))
 (define (close-extra-sender)
   (catch-system-error
    (send-data extra-output-sender terminating-string)
    (set! extra-output-sender #f)))

 (define-syntax-rule (without-extra-output EXPR ...)
   (let ((sender extra-output-sender))
     (dynamic-wind
	 (lambda ()
	   (set! extra-output-sender #f))
	 (lambda ()
	   EXPR ...)
	 (lambda ()
	   (set! extra-output-sender sender)))))

 ;; We provide our own output mechanism, because we have certain
 ;; special needs; most importantly, we want to send output to deco
 ;; sometimes.
 (define dmd-output-port
   (make-soft-port
    (vector

     ;; One character for output.
     (lambda (char)
       (display (string char)))

     ;; A string for output.
     (let ((buffer '())) ;; List of unwritten output strings.
       (lambda (str)
	 ;; Standard output, display directly.
	 (display str original-output-port)
	 ;; Socket to deco, send directly.
	 (and extra-output-sender
	      (catch-system-error
	       (send-data extra-output-sender str)))
	 ;; Logfile, buffer line-wise and output time for each
	 ;; completed line.
	 (if (not (string-index str #\newline))
	     (set! buffer (cons str buffer))
	   (let* ((log (lambda (x)
			 (display x log-output-port)))
		  (init-line (lambda ()
			       (log (strftime "%Y-%m-%d %H:%M:%S "
					      (localtime (current-time)))))))
	     (init-line)
	     (for-each log (reverse buffer))
	     (let* ((lines (string-split str #\newline))
		    (last-line (car (take-right lines 1)))
		    (is-first #t))
	       (for-each (lambda (line)
			   (if is-first
			       (set! is-first #f)
			     (init-line))
			   (log line)
			   (log #\newline))
			 (drop-right lines 1))
	       (set! buffer (if (string-null? last-line)
				'()
			      (list last-line))))))))

     ;; Flush output.
     (lambda ()
      ;; FIXME: Do we need to do something?  Flush the logfile buffer?
       #t)

     ;; Get a character (unused).
     #f

     ;; Close the port.
     (lambda () #t))

    ;; It's an output-only port.
    "w"))

 (set-current-output-port dmd-output-port))

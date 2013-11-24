;; comm.scm -- Communication between processes and general output.
;; Copyright (C) 2013 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;;
;; This file is part of GNU dmd.
;;
;; GNU dmd is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; GNU dmd is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU dmd.  If not, see <http://www.gnu.org/licenses/>.

(define-module (dmd comm)
  #:use-module (dmd support)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (open-connection

            <dmd-command>
            dmd-command?
            dmd-command
            dmd-command-directory
            dmd-command-action
            dmd-command-service
            dmd-command-arguments

            write-command
            read-command
            terminating-string

            original-output-port
            log-output-port
            start-logging
            stop-logging

            be-silent
            be-verbose

            open-extra-sender
            close-extra-sender
            dmd-output-port))


;; Command for dmd.
(define-record-type <dmd-command>
  (%dmd-command action service args directory)
  dmd-command?
  (action    dmd-command-action)                  ; symbol
  (service   dmd-command-service)                 ; symbol
  (args      dmd-command-arguments)               ; list of strings
  (directory dmd-command-directory))              ; directory name

(define* (dmd-command action service
                      #:key (arguments '()) (directory (getcwd)))
  "Return a new command for ACTION on SERVICE."
  (%dmd-command action service arguments directory))

(define* (open-connection #:optional (file default-socket-file))
  "Open a connection to the daemon, using the Unix-domain socket at FILE, and
return the socket."
  ;; The protocol is sexp-based and UTF-8-encoded.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let ((sock    (socket PF_UNIX SOCK_STREAM 0))
          (address (make-socket-address PF_UNIX file)))
      (connect sock address)
      sock)))

(define (read-command port)
  "Receive a command from PORT."
  (match (read port)
    (('dmd-command ('version 0 _ ...)
                   ('action action)
                   ('service service)
                   ('arguments args ...)
                   ('directory directory))
     (dmd-command action service
                  #:arguments args
                  #:directory directory))))

(define (write-command command port)
  "Write COMMAND to PORT."
  (match command
    (($ <dmd-command> action service (arguments ...) directory)
     (write `(dmd-command (version 0)             ; protocol version
                          (action ,action)
                          (service ,service)
                          (arguments ,@arguments)
                          (directory ,directory))
            port))))



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

 (define-public %current-client-socket
   ;; Socket of the client currently talking to the daemon.
   (make-parameter #f))

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
	 (when (%current-client-socket)
           (catch-system-error
            (display str (%current-client-socket))))
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

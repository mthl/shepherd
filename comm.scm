;; comm.scm -- Communication between processes.
;; Copyright (C) 2002 Wolfgang Jährling <wolfgang@pro-linux.de>
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

;; This encapsulates the exact semantics of the communication between
;; deco and dmd.  It provides convenient classes for sending and
;; receiving data.

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
  (without-system-error (delete-file (car args)))
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
		 ;; Usually this will be an interruption of select due
		 ;; to a signal, but in any case we cannot do much
		 ;; else, except for aborting.
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

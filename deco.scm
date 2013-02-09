;; deco.scm -- The `DaEmon COntrol' program.
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

(define program-name "deco")

(use-modules (oop goops)      ;; OO support.
	     (srfi srfi-1)    ;; List library.
	     (srfi srfi-13))  ;; String library.

(debug-enable 'backtrace)

(load "config.scm")
(load "support.scm")
(load "args.scm")
(load "comm.scm")



;; Main program.
(define (main args)
  (let ((socket-file default-socket-file)
	(deco-socket-file default-deco-socket-file)
	(command-args '()))
    (process-args args
		  "ACTION SERVICE [ARG...]"
		  (string-append
		   "Apply ACTION (start, stop, status, etc.) on SERVICE"
		   " with the ARGs.")
		  (lambda (arg)
		    ;; Collect unknown args.
		    (set! command-args (cons arg command-args)))
		  (make <option>
		    ;; It might actually be desirable to have an
		    ;; ``insecure'' setup in some circumstances, thus
		    ;; we provide it as an option.
		    #:long "insecure" #:short #\I
		    #:takes-arg? #f
		    #:description "don't ensure that the setup is secure"
		    #:action (lambda ()
			       (set! insecure? #t)))
		  (make <option>
		    #:long "socket" #:short #\s
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "send commands to FILE"
		    #:action (lambda (file)
			       (set! socket-file file)))
		  (make <option>
		    #:long "result-socket" #:short #\r
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "use FILE to receive responses"
		    #:action (lambda (file)
			       (set! deco-socket-file file))))
    (assert (not (< (length command-args) 2))) ;; FIXME: Error message.
    (set! command-args (reverse command-args))
    (let ((sender (make <sender> socket-file))
	  (receiver (make <receiver> deco-socket-file)))
      ;; Send initial handshake.
      (send-data sender (number->string (+ 2 (length command-args))))
      (send-data sender (getcwd))
      (send-data sender deco-socket-file)
      ;; Send the command.
      (for-each (lambda (arg)
		  (send-data sender arg))
		command-args)
      ;; Receive output.
      (letrec ((next-line (lambda (line)
			    (if (string=? line terminating-string)
				(quit)
			      (begin
				(display line)
				(next-line (receive-data receiver)))))))
	(next-line (receive-data receiver))))))

(main (cdr (command-line)))

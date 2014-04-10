;; halt.scm -- Halt or power off the system.
;; Copyright (C) 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (halt)
  #:use-module (dmd support)
  #:use-module (dmd args)
  #:use-module (dmd comm)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:export (program-name
            main))

(define program-name "halt")



;; Main program.
(define (main . args)
  (false-if-exception (setlocale LC_ALL ""))

  (let ((socket-file default-socket-file)
	(command-args '()))
    (process-args program-name args
		  "ARG..."
		  "Halt or power off the system."
		  not ;; Fail on unknown args.
		  (make <option>
		    #:long "socket" #:short #\s
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "send commands to FILE"
		    #:action (lambda (file)
			       (set! socket-file file))))

    (set! command-args (reverse command-args))
    (with-system-error-handling
     (let ((sock (open-connection socket-file)))
       ;; Send the command without further ado.
       (write-command (dmd-command 'power-off 'dmd) sock)

       ;; Receive output.
       (setvbuf sock _IOLBF)
       (let loop ((line (read-line sock)))
         (unless (eof-object? line)
           (display line)
           (newline)
           (loop (read-line sock))))))))

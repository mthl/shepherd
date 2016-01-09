;; deco.scm -- The `DaEmon COntrol' program.
;; Copyright (C) 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (deco)
  #:use-module (dmd config)
  #:use-module (dmd support)
  #:use-module (dmd args)
  #:use-module (dmd comm)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (program-name
            main))

(define program-name "deco")



(define (run-command socket-file action service args)
  "Perform ACTION with ARGS on SERVICE, and display the result.  Connect to
the daemon via SOCKET-FILE."
  (with-system-error-handling
   (let ((sock (open-connection socket-file)))
     ;; Send the command.
     (write-command (dmd-command action service #:arguments args)
                    sock)

     ;; Receive output.
     (setvbuf sock _IOLBF)
     (let loop ((line (read-line sock)))
       (unless (eof-object? line)
         (display line)
         (newline)
         (loop (read-line sock)))))))

;; Main program.
(define (main . args)
  (false-if-exception (setlocale LC_ALL ""))

  (let ((socket-file default-socket-file)
	(command-args '()))
    (process-args program-name args
		  "ACTION SERVICE [ARG...]"
		  (string-append
		   "Apply ACTION (start, stop, status, etc.) on SERVICE"
		   " with the ARGs.")
		  (lambda (arg)
		    ;; Collect unknown args.
		    (set! command-args (cons arg command-args)))
		  (make <option>
		    #:long "socket" #:short #\s
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "send commands to FILE"
		    #:action (lambda (file)
			       (set! socket-file file))))

    (match (reverse command-args)
      (((and action (or "status" "detailed-status"))) ;one argument
       (run-command socket-file (string->symbol action) 'dmd '()))
      ((action service args ...)
       (run-command socket-file
                    (string->symbol action)
                    (string->symbol service) args))
      (_
       (format (current-error-port)
               (l10n "Usage: deco ACTION [SERVICE [OPTIONS...]]~%"))
       (exit 1)))))

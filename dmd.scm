;; dmd.scm -- Daemon managing Daemons (or Daemons-managing Daemon?)
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

(define program-name "dmd")

(use-modules (ice-9 syncase)  ;; R5RS macros.
	     (ice-9 rdelim)   ;; Line-based I/O.
	     (oop goops)      ;; OO support.
	     (srfi srfi-1)    ;; List library.
	     (srfi srfi-13))  ;; String library.

(debug-enable 'backtrace)

(load "config.scm")
(load "support.scm")
(load "service.scm")
(load "runlevel.scm")
(load "self.scm")
(load "args.scm")
(load "comm.scm")



(define persistency #f)
(define persistency-state-file default-persistency-state-file)

;; Main program.
(define (main args)
  (let ((config-file default-config-file)
	(socket-file default-socket-file)
	(insecure #f)
	(silent #f)
	(logfile default-logfile))
    ;; Process command line arguments.
    (process-args args
		  ""
		  "This is a service manager for Unix and GNU."
		  not ;; Fail on unknown args.
		  (make <option>
		    #:long "persistency" #:short #\p
		    #:takes-arg? #t #:optional-arg? #t #:arg-name "FILE"
		    #:description "use FILE to load and store state"
		    #:action (lambda (file)
			       (set! persistency #t)
			       (and file
				    (set! persistency-state-file file))))
		  (make <option>
		    #:long "quiet"
		    #:takes-arg? #f
		    #:description "synonym for --silent"
		    #:action (lambda ()
			       (set! silent #t)))
		  (make <option>
		    #:long "silent" #:short #\S
		    #:takes-arg? #f
		    #:description "don't do output to stdout"
		    #:action (lambda ()
			       (set! silent #t)))
		  (make <option>
		    ;; It might actually be desirable to have an
		    ;; ``insecure'' setup in some circumstances, thus
		    ;; we provide it as an option.
		    #:long "insecure" #:short #\I
		    #:takes-arg? #f
		    #:description "don't ensure that the setup is secure"
		    #:action (lambda ()
			       (set! insecure #t)))
		  (make <option>
		    #:long "logfile" #:short #\l
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "log actions in FILE"
		    #:action (lambda (file)
			       (set! logfile file)))
		  (make <option>
		    #:long "config" #:short #\c
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "read configuration from FILE"
		    #:action (lambda (file)
			       (set! config-file file)))
		  (make <option>
		    #:long "socket" #:short #\s
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "get commands from socket FILE or from stdin"
		    #:action (lambda (file)
			       (set! socket-file
				     (if (string=? file "none")
					 #f
					 file)))))
    ;; We do this early so that we can abort early if necessary.
    (and socket-file
	 (verify-dir (dirname socket-file) insecure))
    ;; Enable logging as first action.
    (start-logging logfile)
    ;; Make sure we don't write to stdout if not desired.
    (and silent
	 (be-silent))
    ;; Start the dmd service.
    (start dmd-service)
    ;; This _must_ succeed.  (We could also put the `catch' around
    ;; `main', but it is often useful to get the backtrace.)
    (catch #t
	   (lambda ()
	     (load config-file))
	   (lambda (key . args)
	     (caught-error key args)
	     (quit 1)))
    ;; Start what was started last time.
    (and persistency
	 (catch 'system-error
		(lambda ()
		  (start-in-order (read (open-input-file
					 persistency-state-file))))
		(lambda (key . args)
		  (apply local-output (cadr args) (caddr args))
		  (quit 1))))

    (if (not socket-file)
	;; Get commands from the standard input port.
	(while #t (process-command
		   (apply list
			  "." #f ;; Use current dir, no socket output.
			  (string-tokenize (read-line)))))
	;; Process the data arriving at a socket.
	(let ((command-source (make <receiver> socket-file))
	      (greeting #f))
	  (letrec
	      ((next-command
		(lambda ()
		  ;; Get number of messages first.
		  (set! greeting (receive-data command-source))
		  (letrec
		      ((next-message
			(lambda (messages-left msg-list)
			  (and (not messages-left)
			       (begin ;; Not a valid number.
				 (local-output "Invalid data received.")
				 (set! messages-left 0)))
			  (if (zero? messages-left)
			      (process-command (reverse! msg-list))
			      (next-message (- messages-left 1)
					    (cons (receive-data command-source)
						  msg-list))))))
		    (next-message (string->number greeting)
				  '()))
		  (next-command))))
	    (next-command))))))

;; Interpret COMMAND, a command sent by the user, represented as a
;; list of strings.
(define (process-command command)
  (if (< (length command) 4) ;; At least dir, socket, action and service.
      (local-output "Invalid command.")
      (let ((dir (car command))
	    (file (cadr command))
	    (action (string->symbol (caddr command)))
	    (service (string->symbol (cadddr command)))
	    (args (cddddr command)))
	(chdir dir)
	(and file
	     (open-extra-sender file))
	;; We have to catch `quit' so that we can send the
	;; terminator line to deco before we actually quit.
	(catch 'quit
	       (lambda ()
		 (case action
		   ((start) (apply start service args))
		   ((stop) (apply stop service args))
		   ((enforce) (apply enforce service args))
		   ((list-actions) (display-extra-actions service))
		   ;; `enable' and `disable' have the semantics of
		   ;; `extra-action', thus they are handled there.
		   (else (apply extra-action
				(cons* service action args)))))
	       (lambda (key)
		 (and file
		      (close-extra-sender))
		 (quit)))
	(and file
	     (close-extra-sender)))))

(main (cdr (command-line)))

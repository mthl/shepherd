;; self.scm -- Definition of the `dmd' service.
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

(define dmd-service
  (make <service>
    #:docstring "The dmd service is used to operate on dmd itself."
    #:provides '(dmd)
    #:requires '()
    #:respawn #f
    #:start (lambda args
	      (and (isatty? (current-output-port))
		   (display-version))
	      #t)
    #:stop (lambda (unused . args)
	     (local-output "Exiting dmd...")
	     ;; Prevent that we try to stop ourself again.
	     (slot-set! dmd-service 'running #f)
	     ;; Shutdown everything.
	     (let ((running-services '()))
	       (for-each-service
		(lambda (service)
		  (and (running? service)
		       (begin
			 (stop service)
			 (and persistency
			      (set! running-services
				    (cons (canonical-name service)
					  running-services)))))))
	       (and persistency
		    (write-line running-services
				(open-output-file persistency-state-file))))
	     (quit))
    ;; All actions here need to take care that they do not invoke any
    ;; user-defined code without catching `quit', since they are
    ;; allowed to quit, while user-supplied code shouldn't be.
    #:actions
    (make-actions
     ;; Display status.
     (status
      "Display the status of dmd.  I.e. which services are running and
which ones are not."
      (lambda (running)
	(let ((started '()) (stopped '()))
	  (for-each-service
	   (lambda (service)
	     (if (running? service)
		 (set! started (cons (canonical-name service)
				     started))
	       (set! stopped (cons (canonical-name service)
				   stopped)))))
	  (local-output "Started: ~a" started)
	  (local-output "Stopped: ~a" stopped))))
     ;; Look at every service in detail.
     (detailed-status
      "Display detailed information about all services."
      (lambda (running)
	(for-each-service dmd-status)))
     ;; Load a configuration file.
     (load
      "Load the Scheme code from FILE into dmd.  This is potentially
dangerous.  You have been warned."
      (lambda (running file-name)
	(local-output "Loading ~a." file-name)
	;; Every action is protected anyway, so no need for a `catch'
	;; here.  FIXME: What about `quit'?
	(load file-name)))
     ;; Disable output.
     (silent
      "Disable the displaying of information on standard output."
      (lambda (running)
	(be-silent)))
     ;; Enable output.
     (verbose
      "Enable the displaying of information on standard output."
      (lambda (running)
	(be-verbose)))
     ;; Go into the background.
     (daemonize
      "Go into the background.  Be careful, this means that a new
process will be created, so dmd will not get SIGCHLD signals anymore
if previously spawned childs terminate.  Therefore, this action should
usually only be used (if at all) *before* childs get spawned for which
we want to receive these signals."
      (lambda (running)
	(be-silent)
	(if (zero? (primitive-fork))
	    #t
	  (quit))))
     (persistency
      "Safe the current state of running and non-running services.
This status gets written into a file on termination, so that we can
restore the status on next startup.  Optionally, you can pass a file
name as argument that will be used to store the status."
      (opt-lambda (running) ((file #f))
	(set! persistency #t)
	(and file
	     (set! persistency-state-file file))))
     (no-persistency
      "Don't safe state in a file on exit."
      (lambda (running)
	(set! persistency #f)))
     (cd
      "Change the working directory of dmd.  This only makes sense
when in interactive mode, i.e. with `--socket=none'."
      (lambda (running dir)
	(chdir dir)))
     ;; Restart it - that does not make sense, but
     ;; we're better off by implementing it due to the
     ;; default action.
     (restart
      "This does not work for dmd."
      (lambda (running)
	(local-output "You must be kidding."))))))

(register-services dmd-service)

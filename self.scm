;; self.scm -- Definition of the `dmd' service.
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

(define dmd-service
  (make <service>
    #:provides '(dmd)
    #:requires '()
    #:respawn #f
    #:start (lambda ()
	      (and (isatty? (current-output-port))
		   (display-version))
	      #t)
    #:stop (lambda (unused)
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
    ;; All extra-actions need to take care that they do not invoke any
    ;; user-defined code without catching `quit', since they are
    ;; allowed to quit.
    #:extra-actions (make-extra-actions
		     ;; Display status.
		     (status
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
		      (lambda (running)
			(for-each-service default-display-status)))
		     ;; Load a configuration file.
		     (load
		      (lambda (running file-name)
			(local-output "Loading ~a." file-name)
			;; Every extra-action is protected anyway, so
			;; no need for a `catch' here.
			(load file-name)))
		     ;; Disable output.
		     (silent
		      (lambda (running)
			(be-silent)))
		     ;; Enable output.
		     (verbose
		      (lambda (running)
			(be-verbose)))
		     ;; Go into background.  This should be called
		     ;; before respawnable services are started, as
		     ;; otherwise we would not get the SIGCHLD signals
		     ;; when they terminate.
		     (daemonize
		      (lambda (running)
			(be-silent)
			(if (zero? (primitive-fork))
			    #t
			    (quit))))
		     (enable-persistency
		      (lambda (running)
			(set! persistency #t)))
		     (disable-persistency
		      (lambda (running)
			(set! persistency #f)))
		     ;; Restart it - that does not make sense, but
		     ;; we're better off by implementing it due to the
		     ;; default action.
		     (restart
		      (lambda (running)
			(local-output "You must be kidding."))))))

(register-services dmd-service)

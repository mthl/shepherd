;; service.scm -- Representation of services.
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

;; Respawning CAR times in CDR seconds will disable the service.
(define respawn-limit (cons 5 5))

(define-class <service> ()
  ;; List of provided service symbols.  The first one is also called
  ;; the `canonical name' and must be unique to this service.
  (provides #:init-keyword #:provides
	    #:getter provided-by)
  ;; List of required service symbols.
  (requires #:init-keyword #:requires
	    #:init-value '()
	    #:getter required-by)
  ;; If `#t', then assume the `running' slot specifies a PID and
  ;; respawn it if that process terminates.  Otherwise `#f'.
  (respawn? #:init-keyword #:respawn?
	    #:init-value #f
	    #:getter respawn?)
  ;; The action to perform to start the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but it
  ;; must be possible to call it without any argument.  If the
  ;; starting attempt failed, it must return `#f'.  The return value
  ;; will be stored in the `running' slot.
  (start #:init-keyword #:start
	 #:init-value (lambda args #t))
  ;; The action to perform to stop the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but must
  ;; be callable with exactly one argument, which will be the value of
  ;; the `running' slot.  Whatever the procedure returns will be saved
  ;; in the `running' slot again, thus it should return `#f' on
  ;; success (and possibly even on failure).
  (stop #:init-keyword #:stop
	#:init-value (lambda (running . args) #f))
  ;; Additional actions that can be performed with the service.  This
  ;; is a hash with a procedure as the value for every action symbol.
  (extra-actions #:init-keyword #:extra-actions
		 #:init-form (make-extra-actions))
  ;; If this is `#f', it means that the service is not running
  ;; currently.  Otherwise, it is the value that was returned by the
  ;; procedure in the `start' slot when the service was started.
  (running #:init-value #f)
  ;; A service can be disabled if it is respawning too fast; it is
  ;; also possible to enable or disable it manually.
  (enabled? #:init-value #t
	    #:getter enabled?)
  ;; Some services should not be directly stopped, but should not be
  ;; respawned anymore instead.  This field indicates that we are in
  ;; the phase after the stop but before the termination.
  (waiting-for-termination? #:init-value #f)
  ;; This causes the above to be used.  When this is `#t', there is no
  ;; need for a destructor (i.e. no value in the `stop' slot).
  (stop-delay? #:init-keyword #:stop-delay?
	       #:init-value #f)
  ;; The times of the last respawns.
  (last-respawns #:init-form (apply circular-list
				    (make-list (car respawn-limit) 0))))

;; Return the canonical name of the service.
(define-method (canonical-name (obj <service>))
  (car (provided-by obj)))

;; Return whether the service is currently running.
(define-method (running? (obj <service>))
  (and (slot-ref obj 'running) #t))

;; Return a list of all extra-actions implemented by OBJ. 
(define-method (extra-action-list (obj <service>))
  (hash-fold (lambda (key value all)
	       (cons key all))
	     '()
	     (slot-ref obj 'extra-actions)))

;; Return whether OBJ implements the extra-action ACTION.
(define-method (defines-extra-action? (obj <service>) action)
  (and (hashq-ref (slot-ref obj 'extra-actions) action #f) #t))

;; Enable the service, allow it to get started.
(define-method (enable (obj <service>))
  (slot-set! obj 'enabled? #t)
  (local-output "Enabled service ~a." (canonical-name obj)))

;; Disable the service, make it unstartable.
(define-method (disable (obj <service>))
  (slot-set! obj 'enabled? #f)
  (local-output "Disabled service ~a." (canonical-name obj)))

;; Start the service, including dependencies.
(define-method (start (obj <service>) . args)
  (cond ((running? obj)
	 (local-output "Service ~a is already running."
		       (canonical-name obj))
	 #f) ;; FIXME: Is that the right thing?
	((not (enabled? obj))
	 (local-output "Service ~a is currently disabled."
		       (canonical-name obj))
	 #f)
	((let ((conflicts (conflicts-with-running obj)))
	   (or (null? conflicts)
	       (local-output "Service ~a conflicts with running services ~a."
			     (canonical-name obj) conflicts))
	   (not (null? conflicts)))
	 #f)
	(else
	 ;; It is not running and does not conflict with anything
	 ;; that's running, so we can go on and launch it.
	 (let ((problem
		;; Resolve all dependencies.
		(call/ec (lambda (return)
			   (for-each (lambda (symbol)
				       ;; FIXME: enforce?!
				       (or (start symbol)
					   (return symbol)))
				     (required-by obj))
			   #f))))
	   (if problem
	       (local-output "Service ~a depends on ~a."
			     (canonical-name obj)
			     problem)
	       ;; Start the service itself.
	       (slot-set! obj 'running (catch #t
					       (lambda ()
						 (apply (slot-ref obj 'start)
							args))
					       (lambda (key . args)
						 (caught-error key args)
						 #f))))
	   ;; Status message.
	   (local-output (if (running? obj)
			     (l10n "Service ~a has been started.")
			     (l10n "Service ~a could not be started."))
			 (canonical-name obj)))))
  (slot-ref obj 'running))

;; Stop the service, including services that depend on it.  If the
;; latter fails, continue anyway.  Return `#f' if it could be stopped.
(define-method (stop (obj <service>) . args)
  (if (not (running? obj))
      (local-output "Service ~a is not running." (canonical-name obj))
      (if (slot-ref obj 'stop-delay?)
	  (begin
	    (slot-set! obj 'waiting-for-termination? #t)
	    (local-output "Service ~a pending to be stopped."
			  (canonical-name obj)))
	  (begin
	    ;; Stop services that depend on it.
	    (for-each-service
	     (lambda (serv)
	       (and (running? serv)
		    (for-each (lambda (sym)
				(and (memq sym (provided-by obj))
				     (stop serv)))
			      (required-by serv)))))
	    ;; If it is a respawnable service, we have to pretend that
	    ;; it is already stopped, because killing it in the
	    ;; destructor would respawn it immediatelly otherwise.
	    (and (respawn? obj)
		 (slot-set! obj 'running #f))
	    ;; Stop the service itself.
	    (slot-set! obj 'running (catch #t
					   (lambda ()
					     (apply (slot-ref obj 'stop)
						    (slot-ref obj 'running)
						    args))
					   (lambda (key . args)
					     ;; Special case: `dmd' may quit.
					     (and (eq? dmd-service obj)
						  (eq? key 'quit)
						  (apply quit args))
					     (caught-error key args)
					     ;; Don't change anything.
					     (slot-ref obj 'running))))
	    ;; Status message.
	    (let ((name (canonical-name obj)))
	      (if (running? obj)
		  (local-output "Service ~a could not be stopped." name)
		  (local-output "Service ~a has been stopped." name))))))
  (slot-ref obj 'running))

;; Call extra-action ACTION with ARGS.
(define-method (extra-action (obj <service>) action . args)
  (define (default-action running)
    (case action
      ;; Restarting is done in the obvious way.
      ((restart)
       (and running
	    (stop obj))
       (start obj))
      ;; Displaying status via the default implementation.
      ((status)
       (default-display-status obj))
      (else
       (local-output "Service ~a does not have a ~a action."
		     (canonical-name obj)
		     action))))

  ;; Calling default-action will be allowed even when the service is
  ;; not running, as it provides generally useful functionality and
  ;; information.
  (let ((proc (hashq-ref (slot-ref obj 'extra-actions)
			 action
			 default-action)))
    (if (and (not (eq? proc default-action))
	     (not (running? obj)))
	(local-output "Service ~a is not running." (canonical-name obj))
	(catch #t
	       (lambda ()
		 (apply proc (slot-ref obj 'running) args))
	       (lambda (key . args)
		 ;; Special case: `dmd' may quit.
		 (and (eq? dmd-service obj)
		      (eq? key 'quit)
		      (apply quit args))
		 (caught-error key args))))))

;; Return a list of canonical names of the services that conflict with
;; OBJ.
(define-method (conflicts-with (obj <service>))
  (let ((conflicts '()))
    (for-each (lambda (sym)
		(for-each (lambda (s)
			    (set! conflicts (cons (canonical-name s)
						  conflicts)))
			  (lookup-services sym)))
	      (provided-by obj))
    ;; Clean up the result.
    (set! conflicts
	  (delete! (canonical-name obj)
		   (delete-duplicates! conflicts eq?)
		   eq?))
    conflicts))

;; Check if this service provides a symbol that is already provided
;; by any other running services.  If so, return the canonical names
;; of the other services.  Otherwise, return the empty list.
(define-method (conflicts-with-running (obj <service>))
  (let ((conflicts '()))
    (for-each-service
     (lambda (serv)
       (and (running? serv)
	    (for-each (lambda (sym)
			(and (memq sym (provided-by obj))
			     (set! conflicts
				   (cons (canonical-name serv)
					 conflicts))))
		      (provided-by serv)))))
    conflicts))

;; Start OBJ, but first kill all services which conflict with it.
;; FIXME-CRITICAL: Conflicts of indirect dependencies.  For this, we
;; seem to need a similar solution like launch-service.
(define-method (enforce (obj <service>) . args)
  (for-each stop (conflicts-with-running obj))
  (apply start obj args))

;; Display information about the service.
(define-method (default-display-status (obj <service>))
  (local-output "Status of ~a:"
		(canonical-name obj))
  (if (running? obj)
      (local-output "  It is started.")
      (local-output "  It is stopped."))
  (if (enabled? obj)
      (local-output "  It is enabled.")
      (local-output "  It is disabled."))
  (local-output "  Provides ~a." (provided-by obj))
  (local-output "  Requires ~a." (required-by obj))
  (local-output "  Conflicts with ~a." (conflicts-with obj))
  (if (respawn? obj)
      (local-output "  Will be respawned.")
      (local-output "  Will not be respawned.")))

;; Return whether OBJ requires something that is not yet running.
(define-method (depends-resolved? (obj <service>))
  (call/ec (lambda (return)
	     (for-each (lambda (dep)
			 (or (find-running (lookup-services dep))
			     (return #f)))
		       (required-by obj))
	     #t)))



;; Try to start service NAME with PROC.  Used by `start' and `enforce'.
(define (launch-service name proc args)
  (let* ((possibilities (lookup-services name))
	 (which (find-running possibilities)))
    (if (null? possibilities)
	(local-output "No service provides ~a." name)
	(or which
	    ;; None running yet, start one.
	    (set! which
		  (call/ec (lambda (return)
			     (for-each (lambda (service)
					 (and (apply proc service args)
					      (return service)))
				       possibilities)
			     #f)))))
    (or which
	(let ((unknown (find-running (lookup-services 'unknown))))
	  (if (and unknown
		   (defines-extra-action? unknown 'start))
	      (apply extra-action unknown 'start name args)
	      (local-output "Providing ~a impossible." name))))
    (and which #t)))

;; Starting via symbol.
(define-method (start (obj <symbol>) . args)
  (launch-service obj start args))

;; Enforcing via symbol.
(define-method (enforce (obj <symbol>) . args)
  (launch-service obj enforce args))

;; Stopping via symbol.
(define-method (stop (obj <symbol>) . args)
  (let ((which (find-running (lookup-services obj))))
    (if (not which)
	(let ((unknown (find-running (lookup-services 'unknown))))
	  (if (and unknown
		   (defines-extra-action? unknown 'stop))
	      (apply extra-action (car unknown) 'stop obj args)
	      (local-output "No service currently providing ~a." obj)))
	(apply stop which args))))

;; Perform extra-action ACTION via symbol.
(define-method (extra-action (obj <symbol>) action . args)
  (let ((which
	 ;; This is a list because we might apply the action on
	 ;; multiple services.
	 (list (find-running (lookup-services obj)))))
    (and (not (car which))
	 ;; None running, thus apply on all which provide it.
	 (set! which (lookup-services obj)))
    (if (null? which)
	(let ((unknown (find-running (lookup-services 'unknown))))
	  (if (and unknown
		   (defines-extra-action? unknown 'extra-action))
	      (apply extra-action (car unknown) 'extra-action action args)
	      (local-output "No service at all providing ~a." obj)))
	(for-each (case action
		    ((enable) enable)
		    ((disable) disable)
		    (else (lambda (serv)
			    (apply extra-action serv action args))))
		  which))))

;; Display the names of all extra-actions of services providing OBJ.
(define-method (display-extra-actions (obj <symbol>))
  (if (null? (lookup-services obj))
      (local-output "No service at all providing ~a." obj)
      (for-each (lambda (serv)
		  (local-output "~a ~a"
				(canonical-name serv)
				(extra-action-list serv)))
		(lookup-services obj))))



;;; Convenience facillities.

;; Check if any of SERVICES is running.  If this is the case, return
;; it.  If none, return `#f'.  Only the first one found will be
;; returned; this is because this is mainly intended to be applied on
;; the return value of `lookup-services'.
(define (find-running services)
  (call/ec (lambda (return)
	     (for-each (lambda (serv)
			 (and (running? serv)
			      (return serv)))
		       services)
	     #f)))

;; Produce a constructor that execs PROGRAM with ARGS in a child
;; process and returns its pid.
(define (make-childexec-constructor program . args)
  (lambda args
    (let ((pid (primitive-fork)))
      (if (zero? pid)
          (apply execlp program program args)
        pid))))

;; Produce a destructor that sends SIGNAL to the process with the pid
;; given as argument.
(define (make-kill-destructor signal)
  (lambda (pid . args)
    (kill pid signal)
    #f))

;; Produce a constructor that executes a command.
(define (make-system-constructor . command)
  (lambda args
    (zero? (status:exit-val (system (apply string-append command))))))

;; Produce a destructor that executes a command.
(define (make-system-destructor . command)
  (lambda (ignored . args)
    (not (zero? (status:exit-val (system (apply string-append command)))))))

;; Create service with constructor and destructor being set to typical
;; init.d scripts.
(define (make-init.d-service name . stuff)
  (let ((cmd (string-append "/etc/init.d/" name)))
    (apply make <service>
           #:provides (list (string->symbol name))
           #:start (make-system-constructor cmd " start")
           #:stop (make-system-destructor cmd " stop")
           stuff)))

;; Conveniently create a hash table containing the extra-actions of a
;; <service> object.
(define-syntax-rule (make-extra-actions (NAME PROC) ...)
  (let* ((actions-assoc (list (cons 'NAME PROC) ...))
	 (actions (make-hash-table (length actions-assoc))))
    (for-each (lambda (action-pair)
		(hashq-set! actions (car action-pair) (cdr action-pair)))
	      actions-assoc)
    actions))

;; A group of services which can be started and stopped together.  Not
;; comparable with a real runlevel at all, but can be used to emulate
;; a simple kind of runlevel.
(define-syntax-rule (make-service-group NAME (SYM ...) ADDITIONS ...)
  (make <service>
    #:provides '(NAME)
    #:requires '(SYM ...)
    #:stop (lambda (running)
	     (for-each stop '(SYM ...))
	     #f)
    ADDITIONS ...))



;;; Registered services.

;; Current size of the hash table below.  The table will be resized on
;; demand.
(define services-max-cnt 100)

;; Number of used entries in the table below.
(define services-cnt 0)

;; All registered services.
(define services (make-hash-table services-max-cnt))

;;; Perform actions with services:

;; Call PROC once for each registered service.
(define (for-each-service proc)
  (hash-fold (lambda (key value unused)
	       (and (eq? key (canonical-name (car value)))
		    (proc (car value))))
	     #f ;; Unused
	     services))

;; Lookup the services that provide NAME.  Returns a (possibly empty)
;; list of those.
(define (lookup-services name)
  (hashq-ref services name '()))

;; SIGCHLD handler.
(define (respawn-service signum)
  (define (handler return)
    (let ((pid (car (waitpid WAIT_ANY))))
      (for-each-service
       (lambda (serv)
	 (and (respawn? serv)
	      (running? serv)
	      (enabled? serv)
	      (= pid (slot-ref serv 'running))
	      ;; We found it.
	      (begin
		(slot-set! serv 'running #f)
		(if (> (current-time)
		       (+ (cdr respawn-limit)
			  (car (slot-ref serv 'last-respawns))))
		    (if (not (slot-ref serv 'waiting-for-termination?))
			(begin
			  ;; Everything is okay, start it.
			  (local-output "Respawning ~a."
					(canonical-name serv))
			  (set-car! (slot-ref serv 'last-respawns)
				    (current-time))
			  (slot-set! serv 'last-respawns
				     (cdr (slot-ref serv 'last-respawns)))
			  (start serv))
			;; We have just been waiting for the
			;; termination.  The `running' slot has
			;; already been set to `#f' by `stop'.
			(begin
			  (local-output "Service ~a terminated."
					(canonical-name serv))
			  (slot-set! serv 'waiting-for-termination? #f)))
		    (begin
		      (local-output "Service ~a has been disabled."
				    (canonical-name serv))
		      (local-output "  (Respawning too fast.)")
		      (slot-set! serv 'enabled? #f)))
		(return #t)))))))

  (without-extra-output
   (without-system-error
    (call/ec handler))))

;; Install it as the handler.
(sigaction SIGCHLD respawn-service)

;; Add NEW-SERVICES to the list of known services.
(define (register-services . new-services)
  (define (register-single-service new)
    ;; Sanity-checks first.
    (assert (list-of-symbols? (provided-by new)))
    (assert (list-of-symbols? (required-by new)))
    (assert (boolean? (respawn? new)))
    ;; FIXME: Verify consistency: Check that there are no circular
    ;; dependencies, ensure that names are canonical(!), check for
    ;; bogus conflicts/dependencies, whatever else makes sense.

    ;; Insert into the hash table.
    (for-each (lambda (name)
		(let ((old (lookup-services name)))
		  ;; Counting the number of used entries.
		  (and (null? old)
		       (set! services-cnt (1+ services-cnt)))
		  (and (= services-cnt services-max-cnt)
		       (begin
			 ;; Double the size, so that we don't have to
			 ;; do all this too often.
			 (set! services-max-cnt (* 2 services-max-cnt))
			 (set! services
			       (copy-hashq-table services services-max-cnt))))
		  ;; Actually add the new service now.
		  (hashq-set! services name (cons new old))))
	      (provided-by new)))

  (for-each register-single-service new-services))

;;; Tests for validity of the slots of <service> objects.

;; Test if OBJ is a list that only contains symbols.
(define (list-of-symbols? obj)
  (cond ((null? obj) #t)
	((and (pair? obj)
	      (symbol? (car obj)))
	 (list-of-symbols? (cdr obj)))
	(else #f)))

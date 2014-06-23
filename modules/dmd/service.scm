;; service.scm -- Representation of services.
;; Copyright (C) 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Järling <wolfgang@pro-linux.de>
;; Copyright (C) 2014 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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

(define-module (dmd service)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (dmd support)
  #:use-module (dmd comm)
  #:use-module (dmd config)
  #:use-module (dmd system)
  #:replace (system
             system*)
  #:export (<service>
            canonical-name
            running?
            action-list
            lookup-action
            defines-action?
            enable
            disable
            start
            stop
            action
            enforce
            doc
            conflicts-with
            conflicts-with-running
            dmd-status
            depends-resolved?
            launch-service
            first-running
            lookup-running
            lookup-running-or-providing
            make-service-group
            for-each-service
            lookup-services
            respawn-service
            register-services
            required-by
            handle-unknown
            make-forkexec-constructor
            make-kill-destructor
            exec-command
            fork+exec-command
            make-system-constructor
            make-system-destructor
            make-init.d-service

            dmd-service))

;; Conveniently create an actions object containing the actions for a
;; <service> object.  The current structure is a list of actions,
;; where every action has the format ``(name . (proc . doc))''.
(define-syntax make-actions
  (syntax-rules ()
    ((_ (name docstring proc) rest ...)
     (cons (cons 'name (cons proc docstring))
           (make-actions rest ...)))
    ((_ (name proc) rest ...)
     (cons (cons 'name (cons proc "[No documentation.]"))
           (make-actions rest ...)))
    ((_)
     '())))

;; Respawning CAR times in CDR seconds will disable the service.
(define respawn-limit (cons 5 5))

(define-class <service> ()
  ;; List of provided service-symbols.  The first one is also called
  ;; the `canonical name' and must be unique to this service.
  (provides #:init-keyword #:provides
	    #:getter provided-by)
  ;; List of required service-symbols.
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
	 #:init-value (lambda () #t))
  ;; The action to perform to stop the service.  This must be a
  ;; procedure and may take an arbitrary amount of arguments, but must
  ;; be callable with exactly one argument, which will be the value of
  ;; the `running' slot.  Whatever the procedure returns will be
  ;; ignored.
  (stop #:init-keyword #:stop
	#:init-value (lambda (running) #f))
  ;; Additional actions that can be performed with the service.  This
  ;; currently is a list with each element (and thus each action)
  ;; being ``(name . (proc . docstring))'', but users should not rely
  ;; on this.
  (actions #:init-keyword #:actions
	   #:init-form (make-actions))
  ;; If this is `#f', it means that the service is not running
  ;; currently.  Otherwise, it is the value that was returned by the
  ;; procedure in the `start' slot when the service was started.
  (running #:init-value #f)
  ;; A description of the service.
  (docstring #:init-keyword #:docstring
	     #:init-value "[No description].")
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

(define action:name car)
(define action:proc cadr)
(define action:doc cddr)

;; Return the canonical name of the service.
(define-method (canonical-name (obj <service>))
  (car (provided-by obj)))

;; Return whether the service is currently running.
(define-method (running? (obj <service>))
  (and (slot-ref obj 'running) #t))

;; Return a list of all actions implemented by OBJ. 
(define-method (action-list (obj <service>))
  (map action:name (slot-ref obj 'actions)))

;; Return the action ACTION.
(define-method (lookup-action (obj <service>) action)
  (assq action (slot-ref obj 'actions)))

;; Return whether OBJ implements the action ACTION.
(define-method (defines-action? (obj <service>) action)
  (and (lookup-action obj action) #t))

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
		       (canonical-name obj)))
	((not (enabled? obj))
	 (local-output "Service ~a is currently disabled."
		       (canonical-name obj)))
	((let ((conflicts (conflicts-with-running obj)))
	   (or (null? conflicts)
	       (local-output "Service ~a conflicts with running services ~a."
			     (canonical-name obj) conflicts))
	   (not (null? conflicts)))
	 #f) ;; Dummy.
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
               (call-with-blocked-asyncs
                (lambda ()
                  ;; Start the service itself.  Asyncs are blocked so that if
                  ;; the newly-started process dies immediately, the SIGCHLD
                  ;; handler is invoked later, once we have set the 'running'
                  ;; field.
                  (slot-set! obj 'running (catch #t
                                            (lambda ()
                                              (apply (slot-ref obj 'start)
                                                     args))
                                            (lambda (key . args)
                                              (caught-error key args)
                                              #f))))))

	   ;; Status message.
	   (local-output (if (running? obj)
			     (l10n "Service ~a has been started.")
			   (l10n "Service ~a could not be started."))
			 (canonical-name obj)))))
  (slot-ref obj 'running))

;; Stop the service, including services that depend on it.  If the
;; latter fails, continue anyway.  Return `#f' if it could be stopped.
(define-method (stop (obj <service>) . args)
  ;; Block asyncs so the SIGCHLD handler doesn't execute concurrently.
  ;; Notably, that makes sure the handler process the SIGCHLD for OBJ's
  ;; process once we're done; otherwise, it could end up respawning OBJ.
  (call-with-blocked-asyncs
   (lambda ()
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

               ;; Stop the service itself.
               (catch #t
                 (lambda ()
                   (apply (slot-ref obj 'stop)
                          (slot-ref obj 'running)
                          args))
                 (lambda (key . args)
                   ;; Special case: `dmd' may quit.
                   (and (eq? dmd-service obj)
                        (eq? key 'quit)
                        (apply quit args))
                   (caught-error key args)))

               ;; OBJ is no longer running.
               (slot-set! obj 'running #f)

               ;; Status message.
               (let ((name (canonical-name obj)))
                 (if (running? obj)
                     (local-output "Service ~a could not be stopped." name)
                     (local-output "Service ~a has been stopped." name))))))
     (slot-ref obj 'running))))

;; Call action THE-ACTION with ARGS.
(define-method (action (obj <service>) the-action . args)
  (define (default-action running . args)
    ;; All actions which are handled here might be called even if the
    ;; service is not running, so they have to take this into account.
    (case the-action
      ;; Restarting is done in the obvious way.
      ((restart)
       (if running
	   (stop obj)
	 (local-output "~a was not running." (canonical-name obj)))
       (start obj))
      ((status)
       (dmd-status obj))
      (else
       ;; FIXME: Unknown service.
       (local-output "Service ~a does not have a ~a action."
		     (canonical-name obj)
		     the-action))))

  (define (apply-if-pair obj proc)
    (if (pair? obj)
	(proc obj)
      obj))

  (let ((proc (or (apply-if-pair (lookup-action obj the-action)
				 action:proc)
		  default-action)))
    ;; Calling default-action will be allowed even when the service is
    ;; not running, as it provides generally useful functionality and
    ;; information.
    ;; FIXME: Why should the user-implementations not be allowed to be
    ;; called this way?
    (if (and (not (eq? proc default-action))
	     (not (running? obj)))
	(local-output "Service ~a is not running." (canonical-name obj))
      (catch #t
	(lambda ()
	  (if (can-apply? proc (+ 1 (length args)))
	      (apply proc (slot-ref obj 'running) args)
	    ;; FIXME: Better message.
	    (local-output "Action ~a of service ~a can't take ~a arguments."
			  the-action (canonical-name obj) (length args))))
	(lambda (key . args)
	  ;; Special case: `dmd' may quit.
	  (and (eq? dmd-service obj)
	       (eq? key 'quit)
	       (apply quit args))
	  (caught-error key args))))))

;; Display documentation about the service.
(define-method (doc (obj <service>) . args)
  (if (null? args)
      ;; No further argument given -> Normal level of detail.
      (local-output (slot-ref obj 'docstring))
    (case (string->symbol (car args)) ;; Does not work with strings.
      ((full)
       ;; FIXME
       (local-output (slot-ref obj 'docstring)))
      ((short)
       ;; FIXME
       (local-output (slot-ref obj 'docstring)))
      ((action)
       ;; Display documentation of given actions.
       (for-each
	(lambda (the-action)
	  (local-output "~a: ~a"
			the-action
			(let ((action-object
			       (lookup-action obj
					      (string->symbol the-action))))
			  (if action-object
			      (action:doc action-object)
                              (gettext "This action does not exist.")))))
        (cdr args)))
      ((list-actions)
       (local-output "~a ~a"
		     (canonical-name obj)
		     (action-list obj)))
      (else
       ;; FIXME: Implement doc-help.
       (local-output "Unknown keyword.  Try `doc dmd help'.")))))

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
    (delete! (canonical-name obj)
	     (delete-duplicates! conflicts eq?)
	     eq?)))

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
;; FIXME: This should rather be removed and added cleanly later.
(define-method (enforce (obj <service>) . args)
  (for-each stop (conflicts-with-running obj))
  (apply start obj args))

;; Display information about the service.
(define-method (dmd-status (obj <service>))
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
			 (or (lookup-running dep)
			     (return #f)))
		       (required-by obj))
	     #t)))



;; Try to start (with PROC) a service providing NAME.  Used by `start'
;; and `enforce'.
(define (launch-service name proc args)
  (let* ((possibilities (lookup-services name))
	 (which (first-running possibilities)))
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
	(let ((unknown (lookup-running 'unknown)))
	  (if (and unknown
		   (defines-action? unknown 'start))
	      (apply action unknown 'start name args)
	    (local-output "Providing ~a impossible." name))))
    (and which #t)))

;; Starting by name.
(define-method (start (obj <symbol>) . args)
  (launch-service obj start args))

;; Enforcing by name.  FIXME: Should be removed and added cleanly later.
(define-method (enforce (obj <symbol>) . args)
  (launch-service obj enforce args))

;; Stopping by name.
(define-method (stop (obj <symbol>) . args)
  (let ((which (lookup-running obj)))
    (if (not which)
	(let ((unknown (lookup-running 'unknown)))
	  (if (and unknown
		   (defines-action? unknown 'stop))
	      (apply action unknown 'stop obj args)
	    (local-output "No service currently providing ~a." obj)))
      (apply stop which args))))

;; Perform action THE-ACTION by name.
(define-method (action (obj <symbol>) the-action . args)
  (let ((which-services (lookup-running-or-providing obj)))
    (if (null? which-services)
	(let ((unknown (lookup-running 'unknown)))
	  (if (and unknown
		   (defines-action? unknown 'action))
	      (apply action unknown 'action the-action args)
	    (local-output "No service at all providing ~a." obj)))
      (for-each (lambda (s)
		  (apply (case the-action
			   ((enable) enable)
			   ((disable) disable)
			   ((doc) doc)
			   (else
			    (lambda (s . further-args)
			      (apply action s the-action further-args))))
			 s
			 args))
		which-services))))

;; EINTR-safe versions of 'system' and 'system*'.

(define system*
  (EINTR-safe (@ (guile) system*)))

(define system
  (EINTR-safe (@ (guile) system)))



;; Handling of unprovided service-symbols.  This can be called in
;; either of the following ways (i.e. with either three or four
;; arguments):
;;   handle-unknown SERVICE-SYMBOL [ 'start | 'stop ] ARGS
;;   handle-unknown SERVICE-SYMBOL 'action THE_ACTION ARGS
(define (handle-unknown . args)
  (let ((unknown (lookup-running 'unknown)))
    ;; FIXME: Display message if no unknown service.
    (if unknown
	(apply-to-args args
	    (case-lambda
	     ;; Start or stop.
	     ((service-symbol start/stop args)
	      (if (defines-action? unknown start/stop)
		  (apply action unknown start/stop service-symbol args)
		;; FIXME: Bad message.
		(local-output "Cannot ~a ~a." start/stop service-symbol)))
	     ;; Action.
	     ((service-symbol action-sym the-action args)
	      (assert (eq? action-sym 'action))
	      (if (defines-action? unknown 'action)
		  (apply action unknown 'action service-symbol the-action args)
		(local-output "No service provides ~a." service-symbol))))))))

;; Check if any of SERVICES is running.  If this is the case, return
;; it.  If none, return `#f'.  Only the first one found will be
;; returned; this is because this is mainly intended to be applied on
;; the return value of `lookup-services', where no more than one will
;; ever run at the same time.
(define (first-running services)
  (find running? services))

;; Return the running service that provides NAME, or false if none.
(define (lookup-running name)
  (first-running (lookup-services name)))

;; Lookup the running service providing SYM, and return it as a
;; one-element list.  If none is running, return a list of all
;; services which provide SYM.
(define (lookup-running-or-providing sym)
  (define (list-unless-false x)
    (if x (list x) x))

  (or (list-unless-false (lookup-running sym))
      (lookup-services sym)))


;;;
;;; Starting/stopping services.
;;;

(define (default-service-directory)
  "Return the default current directory from which a service is started."
  (if (zero? (getuid))
      "/"
      (or (getenv "HOME")
          (and=> (catch-system-error (getpw (getuid)))
                 passwd:dir)
          (getcwd))))

(define (default-environment-variables)
  "Return the list of environment variable name/value pairs that should be
set when starting a service."
  (environ))

(define* (exec-command command
                       #:key
                       (directory (default-service-directory))
                       (environment-variables (default-environment-variables)))
  "Run COMMAND as the current process from DIRECTORY, and with
ENVIRONMENT-VARIABLES (a list of strings like \"PATH=/bin\".)  File
descriptors 1 and 2 are kept as is, whereas file descriptor 0 (standard
input) points to /dev/null; all other file descriptors are closed prior to
yielding control to COMMAND."
  (match command
    ((program args ...)
     ;; Become the leader of a new session and session group.
     ;; Programs such as 'mingetty' expect this.
     (setsid)

     (chdir directory)
     (environ environment-variables)

     ;; Close all the file descriptors except stdout and stderr.
     (let ((max-fd (max-file-descriptors)))
       (catch-system-error (close-fdes 0))

       ;; Make sure file descriptor zero is used, so we don't end up reusing
       ;; it for something unrelated, which can confuse some packages.
       (dup2 (open-fdes "/dev/null" O_RDONLY) 0)

       (let loop ((i 3))
         (when (< i max-fd)
           (catch-system-error (close-fdes i))
           (loop (+ i 1)))))

     (catch 'system-error
       (lambda ()
         (apply execlp program program args))
       (lambda args
         (format (current-error-port)
                 "exec of ~s failed: ~a~%"
                 program (strerror (system-error-errno args)))
         (primitive-exit 1))))))

(define* (fork+exec-command command
                            #:key
                            (directory (default-service-directory))
                            (environment-variables
                             (default-environment-variables)))
  "Spawn a process that executed COMMAND as per 'exec-command', and return
its PID."
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (exec-command command
                      #:directory directory
                      #:environment-variables environment-variables)
        pid)))

(define make-forkexec-constructor
  (let ((warn-deprecated-form
         ;; Until 0.1, this procedure took a rest list.
         (lambda ()
           (issue-deprecation-warning
            "This 'make-forkexec-constructor' form is deprecated; use
 (make-forkexec-constructor '(\"PROGRAM\" \"ARGS\"...)."))))
    (case-lambda*
     "Produce a constructor that execs COMMAND, a program name/argument list,
in a child process and returns its PID.  COMMAND is started with DIRECTORY as
its current directory, and ENVIRONMENT-VARIABLES as its environment
variables."
     ((command #:key
               (directory (default-service-directory))
               (environment-variables (default-environment-variables)))
      (let ((command (if (string? command)
                         (begin
                           (warn-deprecated-form)
                           (list command))
                         command)))
        (lambda args
          (fork+exec-command command
                             #:directory directory
                             #:environment-variables environment-variables))))
     ((program . program-args)
      ;; The old form, documented until 0.1 included.
      (warn-deprecated-form)
      (make-forkexec-constructor (cons program program-args))))))

;; Produce a destructor that sends SIGNAL to the process with the pid
;; given as argument, where SIGNAL defaults to `SIGTERM'.
(define make-kill-destructor
  (lambda* (#:optional (signal SIGTERM))
    (lambda (pid . args)
      (kill pid signal)
      #f)))

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

;; A group of service-names which can be provided (i.e. services
;; providing them get started) and unprovided (same for stopping)
;; together.  Not comparable with a real runlevel at all, but can be
;; used to emulate a simple kind of runlevel.
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

(define (find-service pred)
  "Return the first service that matches PRED, or #f if none was found."
  (call/ec
   (lambda (return)
     (hash-fold (lambda (name services _)
                  (and=> (find pred services)
                         return))
                #f
                services)
     #f)))

;; Lookup the services that provide NAME.  Returns a (possibly empty)
;; list of those.
(define (lookup-services name)
  (hashq-ref services name '()))

(define waitpid*
  (let ((waitpid (EINTR-safe waitpid)))
    (lambda (what flags)
      "Like 'waitpid', but EINTR-safe, and return (0 . _) when there's no
child left."
      (catch 'system-error
        (lambda ()
          (waitpid what flags))
        (lambda args
          ;; Did we get ECHILD or something?  If we did, that's a problem,
          ;; because this procedure is supposed to be called only upon
          ;; SIGCHLD.
          (let ((errno (system-error-errno args)))
            (local-output "error: 'waitpid' unexpectedly failed with: ~s"
                          (strerror errno))
            '(0 . #f)))))))

(define (respawn-service signum)
  "Handle SIGCHLD, possibly by respawning the service that just died, or
otherwise by updating its state."
  (let loop ()
    (match (waitpid* WAIT_ANY WNOHANG)
      ((0 . _)
       ;; Nothing left to wait for.
       #t)
      ((pid . _)
       (let ((serv (find-service (lambda (serv)
                                   (and (enabled? serv)
                                        (match (slot-ref serv 'running)
                                          ((? number? pid*)
                                           (= pid pid*))
                                          (_ #f)))))))

         ;; SERV can be #f for instance when this code runs just after a
         ;; service's 'stop' method killed its process and completed.
         (when serv
           (slot-set! serv 'running #f)
           (if (and (respawn? serv)
                    (> (current-time)
                       (+ (cdr respawn-limit)
                          (car (slot-ref serv 'last-respawns)))))
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
                   ;; termination.  The `running' slot has already
                   ;; been set to `#f' by `stop'.
                   (begin
                     (local-output "Service ~a terminated."
                                   (canonical-name serv))
                     (slot-set! serv 'waiting-for-termination? #f)))
               (begin
                 (local-output "Service ~a has been disabled."
                               (canonical-name serv))
                 (when (respawn? serv)
                   (local-output "  (Respawning too fast.)"))
                 (slot-set! serv 'enabled? #f))))

         ;; As noted in libc's manual (info "(libc) Process Completion"),
         ;; loop so we don't miss any terminated child process.
         (loop))))))

;; Install it as the handler.
(sigaction SIGCHLD respawn-service SA_NOCLDSTOP)

;; Add NEW-SERVICES to the list of known services.
(define (register-services . new-services)
  (define (register-single-service new)
    ;; Sanity-checks first.
    (assert (list-of-symbols? (provided-by new)))
    (assert (list-of-symbols? (required-by new)))
    (assert (boolean? (respawn? new)))
    ;; Canonical name actually must be canonical.  (FIXME: This test
    ;; is incomplete, since we may add a service later that makes it
    ;; non-cannonical.)
    (assert (null? (lookup-services (canonical-name new))))
    ;; FIXME: Verify consistency: Check that there are no circular
    ;; dependencies, check for bogus conflicts/dependencies, whatever
    ;; else makes sense.

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

(define (deregister-service service-name)
  "For each string in SERVICE-NAME, stop the associated service if
necessary and remove it from the services table.  If SERVICE-NAME is
the special string 'all', remove all services except for dmd.

This will remove a service either if it is identified by its canonical
name, or if it is the only service providing the service that is
requested to be removed."
  (define (deregister service)
    (if (running? service)
        (stop service))
    ;; Remove services provided by service from the hash table.
    (for-each
     (lambda (name)
       (let ((old (lookup-services name)))
         (if (= 1 (length old))
             ;; Only service provides this service, ergo:
             (begin
               ;; Reduce provided services count
               (set! services-cnt (1- services-cnt))
               ;; Remove service entry from services.
               (hashq-remove! services name))
             ;; ELSE: remove service from providing services.
             (hashq-set! services name
                         (remove
                          (lambda (lk-service)
                            (eq? (canonical-name service)
                                 (canonical-name lk-service)))
                          old)))))
     (provided-by service)))
  (define (service-pairs)
    "Return '(name . service) of all user-registered services."
    (filter identity
            (hash-map->list
             (lambda (key value)
               (match value
                 ((service)     ; only one service associated with KEY
                  (and (eq? key (canonical-name service))
                       (not (eq? key 'dmd))
                       (cons key service)))
                 (_ #f)))               ; all other cases: #f.
             services)))

  (let ((name (string->symbol service-name)))
    (cond ((eq? name 'all)
           ;; Special 'remove all' case.
           (let ((pairs (service-pairs)))
             (local-output "Unloading all optional services: '~a'..."
                           (map car pairs))
             (for-each deregister (map cdr pairs))
             (local-output "Done.")))
          (else
           ;; Removing only one service.
           (match (lookup-services name)
             (()                        ; unknown service
              (local-output
               "Not unloading: '~a' is an uknown service." name))
             ((service)             ; only SERVICE provides NAME
              ;; Are we removing a user service窶ｦ
              (if (eq? (canonical-name service) name)
                  (local-output "Removing service '~a'..." name)
                  ;; or a virtual service?
                  (local-output
                   "Removing service '~a' providing '~a'..."
                   (canonical-name service) name))
              (deregister service)
              (local-output "Done."))
             ((services ...)            ; ambiguous NAME
              (local-output
               "Not unloading: '~a' names several services: '~a'."
               name (map canonical-name services))))))))

(define (load-config file-name)
  (local-output "Loading ~a." file-name)
  ;; Every action is protected anyway, so no need for a `catch'
  ;; here.  FIXME: What about `quit'?
  (catch 'system-error
    (lambda ()
      (load-in-user-module file-name))
    (lambda args
      (local-output "Failed to load from '~a': ~a."
                    file-name (strerror (system-error-errno args)))
      #f)))

;;; Tests for validity of the slots of <service> objects.

;; Test if OBJ is a list that only contains symbols.
(define (list-of-symbols? obj)
  (cond ((null? obj) #t)
	((and (pair? obj)
	      (symbol? (car obj)))
	 (list-of-symbols? (cdr obj)))
	(else #f)))



;; The `dmd' service.

(define (shutdown-services)
  "Shut down all the currently running services; update the persistent state
file when persistence is enabled."
  (let ((running-services '()))
    (for-each-service
     (lambda (service)
       (when (running? service)
         (stop service)
         (when persistency
           (set! running-services
                 (cons (canonical-name service)
                       running-services))))))

    (when persistency
      (call-with-output-file persistency-state-file
        (lambda (p)
          (format p "~{~a ~}~%" running-services))))))

(define dmd-service
  (make <service>
    #:docstring "The dmd service is used to operate on dmd itself."
    #:provides '(dmd)
    #:requires '()
    #:respawn #f
    #:start (lambda args
	      (when (isatty? (current-output-port))
                (display-version))
	      #t)
    #:stop (lambda (unused . args)
	     (local-output "Exiting dmd...")
	     ;; Prevent that we try to stop ourself again.
	     (slot-set! dmd-service 'running #f)
             (shutdown-services)
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
     ;; Halt.
     (halt
      "Halt the system."
      (lambda (running)
        (catch 'quit
          (cut stop dmd-service)
          (lambda (key)
            (local-output "Halting...")
            (halt)))))
     ;; Power off.
     (power-off
      "Halt the system and turn it off."
      (lambda (running)
        (catch 'quit
          (cut stop dmd-service)
          (lambda (key)
            (local-output "Shutting down...")
            (power-off)))))
     ;; Load a configuration file.
     (load
      "Load the Scheme code from FILE into dmd.  This is potentially
dangerous.  You have been warned."
      (lambda (running file-name)
        (load-config file-name)))
     ;; Unload a service
     (unload
      "Unload the service identified by SERVICE-NAME or all services
except for dmd if SERVICE-NAME is 'all'.  Stop services before
removing them if needed."
      (lambda (running service-name)
        (deregister-service service-name)))
     (reload
      "Unload all services, then load from FILE-NAME into dmd.  This
is potentialy dangerous.  You have been warned."
      (lambda (running file-name)
        (and (deregister-service "all") ; unload all services
             (load-config file-name)))) ; reload from FILE-NAME
     ;; Go into the background.
     (daemonize
      "Go into the background.  Be careful, this means that a new
process will be created, so dmd will not get SIGCHLD signals anymore
if previously spawned childs terminate.  Therefore, this action should
usually only be used (if at all) *before* childs get spawned for which
we want to receive these signals."
      (lambda (running)
	(if (zero? (primitive-fork))
	    #t
            (primitive-exit 0))))
     (persistency
      "Safe the current state of running and non-running services.
This status gets written into a file on termination, so that we can
restore the status on next startup.  Optionally, you can pass a file
name as argument that will be used to store the status."
      (lambda* (running #:optional (file #f))
               (set! persistency #t)
               (when file
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

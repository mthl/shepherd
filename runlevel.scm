;; runlevel.scm -- Different kinds of runlevels.
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

;; How runlevels (should) work: A runlevel has a enter-selector and a
;; leave-selector, which are called when the runlevels are entered or
;; left.  They are called with the list of currently running services
;; as argument, and return a list of services they want to have
;; running.  How exactly they implement this is up to them, so this is
;; a very generic interface.  The difficult part is to get from the
;; current into the desired state.  I have an idea how to implement it
;; though.  See the Texinfo manual for details.

;; The actual code below is not finished.

;; An abstract runlevel.
(define-class <runlevel> ()
  (name #:init-keyword #:name))

;; Enter the runlevel (and leave the previous).
(define-method (enter (obj <runlevel>))
  ;; Return currently running services.
  (define (compute-current-services)
    (let ((services '()))
      (for-each-service
       (lambda (serv)
	 (and (running? serv)
	      (set! services (cons (canonical-name serv)
				   services)))))
      services))

  ;; Return the canonical names of SERVICES, but with dependencies
  ;; added.
  (define (canonical-with-dependencies services)
    ;; FIXME: Hmmm... conflicting services..?  If A and B both provide
    ;; X and Y, and C depends on X, D depends on Y, we might fail very
    ;; ungracefully.
    (let ((result '()))
      ;; Return the canonical names of SERVICES.
      (define (canon services)
	(map (lambda (s)
	       ;; FIXME: Here, we should avoid adding conflicts.
	       (canonical-name (car (lookup-services s))))
	     services))

      (letrec ((add
		(lambda (name)
		  (set! result (cons name result))
		  (for-each add
			    (canon (required-by
				    (car (loopkup-services name))))))))
	(for-each add (canon services)))
      (delete-duplicates! result)))

  (let ((current-services (compute-current-services))
	(destination-services
	 ;; Find out what to run.
	 (enter-selector obj
			 (leave-selector current-runlevel
					 current-services))))
    (set! current-runlevel obj)
    ;; We don't have the guarantee that `next-services' consists of
    ;; only canonical names, thus it is a bit harder.  We first have
    ;; to create a possible ending point and then try to reach it.  If
    ;; we fail, we restart with the current situation, marking the
    ;; failed service as currently unstartable.  We repeat this until
    ;; we loose completely.
    (let ((unstartables '())
	  (ending-point '()))
      (for-each (lambda (sym)
		  ;; We try to be clever: When there is a service that
		  ;; provides this symbol already running, we use that
		  ;; one.
		  (let ((possibilities))
		    #f)) ;; FIXME
		next-services)
      #f))) ;; FIXME

;; Subclasses must define their own implementation.  This method is
;; called when the runlevel is entered.  SERVICES is the list of the
;; currently running services.  It returns the list of services that
;; should be run.  Dependencies will be resolved later automatically.
(define-method (enter-selector (obj <runlevel>)
			       services)
  (assert #f))

;; Redefining this in subclasses is optional.  It works like `enter',
;; except for that it is called when the runlevel is left.
(define-method (leave-selector (obj <runlevel>)
			       services)
  services) ;; Default is no change.



;; A runlevel where you explicitly specify the services to run.
(define-class <runlevel-exact> (<runlevel>)
  (services #:init-value '()
	    #:init-keyword #:services))

;; FIXME: Implement
(define-method (enter-selector (obj <runlevel-exact>)
			       services)
  #t)



;; A runlevel where you only specify the changes, i.e. which services
;; to start (if not running) and which to stop (if running) on enter
;; and leave.
(define-class <runlevel-changes> (<runlevel>)
  (start-on-enter #:init-value '()
		  #:init-keyword #:start-on-enter)
  (stop-on-enter #:init-value '()
		 #:init-keyword #:stop-on-enter)
  (start-on-leave #:init-value '()
		  #:init-keyword #:start-on-leave)
  (stop-on-leave #:init-value '()
		 #:init-keyword #:stop-on-leave))

;; FIXME: Implement
(define-method (enter-selector (obj <runlevel-changes>)
			       services)
  #t)

;; FIXME: Implement
(define-method (leave-selector (obj <runlevel-changes>)
			       services)
  #t)



;; List of runlevels.
(define runlevels '())

;; The currently active runlevel.
(define current-runlevel (make <runlevel> #:name 'dummy))

(define (register-runlevels . new-runlevels)
  (set! runlevels (append! runlevels new-runlevels)))

;; Start all of SERVICES, which is a list of canonical names (FIXME?),
;; but in a order where all dependencies are fulfilled before we
;; attempt to start a service.  It is assumed that this is possible
;; (FIXME?).
(define (start-in-order services)
  ;; Same thing with an association list where the cdr of each pair is
  ;; `#f' if the service is not yet started.
  (define (start-in-order-assoc assoc-services)
    (while (any not (map cdr assoc-services))
	   (for-each (lambda (x)
		       (let ((service (car (lookup-services (car x)))))
			 (and (not (cdr x))
			      (depends-resolved? service)
			      (set-cdr! x (start service)))))
		     assoc-services)))

  (start-in-order-assoc (map (lambda (x)
			       (cons x #f))
			     services)))


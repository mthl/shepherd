;; _unknown.scm -- An example for an `unknown' service.
;; Copyright (C) 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
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

;; Return true if STR1 lacks a character that exists in STR2, but
;; otherwise both are identical.
(define (lacks-char-from? str1 str2)
  (and (= (string-length str1)
	  (+ (string-length str2) 1))
       (letrec ((next
		 (lambda (pos)
		   (and (not (= pos (string-length str1)))
			(or (string=? str2
				      (string-append
				       (substring str1 0 pos)
				       (substring str1
						  (+ pos 1)
						  (string-length str1))))
			    (next (+ pos 1)))))))
	 (next 0))))

;; Return true if either of STR1 and STR2 lacks a character found in
;; the other one, but otherwise both are identical (e.g. as is the
;; case for "blah" and "bla").
(define (differs-by-missing-char? str1 str2)
  (or (lacks-char-from? str1 str2)
      (lacks-char-from? str2 str1)))

;; Return true if the only difference between STR1 and STR2 is that a
;; successive pair of characters is switched in one of them.
(define (differs-by-switched-chars? str1 str2)
  (and (= (string-length str1)
	  (string-length str2))
       (> (string-length str1) 1)
       (letrec ((next
		 (lambda (pos)
		   (and (not (= pos (string-length str1)))
			(or (string=? str2
				      (string-append
				       (substring str1 0 (- pos 1))
				       (string (string-ref str1 pos)
					       (string-ref str1 (- pos 1)))
				       (substring str1
						  (+ pos 1)
						  (string-length str1))))
			    (next (+ pos 1)))))))
	 (next 1))))

;; Return true if they differ by exactly one character (e.g. as is the
;; case for "blah" and "bleh"), if it isn't the only one.
(define (differs-by-one-char? str1 str2)  
  (and (= (string-length str1)
	  (string-length str2))
       (> (string-length str1) 1)
       (letrec ((next
		 (lambda (pos found-difference)
		   (if (= pos (string-length str1))
		       found-difference
		       (if (char=? (string-ref str1 pos)
				   (string-ref str2 pos))
			   (next (+ pos 1) found-difference)
			   (and (not found-difference)
				(next (+ pos 1) #t)))))))
	 (next 0 #f))))

;; Return true if STR1 and STR2 are identical, except for case
;; (e.g. this gives true for "foobar" and "FooBAR").
(define (differs-only-in-case? str1 str2)
  (and (not (string=? str1 str2))
       (string-ci=? str1 str2)))

;; Return true if STR1 and STR2 are `similar' strings, meaning that
;; they only differ in a minor way.
(define (similar? str1 str2)
  (any (lambda (pred?)
	 (pred? str1 str2))
       (list differs-by-missing-char?
	     differs-by-switched-chars?
	     differs-by-one-char?
	     differs-only-in-case?)))



;; TODO
;;  - We could look for non-running services first on `start' etc.
;;  - We also should do `unknown-action' (if service is known)
;;    - If doing this, we should enable the service to handle it
;;  - Make this the `default unknown service'
;;  - Messages if nothing found.

;; Suggest a service that satisfies PRED?, if given, and has a name
;; similar to SERVICE-SYMBOL.
(define look-for-service
  (case-lambda
   ((service-symbol) (look-for-service service-symbol (lambda (x) #t)))
   ((service-symbol pred?)
    (call/ec
     (lambda (return)
       (for-each-service
	(lambda (s)
	  (and (pred? s)
	       (similar? (symbol->string service-symbol)
			 (symbol->string (canonical-name s)))
	       (begin
		 (format #t "Did you mean ~a maybe?" (canonical-name s))
		 (newline)
		 (return #t)))))
       #f)))))

;; The classical compose.
(define (compose f g)
  (lambda (x)
    (f (g x)))

(define unknown-service
  (make <service>
    #:provides '(unknown)
    #:actions (make-actions
	       (start
		"Called if user wants to start an unknown service."
		(lambda (running service-sym . args)
		  (or (look-for-service service-sym (compose not running?))
		      (look-for-service service-sym))
		  running))
	       (stop
		"Called if user wants to stop an unknown service."
		(lambda (running service-sym . args)
		  (or (look-for-service service-sym running?)
		      (look-for-service service-sym))
		  running))
	       (action
		"Called if user frobs an unknown service."
		(lambda (running service-sym the-action . args)
		  (or (look-for-service service-sym running?)
		      (look-for-service service-sym))
		  running)))))

(register-services unknown-service)
(start unknown-service)

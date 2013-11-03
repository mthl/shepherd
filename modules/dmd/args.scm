;; args.scm -- Command line argument handling.
;; Copyright (C) 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (dmd args)
  #:use-module (oop goops)
  #:use-module (dmd support)
  #:use-module (dmd config)
  #:export (<option>
            process-args))

;; This does mostly the same as getopt-long, except for that it is
;; able to recognize abbreviations for long options, as long as they
;; are not ambigous.  Additionally, output is done in a way that makes
;; localization possible.

(define-class <option> ()
  ;; Short name for the option.  A character, or `#f' if no short name.
  (short #:init-keyword #:short
	 #:init-value #f
	 #:getter short)
  ;; Long name for the option.  A string, or `#f' if no long name.
  (long #:init-keyword #:long
	#:init-value #f
	#:getter long)
  ;; A string describing the option.
  (description #:init-keyword #:description
	       #:init-form (l10n "undocumented option"))
  ;; Specifies whether the procedure in the `action' slot takes an
  ;; argument.  If this is the case, it will be called with the
  ;; argument specified on the command line (If there was none
  ;; specified, the procedure will be called with `#f').
  (takes-arg? #:init-keyword #:takes-arg?
	      #:getter takes-arg?
	      #:init-value #f)
  ;; Name of the argument, if any.
  (arg-name #:init-keyword #:arg-name
	    #:getter arg-name
	    #:init-value "ARG")
  ;; Whether the arg is optional.
  (optional-arg? #:init-keyword #:optional-arg?
		 #:init-value #f)
  ;; The procedure that will be called when the option is found in the
  ;; argument list.
  (action #:init-keyword #:action
	  #:getter action))

(define-method (optional-arg? (obj <option>))
  (and (takes-arg? obj)
       (slot-ref obj 'optional-arg?)))

(define-method (long-option-string (obj <option>))
  (assert (long obj))
  (string-append "--"
		 (long obj)
		 (if (optional-arg? obj) "[" "")
		 (if (takes-arg? obj)
		     (string-append "=" (slot-ref obj 'arg-name))
		   "")
		 (if (optional-arg? obj) "]" "")))

(define-method (display-doc (obj <option>))
  (let ((col 0))
    (define (output text)
      (set! col (+ col (string-length text)))
      (display text))
    (define (fill-to target-col)
      (while (< col target-col)
	     (output (string #\space))))

    (fill-to 2)
    (and (short obj)
	 (output (string #\- (short obj) #\,)))
    (fill-to 6)
    (and (long obj)
	 (output (long-option-string obj)))
    (fill-to 30)
    (output (slot-ref obj 'description)))
  (newline))

;; Interpret command line arguments ARGS according to OPTIONS, passing
;; non-option arguments to DEFAULT.  Uses `program-name' and
;; `bug-address', which must be defined elsewhere.
(define (process-args program-name args args-syntax args-desc
                      default . options)
  ;; If this returns `#f', it means no option that can be abbreviated
  ;; as NAME (or has exactly this name) was found.  If the return
  ;; value is an option, it is exactly that or an abbreviation for it.
  ;; `#t' means that it is ambigous.
  (define (find-long-option name)
    (call/ec (lambda (return)
	       (let ((abbrev-for #f))
		 (for-each (lambda (option)
			     ;; Matches exactly.
			     (and (string=? name (long option))
				  (return option))
			     ;; Abbreviation.
			     (and (string-prefix? name (long option))
				  (if abbrev-for
				      (set! abbrev-for #t)
				    (set! abbrev-for option))))
			   options)
		 abbrev-for))))

  ;; Return the option, or `#f' if none found.
  (define (find-short-option char)
    (call/ec (lambda (return)
	       (for-each (lambda (option)
			   (and (equal? char (short option))
				(return option)))
			 options)
	       #f)))

  ;; Interpret ARG as non-option argument.
  (define (no-option arg)
    (or (default arg)
	((action (find-long-option "help")))))

  ;; Add a few standard options first.
  (set! options
	(cons* (make <option>
		 #:long "version"
		 #:description "display version information and exit"
		 #:action (lambda ()
			    (display-version program-name)
			    (quit)))
	       (make <option>
		 #:long "usage"
		 #:description "display short usage message and exit"
		 #:action (lambda ()
			    (display program-name)
			    (display " ")

			    ;; Short options first.
			    (let ((no-arg "[-") (with-arg ""))
			      (define (add-text opt)
				(and (short opt)
				     (if (takes-arg? opt)
					 (let ((opt-arg? (optional-arg? opt)))
					   (set! with-arg
						 (string-append
						  with-arg
						  " [-"
						  (string (short opt))
						  " "
						  (if opt-arg? "[" "")
						  (arg-name opt)
						  (if opt-arg? "]" "")
						  "]")))
				       (set! no-arg
					     (string-append
					      no-arg
					      (string (short opt)))))))
			      (for-each add-text options)
			      (set! no-arg (string-append no-arg "]"))
			      (display no-arg)
			      (display with-arg))

			    ;; Long options.
			    (for-each
			     (lambda (opt)
			       (and (long opt)
				    (display (string-append
					      " ["
					      (long-option-string opt)
					      "]"))))
			     (cdddr options)) ;; Skip the first three.

			    ;; Non-option arguments.
			    (display " [--] ")
			    (display args-syntax)
			    (newline)
			    (quit)))
	       (make <option>
		 #:long "help"
		 #:description "display this help and exit"
		 #:action (lambda ()
			    (for-each display
				      (list program-name
					    " [OPTIONS...] "
					    args-syntax))
			    (newline)
			    (display args-desc)
			    (newline)
			    (for-each display-doc (reverse! options))
			    (display (string-append "
Mandatory or optional arguments to long options are also mandatory or
optional to the corresponding short options.

Report bugs to: " bug-address ".
" package-name " general home page: <" package-url ">
General help using GNU software: <http://www.gnu.org/gethelp/>
"))
			    (quit)))
	       options))
  (let next-arg ((ptr args))
    (and (not (null? ptr))
	 (let ((arg (car ptr)))
	   ;; Call the procedure for OPTION (with the PARAM, if
	   ;; desired).
	   (define (apply-option option param)
	     (apply (action option)
		    (if (takes-arg? option)
			(cons param '())
		      '())))

	   (cond
	    ;; Long option.
	    ((string-prefix? "--" arg)
	     (if (string=? arg "--")
		 ;; Don't interpret further arguments as options.
		 (begin
		   (for-each no-option (cdr ptr))
		   (set! ptr (cons #f '())))
	       ;; Normal case.
	       (let* ((name (string-drop arg 2))
		      (index (string-index name #\=))
		      (param #f)
		      (target-option #f))
		 (and index
		      (begin
			;; It is of the form `--option=parameter'.
			(set! param (string-drop name (1+ index)))
			(set! name (string-take name index))))
		 ;; Find the right one (as it might be abbreviated).
		 (set! target-option (find-long-option name))
		 (and (boolean? target-option)
		      (begin
			(local-output
			 (if target-option
			     (l10n "Option `--~a' is ambigous.")
			   (l10n "Unknown option: `--~a'."))
			 name)
			(local-output "Try `--help'.")
			(quit 1)))
		 (and (takes-arg? target-option)
		      (not (optional-arg? target-option))
		      (not param)
		      (not (null? (cdr ptr)))
		      (or (not (string-prefix? "-" (cadr ptr)))
			  (string=? "-" (cadr ptr)))
		      (begin
			;; If we reach this point, it means that we
			;; need a parameter, we have none, there is
			;; another argument and it does not look like
			;; an option.  In this case, we obviously use
			;; that as parameter.
			(set! ptr (cdr ptr))
			(set! param (car ptr))))
		 (apply-option target-option param))))
	    ;; Short option.
	    ((string-prefix? "-" arg)
	     (let* ((opt-char (string-ref arg 1))
		    (target-option (find-short-option opt-char))
		    (param #f))
	       (if (not target-option)
		   (begin
		     (local-output "Unknown option: `-~a'." opt-char)
		     (quit 1))
		 (if (takes-arg? target-option)
		     (begin
		       (if (= (string-length arg) 2)
			   ;; Take next argument as param.
			   (if (or (null? (cdr ptr))
				   (and (string-prefix? "-" (cadr ptr))
					(not (string=? "-" (cadr ptr)))))
			       (or (optional-arg? target-option)
				   (begin
				     (local-output
				      "Argument required by `-~a'." opt-char)
				     (quit 1)))
			     (begin
			       (set! ptr (cdr ptr))
			       (set! param (car ptr))))
			 ;; Parameter is the rest of this argument.
			 (set! param (string-drop arg 2)))
		       (apply-option target-option param))
		   ;; Does not take a parameter, thus the
		   ;; following chars are also options without
		   ;; parameter.
		   (for-each (lambda (c)
			       (apply-option (find-short-option c) #f))
			     (string->list (string-drop arg 1)))))))
	    ;; Not interpreted as option.
	    (else
	     (no-option arg)))
	   (next-arg (cdr ptr))))))


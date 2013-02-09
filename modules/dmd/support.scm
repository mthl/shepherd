;; support.scm -- Various general support facilities, shared by deco and dmd.
;; Copyright (C) 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (dmd support)
  #:use-module (dmd config)
  #:export (begin-dmd
            call/ec
            caught-error
            assert
            label
            opt-lambda
            can-apply?
            copy-hashq-table

            catch-system-error
            l10n
            local-output
            display-version

            user-homedir
            default-logfile
            default-config-file
            default-socket-dir
            default-socket-file
            default-deco-socket-file
            default-persistency-state-file

            persistency
            persistency-state-file

            verify-dir))

;; For parts of the code specific to dmd.
(define-syntax begin-dmd
  (lambda (s)
    (syntax-case s ()
      ((_ expr ...)
       #'(begin expr ...)))))

;; Implement `call-with-escape-continuation' with `catch' and `throw'.
;; FIXME: Multiple return values.
(define (call/ec proc)
  (let* ((catch-sym (gensym))
	 (escape (lambda (value)
		   (throw catch-sym value))))
    (catch catch-sym
      (lambda ()
	(proc escape))
      (lambda (sym value)
	value))))

;; Report the caught error.
;; FIXME: Needs some more work.
(define (caught-error key args)
  (case key
    ((wrong-number-of-args)
     (apply (lambda (subr fmt fmt-args data)
	      (format #t "In ~a: " subr)
	      (apply format #t fmt fmt-args)
	      (newline))
	    args))
    (else
     (display key)
     (write args)
     (newline))))

;; Assert that expression EXPR does not evaluate to `#f'.
(define-syntax-rule (assert EXPR)
  (and (not EXPR)
       (begin
	 (local-output "Assertion ~a failed." 'EXPR)
	 (throw 'assertion-failed))))

;; Recursive procedures.
(define-syntax-rule (label NAME PROC)
  (lambda args
    (letrec ((NAME PROC))
      (apply NAME args))))

;; Lambda with optional arguments, call this like:
;; (opt-lambda (req1 req2)
;;             ((opt1 default1) (opt2 default2))
;;    body)
(define-macro (opt-lambda required optional . body)
  `(label opt-lambda-rec
     (lambda args
       (assert (>= (length args) (length ',required)))
       (if (< (length args) (+ (length ',required)
                               (length ',optional)))
           (apply opt-lambda-rec
                  (append args
                          (map cadr
                               (list-tail ',optional
                                          (- (length args)
                                             (length ',optional))))))
         (apply (lambda ,(append required
                                 (map car optional))
                  ,@body)
                args)))))


;; Check whether a list of NUM-ARGS arguments can successfully be
;; applied to PROC.
(define (can-apply? proc num-args)
  (apply-to-args (procedure-property proc 'arity)
		 (lambda (required optional takes-rest)
		   (and (>= num-args required)
			(or takes-rest
			    (<= num-args (+ required optional)))))))

;; Put the data from TABLE into a new hash-table of size SIZE.  Use
;; `eq?' when inserting.  This will be dropped as soon as stable Guile
;; supports resizable hash tables (it's in the Guile CVS already).
(define (copy-hashq-table table size)
  (hash-fold (lambda (key value new-table)
	       (hashq-set! new-table key value)
	       new-table)
	     (make-hash-table size)
	     table))

;; Evaluate `EXPR ...' until a system error occurs, then skip the
;; remaining code.
(define-syntax-rule (catch-system-error EXPR ...)
  (catch 'system-error
    (lambda ()
      EXPR ...)
    (lambda (key . args)
      #f)))



;; Localized version of STR.
(define l10n gettext)

;; Display some text and a newline.
(define-syntax-rule (local-output format-string args ...)
  (begin
    (format #f (gettext format-string) args ...)
    (newline)))

(define* (display-version #:optional (program-name "dmd"))
  (local-output "~a ~a -- ~a" program-name Version copyright))



;; Home directory of the user.
(define user-homedir (passwd:dir (getpwuid (getuid))))

;; Logfile.
(define default-logfile
  (if (zero? (getuid))
      (string-append Prefix-dir "/var/log/dmd.log")
    (string-append user-homedir "/.dmd.log")))

;; Configuration file.
(define default-config-file
  (if (zero? (getuid))
      (string-append Prefix-dir "/etc/dmdconf.scm")
    (string-append user-homedir "/.dmdconf.scm")))

;; The directory where the socket resides.
(define default-socket-dir
  (string-append Prefix-dir
		 "/var/run/dmd/"
		 (passwd:name (getpwuid (getuid)))))

;; Unix domain socket for receiving commands in dmd.
(define default-socket-file
  (string-append default-socket-dir "/default"))

;; Where deco will receive responses.
(define default-deco-socket-file
  (string-append default-socket-dir "/deco"))

;; Saving the state on exit.
(define default-persistency-state-file
  (if (zero? (getuid))
      (string-append Prefix-dir "/var/lib/misc/dmd-state")
    (string-append user-homedir "/.dmd-state")))

;; Global variables set from (dmd).
(define persistency #f)
(define persistency-state-file default-persistency-state-file)

;; Check if the directory DIR exists and create it if it is the
;; default directory, but does not exist.  If INSECURE is false, also
;; checks for the permissions of the directory.
(define (verify-dir dir insecure)
  (and (string=? dir default-socket-dir)
       ;; If it exists already, this is fine, thus ignore errors.
       (catch-system-error
	(mkdir default-socket-dir #o700)))

  ;; Check for permissions.
  (or insecure
      (let ((dir-stat (stat dir)))
	(and (not (and (= (stat:uid dir-stat) (getuid))
		       (= (stat:perms dir-stat) #o700)))
	     (begin
	       (local-output "Socket directory setup is insecure.")
	       (quit 1))))))


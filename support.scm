;; support.scm -- Various general support facilities, shared by deco and dmd.
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

;; Define simple macros in a more readable way.
(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (NAME ARG ...) (RESULT ...))
     (define-syntax NAME
       (syntax-rules ()
	 ((_ ARG ...)
	  (RESULT ...)))))))

;; For parts of the code specific to dmd.
(if (string=? program-name "dmd")
    (define-syntax-rule (begin-dmd EXPR ...)
      (begin EXPR ...))
    (define-syntax-rule (begin-dmd EXPR ...)
      (begin #f)))

;; An obvious alias.  We currently do not use this, though.
(define call/cc call-with-current-continuation)

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
;; FIXME: There are better ways to do this.
(define (caught-error key args)
  (display key)
  (write args)
  (newline))

;; Assert that expression EXPR does not evaluate to `#f'.
(define-syntax-rule (assert EXPR)
  (and (not EXPR)
       (begin
	 (local-output "Assertion ~a failed." 'EXPR)
	 (throw 'assertion-failed))))

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



;; Localized version of STR.  Need to use lambda.  *sigh*
;; FIXME: Implement.
(define l10n
  (lambda (str)
    str))

;; Display some text and a newline.  Need to use lambda.  *sigh*
(define local-output
  (lambda (format-string . args)
    (write-line (apply format #f (l10n format-string) args))))

(define (display-version)
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


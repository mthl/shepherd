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

;; An obvious alias.
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

;; Ignore a system error in case it occurs.
(define-syntax-rule (without-system-error EXPR ...)
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

(define real-output-port (current-output-port))
(define void-output-port (%make-void-port "w"))
(define log-output-port #f)

(define (be-silent)
  (set-current-output-port void-output-port))

(define (be-verbose)
  (set-current-output-port real-output-port))

(define extra-output-sender #f)
(define extra-output-sender-enabled #t)
;; FIXME: That may be ``good enough'', but should be fixed anyway.
(define terminating-string "!§&§&§&§&!") ;; Unlikely to appear in output.

(define (open-extra-sender file)
  (without-system-error
   (set! extra-output-sender (make <sender> file))))

(define (close-extra-sender)
  (without-system-error
   (send-data extra-output-sender terminating-string)
   (set! extra-output-sender #f)))

(define (start-logging file)
  (set! log-output-port (open-file file "wl"))) ;; Line-buffered port.

(define (stop-logging)
  (set! log-output-port #f))

(define-syntax-rule (without-extra-output EXPR ...)
  (catch #t
	 (lambda ()
	   (set! extra-output-sender-enabled #f)
	   EXPR ...
	   (set! extra-output-sender-enabled #t))
	 (lambda (key . args)
	   (set! extra-output-sender-enabled #f)
	   (apply throw (cons key args)))))

;; Display some text and a newline.  Need to use lambda.  *sigh*
(define local-output
  (lambda (format-string . args)
    (let ((text (apply format #f (l10n format-string) args)))
      (write-line text)
      (and log-output-port
	   (write-line (string-append (strftime "%Y-%m-%d %H:%M:%S "
						(localtime (current-time)))
				      text)
		       log-output-port))
      (and extra-output-sender
	   extra-output-sender-enabled
	   (without-system-error
	    (send-data extra-output-sender text))))))

(define (display-version)
  (local-output "~a -- ~a" banner copyright))



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
;; default directory, but does not exist.  If INSECURE? is `#f', also
;; checks for the permissions of the directory.
(define (verify-dir dir insecure?)
  (and (string=? dir default-socket-dir)
       ;; If it exists already, this is fine, thus ignore errors.
       (without-system-error
	(mkdir default-socket-dir #o700)))

  ;; Check for permissions.
  (and (not insecure?)
       (let ((dir-stat (stat dir)))
	 (and (not (and (= (stat:uid dir-stat) (getuid))
			(= (stat:perms dir-stat) #o700)))
	      (begin
		(local-output "Socket directory setup is insecure.")
		(quit 1))))))


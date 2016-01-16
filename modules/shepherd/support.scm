;; support.scm -- Various support facilities, used by herd and dmd.
;; Copyright (C) 2014 A.Sassmannshausen <alex.sassmannshausen@gmail.com>
;; Copyright (C) 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;;
;; This file is part of the GNU Shepherd.
;;
;; The GNU Shepherd is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; The GNU Shepherd is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.

(define-module (shepherd support)
  #:use-module (shepherd config)
  #:use-module (ice-9 match)
  #:export (call/ec
            caught-error
            assert
            label
            can-apply?

            catch-system-error
            with-system-error-handling
            EINTR-safe
            with-atomic-file-output

            l10n
            local-output
            display-version

            user-homedir
            default-logfile
            default-config-file
            default-socket-dir
            default-socket-file
            %system-socket-file
            default-persistency-state-file

            load-in-user-module

            persistency
            persistency-state-file

            verify-dir))

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

;; Check whether a list of NUM-ARGS arguments can successfully be
;; applied to PROC.
(define (can-apply? proc num-args)
  (and (procedure? proc)
       (match (procedure-minimum-arity proc)
         ((required optional rest?)
          (and (>= num-args required)
               (or rest? (<= num-args (+ required optional)))))
         (_ #t))))

;; Evaluate `EXPR ...' until a system error occurs, then skip the
;; remaining code.
(define-syntax-rule (catch-system-error EXPR ...)
  (catch 'system-error
    (lambda ()
      EXPR ...)
    (lambda (key . args)
      #f)))

(define (call-with-system-error-handling thunk)
  "Call THUNK, catching any 'system-error' exception."
  (catch 'system-error
    thunk
    (lambda (key proc format-string format-args . rest)
      (format (current-error-port) "error: ~a: ~a~%" proc
              (apply format #f format-string format-args))
      (quit 1))))

(define-syntax-rule (with-system-error-handling body ...)
  "Evaluate BODY in a context where 'system-error' throws are caught and
turned into user error messages."
  (call-with-system-error-handling
   (lambda ()
     body ...)))

(define (EINTR-safe proc)
  "Wrap PROC so that if a 'system-error' exception with EINTR is raised (that
was possible up to Guile 2.0.9 included) the call to PROC is restarted."
  (lambda args
    (let loop ()
      (catch 'system-error
        (lambda ()
          (apply proc args))
        (lambda args
          (if (= EINTR (system-error-errno args))
              (loop)
              (apply throw args)))))))

(define (with-atomic-file-output file proc)       ;copied from Guix
  "Call PROC with an output port for the file that is going to replace FILE.
Upon success, FILE is atomically replaced by what has been written to the
output port, and PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (let ((result (proc out)))
          (close out)
          (rename-file template file)
          result))
      (lambda (key . args)
        (catch-system-error (delete-file template))))))



;; Localized version of STR.
(define l10n gettext)

;; Display some text and a newline.
(define-syntax-rule (local-output format-string args ...)
  (begin
    (format #t (gettext format-string) args ...)
    (newline)))

(define* (display-version #:optional (program-name "dmd"))
  (local-output "~a (~a) ~a" program-name package-name Version)
  (local-output (l10n "Copyright (C) 2016 the Shepherd authors
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.")))



;; Home directory of the user.
(define user-homedir
  ;; Look for $HOME first, to allow users to override the defaults.  This is
  ;; notably useful when dmd is built in a Guix chroot.
  (or (getenv "HOME")

      ;; When bootstrapping and running as PID 1, /etc/{passwd,shadow} may be
      ;; unavailable.  Gracefully handle that.
      (false-if-exception (passwd:dir (getpwuid (getuid))))
      "/"))

;; dmd default subdirectory if dmd is run as a normal user.
(define user-dmddir (string-append user-homedir "/.dmd.d"))

(define (make-bare-init-file target)
  "Return #t if a bare init file was created at TARGET; #f otherwise.

TARGET should be a string representing a filepath + name."
  (with-output-to-file target
    (lambda ()
      (display (string-append
                ";; init.scm -- default dmd configuration file.

;; Services known to dmd:
;; Add new services (defined using 'make <service>') to dmd here by
;; providing them as arguments to 'register-services'.
""(register-services)

;; Send dmd into the background
""(action 'dmd 'daemonize)

;; Services to start when dmd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
""(for-each start '())
")))))

;; Logfile.
(define default-logfile
  (if (zero? (getuid))
      (string-append %localstatedir "/log/shepherd.log")
      (string-append user-dmddir "/shepherd.log")))

;; Configuration file.
(define (default-config-file)
  "Return the default configuration file---either the user's file, or the
global system configuration file when running as 'root'.  As a side effect,
create a template configuration file if non exists."
  (if (zero? (getuid))
      (string-append %sysconfdir "/dmdconf.scm")
      (let ((config-file (string-append user-dmddir "/init.scm")))
        (catch-system-error (mkdir user-dmddir))
        (if (not (file-exists? config-file))
            (make-bare-init-file config-file))
        config-file)))

;; Socket directory for the system's instance (PID 1).
(define %system-socket-dir
  (string-append %localstatedir "/run/shepherd"))

;; The directory where the socket resides.
(define default-socket-dir
  (if (zero? (getuid))
      %system-socket-dir
      (string-append user-dmddir "/run")))

;; Unix domain socket for receiving commands in dmd.
(define default-socket-file
  (string-append default-socket-dir "/socket"))

;; Location of the socket of the system's instance (PID 1).
(define %system-socket-file
  (string-append %system-socket-dir "/socket"))

;; Saving the state on exit.
(define default-persistency-state-file
  (if (zero? (getuid))
      (string-append %localstatedir "/lib/misc/dmd-state")
      (string-append user-dmddir "/dmd-state")))

;; Global variables set from (dmd).
(define persistency #f)
(define persistency-state-file default-persistency-state-file)

(define (make-dmd-user-module)
  "Return a new module, for use when evaluating the user's configuration,
which has essential bindings pulled in."
  (let ((m (make-fresh-user-module)))
    ;; The typical configuration file wants to do '(make <service> ...)', and
    ;; '(register-services ...)', so provide the relevant bindings by default.
    (module-use! m (resolve-interface '(oop goops)))
    (module-use! m (resolve-interface '(shepherd service)))
    m))

(define (load-in-user-module file)
  "Load FILE in a fresh user module that has essential bindings pulled in."
  (let ((user-module (make-dmd-user-module)))
    (save-module-excursion
     (lambda ()
       (set-current-module user-module)
       (primitive-load file)))))

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

;; herd.scm -- The program to herd the Shepherd.
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

(define-module (herd)
  #:use-module (shepherd config)
  #:use-module (shepherd support)
  #:use-module (shepherd args)
  #:use-module (shepherd comm)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (program-name
            main))

(define program-name "herd")


(define (service-list-error services)
  (format (current-error-port)
          (l10n "~a: error: received an invalid service list:~%~s~%")
          program-name services))

(define-syntax alist-let*
  (syntax-rules ()
    "Bind the given KEYs in EXP to the corresponding items in ALIST.  ALIST
is assumed to be a list of two-element tuples rather than a traditional list
of pairs."
    ((_ alist (key ...) exp ...)
     (let ((key (and=> (assoc-ref alist 'key) car)) ...)
       exp ...))))

(define service-canonical-name
  (match-lambda
    (('service ('version 0 _ ...) (provides (name0 _ ...)) _ ...)
     name0)))

(define (display-status-summary services)
  "Display a summary of the status of all of SERVICES."
  (match services
    (('service-list ('version 0) services ...)
     (call-with-values
         (lambda ()
           (partition (match-lambda
                        (('service ('version 0 _ ...) properties ...)
                         (car (assoc-ref properties 'running))))
                      services))
       (lambda (started stopped)
         (format #t (l10n "Started: ~a~%")
                 (map service-canonical-name started))
         (format #t (l10n "Stopped: ~a~%")
                 (map service-canonical-name stopped)))))
    (_
     (service-list-error services))))

(define (display-detailed-status services)
  "Display the detailed status of SERVICES."
  (match services
    (('service-list ('version 0) services ...)
     (for-each display-service-status services))
    (_
     (service-list-error services))))

(define (display-service-status service)
  "Display the status of SERVICE, an sexp."
  (match service
    (('service ('version 0 _ ...) properties ...)
     (alist-let* properties (provides requires running respawn? enabled?)
       (format #t (l10n "Status of ~a:~%") (first provides))
       (if running
           (begin
             (format #t (l10n "  It is started.~%"))
             (format #t (l10n "  Running value is ~s.~%") running))
           (format #t (l10n "  It is stopped.~%")))
       (if enabled?
           (format #t (l10n "  It is enabled.~%"))
           (format #t (l10n "  It is disabled.~%")))
       (format #t (l10n "  Provides ~a.~%") provides)
       (format #t (l10n "  Requires ~a.~%") requires)
       ;; FIXME: We don't have that information.
       ;; (format #t (l10n "  Conflicts with ~a." (conflicts-with obj)))
       (if respawn?
           (format #t (l10n "  Will be respawned.~%"))
           (format #t (l10n "  Will not be respawned.~%")))))
    (('error ('version 0 _ ...) 'service-not-found service)
     (format (current-error-port)
             (l10n "Service ~a could not be found.~%")
             service)
     (exit 1))
    (('error . _)
     (format (current-error-port)
             (l10n "Something went wrong: ~s~%")
             service))))

(define (run-command socket-file action service args)
  "Perform ACTION with ARGS on SERVICE, and display the result.  Connect to
the daemon via SOCKET-FILE."
  (with-system-error-handling
   (let ((sock    (open-connection socket-file))
         (action* (if (and (eq? service 'dmd) (eq? action 'detailed-status))
                      'status
                      action)))
     ;; Send the command.
     (write-command (dmd-command action* service #:arguments args)
                    sock)

     ;; Receive output.
     (setvbuf sock _IOLBF)

     ;; Interpret the command's output when possible and format it in a
     ;; human-readable way.
     (match (list action service)
       (('status 'dmd)
        (display-status-summary (read sock)))
       (('detailed-status 'dmd)
        (display-detailed-status (read sock)))
       (('status _)
        (display-service-status (read sock)))
       (_
        ;; For other commands, we don't do any interpretation.
        (let loop ((line (read-line sock)))
          (unless (eof-object? line)
            (display line)
            (newline)
            (loop (read-line sock))))))

     (close-port sock))))


;; Main program.
(define (main . args)
  (false-if-exception (setlocale LC_ALL ""))

  (let ((socket-file default-socket-file)
	(command-args '()))
    (process-args program-name args
		  "ACTION SERVICE [ARG...]"
		  (string-append
		   "Apply ACTION (start, stop, status, etc.) on SERVICE"
		   " with the ARGs.")
		  (lambda (arg)
		    ;; Collect unknown args.
		    (set! command-args (cons arg command-args)))
		  (make <option>
		    #:long "socket" #:short #\s
		    #:takes-arg? #t #:optional-arg? #f #:arg-name "FILE"
		    #:description "send commands to FILE"
		    #:action (lambda (file)
			       (set! socket-file file))))

    (match (reverse command-args)
      (((and action (or "status" "detailed-status"))) ;one argument
       (run-command socket-file (string->symbol action) 'dmd '()))
      ((action service args ...)
       (run-command socket-file
                    (string->symbol action)
                    (string->symbol service) args))
      (_
       (format (current-error-port)
               (l10n "Usage: herd ACTION [SERVICE [OPTIONS...]]~%"))
       (exit 1)))))

;; Local Variables:
;; eval: (put 'alist-let* 'scheme-indent-function 2)
;; End:

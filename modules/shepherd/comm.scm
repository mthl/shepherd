;; comm.scm -- Communication between processes and general output.
;; Copyright (C) 2013, 2014, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;; Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
;; Copyright (C) 2018 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (shepherd comm)
  #:use-module (shepherd support)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (open-connection

            <shepherd-command>
            shepherd-command?
            shepherd-command
            shepherd-command-directory
            shepherd-command-action
            shepherd-command-service
            shepherd-command-arguments

            <command-reply>
            command-reply
            command-reply?
            command-reply-command
            command-reply-result
            command-reply-error
            command-reply-messages

            write-command
            read-command

            write-reply
            result->sexp
            report-command-error

            log-output-port
            start-logging
            stop-logging
            make-shepherd-output-port

            %current-client-socket
            %current-logfile-date-format))


;; Command for shepherd.
(define-record-type <shepherd-command>
  (%shepherd-command action service args directory)
  shepherd-command?
  (action    shepherd-command-action)             ; symbol
  (service   shepherd-command-service)            ; symbol
  (args      shepherd-command-arguments)          ; list of strings
  (directory shepherd-command-directory))         ; directory name

(define* (shepherd-command action service
                           #:key (arguments '()) (directory (getcwd)))
  "Return a new command for ACTION on SERVICE."
  (%shepherd-command action service arguments directory))

(define* (open-connection #:optional (file default-socket-file))
  "Open a connection to the daemon, using the Unix-domain socket at FILE, and
return the socket."
  ;; The protocol is sexp-based and UTF-8-encoded.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let ((sock    (socket PF_UNIX SOCK_STREAM 0))
          (address (make-socket-address PF_UNIX file)))
      (catch 'system-error
        (lambda ()
          (connect sock address)
          (setvbuf sock _IOFBF 1024))
        (lambda (key proc format-string format-args errno . rest)
          ;; Guile's 'connect' throws an exception that doesn't specify
          ;; FILE.  Augment it with this information.
          (apply throw
                 key proc
                 "~A: ~A" (list file (strerror (car errno)))
                 (list errno) rest)))
      sock)))

(define (read-command port)
  "Receive a command from PORT; return the command of #f if something went
wrong---premature end-of-file, invalid sexp, etc."
  (catch 'read-error
    (lambda ()
      (match (read port)
        (('shepherd-command ('version 0 _ ...)
                            ('action action)
                            ('service service)
                            ('arguments (args ...))
                            ('directory directory))
         (shepherd-command action service
                           #:arguments args
                           #:directory directory))
        (_                                        ;EOF or unmatched sexp
         #f)))
    (lambda _
      ;; Invalid sexp.
      #f)))

(define (write-command command port)
  "Write COMMAND to PORT."
  (match command
    (($ <shepherd-command> action service (arguments ...) directory)
     (write `(shepherd-command (version 0)        ; protocol version
                               (action ,action)
                               (service ,service)
                               (arguments ,arguments)
                               (directory ,directory))
            port)

     ;; PORT may be buffered so make sure the command goes out.
     (force-output port))))


;; Replies to commands.

(define-record-type <command-reply>
  (command-reply command result error messages)
  command-reply?
  (command  command-reply-command)                ;command
  (result   command-reply-result)                 ;sexp | #f
  (error    command-reply-error)                  ;#f | sexp
  (messages command-reply-messages))              ;list of strings

(define (write-reply reply port)
  "Write REPLY to PORT."
  (match reply
    (($ <command-reply> command result error (messages ...))
     ;; Use 'result->sexp' to convert RESULT to an sexp.  We don't do that for
     ;; ERROR because using GOOPS methods doesn't work for SRFI-35 error
     ;; conditions, and that's what we're using here. (XXX)
     (write `(reply (version 0)
                    (result ,(result->sexp result))
                    (error ,error)
                    (messages ,messages))
            port)

     ;; PORT may be buffered so make sure the command goes out.
     (force-output port))))

;; This generic function must be extended to provide sexp representations of
;; results that go in <command-reply> objects.
(define-generic result->sexp)

(define-method (result->sexp (bool <boolean>)) bool)
(define-method (result->sexp (number <number>)) number)
(define-method (result->sexp (symbol <symbol>)) symbol)
(define-method (result->sexp (string <string>)) string)
(define-method (result->sexp (list <list>)) (map result->sexp list))
(define-method (result->sexp (kw <keyword>)) kw)
(define-method (result->sexp (obj <top>)) (object->string obj))

(define (report-command-error error)
  "Report ERROR, an sexp received by a shepherd client in reply to COMMAND, a
command object."
  (match error
    (('error ('version 0 _ ...) 'service-not-found service)
     ;; TRANSLATORS: Strings occasionally contain escape sequences starting
     ;; with '~' (tilde).  For example, '~a' corresponds to '%s' in C printf
     ;; syntax and '~%' corresponds to '\n'.  These must be preserved as is.
     ;; See
     ;; <https://www.gnu.org/software/guile/manual/html_node/Formatted-Output.html>
     ;; for more info.
     (report-error (l10n "service '~a' could not be found")
                   service))
    (('error ('version 0 _ ...) 'action-not-found action service)
     (report-error (l10n "service '~a' does not have an action '~a'")
                   service action))
    (('error ('version 0 _ ...) 'action-exception action service
             key (args ...))
     (report-error (l10n "exception caught while executing '~a' \
on service '~a':")
                   action service)
     (print-exception (current-error-port) #f key args))
    (('error . _)
     (report-error (l10n "something went wrong: ~s")
                   error))
    (#f                                           ;not an error
     #t)))



(define log-output-port
  ;; Port for logging.  This must always be a valid port, never `#f'.
  (make-parameter (%make-void-port "w")))

(define (start-logging file)                      ;deprecated
  (let ((directory (dirname file)))
    (unless (file-exists? directory)
      (mkdir directory)))
  (log-output-port (open-file file "al")))
(define (stop-logging)                            ;deprecated
  (close-port (log-output-port))
  (log-output-port (%make-void-port "w")))

(define %current-client-socket
  ;; Socket of the client currently talking to the daemon.
  (make-parameter #f))

(define %current-logfile-date-format
  ;; 'strftime' format strings for entries in the log file.
  (make-parameter default-logfile-date-format))

;; We provide our own output mechanism, because we have certain
;; special needs; most importantly, we want to send output to herd
;; sometimes.
(define* (make-shepherd-output-port
          #:optional (original-output-port (current-output-port)))
  (make-soft-port
   (vector

    ;; One character for output.
    (lambda (char)
      (display (string char)))

    ;; A string for output.
    (let ((buffer '())) ;; List of unwritten output strings.
      (lambda (str)
        ;; When herd is connected, send it the output; otherwise, in the
        ;; unlikely case nobody is listening, send to the standard output.
        (if (%current-client-socket)
            (catch-system-error
             (display str (%current-client-socket)))
            (display str original-output-port))

        ;; Logfile, buffer line-wise and output time for each
        ;; completed line.
        (if (not (string-index str #\newline))
            (set! buffer (cons str buffer))
            (let* ((log (lambda (x)
                          (display x (log-output-port))))
                   (init-line (lambda ()
                                (log (strftime (%current-logfile-date-format)
                                               (localtime (current-time)))))))
              (init-line)
              (for-each log (reverse buffer))
              (let* ((lines (string-split str #\newline))
                     (last-line (car (take-right lines 1)))
                     (is-first #t))
                (for-each (lambda (line)
                            (if is-first
                                (set! is-first #f)
                                (init-line))
                            (log line)
                            (log #\newline))
                          (drop-right lines 1))
                (set! buffer (if (string-null? last-line)
                                 '()
                                 (list last-line))))))))

    ;; Flush output.
    (lambda ()
      ;; FIXME: Do we need to do something?  Flush the logfile buffer?
      #t)

    ;; Get a character (unused).
    #f

    ;; Close the port.
    (lambda () #t))

   ;; It's an output-only port.
   "w"))


;; wolfgangj.scm -- Personal dmd configuration of Wolfgang Jährling.
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



;; A few notes about the computer this setup is for: It is a PC
;; running Debian GNU/Linux 3.0, on which I am usually using screen,
;; which is why I need few terminals.  I am using it to connect to the
;; internet via ISDN, but I'm also using it in my local network, in
;; which case it is running Apache.  Because I don't want to have
;; Apache running when I am online, I made Apache conflict with the
;; ISDN setup.

;; Laziness.
(define (number->symbol num)
  (string->symbol (number->string num)))

;; Even more laziness.
(define cat string-append)

;; Some values for this system.
(define getty "/sbin/getty")
(define ifconfig "/sbin/ifconfig")
(define isdnctrl "/usr/sbin/isdnctrl")

(define inet-interface "ippp0")
(define inet-dial (cat isdnctrl " dial " inet-interface))
(define inet-hangup (cat isdnctrl " hangup " inet-interface))

(define local-interface "eth0")
(define local-ip "192.168.0.9")

(define init.d-dir "/etc/init.d/")
(define apache (cat init.d-dir "apache"))
(define inet (cat init.d-dir "isdnutils"))
(define exim (cat init.d-dir "exim"))

;; Create a service providing a terminal.
(define (make-term num)
  (let ((sym (symbol-append 'term- (number->symbol num))))
    (make <service>
      #:provides (list sym)
      #:respawn? #t
      #:start (make-childexec-constructor getty "38400"
					  (cat "tty" (number->string num)))
      #:stop-delay? #t)))

;; Number of terminals created by default.
(define default-terms 3)

;; How many terms have been created with `add-new-term'.
(define term-counter 0)

;; Add a new terminal to the list of registered services.
(define (add-new-term)
  (set! term-counter (+ term-counter 1))
  (register-services (make-term term-counter)))

(register-services
 (make <service>
   #:provides '(term)
   #:extra-actions (make-extra-actions
		    (create
		     (lambda (running)
		       (add-new-term)))
		    (counter-set
		     (lambda (running num)
		       (set! term-counter (string->number num))))
		    (status
		     (lambda (running)
		       (local-output "Terminal counter is at ~a."
				     term-counter)))))
 (make <service>
   #:provides '(apache insecurity)
   #:requires '(local-net)
   #:start (make-system-constructor apache " start")
   #:stop (make-system-destructor apache " stop"))
 (make <service>
   #:provides '(inet insecurity)
   #:start (make-system-constructor inet " start")
   #:stop (make-system-destructor inet " stop")
   #:extra-actions (make-extra-actions
		    (dial
		     (lambda (running)
		       (system inet-dial)
		       #t))
		    (hangup
		     (lambda (running)
		       (system inet-hangup)
		       #t))))
 (make <service>
   #:provides '(local-net)
   #:start (make-system-constructor ifconfig " " local-interface " " local-ip)
   #:stop (make-system-destructor ifconfig " " local-interface " down"))
 (make <service>
   #:provides '(exim mailer-daemon)
   #:requires '(inet)
   #:start (make-system-constructor exim " start")
   #:stop (make-system-destructor exim " stop")))

;; Create a few terminals.
(letrec ((loop (lambda (i)
		 (and (not (zero? i))
		      (begin
			(add-new-term)
			(loop (- i 1)))))))
  (loop default-terms))

;; Go into background.
(extra-action 'dmd 'daemonize)

;; Setup internet, a mailer and a few terms.
(for-each start
	  (append '(term inet mailer-daemon)
		  (map (lambda (x)
			 (symbol-append 'term- (number->symbol x)))
		       (iota default-terms 1))))

;; dmd-gettext.scm -- Extract translatable strings from source code.
;; Copyright (C) 2002 Wolfgang Jährling <wolfgang@pro-linux.de>
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

(define l10n-keywords '(l10n local-output))

(define (puts . text)
  (for-each display text)
  (newline))

(define (found-string str orig)
  (puts "#: " (source-property orig 'filename)
	":" (source-property orig 'line))
  (puts "msgid \"" str "\"")
  (puts "msgstr \"\"")
  (puts))

(define (extract-strings port)
  (letrec ((next-expr
	    (lambda (data)
	      (or (eof-object? data)
		  (begin
		    (letrec ((look-at
			      (lambda (expr)
				(if (and (list? expr)
					 (not (null? expr)))
				    (begin
				      (and (memq (car expr)
						 l10n-keywords)
					   (found-string (cadr expr) expr))
				      (for-each look-at expr))
				    (and (pair? expr)
					 (begin
					   (look-at (car expr))
					   (look-at (cdr expr))))))))
		      (look-at data))
		    (next-expr (read port)))))))
    (next-expr (read port))))

;; Safe file positions of the expressions we read.  It is enabled by
;; default, but we want to be sure.
(read-enable 'positions)

;; Iterate over all given files.
(for-each extract-strings
	  (map open-input-file
	       (cdr (command-line))))

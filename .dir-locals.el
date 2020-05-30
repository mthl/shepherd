;; Per-directory local variables for GNU Emacs 23 and later.

((nil
  . ((fill-column . 78)
     (tab-width   .  8)
     (sentence-end-double-space . t)

     ;; For use with 'bug-reference-prog-mode'.
     (bug-reference-url-format . "http://bugs.gnu.org/%s")
     (bug-reference-bug-regexp
      . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")))
 (scheme-mode
  . ((indent-tabs-mode . nil)
     (eval . (put 'with-blocked-signals 'scheme-indent-function 1))))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72))))

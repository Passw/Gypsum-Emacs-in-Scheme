(define-library (gypsum elisp-eval pretty)
  ;; This library defines an extension to `(gypsum pretty)`
  ;; with rules specific to pretty-printing Emacs Lisp.
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (rapid match) match guard)
    (only (gypsum pretty)
          pretty   print   qstr   pp-type?
          indent-by   newline-indent   line-break
          bracketed   form   join-by   join-lines
          )
    )
  (export elisp-pretty)
  (include "pretty.scm")
  )

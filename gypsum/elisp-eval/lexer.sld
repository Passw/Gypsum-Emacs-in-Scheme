(define-library (gypsum elisp-eval lexer)
  (import
    (scheme base)
    (only (scheme char)
          char-upcase
          char-alphabetic?
          char-numeric?
          char-whitespace?
          )
    (only (srfi 14)
          string->char-set
          char-set-contains?
          )
    (only (srfi 60) ash logior)
    )
  ;; -------- Regular expressions --------
  (cond-expand
    (guile
     (import
       (only (guile)
             make-regexp
             regexp-exec
             )
       (only (ice-9 regex)
             match:substring
             )
       )))
  ;; -------- File port extensions --------
  (cond-expand
    (guile
     (import
       (only (guile)
             file-port?
             port-filename
             port-line
             port-column
             unread-char
             set-source-property!
             )
       )))
  ;; ----------------------------------------
  (export
   get-lexer get-lexer/1
   )
  (include "lexer.scm"))

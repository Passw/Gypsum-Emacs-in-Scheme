(define-library (gypsum elisp-eval parser)
  (import
    (scheme base)
    (scheme write);;DEBUG
    (only (scheme lazy)
          promise?
          delay
          force
          )
    (gypsum elisp-eval lexer)
    )
  ;; -------- Hash Tables --------
  (cond-expand
    ((or guile gambit)
     (import
       (only (srfi 69)
             make-hash-table
             hash-table-ref
             hash-table-set!
             )
       ))
    (mit)
    (else
     (import
       (only (srfi 125)
             make-hash-table
             hash-table-ref
             hash-table-set!
             )
       )))
  ;; -------- File port extensions --------
  (cond-expand
    (guile
     (import
       (only (guile)
             port-filename
             port-line
             port-column
             unread-char
             set-source-properties!
             source-properties
             )
       )))
  (export
   read-elisp
   )
  (include "parser.scm"))

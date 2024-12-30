(import
  (only (scheme load) load)
  (only (scheme repl) interaction-environment))

(cond-expand
  (guile
   (define (path str)
     (string-append (getcwd) "/" str))
   )
  (else
   (define (path str) str)
   ))

(for-each
 (lambda (filepath)
   (load (path filepath) (interaction-environment))
   )
 (list 
  "gypsum/lens-tests.scm"
  "gypsum/lens/vector-tests.scm"
  "gypsum/lens/bin-hash-table-tests.scm"
  "gypsum/pretty-tests.scm"
  "gypsum/keymap-tests.scm"
  "gypsum/concurrent-tests.scm"
  "gypsum/cursor-tests.scm"
  "gypsum/elisp-eval/environment-tests.scm"
  "gypsum/elisp-eval/format-tests.scm"
  "gypsum/elisp-eval-tests.scm"
  ))

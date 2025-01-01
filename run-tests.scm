(import
  (scheme base)
  (scheme write)
  (rename (scheme load) (load scheme/load))
  (only (scheme repl) interaction-environment))

(cond-expand
  (guile
   (define (path str)
     (string-append (getcwd) "/" str))
   )
  (else
   (define (path str) str)
   ))

(cond-expand
  (gambit
   (define (load path . env) (scheme/load path)))
  (else
   (define load scheme/load))
  )

(define test-programs
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

(for-each
 (lambda (filepath)
   (display "----------------------------------------\n")
   (display (path filepath))
   (newline)
   (load (path filepath) (interaction-environment))
   )
 test-programs)

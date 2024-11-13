(import
  (scheme base)
  (gypsum compat)
  (only (srfi 64)
        test-begin test-end
        test-assert test-equal))

(cond-expand
  (guile-3
   (import
     (only (srfi 69)
           make-hash-table
           hash-table-size
           alist->hash-table
           hash-table->alist))
   )
  (else))

(cond-expand
  ((or gambit stklos)
   ;; NOTE: see lens.sld import of (srfi 69) for why this is necessary.
   (import (srfi 64))
   (import (srfi 69))
   )
  (else))

;; -------------------------------------------------------------------------------------------------

(test-begin "gypsum_compat")

(cond-expand
  ((or guile-3 gambit)
   (define eht (make-hash-table))
   (test-assert (hash-table-empty? eht))
   )
  (else))

(cond-expand
  (gambit
   (test-assert 6
     (vector-fold + 0 (vector 0 1 2 3)))
   (test-assert 36
     (vector-fold + 0
      (vector 6 7 8 400 500 600)
      (vector 3 4 5 200 300)
      (vector 0 1 2 100)
      (vector 0 0 0)
      ))
   )
  (else)
  )

(test-end "gypsum_compat")

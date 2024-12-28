
(test-begin "gypsum_vector")

(cond-expand
  ((or guile-3 gambit (library (srfi 43)) (library (srfi 133)))
   ;; Nothing is done here, we assume the imported vector-fold
   ;; procedure is tested elsewhere.
   )
  (else
   (test-assert 6
     (vector-fold + 0 (vector 0 1 2 3)))
   (test-assert 36
     (vector-fold + 0
      (vector 6 7 8 400 500 600)
      (vector 3 4 5 200 300)
      (vector 0 1 2 100)
      (vector 0 0 0)
      ))
   ))

(test-end "gypsum_vector")

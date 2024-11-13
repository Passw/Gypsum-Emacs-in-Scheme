(define-library (gypsum compat)
  ;; Symbols defined for cross-compatability

  (import
    (scheme base)
    )

  (cond-expand
    (guile
     (import
       (only (srfi 69) hash-table-size))
     )
    (gambit
     (import (srfi 69)))
    (else))

  (cond-expand
    ((or guile gambit)
     (export
      hash-table-empty?
      )
     (begin
       (define (hash-table-empty? ht)
         (= 0 (hash-table-size ht)))
       )
     )
    (else
     (import
       (only (srfi 125) hash-table-empty?))
     (export
      hash-table-empty?
      ))
    )

  (cond-expand
    ((or guile gambit)
     (export
      vector-fold
      )
     (begin
       (define (vector-fold kons knil . veclist)
         ;; This is an implementation of SRFI-133 `VECTOR-FOLD`
         ;;------------------------------------------------------------------
         (let ((minlen
                (let loop ((veclist veclist) (minlen #f))
                  (cond
                   ((null? veclist) (if minlen minlen 0))
                   (else
                    (let ((thislen (vector-length (car veclist))))
                      (loop (cdr veclist)
                            (if minlen (min thislen minlen) thislen)))))))
               )
           (cond
            ((= 0 minlen) knil)
            (else
             (let loop ((i 0) (state knil))
               (cond
                ((>= i minlen) state)
                (else
                 (loop (+ 1 i)
                       (apply kons state
                              (map (lambda (vec) (vector-ref vec i))
                                   veclist))))))))
           ))
       ))
    (else
     (import
       (only (srfi 133) vector-fold))
     (export
      vector-fold
      ))
    )
  )

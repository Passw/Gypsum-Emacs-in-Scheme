(import
  (scheme base)
  (scheme case-lambda)
  (srfi 64) ;;testing
  ;;(srfi  1) ;;lists
  (only (scheme file) open-input-file open-binary-input-file)
  (only (srfi 111) box box? unbox set-box!)
  (gypsum elisp-eval environment)
  (gypsum lens)
  (gypsum pretty)
  (only (gypsum cursor) new-cursor cursor-step!)
  (only (gypsum lens vector)
        mutable-vector-type? =>mvector-index!
        new-mutable-vector
        mutable-vector-length
        mutable-vector-append!)
  (only (gypsum elisp-eval parser) read-elisp)
  (prefix (gypsum editor-impl) *impl/)
  )
(cond-expand
  ((or guile gambit stklos)
   (import
     (only (srfi 69)
           hash-table-size
           hash-table-ref/default
           alist->hash-table))
   )
  ((mit))
  (else
   (import (srfi 125))))

(test-begin "gypsum_elisp_eval_environment_tests")

(define test-elisp-env (new-empty-environment))

(test-assert (elisp-environment-type? test-elisp-env))

(lens-set "Hello, world!" test-elisp-env (=>env-obarray-key! "hello"))

(test-assert "Hello, world!"
  (view test-elisp-env (=>env-obarray-key! "hello")))

;;;;---- these use `ELSTKFRM-ZIP-ARGS` to run tests ----
;;
;;(define test-elstkfrm #f)
;;(define test-stk-count #f)
;;(define test-stk-error #f)
;;
;;(define (elstkfrm-trial syms opts rest args)
;;  (let-values
;;      (((elstkfrm count stk-err)
;;        (elstkfrm-zip-args syms opts rest args)
;;        ))
;;    (set! test-elstkfrm elstkfrm)
;;    (set! test-stk-count count)
;;    (set! test-stk-error stk-err)
;;    #t))
;;
;;(define (elstkfrm-expect elstkfrm stk-count stk-error)
;;  (and (equal? test-elstkfrm elstkfrm)
;;       (eqv? test-stk-count stk-count)
;;       (equal?
;;        (if test-stk-error (view test-stk-error =>elisp-eval-error-message) #f)
;;        stk-error
;;        ))
;;  )

;;---- these use `ELSTKFRM-FROM-ARGS` to run tests ----

(define test-elstkfrm #f)
 ;; Test results are stored in a global variable so that tests can
 ;; more easily be debugged in a REPL.

(define (elstkfrm-trial syms opts rest args)
  (let ((func (new-lambda)))
    (lens-set (map symbol->string syms) func =>lambda-args!)
    (lens-set (map symbol->string opts) func =>lambda-optargs!)
    (lens-set (if rest (symbol->string rest) #f) func =>lambda-rest!)
    (set! test-elstkfrm (elstkfrm-from-args func args))
    #t))


(define (elstkfrm-expect assocs stk-count stk-error)
  (if stk-error
      (equal? stk-error (view test-elstkfrm =>elisp-eval-error-message))
      (and
       (= stk-count (hash-table-size test-elstkfrm))
       (let loop ((assocs assocs))
         (cond
          ((null? assocs) #t)
          (else
           (let*((pair (car assocs))
                 (key (symbol->string (car pair)))
                 (expected (cdr pair))
                 (actual (hash-table-ref/default test-elstkfrm key #f))
                 )
             (if (equal? expected actual)
                 (loop (cdr assocs))
                 (error
                  "the \"elstkfrm\" did not have expected value at key"
                  #:key key #:expected expected #:actual actual
                  ))))
              )))
      ))


(define (elstkfrm-expect-not-enough)
  (elstkfrm-expect #f #f "not enough arguments"))

(define (elstkfrm-expect-too-many)
  (elstkfrm-expect #f #f "too many arguments"))

(elstkfrm-trial '() '() #f '())
(test-assert (elstkfrm-expect '() 0 #f))

(elstkfrm-trial '(zero one) '() #f '(0 1))
(test-assert (elstkfrm-expect '((one . 1) (zero . 0)) 2 #f))

(elstkfrm-trial '(zero one) '() #f '())
(test-assert (elstkfrm-expect-not-enough))

(elstkfrm-trial '(zero one) '() #f '(0))
(test-assert (elstkfrm-expect-not-enough))

(elstkfrm-trial '(zero one) '() #f '(0 1 2))
(test-assert (elstkfrm-expect-too-many))

(elstkfrm-trial '(zero one) '(two) #f '(0 1 2))
(test-assert
    (elstkfrm-expect
     '((two . 2) (one . 1) (zero . 0)) 3 #f)
  )

(elstkfrm-trial '(zero one) '(two three) #f '(0 1 2))
(test-assert
    (elstkfrm-expect
     '((two . 2) (one . 1) (zero . 0)) 3 #f)
  )

(elstkfrm-trial '(zero one) '(two three) #f '(0 1 2 3))
(test-assert
    (elstkfrm-expect
     '((three . 3) (two . 2) (one . 1) (zero . 0)) 4 #f)
  )

(elstkfrm-trial '(zero one) '(two three) #f '(0 1 2 3 4))
(test-assert (elstkfrm-expect-too-many))

(elstkfrm-trial '(zero one) '(two three) 'rest '(0 1 2 3 4))
(test-assert
    (elstkfrm-expect
     '((rest 4) (three . 3) (two . 2) (one . 1) (zero . 0)) 5 #f))

(elstkfrm-trial '() '(zero one) #f '())
(test-assert (elstkfrm-expect '() 0 #f))

(elstkfrm-trial '() '(zero one) #f '(0))
(test-assert (elstkfrm-expect '((zero . 0)) 1 #f))

(elstkfrm-trial '() '(zero one) #f '(0 1))
(test-assert (elstkfrm-expect '((one . 1) (zero . 0)) 2 #f))

(elstkfrm-trial '() '(zero one) #f '(0 1 2))
(test-assert (elstkfrm-expect-too-many))

(elstkfrm-trial '() '(zero one) 'rest '(0 1 2))
(test-assert (elstkfrm-expect '((rest 2) (one . 1) (zero . 0)) 3 #f))

(elstkfrm-trial '() '(zero one) 'rest '(0 1 2 3))
(test-assert (elstkfrm-expect '((rest 2 3) (one . 1) (zero . 0)) 3 #f))

(env-push-new-elstkfrm!
 test-elisp-env 3
 (list (new-symbol "zero" 0)
       (new-symbol "one"  1)
       (new-symbol "two"  2)))
(test-equal 2
  (view test-elisp-env =>env-lexstack*! =>car (=>hash-key! "two") =>sym-value*!)
  )
(test-equal (list 2 1 0)
  (let ((lookup
         (lambda (name)
           (view
            test-elisp-env
            (=>env-symbol! name)
            =>sym-value*!)))
        )
    (list
     (lookup "two")
     (lookup "one")
     (lookup "zero")
     )))

(lens-set #f test-elisp-env =>env-lexical-mode?!)
(env-push-new-elstkfrm!
 test-elisp-env 3
 (list (new-symbol "three" 3)
       (new-symbol "four"  4)
       (new-symbol "five"  5)))

(test-equal (list 3 4 5)
  (let ((lookup
         (lambda (name)
           (view
            test-elisp-env
            (=>env-symbol! name)
            =>sym-value*!)))
        )
    (list
     (lookup "three")
     (lookup "four")
     (lookup "five")
     )))

(lens-set #t test-elisp-env =>env-lexical-mode?!)

(test-end "gypsum_elisp_eval_environment_tests")

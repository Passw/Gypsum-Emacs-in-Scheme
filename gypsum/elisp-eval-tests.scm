(import
  (scheme base)
  (scheme case-lambda)
  (srfi 64) ;;testing
  ;;(srfi  1) ;;lists
  (only (scheme file) open-input-file open-binary-input-file)
  (only (srfi 111) box box? unbox set-box!)
  (gypsum elisp-eval)
  (gypsum lens)
  (gypsum pretty)
  (only (gypsum cursor) new-cursor cursor-step!)
  (only (gypsum match)
        run-matcher run-matcher/cc
        matcher-state-type?
        match-success-type?
        match-fail-type?
        get-returned-value
        =>matcher-state-input!
        =>matcher-state-output!
        )
  (only (gypsum lens vector)
        mutable-vector-type? =>mvector-index!
        new-mutable-vector
        mutable-vector-length
        mutable-vector-append!)
  (only (gypsum elisp-eval parser) read-elisp)
  )
(cond-expand
  ((or guile gambit)
   (import
     (only (srfi 69) hash-table-size hash-table-ref/default))
   )
  ((mit))
  (else
   (import (srfi 125))))


(test-begin "gypsum_elisp_eval_tests")

;;--------------------------------------------------------------------------------------------------

(define test-elisp-env (new-elisp-env))

(test-assert (elisp-environment-type? test-elisp-env))

(env-intern!
 test-elisp-env
 '("hello" . "Hello, world!"))

(test-assert "Hello, world!"
  (view (env-intern-soft test-elisp-env "hello") =>sym-value!))

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

(define (elstkfrm-trial syms opts rest args)
  (let ((func (new-lambda)))
    (lens-set (map symbol->string syms) func =>lambda-args!)
    (lens-set (map symbol->string opts) func =>lambda-optargs!)
    (lens-set (if rest (symbol->string rest) #f) func =>lambda-rest!)
    (set! test-elstkfrm (elstkfrm-from-args func args))
    #t))


;;(define (elstkfrm-expect assocs stk-count stk-error)
;;  
;;  )
;;
;;
;;(define (elstkfrm-expect-not-enough)
;;  (elstkfrm-expect #f #f "not enough arguments"))
;;
;;(define (elstkfrm-expect-too-many)
;;  (elstkfrm-expect #f #f "too many arguments"))
;;
;;(elstkfrm-trial '() '() #f '())
;;(test-assert (elstkfrm-expect '() 0 #f))
;;
;;(elstkfrm-trial '(zero one) '() #f '(0 1))
;;(test-assert (elstkfrm-expect '((one . 1) (zero . 0)) 2 #f))
;;
;;(elstkfrm-trial '(zero one) '() #f '())
;;(test-assert (elstkfrm-expect-not-enough))
;;
;;(elstkfrm-trial '(zero one) '() #f '(0))
;;(test-assert (elstkfrm-expect-not-enough))
;;
;;(elstkfrm-trial '(zero one) '() #f '(0 1 2))
;;(test-assert (elstkfrm-expect-too-many))
;;
;;(elstkfrm-trial '(zero one) '(two) #f '(0 1 2))
;;(test-assert
;;    (elstkfrm-expect
;;     '((two . 2) (one . 1) (zero . 0)) 3 #f)
;;  )
;;
;;(elstkfrm-trial '(zero one) '(two three) #f '(0 1 2))
;;(test-assert
;;    (elstkfrm-expect
;;     '((two . 2) (one . 1) (zero . 0)) 3 #f)
;;  )
;;
;;(elstkfrm-trial '(zero one) '(two three) #f '(0 1 2 3))
;;(test-assert
;;    (elstkfrm-expect
;;     '((three . 3) (two . 2) (one . 1) (zero . 0)) 4 #f)
;;  )
;;
;;(elstkfrm-trial '(zero one) '(two three) #f '(0 1 2 3 4))
;;(test-assert (elstkfrm-expect-too-many))
;;
;;(elstkfrm-trial '(zero one) '(two three) 'rest '(0 1 2 3 4))
;;(test-assert
;;    (elstkfrm-expect
;;     '((rest 4) (three . 3) (two . 2) (one . 1) (zero . 0)) 5 #f))
;;
;;(elstkfrm-trial '() '(zero one) #f '())
;;(test-assert (elstkfrm-expect '() 0 #f))
;;
;;(elstkfrm-trial '() '(zero one) #f '(0))
;;(test-assert (elstkfrm-expect '((zero . 0)) 1 #f))
;;
;;(elstkfrm-trial '() '(zero one) #f '(0 1))
;;(test-assert (elstkfrm-expect '((one . 1) (zero . 0)) 2 #f))
;;
;;(elstkfrm-trial '() '(zero one) #f '(0 1 2))
;;(test-assert (elstkfrm-expect-too-many))
;;
;;(elstkfrm-trial '() '(zero one) 'rest '(0 1 2))
;;(test-assert (elstkfrm-expect '((rest 2) (one . 1) (zero . 0)) 3 #f))
;;
;;(elstkfrm-trial '() '(zero one) 'rest '(0 1 2 3))
;;(test-assert (elstkfrm-expect '((rest 2 3) (one . 1) (zero . 0)) 3 #f))


;;--------------------------------------------------------------------------------------------------

(define test-elisp-progn-var-scope-test
  '(progn
     (message "------------------------------")
     (setq glo "top")
     (defun printglo (who) (message (format "%s: glo = %s" who glo)))
     (defun runfn (sym)
       (message (format-message "--begin-- %s" sym))
       (funcall sym)
       (message (format-message "----end-- %s" sym))
       )
     (defun fn-A ()
       (printglo 'fn-A)
       (setq glo "in-fn-A")
       (printglo 'fn-A)
       )
     (defun fn-B ()
       (printglo 'fn-B)
       (let ((glo "in-fn-B"))
         (printglo 'fn-B-let1)
         (runfn 'fn-A)
         (printglo 'fn-B-let2)
         (setq glo "fn-B-after-setq")
         (printglo 'fn-b-let3)
         (runfn 'fn-A)
         (printglo 'fn-b-let4)
         )
       (printglo 'fn-B))
     (runfn 'fn-A)
     (printglo 'top)
     (setq glo "top")
     (printglo 'top-reset-A)
     (runfn 'fn-B)
     (printglo 'top)
     (message "------------------------------")
     t)
  )

(define lexical-scope-test-expected-result
  "------------------------------
--begin-- fn-A
fn-A: glo = top
fn-A: glo = in-fn-A
----end-- fn-A
top: glo = in-fn-A
top-reset-A: glo = top
--begin-- fn-B
fn-B: glo = top
fn-B-let1: glo = top
--begin-- fn-A
fn-A: glo = top
fn-A: glo = in-fn-A
----end-- fn-A
fn-B-let2: glo = in-fn-A
fn-b-let3: glo = in-fn-A
--begin-- fn-A
fn-A: glo = in-fn-A
fn-A: glo = in-fn-A
----end-- fn-A
fn-b-let4: glo = in-fn-A
fn-B: glo = in-fn-A
----end-- fn-B
top: glo = in-fn-A
------------------------------
")

(define dynamic-scope-test-expected-result
  "------------------------------
--begin-- fn-A
fn-A: glo = top
fn-A: glo = in-fn-A
----end-- fn-A
top: glo = in-fn-A
top-reset-A: glo = top
--begin-- fn-B
fn-B: glo = top
fn-B-let1: glo = in-fn-B
--begin-- fn-A
fn-A: glo = in-fn-B
fn-A: glo = in-fn-A
----end-- fn-A
fn-B-let2: glo = in-fn-A
fn-b-let3: glo = fn-B-after-setq
--begin-- fn-A
fn-A: glo = fn-B-after-setq
fn-A: glo = in-fn-A
----end-- fn-A
fn-b-let4: glo = in-fn-A
fn-B: glo = top
----end-- fn-B
top: glo = top
------------------------------
")


;; Raw results of evaluation
(define test-elisp-matcher-state #f)
(define test-elisp-eval-error #f)


(define (reset-elisp-eval)
  (set! test-elisp-env (new-elisp-env)))


(define (test-elisp-eval progn)
  (let-values (((st err) (elisp-eval! progn test-elisp-env)))
    (set! test-elisp-eval-error err)
    (when err (display err) (newline))
    (cond
     ((match-fail-type? st) (display st) (newline))
     ((match-success-type? st)
      (display (get-returned-value st)) (newline)
      (get-returned-value st))
     ((matcher-state-type? st)
      (set! test-elisp-env (view st =>matcher-state-input!))
      (set! test-elisp-matcher-state st)
      (view st =>matcher-state-output!)))
    ))


(define (test-elisp-reset-env)
  (set! test-elisp-env (new-elisp-env)))


(test-equal "hello" (test-elisp-eval "hello"))

;; TODO: create an empty `<INTERP-STATE>` and use it to run some of
;; the evaluators, starting with `ELSTKFRM-FROM-ARGS`.


(test-eqv 3 (test-elisp-eval '(+ 1 2)))

(test-eqv 5
  (test-elisp-eval
   '(progn
     (setq a 2)
     (setq b 3)
     (+ a b)
     )))

(test-eqv 8
  (test-elisp-eval
   '(progn
     (setq a 3)
     (setq b 5)
     (setq c (+ a b))
     c)))

(test-eqv 13
  (test-elisp-eval
   '(let*((a 5) (b 8)) (+ a b))))

(test-eqv 21
  (test-elisp-eval
   '(let*((a 8) (b (+ 5 a))) (+ a b))))

;;--------------------------------------------------------------------------------------------------

(test-end "gypsum_elisp_eval_tests")

;;--------------------------------------------------------------------------------------------------

(define *verbose* (make-parameter 1))

(define (file-read-all-forms bx filepath)
  (let*((verbose (*verbose*))
        (mutvec (unbox bx))
        (mutvec (if mutvec mutvec (new-mutable-vector 64))))
    (call-with-port (open-input-file filepath)
      (lambda (port)
        (let loop ()
          (let ((form (read-elisp port)))
            (cond
             ((eof-object? form) mutvec)
             (else
              (mutable-vector-append! mutvec form)
              (when (> verbose 1)
                (pretty (print (mutable-vector-length mutvec) ": " form (line-break))))
              (loop)
              ))))))
    (when (> verbose 0)
      (pretty
       (print
        ";;read " (mutable-vector-length mutvec)
        " forms from " (qstr filepath) (line-break)
        )))
    (set-box! bx mutvec)
    (values)
    ))

(define *subr.el* (box #f))
(define *map.el* (box #f))
(define *pp.el* (box #f))

(define (load-subr-forms) (file-read-all-forms *subr.el* "./elisp/subr.el"))
(define (load-map-forms) (file-read-all-forms *map.el* "./elisp/subr.el"))
(define (load-pp-forms) (file-read-all-forms *pp.el* "./elisp/pp.el"))

(define (make-indexer loader b)
  (lambda (i)
    (unless (unbox b) (loader))
    (let ((b (unbox b)))
      (if (>= i (mutable-vector-length b)) #f
          (view b (=>mvector-index! i)))
      )))

(define subr.el (make-indexer load-subr-forms *subr.el*))
(define map.el (make-indexer load-map-forms *subr.el*))
(define pp.el (make-indexer load-pp-forms *pp.el*))

(define current-document #f)

(define (edit bx filepath)
  (let ((result (file-read-all-forms bx filepath)))
    (cond
     ((and result (mutable-vector-type? result)) (set! current-document result))
     (else (error "file-read-all-forms returned non-mutable-vector type object" result)))
    ))

(define current-form #f)

(define (select-form form.el linenum)
  (let ((form (form.el linenum)))
    (set! current-form (new-cursor form))
    (display ";;cursor is now inspecting form:\n")
    (write form) (newline)
    ))



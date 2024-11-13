(import
  (scheme base)
  (gypsum lens)
  (gypsum match)
  (srfi 64)
  )

(test-begin "gypsum_match")


(define push-stack (put-with cons))
(define increment (next (lambda (n) (+ 1 n))))
(define dbgin (/input (lambda (in) (display "input: ")(write in)(newline) skip)))
(define dbgout (/output (lambda (out) (display "outpu: ")(write out)(newline) skip)))

(test-equal '(6)
  ;; Testing `CHECK`, `NEXT`, and `PUT`
  (run-matcher 5 '()
   (check odd?)
   increment
   push-stack
   ))

(test-equal "!! match failed !!"
  ;; Testing `EITHER`
  (match-fail-message
   (run-matcher 'Z '()
    (either
     (check (lambda (s) (eq? s 'A)))
     (fail "that didn't work")
     (check (lambda (s) (eq? s 'B)))
     (fail "that was no good either")
     (check (lambda (s) (eq? s 'C)))
     (fail "!! match failed !!")
     ))))


(define test-into
  (try increment ;; state is 1
       push-stack        ;; output is (1)
       (into =>self      ;; (into =>self ...) copies input onto internal stack
         (try increment  ;; state is 2
              push-stack ;; output is (2 1)
         ))              ;; internal stack is popped, state is 1
       push-stack ;; output is (1 2 1)
       (success)
       )
  )

(test-equal '(1 2 1) (run-matcher 0 '() test-into))

(define test-derive-check
  (either
   (try (check> 0) (put #t))
   (put #f))
  )

(test-assert (run-matcher 1 '() test-derive-check))

(define test-monad-apply
  ;; Testing `MONAD-APPLY` and `RETURN`
  (monad-apply
   (lambda (x y) (put (+ (* x x) (* y y))))
   (try (check-eq car 'X) (next cdr) (check car number?) (return car))
   (try (next cdr) (check-eq car 'Y) (next cdr) (check car number?) (return car))
   ))

(test-equal 25 (run-matcher '(X 3 Y 4) #f test-monad-apply))

(define (test-effect)
  (call-with-port (open-output-string)
    (lambda (port)
      (run-matcher
       0 '()
       (either
        (try increment
             (effect (lambda _ (display "one " port) #f)))
        (try increment
             (effect (lambda _ (display "two " port) (fail))))
        (try increment
             (effect (lambda (st) (write (view st =>matcher-state-input!) port) (values))))
        ))
      (get-output-string port))))


(test-equal "one two 1" (test-effect))


(define (make-collatz pause-if)
  ;; Testing `TRY`, `EITHER`, `CHECK`, `SUCCESS`, `SKIP`, `/INPUT`, `PAUSE`
  (let ((maybe-pause
         (cond
          ((procedure? pause-if)
           (either
            (try (check (lambda i (apply pause-if i)))
                 (/input pause))
            skip))
          ((integer? pause-if)
           (either
            (try (check (lambda (i) (= i pause-if)))
                 (/input (lambda (i) (pause i))))
            skip))
          (else skip)
          ))
        )
    (many
     push-stack
     maybe-pause
     (either
      (try (check (lambda (n) (<= n 1))) (success))
      (try (check odd?) (next (lambda (n) (+ 1 (* 3 n)))))
      (try (check even?) (next (lambda (n) (quotient n 2))))
      (fail "not an integer")
      )))
  )

(define collatz (make-collatz #f))

(define (test-collatz init) (run-matcher init '() collatz))

(test-equal '(1 2 4 8 16 5 10 20 40 13 26 52 17 34 11)
  (test-collatz 11))

(define collatz-31-result
  '(1 2 4 8 16 5 10 20 40 80 160 53 106 35 70 23 46 92 184 61 122
    244 488 976 325 650 1300 433 866 1732 577 1154 2308 4616 9232
    3077 6154 2051 4102 1367 2734 911 1822 3644 7288 2429 4858 1619
    3238 1079 2158 719 1438 479 958 319 638 1276 425 850 283 566
    1132 377 754 251 502 167 334 668 1336 445 890 1780 593 1186 395
    790 263 526 175 350 700 233 466 155 310 103 206 412 137 274 91
    182 364 121 242 484 161 322 107 214 71 142 47 94 31))

(test-equal collatz-31-result (test-collatz 31))

(define collatz/pause (make-collatz 9232))

(define (test-collatz/cc init) (run-matcher/cc init '() collatz/pause))

;; Testing `PAUSE`, `CAN-RESUME?`, `RUN-RESUME`.
(let*-values
    (((st result) (test-collatz/cc 31))
     ((results-list)
      (list
       (can-resume? st)
       result
       (let() (set! st (run-resume st)) (can-resume? st))
       ))
     )
  ;;(display "results: ")(write results-list)(newline)
  (test-equal '(#t (9232) #f) results-list)
  (test-equal collatz-31-result (view st =>matcher-state-output!))
  )


(test-end "gypsum_match")

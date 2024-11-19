
;;--------------------------------------------------------------------------------------------------
;; Define the state of the pattern matcher monad

(define-record-type <matcher-state-type>
  ;; The matcher state takes an input, which is the data type being
  ;; inspected for matching, and an output, which is a result data
  ;; type being constructed.
  ;;------------------------------------------------------------------
  (make<matcher-state> in out istk cc play)
  matcher-state-type?
  (in    matcher-state-input   set!matcher-state-input)
  (out   matcher-state-output  set!matcher-state-output)
  (istk  matcher-state-stack   set!matcher-state-stack)
  (cc    matcher-state-cont    set!matcher-state-cont)
  (play  matcher-state-play    set!matcher-state-play )
  )

(define (matcher-state-copy st)
  (make<matcher-state>
   (matcher-state-input st)
   (matcher-state-output st)
   (matcher-state-stack st)
   (matcher-state-cont st)
   (matcher-state-play st)
   ))

(define =>matcher-state-input!
  (record-unit-lens matcher-state-input set!matcher-state-input))

(define =>matcher-state-stack!
  (record-unit-lens matcher-state-stack set!matcher-state-stack))

(define =>matcher-state-output!
  (record-unit-lens matcher-state-output set!matcher-state-output))

(define =>matcher-state-cont!
  (record-unit-lens matcher-state-cont set!matcher-state-cont))

(define =>matcher-state-play!
  (record-unit-lens matcher-state-play set!matcher-state-play))

;;--------------------------------------------------------------------------------------------------

(define-record-type <match-success-type>
  ;; Used to explicitly halt a matching computation with a successful
  ;; result.
  ;;------------------------------------------------------------------
  (make<match-success> return state)
  match-success-type?
  (return  get-returned-value)
  (state   get-returned-state)
  )

(define-record-type <match-fail-type>
  ;; This is not an error object that can be thrown, rather it is
  ;; useful for indicating why a pattern matcher failed, and can help
  ;; produce better messages for programmers.
  ;;------------------------------------------------------------------
  (make<match-fail>  message  failed-on-input  irritants)
  match-fail-type?
  (message          match-fail-message)
  (failed-on-input  match-fail-input)
  (irritants        match-fail-irritants)
  )

;;--------------------------------------------------------------------------------------------------
;; Define the monad itself.

(define-record-type <matcher-monad-type>
  (%monad proc)
  matcher-monad-type?
  (proc matcher-monad-proc)
  )

(define (%type-check predicate expected . actions)
  ;; Make sure all arguments to a procedure satisfy the
  ;; `MATCHER-MONAD-TYPE?` predicate.
  ;;------------------------------------------------------------------
  (cond
   ((null? actions) (values))
   ((predicate (car actions))
    (apply %type-check predicate expected (cdr actions)))
   (else (error (string-append "not a " expected) (car actions)))
   ))

(define (%ensure-all-monads . actions)
  (apply %type-check matcher-monad-type? "pattern matcher monad" actions))

(define (%run st m)
  ;; On every step, the state is inspected. The monad is applied if
  ;; the state satisfies the `MATCHER-STATE-TYPE?` predicate. If the
  ;; state is of any other type, the monad "fails" and that state is
  ;; returned as-is.  Combinators such as `either` will ignore
  ;; failures and try alternative monads if there are any available.
  (cond
   ((matcher-state-type? st) ((matcher-monad-proc m) st))
   ((match-success-type? st) st)
   ((match-fail-type? st) st)
   (else (error "(on bind) invalid state" m st))
   ))


(define (%run-matcher input cc final . procs)
  ;; Initializes the matcher state, applies `PROCS` to `TRY` to
  ;; construct the monad, and applies the monad and state to
  ;; `%RUN`. The `FINAL` is used to adapt the final number of values
  ;; returned based on the continuation from which this procedure was
  ;; called: `RUN-MATCHER` requires only one value be returned,
  ;; `RUN-MATCHER/CC` needs three `VALUES`.
  (let*((st (make<matcher-state> input #f '() cc #f))
        (result (%run st (apply try procs)))
        )
    (final result)
    ))


;;--------------------------------------------------------------------------------------------------
;; Exported procedures for starting a monadic computation, and
;; resuming it if it is ever `PAUSE`d.


(define (run-matcher input . procs)
  ;; Initialize a pattern matcher state with an input and an output.
  ;; There must any number of primitive matching actions (combinators)
  ;; applied after the first two arguments, these will all be applied
  ;; to the `TRY` procedure, and when the run completes, the output
  ;; state will be returned.
  ;;
  ;; NOTE: This matcher cannot be paused, or calling `PAUSE` will
  ;; result in an exception being raised. To run a matcher that can be
  ;; paused, use `RUN-MTCHER/CC`.
  ;;------------------------------------------------------------------
  (apply %run-matcher input #f
   (lambda (result)
     (cond
      ((matcher-state-type? result) (matcher-state-output result))
      ((match-success-type? result) (get-returned-value result))
      ((match-fail-type? result) result)
      (else (error "(on run) invalid state" result))
      ))
   procs))


(define (run-matcher/cc input . procs)
  ;; Initialize a pattern matcher state with an input and an output.
  ;; There must any number of primitive matching actions (combinators)
  ;; applied after the first two arguments, these will all be applied
  ;; to the `TRY` procedure and the result will be returned. This
  ;; procedure differs from `RUN-MACHER` in that it allows use of the
  ;; `PAUSE` monad. It always returns 2 values.
  ;; 
  ;;  1. the state of the matcher when `PAUSE` was evaluated, or when
  ;;     the matching program ended (regardless of sucess or failure).
  ;;     This object can be passed to `RUN-RESUME`.
  ;;
  ;;  2. is `#F` if the matcher ran to the end of its program, or is
  ;;     a list of values that were used to call `PAUSE` if `PAUSE`
  ;;     was called.
  ;;------------------------------------------------------------------
  (let*-values
      (((st args resume)
        (call/cc
         (lambda (cc)
           (apply
            %run-matcher input cc
            (lambda (st) (values st #f #f)) procs)
           )))
       )
    (values
     (if (matcher-state-type? st)
         (lens-set resume st =>matcher-state-play!)
         st)
     args)
    ))


(define (can-resume? st)
  ;; A predicate that inspects a pattern matcher state `ST` such as what
  ;; might have been returned by `RUN-MATCHER` or `RUN-MATCHER-CC`.
  ;; This predicate returns true if that matcher had been paused by a
  ;; call to `PAUSE`, and if so it can be resumed with `RUN-RESUME`.
  (cond
   ((matcher-state-type? st) (if (matcher-state-play st) #t #f))
   ((or (match-fail-type? st) (match-success-type? st)) #f)
   (else (error "not a valid matcher state" st))
   ))


(define (run-resume st)
  ;; If the given pattern matcher state `ST` can be resumed, as
  ;; determined by the `CAN-RESUME` procedure, this procedure resumes
  ;; the pattern matcher. If the `ST` cannot be resumed, it is
  ;; returned as-is.
  (cond
   ((matcher-state-type? st)
    (let ((play (matcher-state-play st)))
      (if play (play (lens-set #f st =>matcher-state-play!)) st)
      ))
   ((match-success-type? st) st)
   ((match-fail-type? st) st)
   (else (error "not a valid matcher state" st))
   ))


;;--------------------------------------------------------------------------------------------------
;; The monadic combinators (procedures that construct monadic procedures).


(define (try . actions)
  ;; This is a monadic form of the Scheme `and` statement, it could
  ;; also be called `do-and`.  This procedure constructs a pattern
  ;; matching procedure from a list of actions, each one being a
  ;; primitive action, each one will be evaluated in the order
  ;; given. If any one of them fail, the result returned by the failed
  ;; primitive is returned. Otherwise the updated matcher state is
  ;; returned.
  ;;------------------------------------------------------------------
  (apply %ensure-all-monads actions)
  (%monad
   (lambda (st)
     (let loop ((st st) (actions actions))
       (cond
        ((pair? actions)
         (let*((next (car actions))
               (st (%run st next))
               )
           (cond
            ((matcher-state-type? st) (loop st (cdr actions)))
            ((match-success-type? st) st)
            ((match-fail-type? st) st)
            (else (error "(try) invalid state" st))
            )))
        (else st)
        )))))


(define (either . actions)
  ;; This is the monadic form of the Scheme `or` statement, it could
  ;; also be called `do-or`.  This procedure consturcts a pattern
  ;; matching procedure from a list of actions, each one being a
  ;; primitive action, each one will be evaluated in the order given
  ;; until one of the actions returns a successful result. The first
  ;; action to not fail will have its result returned and no other
  ;; `ACTION`s will be tried after it. If all `ACTION`s fail, the last
  ;; failed result is returned.
  ;;------------------------------------------------------------------
  (apply %ensure-all-monads actions)
  (%monad
   (lambda (st)
     (let loop
         ((actions actions)
          (prev-fail
           (make<match-fail> "no match" (matcher-state-input st) '()))
          )
       (cond
        ((pair? actions)
         (let*((next (car actions))
               (new-st (%run (matcher-state-copy st) next))
               )
           (cond
            ((matcher-state-type? new-st) new-st)
            ((match-success-type? new-st) new-st)
            ((match-fail-type? new-st) (loop (cdr actions) new-st))
            (else (error "(either) invalid state" new-st))
            )))
        (else prev-fail)
        )))))


(define do-and
  ;; A synonym for `try`
  ;;------------------------------------------------------------------
  try)

(define do-or
  ;; A synonym for `either`
  ;;------------------------------------------------------------------
  either)


(define (many . repeat)
  ;; Given a sequence of sequence of actions, these actions are
  ;; repeated indefinitely until one of them fails. As soon as one
  ;; action fails, the state prior to the failed action is retuend.
  ;;------------------------------------------------------------------
  (apply %ensure-all-monads repeat)
  (%monad
   (lambda (st)
     (let loop ((this st) (prev st) (actions repeat))
       (cond
        ((null? actions) (loop this prev repeat))
        ((matcher-state-type? this)
         (loop (%run this (car actions)) this (cdr actions)))
        ((match-success-type? this) this)
        ((match-fail-type? this) prev)
        (else (error "(many) invalid state" this)))
       ))))


(define (effect proc)
  ;; Perform some side-effect. It is of course possible to insert
  ;; effectful statements such as `display` into the predicates or
  ;; procedures you pass to monads such as `CHECK` or `PUT`, but you
  ;; cannot pass a `DISPLAY` statement as an argument to a `TRY` or
  ;; `EITHER` monad because these will raise exceptions unless you
  ;; pass a monad as an argument. So the `EFFECT` monad is provided as
  ;; an clean and easy way to perform effectful computations (like
  ;; `DISPLAY` or `READ`) within the monadic computation.
  ;;
  ;; The argument `PROC` passed to this monad must be a procedure
  ;; which takes a single argument. The argument applied to `PROC`
  ;; when `EFFECT` evaluates will be the current matcher state which
  ;; you can modify with the `=>MATCHER-STATE-INPUT!` and
  ;; `=>MATCHER-STATE-OUTPUT!` lenses. When `PROC` returns, you have a
  ;; choice of which type of value to return:
  ;;
  ;;   - `<MATCHER-STATE-TYPE>` an updated state which become the
  ;;     current monadic state after `EFFECT` completes evaluation.
  ;;
  ;;   - `<MATCHER-MONAD-TYPE>` such as one constructed by `TRY`,
  ;;     `EITHER`, or `FAIL`. This monad will be evaluated immediately
  ;;     after the `EFFECT` completes evaluation.
  ;;
  ;;   - `#F` returns a monad constructed by `FAIL`.
  ;;
  ;;   - `#T` indicates that the `EFFECT` monad succeeded and so if it
  ;;     is one of the arguments to `TRY` the next element of the
  ;;     `TRY` should be tried, if it is an argument to `EITHER` the
  ;;     `ETHER` block should stop and return success.
  ;;------------------------------------------------------------------
  (%type-check procedure? "procedure" proc)
  (%monad
   (lambda (st)
     (call-with-values (lambda () (proc st))
       (lambda args
         (cond
          ((null? args) st)
          ((and (not (null? (cdr args))))
           (error "effect return more than one values" proc args))
          (else
           (let ((result (car args)))
             (cond
              ((matcher-monad-type? result) (%run st result))
              ((matcher-state-type? result) result)
              ((match-fail-type?    result) result)
              ((match-success-type? result) result)
              ((eq? result #f) (%run st (fail)))
              ((eq? result #t) st)
              (else (error "effect returned invalid data" proc result))
              )))))))))


(define (check . predicates)
  ;; This is a "guard" primitive. It returns monadic success or
  ;; failure based on the result of the applying the current input to
  ;; a predicate composed of the given `PREDICATES` applied to
  ;; `ENDO-VIEW` from the `(GYPSUM LENS)` library.
  ;;------------------------------------------------------------------
  (let ((predicate (apply endo-view predicates)))
    (%monad
     (lambda (st)
       (cond
        ((predicate (matcher-state-input st)) st)
        (else (%run st (apply fail "does not satisfy predicate" predicates)))
        )))
    ))

(define (is . predicates)
  ;; This is a "guard" primitive similar to `CHECK`, except that it
  ;; assumes the pattern matcher input state is a `CURSOR-OBJECT` and
  ;; applies the predicate to the value under the cursor, not to the
  ;; cursor object itself.
  ;;------------------------------------------------------------------
  (apply check cursor-ref predicates)
  )


(define next
  ;; Apply the functor to the current input, updating it. If no
  ;; functor arguments is given, it is assumed that the current input
  ;; is a `<CURSOR-TYPE>` (see the `(GYPSUM CURSOR)` library), and the
  ;; default functor `CURSOR-NEXT!` is applied.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (next (lambda (obj) (cursor-step! obj) obj)))
    (fs
     (%monad
      (lambda (st)
        (update
         (apply endo-view fs)
         st =>matcher-state-input!))))))


(define (put-with update-output)
  ;; This monadic procedure takes an ordinary procedure
  ;; `UPDATE-OUTPUT` which takes two arguemnts: (1) the current
  ;; matcher input and (2) the current matcher output. `UPDATE-OUTPUT`
  ;; must return a new value for the current matcher output.  See also
  ;; the `PUT` monad.
  ;;------------------------------------------------------------------
  (%type-check procedure? "procedure" update-output)
  (%monad
   (lambda (st)
     (let*((new-output
            (update-output
             (matcher-state-input st)
             (matcher-state-output st)))
           )
       (cond
        ((or (match-success-type? new-output)
             (match-fail-type? new-output)
             (matcher-state-type? new-output))
         (error "cannot evaluate monad in `PUSH procedure" update-output))
        (else (lens-set new-output st =>matcher-state-output!))
        )))
   ))


(define put-const
  ;; This monad is similar to `PUT-WITH` but ignores input and output
  ;; and simply stored the given value to the current output,
  ;; overwriting it.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (put-with (lambda (i o) o)))
    ((val) (put-with (lambda _ val)))))


(define (put . fs)
  ;; Using this monad requires that the input be a cursor. This monad
  ;; is similar to `PUT-WITH`, but applies the composition of all of
  ;; it's function arguments to the result of `CURSOR-REF` on the
  ;; current input cursor.
  ;;------------------------------------------------------------------
  (put-with (lambda (i o) ((apply endo-view fs) (cursor-ref i))))
  )


(define (next-then then-do . input-step)
  ;; This monad is useful when you want to take many inputs in
  ;; sequence and apply them as arguments to a function using
  ;; `MONAD-APPLY`. It is defined as:
  ;;
  ;; ```
  ;; (/input
  ;   (lambda previous-input
  ;;    (try (apply next input-step)
  ;;         (apply then-do previous-input))))
  ;; ```
  ;;
  ;; It is most useful when the `THEN-DO` argument is the primitive
  ;; `PUT` or `RETURN-CONST` monad, or a monad that evaluates one of
  ;; these primitives (the `THEN-DO` argument must be a monad).
  ;;
  ;; What this does is copies the current input out of the pattern
  ;; matcher state, advances the state to the next input element by
  ;; applying the given `INPUT-STEPS` to the `NEXT` monad, then
  ;; applies the copy of the previous input state to the `THEN-DO`
  ;; monad. If the `THEN-DO` monad is `PUT` then it overwrites
  ;; whatever output was there before. If the `THEN-DO` monad is
  ;; `RETURN-CONST` then the input is returned to the calling
  ;; continuation (which is necessarily either `MONAD-APPLY` or
  ;; `RUN-MATCHER`).
  ;;------------------------------------------------------------------
  (%type-check matcher-monad-type? "pattern matcher monad" then-do)
  (/input
   (lambda previous-input
     (try
      (apply next input-step)
      (apply then-do previous-input)
      )))
  )


(define skip
  ;; This matching function is a no-op (does nothing at all). This is
  ;; useful when you have a pattern where you want to do something
  ;; like `PUSH` or `NEXT` depending on one condition, and do nothing
  ;; on some other condition. It is also "useful" for creating
  ;; infinite loops that do nothing, for example if you evaluate
  ;; `(MANY SKIP)`. This is not a procedure, so you use it without
  ;; applying it, that is `(TRY SKIP)` as opposed to `(TRY (SKIP))`
  ;;------------------------------------------------------------------
  (%monad (lambda (st) st)))


(define (/output monad)
  ;; This "with output" procedure allows you to apply a monad directly
  ;; to the current output, which allows you to (for example) create
  ;; control flow conditions. You can, for example, create conditional
  ;; statements that decide which monadic action to do next based on
  ;; the values of the current output. When evaluated, a single value
  ;; (the current output) is applied to `MONAD`.
  ;;------------------------------------------------------------------
  (%monad (lambda (st) (%run st (monad (matcher-state-output st))))))


(define (/input monad)
  ;; Similar to the `/OUTPUT`, the "with input" procedure is applied
  ;; to the current input and returns a monadic action to perform,
  ;; allowing you to create conditional statements that decide which
  ;; monad to evaluate based on the current input. This is diferent
  ;; from `NEXT` which must return an updated input. You can apply
  ;; `NEXT` within the `MONAD` procedure passed to `/INPUT`, but you
  ;; cannot evaluate `/INPUT` as a procedure passed to `/INPUT`.
  ;;------------------------------------------------------------------
  (%monad (lambda (st) (%run st (monad (matcher-state-input st))))))


(define success
  ;; The `SUCCESS` monad is similar to `RETURN` but rather than
  ;; returning a value selected from the current input, `SUCCESS`
  ;; returns the current output. It can take zero or one arguments, if
  ;; one argument is applied it must be a procedure which is itself
  ;; applied to the current output to select a part of the output to
  ;; be returned as a result.
  ;;------------------------------------------------------------------
  (case-lambda
    (()
     (%monad
      (lambda (st) (make<match-success> (matcher-state-output st) st))))
    ((get . getters)
     (%monad
      (lambda (st)
        (make<match-success>
         ((apply endo-view get getters) (matcher-state-output st))
         st))))
    ))


(define fail
  ;; Halt matching, construct a useful message for programmers to see
  ;; why matching failed. The `EITHER` primitive will ignore these
  ;; messages if there are any alternative `TRY` branches that have
  ;; not been evaluated yet. If you want to fail completely, simply
  ;; raise an `ERROR`, or use `CALL/CC` to construct a non-local
  ;; return that can be used.
  ;;
  ;; You may call `FAIL` with no arguments; `(FAIL)` has the same
  ;; semantics as `(FAIL #F)`.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (fail #f))
    ((message . irritants)
     (%monad
      (lambda (st) (make<match-fail> message (matcher-state-input st) irritants))))))


(define return-if
  ;; The `RETURN-IF` monad is intended to be used in coordination with
  ;; the `MONAD-APPLY` monad. This monad is equivalent to:
  ;;
  ;; (try (check checker) (put selecter) (next stepper) (success))
  ;;
  ;; This checks the current input, places the current input to the
  ;; current output, and then steps the current input. After stepping,
  ;; matching is halted with the current output value as the return
  ;; value.
  ;;
  ;; If `RETURN-IF` is evaluated anywhere within a `MONAD-APPLY`
  ;; monad, `MONAD-APPLY` will capture the value that was returned and
  ;; apply that value to the procedure to which `MONAD-APPLY` is being
  ;; applied.
  ;;
  ;; See also the `RETURN-CONST` monad.
  ;;------------------------------------------------------------------
  (case-lambda
    ((checker) (return-if checker #f #f))
    ((checker selector) (return-if checker selector #f))
    ((checker selector stepper)
     (try
      (cond
       ((procedure? checker) (check checker))
       ((matcher-monad-type? checker) checker)
       (else (error "checker argument not a monad or procedure" checker))
       )
      (cond
       ((not selector) (put))
       ((procedure? selector) (put-with selector))
       ((matcher-monad-type? selector) selector)
       (else (error "selector argument not a monad or procedure" selector))
       )
      (cond
       ((not stepper) (next))
       ((procedure? stepper) (next stepper))
       ((matcher-monad-type? stepper) stepper)
       (else (error "stepper argument not a monad or procedure" stepper))
       )
      (success))
     )))


(define (return-const val)
  ;; Similar to `RETURN` but returns a constant value `VAL` and
  ;; ignores the current input and current output.
  ;;------------------------------------------------------------------
  (%monad (lambda (st) (make<match-success> val st))))


(define (monad-apply app-monad . arg-monads)
  ;; This procedure evaluates a series of `ARG-MONADS` given as
  ;; arguments and collects all results returned by `RETURN`. It then
  ;; applies each of the results to the given procedure
  ;; `APP-MONAD`. The `APP-MONAD` must evaluate to a monad, it is
  ;; common to wrap the applied result in a `PUT-CONST` or
  ;; `RETURN-CONST` monad constructor.  See the `RETURN` monad
  ;; documentation above for an example.
  ;;
  ;; For any one of the `ARG-MONADS`, if a monad does not `RETURN` any
  ;; values, the value of the current output when a monad completes
  ;; computing will be applied instead.
  ;;
  ;; IF any one of the `ARG-MONADS` evaluate to `fail`, it simply
  ;; returns the `FAIL` result. This allows you to use many
  ;; `MOAND-APPLY` monads within an `EITHER` monad and whichever one
  ;; succeds first can become the final result of evaluation, and the
  ;; `EITHER` monad takes care of backtracking to the state prior to
  ;; the failure.
  ;;
  ;; For an example of how to use `MONAD-APPLY`, refer to the
  ;; documentation for the `RETURN` monad.
  ;;------------------------------------------------------------------
  (%type-check procedure? "procedure" app-monad)
  (apply %ensure-all-monads arg-monads)
  (%monad
   (lambda (st)
     (let loop ((st st) (arg-monads arg-monads) (stack '()))
       (cond
        ((null? arg-monads)
         (%run st (apply app-monad (reverse stack))))
        (else
         (let*((proc   (car arg-monads))
               (result (%run st proc)))
           (cond
            ((matcher-state-type? result)
             (loop
              result
              (cdr arg-monads)
              (cons (matcher-state-output result) stack)))
            ((match-success-type? result)
             (loop
              (get-returned-state result)
              (cdr arg-monads)
              (cons (get-returned-value result) stack)))
            ((match-fail-type? result)
             result)
            (else (error "procedure did not evaluate to monad" proc))
            ))))))))


(define (pause . args)
  ;; This procedure makes use of continuations to pause evaluation of
  ;; a pattern matcher. The matcher must have been evaluated by
  ;; `RUN-MATCHER/CC` instead of `RUN-MATCHER`, otherwise it will
  ;; raise an exception.
  ;;------------------------------------------------------------------
  (%monad
   (lambda (st)
     (let*((values-to-cc (matcher-state-cont st)))
       (cond
        (values-to-cc
         (call/cc
          (lambda (resume)
            (values-to-cc
             (lens-set resume st =>matcher-state-play!)
             args resume))))
        (else (error "`PAUSE` not evaluated in call to `RUN-MATCH/CC`")))
       ))
   ))


;;--------------------------------------------------------------------------------------------------
;; Useful for inspecting particular data types


(define (into select monad)
  ;; This monad is to select an element of current input, using that
  ;; element as the current input temporarily. For example, if you are
  ;; inspecting an associative list, you can select on the `CAR` of
  ;; the current input by passing `CAR` as the `SELECT` argument to
  ;; select the cons cell at the head of the current list. The `MONAD`
  ;; argument applied to this function is then evaluated with the new
  ;; current input, for example `CHECK` will inspect the cons cell
  ;; selected by the given `SELECT` argument. When `MONAD` completes
  ;; evaluation, the previous current input is restored.
  ;;
  ;; The `SELECT` argument may also be a LENS, in which case it is
  ;; applied to `VIEW` with the current input.
  ;;------------------------------------------------------------------
  (%type-check matcher-monad-type? "procedure" monad)
  (let ((select
         (cond
          ((lens-type? select) (endo-view select))
          ((procedure? select) select)
          (else (error "not a procedure or lens" select))
          )))
    (%monad
     (lambda (st)
       (let*((st
              (update
               (lambda (stack)
                 (cons
                  (select (view st =>matcher-state-input!))
                  stack))
               st =>matcher-state-stack!))
             (result (%run st monad))
             )
         (cond
          ((matcher-state-type? result)
           (let-values
               (((result rec)
                 (update&view (lambda (stack) (values (cdr stack) (car stack)))
                  result =>matcher-state-stack!))
                 )
             (lens-set rec result =>matcher-state-input!)))
          (else result))
         )))))

;;--------------------------------------------------------------------------------------------------

(define (apply-deref deref)
  (lambda (cur-input)
    (cond
     ((procedure? deref) (deref cur-input))
     ((lens-type? deref) (view cur-input deref))
     (else (error "not a dereferencing procedure or lens" deref))
     )))


(define (%derive-guard check/is equivalence)
  (case-lambda
    ((a) (check/is (lambda (b) (equivalence b a))))
    ((deref a) (check/is (lambda (b) (equivalence ((apply-deref deref) b) a))))
    ))


(define (derive-check equivalence)
  ;; The `MAKE-CHECK` procedure is used to define variants of the
  ;; `CHECK` procedure. The `MAKE-CHECK` procedure is used to derive
  ;; the procedures `CHECK-EQ`, `CHECK-EQV`, `CHECKE-EQUAL` and so
  ;; on. All of the procedures constructed by `MAKE-CHECK` take one or
  ;; two arguments:
  ;;
  ;;  - passing just one argument will mean to apply that argument to
  ;;    the equivalence along with the current input of the pattern
  ;;    matcher. For example, `(CHECK-EQUAL "yes")` will create a
  ;;    `CHECK` monad that tests if the current input is `EQUAL?` to
  ;;    the string `"true"`.
  ;;
  ;;  - pass two arguments: (1) a dereferencer and (2) an equivalence
  ;;    function.  This is similar to calling `MAKE-CHECK` with 1
  ;;    argument, except that the current input is dereferenced before
  ;;    being applied to the current input. For example, evaluating
  ;;    the expression `(CHECK-EQUAL CAR 0)` will construct a `CHECK`
  ;;    monad that first applies `CAR` to the current input, then
  ;;    applies that and 0 as an argument to the `EQUAL?` equivalence.
  ;;
  ;;    The dereferencer is typically a procedure like `CAR`, but it
  ;;    may also be a value of one of three other types:
  ;;
  ;;    - `LENS-TYPE?` will dereference the current input by applying
  ;;      the `VIEW` operation to the given lens.
  ;;
  ;;    - `#T` will dereference the current input by applying
  ;;      `CURSOR-OBJECT` to the current input. Since the current
  ;;      input is often a cursor object, a convenient short-hand for
  ;;      dereferencing the object of the cursor was needed.
  ;;
  ;;    - `#F` will not dereference the current input, similar to
  ;;      passing only one argument.
  ;;
  ;; **Note:** that whatever `EQUIVALENCE` is supplied, the current
  ;; input is applied as the first argument. So the monad `(CHECK> 0)`
  ;; is checking if the current input is greater than zero.
  ;;------------------------------------------------------------------
  (%derive-guard check equivalence)
  )

(define check-eq    (derive-check eq?))
(define check-eqv   (derive-check eqv?))
(define check-equal (derive-check equal?))
(define check=      (derive-check =))
(define check>      (derive-check >))
(define check>=     (derive-check >=))
(define check<      (derive-check <))
(define check<=     (derive-check <=))

(define (derive-is equivalence)
  ;; Similar to `DERIVE-CHECK` except that the `EQUIVALENCE` is appled
  ;; to `IS` rather than `CHECK`.
  ;;------------------------------------------------------------------
  (%derive-guard is equivalence))

(define is-eq    (derive-is eq?))
(define is-eqv   (derive-is eqv?))
(define is-equal (derive-is equal?))
(define is=      (derive-is =))
(define is>      (derive-is >))
(define is>=     (derive-is >=))
(define is<      (derive-is <))
(define is<=     (derive-is <=))

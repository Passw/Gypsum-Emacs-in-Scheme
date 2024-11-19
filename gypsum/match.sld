(define-library (gypsum match)
  ;; A monadic pattern matcher that uses mostly pure and lazy
  ;; primitive functions to inspect complex data structures and
  ;; construct results. This library relies heavily upon the `(GYPSUM LENS)`
  ;; library in constructing the primitives.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme write)
    (only (gypsum lens)
          view endo-view
          update update&view lens-set
          lens-type? record-unit-lens
          )
    (only (gypsum cursor)
          cursor-object cursor-ref cursor-step!)
    )
  (export
   ;; ---------- The monadic combinators ----------
   ;; These all construct an object that satisfy the
   ;; `MATCHER-MONAD-TYPE?` predicate.
   try  either  many  check  next  next-then  into
   put  put-const  put-with  return-if  success
   /input  /output  effect  skip  fail  pause
   return-const  monad-apply
   matcher-monad-type?

   do-and ;; another name for `TRY`
   do-or  ;; another name for `EITHER`

   ;; ---------- Handy combinators -----------
   check-eq  check-eqv  check-equal
   check=  check>  check>=  check<  check<=
   derive-check

   is  is-eq  is-eqv  is-equal
   is=  is>  is>=  is<  is<=
   derive-is

   ;; ---------- The matcher state -----------
   ;; How to actually evaluate a matcher monad
   run-matcher  run-matcher/cc  can-resume?  run-resume

   =>matcher-state-input!
   =>matcher-state-output!
   matcher-state-stack
   matcher-state-type?
   match-success-type?  get-returned-value  get-returned-state
   match-fail-type?  match-fail-message  match-fail-input  match-fail-irritants

   ;; Handling match failures
   match-fail-type?
   match-fail-message
   match-fail-irritants
   match-fail-input

   )
  (include "match.scm")
  )

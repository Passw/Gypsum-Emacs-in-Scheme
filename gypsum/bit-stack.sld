(define-library (gypsum bit-stack)
  ;; A stack of bits packed into a bytevector. This is used by Emacs
  ;; Lisp Environment objects when tracking flags on stack frames.
  ;;------------------------------------------------------------------
  (import
    (scheme base)
    (scheme case-lambda)
    (only (srfi 60) bit-set? bitwise-and bitwise-ior bitwise-xor ash))
  (export
   new-bit-stack
   bit-stack-count
   bit-stack-push!
   bit-stack-look
   bit-stack-pop!
   bit-stack-ref
   )
  (include "bit-stack.scm"))

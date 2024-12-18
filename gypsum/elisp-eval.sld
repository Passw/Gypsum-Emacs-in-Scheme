(define-library (gypsum elisp-eval)
  ;; This library defines functions and record data types that are
  ;; essential to the operation of an Emacs Lisp interpreter. When
  ;; evaluating Emacs Lisp code, it is first translated to Scheme
  ;; code, or rather a list of Scheme forms. These Scheme forms are
  ;; then evaluated using the Scheme `EVAL` function. The APIs in this
  ;; library are always available to be used by `EVAL` when evaluating
  ;; Scheme forms that have been translated from Emacs Lisp. although
  ;; the actual Scheme `ENVIRONMENT` object used to `EVAL` Emacs Lisp
  ;; is defined in the `(GYPSUM ELISP)` library.
  (import
    (scheme base)
    (scheme eval)
    (scheme cxr)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (srfi 1) assq)
    (only (gypsum editor command) command-type? command-procedure)
    (only (gypsum compat) hash-table-empty?)
    (only (gypsum lens)
          unit-lens  record-unit-lens  lens
          lens-set  lens-set!  endo-view  view
          update  endo-update  update&view
          *default-hash-table-constructor*
          default-unit-lens-updater  default-unit-lens-setter
          =>canonical  =>view-only-lens  =>hash-key!  =>hash-key*!)
    (only (gypsum lens vector) mutable-vector-type?)
    (only (gypsum cursor)
          new-cursor  cursor-ref  cursor-step!
          cursor-end?  cursor-type?
          cursor-collect-list  new-cursor-if-iterable)
    (only (rapid match) match match* -> unquote guard)
    )

  (cond-expand
    ((or guile-3 gambit stklos)
     (import
       (only (srfi 69)
             hash-table?
             make-hash-table
             alist->hash-table
             hash-table-set!
             hash-table-delete!
             hash-table-ref/default
             string-hash)
       (only (srfi srfi-13)
             string-hash)
       ))
    (else))

  (export
   ;; Quoting Scheme literals
   elisp-quote-scheme  elisp-unquote-scheme

   ;; Converting data between Scheme and Elisp
   scheme->elisp  elisp->scheme  pure  pure*

   ;; The interpreter
   elisp-intern!  elisp-eval!

   ;; Environment objects
   new-environment  elisp-environment-type?
   *default-obarray-size*  *elisp-init-env*  elisp-reset-init-env!

   =>interp-cur!  =>interp-env!  =>interp-stk!
   =>env-obarray-key!
   
   ;; Symbol objects
   sym-type?  new-symbol
   =>sym-name  =>sym-value!  =>sym-function!  =>sym-plist!
   ensure-string

   ;; Function objects
   lambda-type?  new-lambda
   =>lambda-kind!  =>lambda-args!  =>lambda-optargs!  =>lambda-rest!
   =>lambda-docstring!  =>lambda-declares!  =>lambda-lexenv!  =>lambda-body!

   ;; Macro objects
   make<macro>  macro-procedure

   ;; Error objects
   elisp-eval-error-type?  =>elisp-eval-error-message  =>elisp-eval-error-irritants

   ;; Stack frames
   new-elstkfrm  stack-lookup
   =>elstkfrm-lexstack-key*!  =>elstkfrm-dynstack-key*!  =>elstkfrm*!
   =>elstkfrm-lexstack*!   =>elstkfrm-dynstack*!
   elstkfrm-from-args
   )

  (include "elisp-eval.scm")
  )

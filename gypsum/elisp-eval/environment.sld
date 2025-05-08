(define-library (gypsum elisp-eval environment)
  ;; This library defines functions and record data types that are
  ;; essential to the operation of an Emacs Lisp interpreter, but the
  ;; interpreter itself is not defined here. The interpreter includes
  ;; many built-in procedures which are defined across a few different
  ;; Scheme libraries, all of which import this library. The
  ;; interpreter itself is defined in the `(GYPSUM ELISP-EVAL)`
  ;; library.
  (import
    (scheme base)
    (scheme eval)
    (scheme cxr)
    (scheme case-lambda)
    (only (scheme write) display write)
    (only (srfi 1) assq)
    (only (gypsum editor command) command-type? command-procedure)
    (only (gypsum bit-stack)
          new-bit-stack
          bit-stack-push!
          bit-stack-pop!
          bit-stack-look)
    (only (gypsum hash-table)
          hash-table?
          hash-table-empty?
          make-hash-table
          alist->hash-table
          hash-table->alist
          hash-table-set!
          hash-table-delete!
          hash-table-ref/default
          hash-table-walk
          string-hash
          )
    (only (gypsum lens)
          unit-lens  record-unit-lens  lens
          lens-set  lens-set!  endo-view  view
          update  endo-update  update&view
          *default-hash-table-constructor*
          default-unit-lens-updater  default-unit-lens-setter
          =>canonical  =>view-only-lens  =>hash-key!  =>hash-key*!)
    (only (gypsum pretty) pretty print bracketed line-break qstr)
    (only (gypsum lens vector) mutable-vector-type?)
    (only (gypsum cursor)
          new-cursor  cursor-ref  cursor-step!
          cursor-end?  cursor-type?
          cursor-collect-list  new-cursor-if-iterable)
    (only (chibi match) match)
    )

  (export
   ;; Quoting Scheme literals
   elisp-quote-scheme-type?  elisp-quote-scheme
   elisp-unquote-scheme  elisp-quote-scheme-equal?
   elisp-unquoted-form  elisp-unquoted-form-type?
   elisp-unquoted-get-form  elisp-unquoted-form-equal?
   elisp-spliced-form?  elisp-backquoted-form?

   ;; Converting data between Scheme and Elisp
   scheme->elisp  elisp->scheme  elisp-null?
   pure  pure*  pure*-typed  pure*-numbers  pure-raw

   ;; Emacs Lisp constant symbols
   nil t

   ;; Environment objects
   new-empty-environment
   elisp-environment-type?
   env-push-trace!  env-pop-trace!  env-trace!
   env-push-new-elstkfrm!
   env-pop-elstkfrm!
   env-resolve-function
   =>env-symbol!
   env-intern!    ;; implements the ELisp `intern` function
   env-setq-bind! ;; implements the ELisp `setq` macro
   env-alist-defines!
   env-reset-stack
   print-stack-frames
   print-trace
   show-trace ;; prints both trace and stack-frames to standard output port
   *default-obarray-size*
   *elisp-input-port*
   *elisp-output-port*
   *elisp-error-port*

   =>interp-cur!  =>interp-env!  =>interp-stk!
   =>env-obarray-key!
   =>env-lexstack*!
   =>env-obarray*!
   =>env-lexical-mode?!
   =>env-stack-trace*!

   ;; Symbol objects
   sym-type?  sym-name  new-symbol  new-symbol-value
   =>sym-value*!  =>sym-function*!  =>sym-plist*!
   =>sym-name  =>sym-value!  =>sym-function!  =>sym-plist!
   ensure-string  symbol/string?  any-symbol?
   sym-name-equal?

   ;; Function objects
   lambda-type?  new-lambda  lambda-copy-into!
   =>lambda-kind!  =>lambda-args!
   =>lambda-optargs!  =>lambda-rest!
   =>lambda-docstring!  =>lambda-declares!
   =>lambda-lexenv!  =>lambda-body!
   =>lambda-declares*!  =>lambda-interactive*!
   =>lambda-body*!  =>lambda-kind*!
   =>lambda-docstring*!

   ;; Macro objects
   make<macro>  macro-type?
   macro-procedure  elisp-void-macro
   make<syntax> syntax-type? syntax-eval

   ;; Error objects
   raise-error-impl*  eval-raise  eval-error
   elisp-eval-error-type?
   =>elisp-eval-error-message
   =>elisp-eval-error-irritants
   print-trace  print-stack-frames

   ;; Stack frames
   new-elstkfrm  stack-lookup =>elstkfrm*!
   =>elstkfrm-lexstack-key*!
   =>elstkfrm-dynstack-key*!
   =>elstkfrm-lexstack*!
   =>elstkfrm-dynstack*!
   elstkfrm-from-args
   elstkfrm-sym-intern!
   )

  (include "environment.scm")
  )

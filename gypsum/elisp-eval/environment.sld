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
          =>car  =>canonical  =>view-only-lens
          =>hash-key!  =>hash-key*!
          )
    (only (gypsum pretty) pretty print bracketed line-break qstr)
    (only (gypsum lens vector) mutable-vector-type?)
    (only (gypsum lexer) source-file-location-type?)
    (only (gypsum elisp-eval parser)
          elisp-quote-scheme-type?
          write-parser-location
          )
    (only (chibi match) match)
    )

  (export
   ;; Converting data between Scheme and Elisp
   scheme->elisp  elisp->scheme  elisp-null?
   pure  pure*  pure*-typed  pure*-numbers  pure-raw

   ;; Emacs Lisp constant symbols
   nil t

   ;;----------------------------------------
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
   env-reset-stack!
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

   ;;----------------------------------------
   ;; Symbol objects
   sym-type?  sym-name  new-symbol  new-symbol-value
   =>sym-value*!  =>sym-function*!  =>sym-plist*!
   =>sym-name  =>sym-value!  =>sym-function!  =>sym-plist!
   ensure-string  symbol/string?  any-symbol?
   sym-name-equal?

   ;;----------------------------------------
   ;; Function objects
   lambda-type?  new-lambda  lambda-copy-into!
   =>lambda-kind!  =>lambda-args!
   =>lambda-optargs!  =>lambda-rest!
   =>lambda-docstring!  =>lambda-declares!
   =>lambda-lexenv!  =>lambda-body!
   =>lambda-declares*!  =>lambda-interactive*!
   =>lambda-body*!  =>lambda-kind*!
   =>lambda-docstring*!  =>lambda-location*!

   ;;----------------------------------------
   ;; Macro objects
   make<macro>  macro-type?
   macro-procedure  elisp-void-macro
   make<syntax> syntax-type? syntax-eval

   ;;----------------------------------------
   ;; Error objects
   raise-error-impl*  eval-raise  eval-error
   make<elisp-eval-error>
   elisp-eval-error-type?
   elisp-eval-error-equal?
   =>elisp-eval-error-message
   =>elisp-eval-error-irritants
   =>elisp-eval-error-stack-trace

   ;;----------------------------------------
   ;; Stack frames
   new-elstkfrm  stack-lookup =>elstkfrm*!
   =>elstkfrm-lexstack-key*!
   =>elstkfrm-dynstack-key*!
   =>elstkfrm-lexstack*!
   =>elstkfrm-dynstack*!
   elstkfrm-from-args
   elstkfrm-sym-intern!

   ;;----------------------------------------
   ;; Stack traces
   new-stack-trace-frame  stack-trace-frame-type?
   =>stack-trace-location*!  =>stack-trace-symbol*!
   =>stack-trace-func!  =>stack-trace-frames*!
   =>env-trace-location*!  =>env-trace-symbol*!
   =>env-trace-func*!  =>env-trace-frames*!
   write-stack-trace-frame
   print-stack-frame  print-all-stack-frames

   ;; A whole stack trace
   env-get-stack-trace
   elisp-stack-trace-type?
   elisp-stack-trace->vector
   elisp-stack-trace-ref
   write-elisp-stack-trace
   )

  (include "environment.scm")
  )

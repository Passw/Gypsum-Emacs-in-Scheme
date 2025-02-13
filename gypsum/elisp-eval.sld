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
    (scheme cxr)
    (scheme case-lambda)
    (only (scheme file) open-input-file)
    (only (scheme write) display write)
    (only (srfi 1) assq)
    (only (gypsum editor command) command-type? command-procedure)
    (only (gypsum hash-table) hash-table-empty? hash)
    (only (gypsum pretty) pretty print line-break)
    (only (gypsum lens)
          unit-lens  record-unit-lens  lens
          lens-set  lens-set!  endo-view  view
          update  endo-update  update&view
          *default-hash-table-constructor*
          default-unit-lens-updater  default-unit-lens-setter
          =>canonical  =>view-only-lens  =>encapsulate
          =>hash-key!  =>hash-key*!
          )
    (only (gypsum lens vector) mutable-vector-type?)
    (only (gypsum cursor)
          new-cursor  cursor-ref  cursor-step!
          cursor-end?  cursor-type?
          cursor-collect-list  new-cursor-if-iterable
          )
    (only (rapid match) match)
    (prefix (gypsum editor-impl) *impl/)
    (only (gypsum elisp-eval parser) read-elisp)
    (only (gypsum elisp-eval environment)
          elisp-quote-scheme-type?
          elisp-quote-scheme
          elisp-unquote-scheme
          scheme->elisp  elisp->scheme  elisp-null?
          pure  pure*  pure*-typed  pure*-numbers
          new-empty-environment   elisp-environment-type?  env-alist-defines!
          env-push-new-elstkfrm!   env-pop-elstkfrm!  env-trace!
          env-resolve-function   env-intern!   env-setq-bind!
          elstkfrm-from-args   elstkfrm-sym-intern!
          *default-obarray-size*
          *elisp-input-port*  *elisp-output-port*  *elisp-error-port*
          =>interp-cur!  =>interp-env!  =>interp-stk!
          =>env-obarray-key!   =>env-symbol!
          =>env-lexstack*!  =>env-obarray*!  =>env-lexical-mode?!
          sym-type?  sym-name  new-symbol  new-symbol-value
          =>sym-name  =>sym-value*!  =>sym-function*!  =>sym-plist*!
          =>sym-value!  =>sym-function!  =>sym-plist!
          ensure-string  symbol/string?  any-symbol?
          nil  t
          lambda-type?  new-lambda  lambda-copy-into!
          =>lambda-kind!  =>lambda-args!
          =>lambda-optargs!  =>lambda-rest!
          =>lambda-docstring!  =>lambda-declares!
          =>lambda-lexenv!  =>lambda-body!
          =>lambda-declares*!  =>lambda-interactive*!
          =>lambda-body*!  =>lambda-kind*!
          =>lambda-docstring*!
          make<macro>  macro-type?  macro-procedure  elisp-void-macro
          make<syntax> syntax-type? syntax-eval
          elisp-eval-error-type?  raise-error-impl*
          =>elisp-eval-error-message
          =>elisp-eval-error-irritants
          eval-raise  eval-error
          print-trace  print-stack-frames
          )
    (only (gypsum elisp-eval format) format format-to-port)
    (only (gypsum keymap)
          keymap  keymap-type?  keymap-layer
          =>keymap-label!
          =>keymap-layer-index!
          =>keymap-top-layer!
          )
    )

  (cond-expand
    ((or guile-3 gambit stklos)
     (import
       (only (srfi 69)
             hash-table?
             make-hash-table
             alist->hash-table
             hash-table->alist
             hash-table-set!
             hash-table-delete!
             hash-table-ref/default
             hash-table-walk
             string-hash)
       (only (srfi srfi-13)
             string-hash)
       ))
    (else))

  (export
   ;; Initializing environments
   new-environment  elisp-reset-init-env!

   ;; The interpreter
   elisp-eval!  elisp-load!  eval-iterate-forms
   =>elisp-symbol!

   ;; Re-exporting symbols from (GYPSUM ELISP-EVAL ENVIRONMENT):
   ;;------------------------------------------------------------

   ;; Quoting Scheme literals
   elisp-quote-scheme  elisp-unquote-scheme

   ;; Converting data between Scheme and Elisp
   scheme->elisp  elisp->scheme  elisp-null?  pure  pure*

   ;; Environment objects
   elisp-environment-type?  elisp-intern!   =>env-obarray-key!
   =>env-symbol!

   *the-environment*
   *default-obarray-size*
   *elisp-init-env*
   *elisp-input-port*
   *elisp-output-port*
   *elisp-error-port*
   
   ;; Symbol objects
   sym-type?  new-symbol
   =>sym-name  =>sym-value!  =>sym-function!  =>sym-plist!
   =>sym-value*!
   nil  t

   ;; Macro objects
   make<macro>  macro-procedure

   ;; Error objects
   elisp-eval-error-type?  =>elisp-eval-error-message  =>elisp-eval-error-irritants
   elisp-show-trace
   )

  (include "elisp-eval.scm")
  )

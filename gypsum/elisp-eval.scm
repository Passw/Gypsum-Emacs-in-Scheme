
;;--------------------------------------------------------------------
;; Quoting Scheme data for use in the elisp interpreter.

(define-record-type <elisp-quote-scheme-type>
  ;; This type is for wraps a Scheme value to be used directly in the
  ;; Elisp interpreter. This helps to prevents things like strings,
  ;; vectors, and list constants from being evaluated as lists in the
  ;; Elisp environment. Being wrapped in this data type indicates to
  ;; the Elisp interpreter/compiler that the value should be used
  ;; as-is.
  ;;------------------------------------------------------------------
  (elisp-quote-scheme scheme-value)
  elisp-quote-scheme-type?
  (scheme-value  elisp-unquote-scheme)
  )


;;--------------------------------------------------------------------
;; Symbol objects

(define-record-type <sym-type>
  ;; Symbol
  (make<sym> name value func plist)
  sym-type?
  (name   sym-name)
  (value  sym-value     set!sym-value)
  (func   sym-function  set!sym-function)
  (plist  sym-plist     set!sym-plist)
  )

(define nil (make<sym> "nil" '() #f '()))
(define t   (make<sym> "t"   #t  #f '()))


(define =>sym-name
  ;; The symbol name is the only read-only field. Setting or updating
  ;; this lens creates a copy of the symbol. **NOTE** that this is the
  ;; only symbol lens that does not have a bang (`!`) in the name.
  ;;------------------------------------------------------------------
  (let ((setter
         (lambda (sym name)
           (make<sym> name (sym-value sym) (sym-function sym) (sym-plist sym)))
         ))
    (unit-lens
     sym-name setter
     (default-unit-lens-updater sym-name setter)
     '=>sym-name)))

(define =>sym-value*! (record-unit-lens sym-value set!sym-value '=>sym-value*!))
(define =>sym-function*! (record-unit-lens sym-function set!sym-function '=>sym-function*!))
(define =>sym-plist*! (record-unit-lens sym-plist set!sym-plist '=>sym-plist*!))

(define (copy-symbol name sym)
  (make<sym> name (sym-value sym) (sym-function sym) (sym-plist sym)))


(define new-symbol
  (case-lambda
    ((name) (new-symbol name #f #f))
    ((name val) (new-symbol name val #f))
    ((name val func)
     (cond
      ((string? name) (make<sym> name val func '()))
      (else (error "not a string" name))))))


(define (sym-defun name func) (new-symbol name #f func))


(define (blank-symbol? sym)
  (not (or (sym-value sym)
           (sym-function sym)
           (sym-plist sym)))
  )

(define (canon-sym name =>lens label)
  (unit-lens
   (lambda (sym) (if sym (view sym =>lens) #f))
   (lambda (sym val) (lens-set val (if sym sym (new-symbol name)) =>lens))
   (lambda (upd sym) (update&view upd (if sym sym (new-symbol name)) =>lens))
   label))

(define (=>sym-value!    name) (canon-sym name =>sym-value*!    '=>sym-value!))
(define (=>sym-function! name) (canon-sym name =>sym-function*! '=>sym-function!))
(define (=>sym-plist!    name) (canon-sym name =>sym-plist*!    '=>sym-plist!))

(define (symbol/string? o) (or (symbol? o) (sym-type? o) (string? o)))
(define (any-symbol? o) (or (symbol? o) (sym-type? o)))

(define (ensure-string name)
  (cond
   ((string? name) name)
   ((symbol? name) (symbol->string name))
   (else (error "not a symbol or string" name))
   ))

;;--------------------------------------------------------------------
;; Built-in Macro types

(define-record-type <macro-type>
  (make<macro> proc)
  macro-type?
  (proc macro-procedure)
  )

;;--------------------------------------------------------------------
;; Lambdas of all kinds, including macros and built-ins

(define-record-type <lambda-type>
  (make<lambda> kind args optargs rest doc decls inter lexenv body)
  lambda-type?
  (kind     lambda-kind         set!lambda-kind)
  (args     lambda-args         set!lambda-args)
  (optargs  lambda-optargs      set!lambda-optargs)
  (rest     lambda-rest         set!lambda-rest)
  (doc      lambda-docstring    set!lambda-docstring)
  (decls    lambda-declares     set!lambda-declares)
  (inter    lambda-interactive  set!lambda-interactive)
  (lexenv   lambda-lexenv       set!lambda-lexenv)
  (body     lambda-body         set!lambda-body)
  )

(define =>lambda-kind*! (record-unit-lens lambda-kind set!lambda-kind '=>lambda-kind*!))
(define =>lambda-args*! (record-unit-lens lambda-args set!lambda-args '=>lambda-args*!))
(define =>lambda-optargs*! (record-unit-lens lambda-optargs set!lambda-optargs '=>lambda-optargs*!))
(define =>lambda-rest*! (record-unit-lens lambda-rest set!lambda-rest '=>lambda-rest*!))
(define =>lambda-docstring*! (record-unit-lens lambda-docstring set!lambda-docstring '=>lambda-docstring*!))
(define =>lambda-declares*! (record-unit-lens lambda-declares set!lambda-declares '=>lambda-declares*!))
(define =>lambda-interactive*! (record-unit-lens lambda-interactive  set!lambda-interactive '=>lambda-interactive*!))
(define =>lambda-lexenv*! (record-unit-lens lambda-lexenv set!lambda-lexenv '=>lambda-lexenv*!))
(define =>lambda-body*! (record-unit-lens lambda-body set!lambda-body '=>lambda-body*!))

(define (empty-lambda? o)
  (not (or (lambda-kind o)      
           (lambda-args o)      
           (lambda-optargs o)   
           (lambda-rest o)      
           (lambda-docstring o) 
           (lambda-declares o)  
           (lambda-lexenv o)    
           (lambda-body o)      
           ))
  )

(define new-lambda
  (case-lambda
    (() (new-lambda 'lambda))
    ((kind) (make<lambda> kind '() '() #f #f #f #f #f #f))
    ((kind args) (new-lambda kind args '() #f #f #f))
    ((kind args opts) (new-lambda kind args opts #f #f #f))
    ((kind args opts rest) (new-lambda kind args opts rest #f #f))
    ((kind args opts rest body) (new-lambda kind args opts rest body #f))
    ((kind args opts rest body docstr)
     (make<lambda> kind
      (map ensure-string args)
      (map ensure-string opts)
      (if rest (ensure-string rest) #f)
      docstr #f #f #f body))
    ))

(define (canon-lambda unit) (=>canonical unit new-lambda empty-lambda?))

(define =>lambda-kind! (canon-lambda =>lambda-kind*!))
(define =>lambda-args! (canon-lambda =>lambda-args*!))
(define =>lambda-optargs! (canon-lambda =>lambda-optargs*!))
(define =>lambda-rest! (canon-lambda =>lambda-rest*!))
(define =>lambda-docstring! (canon-lambda =>lambda-docstring*!))
(define =>lambda-declares! (canon-lambda =>lambda-declares*!))
(define =>lambda-lexenv! (canon-lambda =>lambda-lexenv*!))
(define =>lambda-body! (canon-lambda =>lambda-body*!))

;;--------------------------------------------------------------------

(define-record-type <elisp-eval-error-type>
  (make<elisp-eval-error> message irritants)
  elisp-eval-error-type?
  (message    elisp-eval-error-message    set!elisp-eval-message)
  (irritants  elisp-eval-error-irritants  set!elisp-eval-irritants)
  )


(define =>elisp-eval-error-message
  (record-unit-lens
   elisp-eval-error-message
   set!elisp-eval-message
   '=>elisp-eval-error-message))

(define =>elisp-eval-error-irritants
  (record-unit-lens
   elisp-eval-error-irritants
   set!elisp-eval-irritants
   '=>elisp-eval-error-irritants))


;;--------------------------------------------------------------------
;; The stack

(define new-elstkfrm
  (case-lambda
    ((size) (new-elstkfrm size '()))
    ((size bindings)
     (let ((elstkfrm (make-hash-table string=? string-hash #:weak #f size)))
       (let loop ((bindings bindings))
         (cond
          ((null? bindings) elstkfrm)
          (else
           (let ((sym (car bindings)))
             (hash-table-set! elstkfrm (ensure-string (sym-name sym)) sym)
             (loop (cdr bindings))
             ))))))))


(define (stack-lookup stack name)
  (let loop ((stack stack))
    (cond
     ((null? stack) #f)
     (else
      (let ((sym (view (car stack) (=>hash-key! name))))
        (if sym sym (loop (cdr stack)))))
     )))


(define (stack-bind-top updater whole-stack name)
  ;; Apply a new `<SYM-TYPE>`object to the given `UPDATER` procedure
  ;; which must two values, a `<SYM-TYPE>` object (or `#F`) and a
  ;; return value. Then modify the top-most hash table in the list
  ;; `WHOLE-STACK` so that the returned `<SYM-TYPE>` object is
  ;; associated to the `SYM-NAME` of that `<SYM-TYPE>` object. If the
  ;; updater returns only `#F` as the first value, no update is
  ;; performed on `WHOLE-STACK`. If `WHOLE-STACK` is updated, a new
  ;; first `CONS` cell is created even if the hash table pointer does
  ;; not change, `SET-CAR!` is not used.
  (let-values (((sym return) (updater (new-symbol name))))
   (cond
    ((sym-type? sym)
     (values
      (cons
       (lens-set sym (car whole-stack) (=>hash-key! name))
       (cdr whole-stack))
      return))
    ((not sym) (values whole-stack return))
    (else (error "updater returned non-sym-type value on name" name sym))
    )))


(define (stack-update! updater whole-stack name newsym)
  ;; The second value returned by the updater is wrapped in a list if
  ;; successful, is #f if lookup fails. If the name is not bound, the
  ;; `NEWSYM` thunk is applied to the same arguments that were applied
  ;; to this procedure. If `NEWSYM` is `#F` no updates take place and
  ;; `WHOLE-STACK` and `#F` are both returned as values.
  (let loop ((stack whole-stack))
    (cond
     ((null? stack)
      (when newsym (newsym))
      (values whole-stack #f))
     (else
      (let*((hash (car stack))
            (sym (view hash (=>hash-key! name)))
            )
        (cond
         ((not sym) (loop (cdr stack)))
         ((sym-type? sym)
          (let-values (((updated-sym return) (updater sym)))
            (unless (eq? sym updated-sym)
              (hash-table-delete! hash (sym-name sym))
              (hash-table-set! hash name sym))
            (values whole-stack (list return))
            ))
         (else
          (error "non-symbol object bound to stack variable" name sym)))
        )))))


(define (=>stack! name on-not-found)
  (let ((getter (lambda (stack) (stack-lookup stack name)))
        (updater (lambda (updater stack) (stack-update! updater stack name on-not-found)))
        )
    (unit-lens getter (default-unit-lens-setter updater) updater `(=>stack! ,name))
    ))


(define (elstkfrm-sym-intern! elstkfrm name val)
  (let*((name (ensure-string name))
        (obj (new-symbol name val))
        )
    (hash-table-set! elstkfrm name obj)
    obj))


(define (elstkfrm-zip-args syms opts rest args)
  ;; Construct an association list mapping symbols to
  ;; arguments. Returns three values: (1) the association list, (2)
  ;; the number of elements in the association list, an (3), an error
  ;; message if the pattern (mapping symbols to arguments) does not
  ;; match. If (3) is not `#F` then (1) and (2) must be `#F`. If (1)
  ;; and (2) are not `#F` then (3) must be `#F`.
  (let loop ((stack '()) (syms syms) (opts opts) (args args) (count 0))
    (cond
     ((null? args)
      (cond
       ((pair? syms)
        (values #f #f (make<elisp-eval-error> "not enough arguments" #f)))
       ((null? syms)
        (values stack count #f))
       (else (error "not a list" syms)))
      )
     ((pair? args)
      (cond
       ((pair? syms)
        (loop
         (cons (cons (car syms) (car args)) stack)
         (cdr syms) opts (cdr args) (+ 1 count))
        )
       ((null? syms)
        (cond
         ((pair? opts)
          (loop
           (cons (cons (car opts) (car args)) stack)
           '() (cdr opts) (cdr args) (+ 1 count))
          )
         ((null? opts)
          (cond
           (rest (values (cons (cons rest args) stack) (+ 1 count) #f))
           (else (values #f #f (make<elisp-eval-error> "too many arguments" #f)))
           ))
         (else (error "(optional bindings) not a list" opts))
         ))
       (else (error "(required bindings) not a list" syms))
       ))
     (else (error "(applied arguments) not a list" args)))
    ))


(define (elstkfrm-from-args func args)
  ;; Construct a stack frame by binding arguments values to the
  ;; argument symbol names in the givem `FUNC`, which must be a
  ;; `<LAMBDA-TYPE>`. The values are bound in the lexical (not
  ;; dynamic) scope.
  (let-values
      (((assocs count failed)
        (elstkfrm-zip-args
         (lambda-args func)
         (lambda-optargs func)
         (lambda-rest func)
         args)
        ))
    (cond
     (failed
      (lens-set (list func args) failed =>elisp-eval-error-irritants))
     (else
      (alist->hash-table assocs)))
    ))

;;--------------------------------------------------------------------

(define-record-type <elisp-environment-type>
  ;; This is the environment object used for the Emacs Lisp evaluator.
  ;; Use `NEW-ENVIRONMENT` to construct an object of this type.
  ;;------------------------------------------------------------------
  (make<elisp-environment> env dyn lex mode halt)
  elisp-environment-type?
  (env  env-obarray   set!env-obarray)  ;;environment (obarray)
  (dyn  env-dynstack  set!env-dynstack) ;;dynamically bound variable stack frames
  (lex  env-lexstack  set!env-lexstack) ;;lexically bound variable stack frames
  (mode env-lxmode    set!env-lxmode)   ;;lexical binding mode
  (halt env-fail-cc   set!env-fail-cc)  ;;continuation to `ELISP-EVAL!`
  )

(define =>env-dynstack*!
  (record-unit-lens env-dynstack set!env-dynstack '=>env-dynstack*!))

(define =>env-lexstack*!
  (record-unit-lens env-lexstack set!env-lexstack '=>env-lexstack*!))

(define =>env-obarray*!
  (record-unit-lens env-obarray set!env-obarray '=>env-obarray*!))

(define (=>env-obarray-key! name)
  (lens =>env-obarray*!
        (=>canonical (=>hash-key! name) new-empty-obarray hash-table-empty?)))

(define =>env-lexical-mode?!
  (record-unit-lens env-lxmode set!env-lxmode '=>env-lexical-mode?!))

(define =>env-fail-cc*!
  (record-unit-lens env-fail-cc set!env-fail-cc '=>env-fail-cc*!))

(define =>env-stack-lens*!
  ;; Select a lens for the lexical or dynamic variable stack depending
  ;; on the current lexical binding mode.
  (let ((getter
         (lambda (st)
           (if (env-lxmode st)
               (view st =>env-lexstack*!)
               (view st =>env-dynstack*!))))
        (updater
         (lambda (updater st)
           (if (env-lxmode st)
               (update&view updater st =>env-lexstack*!)
               (update&view updater st =>env-dynstack*!))))
        )
    (unit-lens
     getter
     (default-unit-lens-setter updater)
     updater
     '=>env-stack-lens*!)))


(define (env-pop-elstkfrm! st)
  (update
   (lambda (stack) (if (null? stack) '() (cdr stack)))
   st =>env-stack-lens*!))


(define (env-push-new-elstkfrm! st size bindings)
  ;; Inspect the lexical binding mode and push a new stack frame on
  ;; the appropriate stack (lexical or dynamic stack). Return the
  ;; empty stack frame that was pushed so it can be updated by the
  ;; calling procedure.
  (let ((elstkfrm (new-elstkfrm size bindings)))
    (update
     (lambda (stack) (cons elstkfrm stack))
     st =>env-stack-lens*!)
    elstkfrm))


(define (env-sym-lookup st name)
  (or (view st =>env-lexstack*! (=>stack! name #f))
      (view st =>env-dynstack*! (=>stack! name #f))
      (view st (=>env-obarray-key! name))
      ))


(define (env-sym-update updater st name)
  (let-values
      (((stack return)
        (update&view updater
         st =>env-lexstack*! (=>stack! name #f))
        ))
    (cond
     ((not return)
      (let-values
          (((stack return)
            (update&view updater
             st =>env-dynstack*! (=>stack! name #f))
            ))
        (cond
         ((not return)
          (update&view updater st (=>env-obarray-key! name))
          )
         (else (values st (car return)))
         )))
     (else (values st (car return)))
     )))


(define (=>env-symbol! name)
  ;; A lens that looks up a symbol in the lexical stack, dynamic
  ;; stack, or global obarray, and if non exist, a new symbol may be
  ;; interned into the global obarray if the next composed lens
  ;; returns a non-empty symbol object.
  (let ((getter (lambda (st) (env-sym-lookup st name)))
        (updater
         (lambda (updater st)
           (env-sym-update
            (lambda (updater obj)
              (if (not obj)
                  (updater (new-symbol name))
                  (updater obj)))
            st name)))
        )
    (unit-lens
     getter
     (default-unit-lens-setter updater)
     updater
     `(=>env-symbol ,name))
    ))


(define (env-dynstack-update updater st name newsym)
  ;; Part of the Elisp "SETQ" semantics. This procedure tries to
  ;; update just the dynamic variable stack. If there is no variable
  ;; bound to `NAME` then apply `UPDATER`, `ST`, and `NAME` to the
  ;; `NEWSYM` procedure. `NEWSYM` must return two values, the updated
  ;; `ST` and an arbitrary return value for the `UPDATE&VIEW` lens.
  (update&view updater st
    =>env-dynstack*!
    (=>stack! name (lambda () (newsym updater st name)))))


(define (env-stack-update updater st name newsym)
  ;; Part of the Elisp "SETQ" semantics. This procedure updates the
  ;; lexical variable stack, or if in dynamic binding mode, updating
  ;; the dynamic variable stack. If there is no variable bound to
  ;; `NAME` then apply `UPDATER`, `ST`, and `NAME` to the `NEWSYM`
  ;; procedure. `NEWSYM` must return two values, the updated `ST` and
  ;; an arbitrary return value for the `UPDATE&VIEW` lens.
  (if (env-lxmode st)
      (update&view
       updater st =>env-lexstack*!
       (=>stack! name (lambda () (env-dynstack-update updater st name newsym))))
      (env-dynstack-update updater st name newsym)))


(define (env-intern! updater st name)
  ;; Part of the Elisp "SETQ" semantics. Interns a new symbol in the
  ;; obarray, replacing it if it already exists (although this
  ;; procedure is only called by procedures that have already checked
  ;; if the symbol exists and is called when it does not
  ;; exist). Before interning the new symbol, the symbol is applied to
  ;; the `UPDATER` procedure passed as an argument to this procedure.
  (let-values
      (((sym return) (updater (new-symbol name))))
    (values
     (lens-set sym st (=>env-obarray-key! name))
     return)))


(define (env-setq-bind! st updater name)
  ;; This procedure implements the `SETQ` semantics. It tries to
  ;; update an existing symbol bound to `NAME` anywhere in the lexical
  ;; stack, the dynamic stack, or the global "obarray", but if no such
  ;; `NAME` is bound anywhere, a new symbol is initerned in the global
  ;; obarray.
  (env-stack-update updater st name env-intern!))


(define (hash-env-intern-soft hash name)
  (hash-table-ref/default hash (ensure-string name) #f))


(define (env-resolve-function st head)
  (let ((head
         (cond
          ((sym-type? head) head)
          ((symbol? head) (env-sym-lookup st (symbol->string head)))
          (else head)))
        )
    (cond
     ((sym-type? head) (sym-function head))
     (else head))
    ))


(define *default-obarray-size* 32749)
  ;; ^ At the time of this writing, the size of the `OBARRAY` object
  ;; in my Emacs was 15121. I am choosing a prime-number size here
  ;; close to a power of 2, that is 2^15, which is roughly double that
  ;; which I would need for my Emacs. This will make one element per
  ;; cell highly likely, with plenty of room to spare for many more
  ;; symbols.


(define new-empty-obarray
  (case-lambda
    (() (new-empty-obarray *default-obarray-size*))
    ((size) (make-hash-table string=? string-hash #:weak #f size))
    )
  )


(define new-empty-env
  (case-lambda
    (() (new-empty-env *default-obarray-size*))
    ((size)
     (make<elisp-environment>
      (new-empty-obarray size)
      '() '() #t #f))))


;;====================================================================
;; The publicly exported evaluator procedures.

(define *the-environment*
  (make-parameter (new-empty-env *default-obarray-size*)))


(define (elisp-intern! . assocs)
  ;; This procedure is exported, so mostly used by users of this
  ;; library to directly update an Emacs Lisp environment from within
  ;; a Scheme procedure without having to use `ELISP-EVAL!`. It takes
  ;; an arbitrary number of pair arguments (cons cells) associating a
  ;; symbol or string to an arbitrary value. This procedure then
  ;; creates a symbol for each value and stores the value into the
  ;; symbol. The value will be stored into the `=>SYM-VALUE!` field of
  ;; a `<SYM-TYPE>` object unless it is a `<LAMBDA-TYPE>`, a Scheme
  ;; procedure, or a `<MATCHER-MONAD-TYPE>`, in which case it is
  ;; stored into the `=>SYM-FUNCTION!`
  ;;------------------------------------------------------------------
  (let ((st (*the-environment*)))
    (for-each
     (lambda (pair)
       (let*-values
           (((name) (ensure-string (car pair)))
            ((st sym)
             (update&view
              (lambda (sym)
                (cond
                 (sym (values sym sym))
                 (else (let ((sym (new-symbol name))) (values sym sym)))
                 ))
              st (=>env-obarray-key! name)))
            ((val) (cdr pair))
            )
         (cond
          ((or (lambda-type? val)
               (procedure? val)
               (macro-type? val))
           (set!sym-function sym val))
          (else (set!sym-value sym val)))
         ))
     assocs)))


(define elisp-eval!
  ;; Evaluate an Emacs Lisp expression that has already been parsed
  ;; from a string into a list or vector data structure. The result of
  ;; evaluation is two values:
  ;;
  ;;  1. an `<MATCHER-STATE-TYPE>` with an `<ELISP-ENVIRONMENT-TYPE>`
  ;;     in the `=>MATCHER-STATE-INPUT!` field, to extract the value
  ;;     of the last evaluated Emacs Lisp sub-expression of the given
  ;;     `EXPR` argument, use `ELISP-EVAL->SCHEME`.
  ;;
  ;;  2. The exception that occurred, or `#F` if there was no
  ;;     exception.
  ;;------------------------------------------------------------------
  (case-lambda
    ((expr) (elisp-eval! expr (*the-environment*)))
    ((expr env)
     (call/cc
      (lambda (halt)
        (lens-set halt (*the-environment*) =>env-fail-cc*!)
        (let ((return (eval-form (scheme->elisp expr))))
          (lens-set #f (*the-environment*) =>env-fail-cc*!)
          return
          ))))))

;;====================================================================
;; The interpreting evaluator. Matches patterns on the program and
;; immediately executes each form or symbol.

(define (eval-push-new-elstkfrm! size bindings)
  ;; Inspect the lexical binding mode and push a new stack frame on
  ;; the appropriate stack (lexical or dynamic stack). Return the
  ;; empty stack frame that was pushed so it can be updated by the
  ;; calling procedure.
  (env-push-new-elstkfrm! (*the-environment*) size bindings))


(define (eval-pop-elstkfrm!)
  (env-pop-elstkfrm! (*the-environment*)))


(define (eval-sym-lookup sym)
  ;; This procedure is exported, so mostly used by users of this
  ;; library to lookup an Emacs Lisp environment symbol from within a
  ;; Scheme procedure without having to use `ELISP-EVAL!`. This
  ;; procedure will return an Emacs Lisp symbol object of type
  ;; `<SYM-TYPE>` or `#f` if nothing is bound to the symbol.
  (env-sym-lookup (*the-environment*) sym))


(define (eval-raise err-obj)
  (let ((halt (view (*the-environment*) =>env-fail-cc*!)))
    (halt err-obj)))


(define (eval-error message . irritants)
  (eval-raise (make<elisp-eval-error> message irritants)))


(define (eval-apply-proc func arg-exprs)
  ;; This is how built-in Scheme procedure are applied from within Emacs Lisp.
  (let loop ((arg-exprs arg-exprs) (arg-vals '()))
    (cond
     ((null? arg-exprs) (apply func (reverse arg-vals)))
     ((pair? arg-exprs)
      (let ((result (eval-form (car arg-exprs))))
        (loop (cdr arg-exprs) (cons result arg-vals))
        )))))


(define (eval-apply-lambda func args)
  ;; This is how Emacs Lisp-defined lambdas and functions are applied
  ;; from within Emacs Lisp. This procedure *DOES NOT* do macro
  ;; expansion regardless of the `LAMBDA-KIND` of the `FUNC` argument.
  (eval-apply-proc
   (lambda args
     (let*((elstkfrm (elstkfrm-from-args func args))
           (st (*the-environment*))
           (old-stack (env-lexstack st))
           (st (lens-set (list elstkfrm) st =>env-lexstack*!))
           (return (eval-progn-body (lambda-body func))) ;; apply
           )
       (lens-set old-stack st =>env-lexstack*!)
       return
       ))
   args))


(define (eval-apply-as-proc func)
  ;; Wraps a Elisp lambda application into a scheme procedure application
  (lambda args
    (cond
     ((lambda-type? func) (apply eval-apply-lambda func args))
     ((procedure? func)
      (scheme->elisp (apply func (map elisp->scheme args))))
     ((command-type? func)
      (scheme->elisp (apply (command-procedure func) (map elisp->scheme args))))
     (else (eval-error "wrong type argument" func #:expecting "function"))
     )))


(define (eval-bracketed-form head arg-exprs)
  ;; This is the actual `APPLY` procedure for Emacs Lisp. The `HEAD`
  ;; argument is resolved to a procedure, macro, command, or lambda.
  ;; If `HEAD` resolves to a lambda, and `LAMBDA-KIND` is `'MACRO`,
  ;; the macro is expanded by evaluating the result with `eval-form`
  ;; before a value is returned.
  (let ((func (env-resolve-function (*the-environment*) head))
        )
    (cond
     ((macro-type?   func) (apply (macro-procedure func) head arg-exprs))
     ((lambda-type?  func)
      (let ((return (eval-apply-lambda func arg-exprs)))
        (cond ;; macro expand (if its a macro)
         ((eq? 'macro (lambda-kind func)) (eval-form return))
         (else return)
         )))
     ((command-type? func) (eval-apply-proc (command-procedure func) arg-exprs))
     ((procedure?    func) (eval-apply-proc func arg-exprs))
     (else (eval-error "invalid function" head))
     )))


(define (eval-form expr)
  ;; This is where evaluation begins. This is the actual `EVAL`
  ;; procedure for Emacs Lisp.
  (match expr
    (() '())
    ((,head ,args ...) (eval-bracketed-form head args))
    (,literal
     (cond
      ((symbol? literal)
       (let ((return (eval-sym-lookup (symbol->string literal))))
         (cond
          ((not return) (eval-error "void variable" literal))
          ((sym-type? return) (sym-value return))
          (else return))))
      ((elisp-quote-scheme-type? literal) (elisp-unquote-scheme literal))
      (else literal)
      ))))


(define (eval-args-list arg-exprs)
  (let loop ((arg-exprs arg-exprs))
    (match arg-exprs
      (() '())
      ((,expr ,more ...)
       (let ((result (eval-form expr)))
         (if (elisp-eval-error-type? result) result
             (cons result (loop more)))
         )))))


(define (eval-progn-body exprs)
  (let loop ((exprs exprs))
    (match exprs
      (() '())
      ((,final) (eval-form final))
      ((,head ,more ...) (eval-form head) (loop more))
      )))

;;--------------------------------------------------------------------
;; Built-in macros
;;
;; Note that these macros always begin with a (next) statement. This
;; is because the macro evaluator for built-in macros does not skip
;; over the head of the form before evaluating the macro. This makes
;; built-in macro bahvior a bit more Scheme-like.

(define elisp-progn
  (make<macro>
   (lambda expr
     (eval-progn-body (cdr expr)))))

(define elisp-prog1
  (make<macro>
   (lambda expr
     (match (cdr expr)
       (() (eval-error "wrong number of arguments" "prog1" 0))
       ((,first ,more ...)
        (let ((return (eval-form first)))
          (eval-progn-body more)
          return
          ))))))

(define elisp-prog2
  (make<macro>
   (lambda expr
     (define (fail-nargs n)
       (eval-error "wrong number of arguments" "prog2" n))
     (match (cdr expr)
       (() (fail-nargs 0))
       ((,any) (fail-nargs 1))
       ((,first ,second ,more ...)
        (eval-form first)
        (let ((return (eval-form second)))
          (eval-progn-body more)
          return
          ))))))


(define (elisp-logic conjunction)
  (make<macro>
   (lambda exprs
     (let loop ((exprs (cdr exprs)))
       (match exprs
         (() (if conjunction #t '()))
         ((,final) (eval-form final))
         ((,next ,more ...)
          (let ((success (eval-form next)))
            (if conjunction
                (if (elisp-null? success) '() (loop more))
                (if (elisp-null? success) (loop more) #t))
            )))))))

(define elisp-and (elisp-logic #t))
(define elisp-or  (elisp-logic #f))


(define elisp-if
  (make<macro>
   (lambda exprs
     (match exprs
       (() (eval-error "wrong number of arguments" "if" 0))
       ((if ,cond-expr) (eval-error "wrong number of arguments" "if" 1))
       ((if ,cond-expr ,then-exprs ,else-exprs ...)
        (let ((result (eval-form cond-expr)))
          (if (not (elisp-null? result))
              (eval-form then-exprs)
              (eval-progn-body else-exprs)
              )))))))


(define elisp-cond
  (make<macro>
   (lambda exprs
    (define (eval-cond test body)
      (let ((result (eval-form test)))
        (if (not (elisp-null? result))
            (values #t (eval-progn-body body))
            (values #f #f))))
     (let loop ((exprs (cdr exprs)))
       (match exprs
         (() '())
         ((() ,more ...) (loop more))
         (((,cond-expr ,body ...) ,more ...)
          (let-values
              (((success return) (eval-cond cond-expr body)))
            (if success return (loop more))
            )))))))


(define elisp-while
  (make<macro>
   (lambda exprs
     (match (cdr exprs)
       (() (eval-error "wrong number of arguments" "while" 0))
       ((,cond-expr ,body ...)
        (let loop ()
          (let ((success (eval-form cond-expr)))
            (if (not (elisp-null? success))
                (let () (eval-progn-body body) (loop))
                '() ;; while forms always evaluate to nil
                ))))))))


(define elisp-dotimes
  (make<macro>
   (lambda exprs
     (define (fail-nargs n)
       (eval-error "wrong number of arguments" "dotimes" n))
     (match (cdr exprs)
       (() (fail-nargs 0))
       ((,any) (fail-nargs 1))
       ((() ,any ...) (eval-error "expecting variable name" "dotimes" '()))
       (((,var) ,any ...) (eval-error "expecting initial number" "dotimes" var))
       (((,var ,limit-expr ,final-exprs ...) ,body ...)
        (cond
         ((symbol? var)
          (let ((limit (eval-form limit-expr)))
            (cond
             ((integer? limit)
              (cond
               ((<= limit 0) (eval-progn-body final-exprs))
               (else
                (let*((st (*the-environment*))
                      (elstkfrm (env-push-new-elstkfrm! st 1 '()))
                      (obj (elstkfrm-sym-intern! elstkfrm (symbol->string var) 0))
                      )
                  (let loop ((n 0))
                    (cond
                     ((>= n limit)
                      (let ((return (eval-progn-body final-exprs)))
                        (env-pop-elstkfrm! st)
                        return
                        ))
                     (else
                      (lens-set n obj =>sym-value*!)
                      (eval-progn-body body)
                      (loop (+ n 1))
                      )))))))
             (else (eval-error "wrong type argument" "dotimes" limit #:expecting "integer"))
             )))
         (else (eval-error "wrong type argument" "dotimes" var #:expecting "symbol"))
         ))))))


(define elisp-dolist
  (make<macro>
   (lambda exprs
     (define (fail-nargs n)
       (eval-error "wrong number of arguments" "dolist" n))
     (define (run var list-expr result-expr body)
       (define (final) (if result-expr (eval-form result-expr) '()))
       (cond
        ((symbol? var)
         (let*((elems (eval-form list-expr)))
           (cond
            ((or (null? elems)
                 (and (vector? elems)
                      (>= 0 (vector-length elems))))
             (final)
             )
            ((or (pair? elems)
                 (and (vector? elems)
                      (< 0 (vector-length elems))))
             (let*((cur (new-cursor elems))
                   (st (*the-environment*))
                   (elstkfrm (env-push-new-elstkfrm! st 1 '()))
                   (obj (elstkfrm-sym-intern! elstkfrm (symbol->string var) 0))
                   )
               (let loop ()
                 (cond
                  ((cursor-end? cur)
                   (env-pop-elstkfrm! st)
                   (final))
                  (else
                   (lens-set (cursor-ref cur) obj =>sym-value*!)
                   (eval-progn-body body)
                   (cursor-step! cur)
                   (loop)
                   )))))
            (else
             (eval-error "wrong type argument" "dotimes" elems #:expecting "list")
             ))))
        (else (eval-error "wrong type argument" "dotimes" var #:expecting "symbol")))
       )
     (match (cdr exprs)
       (((,var ,list-expr) ,body ...)
        (run var list-expr #f body))
       (((,var ,list-expr ,result-expr) ,body ...)
        (run var list-expr result-expr body))
       (,any (eval-error "wrong number of arguments" "dolist" any))
       ))))


(define elisp-setq
  (make<macro>
   (lambda  args
     (let ((bind!
            (lambda (sym val)
              (cond
               ((symbol? sym)
                (env-setq-bind! ;; lookup the symbol object for `SYM`, or intern a new one.
                 (*the-environment*)
                 (lambda (sym-obj)
                   (values
                    (lens-set val sym-obj =>sym-value*!) ;; Update the interned symbol,
                    sym-obj))
                 (symbol->string sym)))
               (else
                (eval-error "wrong type argument, expecting symbol" sym)
                ))))
           (argc (length args))
           )
       (cond
        ((even? argc) ;; args includes "setq" symbol, argc should be odd
         (eval-error "wrong number of arguments, setq" (- argc 1))
         )
        (else
         (let loop ((args (cdr args)) (argc 0) (last-bound '()))
           (match args
             (() last-bound)
             ((,sym ,expr ,more ...)
              (let ((val (eval-form expr)))
                (cond
                 ((elisp-eval-error-type? val) val)
                 (else
                  (bind! sym val)
                  (loop more (+ 2 argc) val)
                  ))))))))))))


(define elisp-let
  (make<macro>
   (lambda expr
     (let ((st (*the-environment*)))
       (match (cdr expr)
         (() '())
         (((,bindings ...) ,progn-body ...)
          (let loop ((unbound bindings) (bound '()) (size 0))
            (match unbound
              (()
               (env-push-new-elstkfrm! st size (reverse bound))
               (let ((result (eval-progn-body progn-body)))
                 (env-pop-elstkfrm! st)
                 result))
              (((,sym) ,more ...)
               (loop more (cons (new-symbol sym '()) bound) (+ 1 size))
               )
              (((,sym ,expr) ,more ...)
               (let ((result (eval-form expr)))
                 (loop more
                       (cons (new-symbol (ensure-string sym) result) bound)
                       (+ 1 size))
                 ))
              (((,sym ,expr ,extra ...) ,more ...)
               (eval-error "bindings can have only one value form" (car unbound))
               )
              ((,sym ,more ...)
               (loop more (cons (new-symbol sym '()) bound) (+ 1 size))
               )
              (,otherwise
               (eval-error "wrong type argument, expecting list" otherwise)
               ))))
         (,otherwise
          (eval-error "wrong type argument, expecting list" otherwise)
          ))))))


(define elisp-let*
  (make<macro>
   (lambda expr
     (let ((st (*the-environment*)))
       (match (cdr expr)
         (() '())
         (((,bindings ...) ,progn-body ...)
          (let ((elstkfrm (env-push-new-elstkfrm! st (length bindings) '())))
            (let loop ((unbound bindings))
              (match unbound
                (()
                 (let ((result (eval-progn-body progn-body)))
                   (env-pop-elstkfrm! st)
                   result))
                (((,sym) ,more ...)
                 (elstkfrm-sym-intern! elstkfrm sym '())
                 (loop more)
                 )
                (((,sym ,expr) ,more ...)
                 (let ((result (eval-form expr)))
                   (elstkfrm-sym-intern! elstkfrm sym result)
                   (loop more)
                   ))
                (((,sym ,expr ,extra ...) ,more ...)
                 (eval-error "bindings can have only one value form" (car unbound))
                 )
                ((,sym ,more ...)
                 (elstkfrm-sym-intern! elstkfrm sym '())
                 (loop more)
                 )
                (,otherwise
                 (eval-error "wrong type argument, expecting list" otherwise)
                 )))))
         (,otherwise
          (eval-error "wrong type argument, expecting list" otherwise)
          ))))))


(define elisp-lambda
  (make<macro>
   (lambda expr
     (let ((expr (cdr expr)))
       (match expr
         (() (new-lambda))
         (((,args ...) ,body ...)
          (eval-defun-args-body 'lambda args body))
         (,args (eval-error "invalid lambda" args))
         )))))


(define elisp-defun-defmacro
  (make<macro>
   (lambda expr
     (let ((def  (car expr))
           (expr (cdr expr))
           )
       (match expr
         (() (eval-error "wrong number of arguments" '()))
         ((,sym) (eval-error "wrong number of arguments" sym))
         ((,sym (,args ...) ,body ...)
          (cond
           ((symbol? sym)
            (let*((func   (eval-defun-args-body def args body))
                  (name   (symbol->string sym))
                  (=>name (=>env-obarray-key! name))
                  (st     (*the-environment*))
                  (obj    (view st =>name))
                  (obj
                   (if obj obj
                       (let ((obj (new-symbol name)))
                         (lens-set! obj st =>name)
                         obj)))
                  )
              (cond
               ((eq? def 'defun) func)
               ((eq? def 'defmacro) (set!lambda-kind func 'macro) func)
               (else (error "expecting function head to be 'defun or 'defmacro" def))
               )
              (cond
               ((sym-type? obj) ;; update and return the interned symbol object
                (lens-set func obj =>sym-function*!))
               (else (eval-error "failed to intern symbol" sym obj)))
              ))
           (else (eval-error "wrong type argument, expected symbol" sym))
           ))
         ((,name ,args ,body ...) (eval-error "malformed arglist" args))
         )))))


(define (set-function-body! func body-exprs)
  ;; This procedure scans through a `BODY-EXPR` for `DECLARE` and
  ;; `INTERACTIVE` forms and both executes them, updating the given
  ;; `FUNC` in place, and also removes them from the `BODY-EXPRS` so
  ;; that the evaluator never executes them directly.
  (define (filter body-exprs)
    (match body-exprs
      (() '())
      (((declare ,args ...) ,body-exprs ...)
       (let loop ((args args))
         (match args
           (((,sym ,vals ...) ,args ...)
            (guard (symbol? sym))
            (lens-set vals
             func =>lambda-declares*! (=>hash-key! (symbol->string sym)))
            (loop args))
           ((,any ,args ...) ;; ignore malformed delcarations
            (loop args))
           ))
       (filter body-exprs)
       )
      (((interactive ,args ...) ,body-exprs ...)
       (lens-set args func =>lambda-interactive*!)
       (filter body-exprs)
       )
      ((,any-expr ,body-exprs ...) ;; anything else, leave it alone
       (cons any-expr (filter body-exprs)))
      ))
  (lens-set (filter body-exprs) func =>lambda-body*!)
  func
  )


(define (eval-defun-args-body kind arg-exprs body-expr)
  (define (non-symbol-error sym)
    (eval-error "invalid function, arguments declaration expect symbol" sym)
    )
  (define (get-body func)
    (match body-expr
      ((,docstr ,body-expr ...)
       (cond
        ((string? docstr)
         (set!lambda-docstring func docstr)
         (set-function-body! func body-expr)
         func)
        (else
         (set-function-body! func (cons docstr body-expr))
         func)))
      (,body-expr
       (set-function-body! func body-expr)
       func)
      ))
  (define (get-rest-args arg-exprs args optargs)
    (match arg-exprs
      (() (eval-error "invalid function, no argument declaration after &rest delimiter"))
      ((,sym)
       (cond
        ((symbol? sym)
         (get-body (new-lambda kind (reverse args) (reverse optargs) sym)))
        (else (non-symbol-error sym))
        ))
      ))
  (define (get-opt-args arg-exprs args optargs)
    (match arg-exprs
      (() (get-body (new-lambda kind (reverse args) (reverse optargs))))
      ((&optional ,more ...)
       (eval-error "invalid function, more than one &optional argument delimiter" arg-exprs))
      ((&rest ,arg-exprs ...)
       (get-rest-args arg-exprs args optargs))
      ((,sym ,arg-exprs ...)
       (cond
        ((symbol? sym) (get-opt-args arg-exprs args (cons sym optargs)))
        (else (non-symbol-error sym))
        ))
      ))
  (define (get-args arg-exprs args)
    (match arg-exprs
      (() (get-body (new-lambda kind (reverse args))))
      ((&optional ,arg-exprs ...) (get-opt-args arg-exprs args '()))
      ((&rest ,arg-exprs ...) (get-rest-args arg-exprs args '()))
      ((,sym ,arg-exprs ...)
       (cond
        ((symbol? sym) (get-args arg-exprs (cons sym args)))
        (else (non-symbol-error sym))
        ))
      ))
  (get-args arg-exprs '()))

;;--------------------------------------------------------------------------------------------------
;; Procedures that operate on procedures. These are called by the
;; evaluator after pattern matching determines that the correct number
;; and type of arguments have been supplied.

(define (view-on-symbol st name/sym viewer)
  (cond
   ((sym-type? name/sym) (viewer name/sym))
   ((symbol/string? name/sym)
    (view st (=>env-obarray-key! (symbol->string name/sym))))
   (else (eval-error "wrong type argument" name/sym #:expecting "symbol"))
   ))

(define (update-on-symbol st name/sym updater)
  ;; A procedure that operates on a symbol given it's name or the
  ;; `<SYM-TYPE>` object itself.
  (cond
   ((sym-type? name/sym)
    (let-values
        (((obj return) (updater name/sym)))
      return
      ))
   ((symbol/string? name/sym)
    (update&view
     updater st
     (=>env-obarray-key! (symbol->string name/sym))))
   ))

(define (eval-make-symbol name) (new-symbol (ensure-string name)))

(define (eval-symbol-name st sym)
  (view-on-symbol st sym (lambda (obj) (view obj =>sym-name))))

(define (eval-intern st name)
  (let-values
      (((obj _)
        (env-intern! (lambda (obj) (values obj obj)) st (ensure-string name))))
    obj
    ))

(define (eval-intern-soft st name)
  (view-on-symbol st name (lambda (obj) obj)))

(define (eval-unintern st name)
  (update&view
   (lambda (obj) (if (not obj) (values #f #f) (values #f #t)))
   (=>env-obarray-key! name)
   ))

(define eval-mapatoms
  (case-lambda
    ((func) (eval-mapatoms func (*the-environment*)))
    ((func st)
     (let ((hash (view st =>env-obarray*!)))
       (cond
        ((hash-table? hash)
         (hash-table-walk hash (lambda (_key obj) (func obj)))
         #f)
        (else #f)
        )))))

(define (eval-symbol-plist st sym)
  (update-on-symbol st sym (lambda (obj) (view obj (=>sym-plist! sym)))))

(define (plist-to-dict plist)
  (define (filter plist)
    (cond
     ((null? plist) '())
     ((pair? plist)
      (let*((assoc (car plist))
            (name (car assoc))
            (name (if (symbol? name) (symbol->string name) name))
            (val (cdr assoc))
            (next (lambda () (filter (cdr plist))))
            )
        (cond
         ((string? name) (cons (cons name val) (next)))
         (else (next))
         )))
     (else '())))
  (cond
   ((hash-table? plist) plist)
   ((pair? plist) (alist->hash-table (filter plist)))
   (else #f)
   ))

(define (eval-setplist st name plist)
  ;; Unlike Elisp, the type of `PLIST` is checked and a hash table is
  ;; constructed. Non-alist or non-hash-table values are silently
  ;; ignored.
  (let*((plist (plist-to-dict plist)))
    (when plist
      (update-on-symbol st name (lambda (obj) (lens-set plist obj (=>sym-plist! name)))))
    plist
    ))

(define (eval-set st sym val)
  ;; TODO: check if `SYM` satisfies `SYMBOL?` or `SYM-TYPE?` and act accordingly.
  (update-on-symbol st sym
   (lambda (obj) (values (lens-set val obj (=>sym-value! sym)) val))))

(define (eval-get st sym prop)
  (view-on-symbol st sym (lambda (obj) (view obj (=>hash-key! prop)))))

(define (eval-put st sym prop val)
  (update-on-symbol st sym
   (let ((prop (ensure-string prop)))
     (values (lens-set val st (=>env-obarray-key! sym) (=>hash-key! prop)) val)
     )))

(define (eval-boundp st sym)
  (view-on-symbol st sym (lambda (obj) (not (not (view obj (=>sym-value! sym)))))))

(define (eval-makunbound st sym)
  (update-on-symbol st sym
   (lambda (obj) (values (lens-set #f obj (=>sym-value! sym)) obj))))

(define (eval-symbol-function st sym)
  (view-on-symbol st sym (lambda (obj) (view obj (=>sym-function! sym)))))

(define (eval-fboundp st sym)
  (not (not (eval-symbol-function st sym))))

(define (eval-fset st sym func)
  (update-on-symbol st sym
   (lambda (obj) (values (lens-set (elisp->scheme func) obj =>sym-function!) func))))

(define (eval-fmakunbound st sym)
  (view-on-symbol st sym (lambda (obj) (view st (=>env-obarray-key! sym)))))

;;--------------------------------------------------------------------------------------------------
;; Interface between Scheme and Elisp

(define (elisp-null? val) (or (null? val) (not val)))

(define (scheme->elisp val)
  (define (replace val)
    (cond
     ((pair? val)
      (let ((head (car val)) (tail (cdr val)))
        (cond
         ((eq? head 'unquote)    (cons '|,| tail))
         ((eq? head 'quasiquote) (cons '|`| tail))
         (else val))))
     (else val)
     ))
  (let loop ((val (replace val)))
    (cond
     ((not   val) '())
     ((pair? val)
      (let ((head (car val)) (tail (cdr val)))
        (cons
         (if (pair? head) (scheme->elisp head) (loop head))
         (loop tail))))
     ((char? val) (char->integer val))
     (else val)
     )))


(define (elisp->scheme val)
  (define (replace val)
    (cond
     ((pair? val)
      (let ((head (car val)) (tail (cdr val)))
        (cond
         ((eq? head '|`|) (cons 'quasiquote tail))
         ((eq? head '|,|) (cons 'unquote    tail))
         (else val)
         )))
     (else val)
     ))
  (let loop ((val (replace val)))
    (cond
     ((pair? val)
      (let ((head (car val)) (tail (cdr val)))
        (cons
         (if (pair? val) (elisp->scheme head) (loop head))
         (elisp->scheme tail))))
     (else val)
     )))


(define elisp-void-macro
  ;; Some symbols like `DECLARE` and `INTERACTIVE` are specially
  ;; handled by the evaluator, they are usually caught by the pattern
  ;; matcher and evaluated during parsing. If an end user enters these
  ;; symbols into a REPL, they would result in an "void variable"
  ;; execption. To prevent exceptions of this nature, these symbols
  ;; are bound to this void macro.
  (make<macro> (lambda args '())))


(define (pure* proc)
  ;; Construct a procedure that always ignores it's first
  ;; argument. This is becuase whenever a built-in functions is
  ;; applied by the Emacs Lisp interpreter, the Emacs Lisp environment
  ;; object as the first argument. Use `PURE*` to wrap up a function
  ;; like + in a lambda that ignores that first argument, and takes an
  ;; arbitrary number of arguments.
  ;;------------------------------------------------------------------
  (lambda args (scheme->elisp (apply proc (map elisp->scheme args))))
  )

(define (pure*-typed sym type-sym type-ok? proc)
  (lambda args
    (let ((args (map scheme->elisp args)))
      (let loop ((checking args))
        (match checking
          (() (scheme->elisp (apply proc args)))
          ((,arg ,checking ...)
           (if (type-ok? arg)
               (loop checking)
               (eval-error "wrong type argument" sym arg #:expecting type-sym))
           ))))))

(define (pure*-numbers sym proc)
  (pure*-typed sym "number" number? proc))

(define (pure n sym proc)
  ;; Like `PURE*`, but construct a procedure that takes exactly N+1
  ;; arguments and applies them all (except the first argument, which
  ;; is a reference to the environment) to `PROC`.
  (lambda args
    (let ((count (length args)))
      (cond
       ((> count n) (eval-error "not enough arguments" sym n args))
       ((< count n) (eval-error "too many arguments" sym n args))
       (else (scheme->elisp (apply proc (map scheme->elisp args))))
       ))))

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;;--------------------------------------------------------------------------------------------------
;; Built-in functions

(define (eval-apply collect args)
  (match args
    (() (eval-error "wrong number of arguments" args))
    ((,func ,args ...) (eval-bracketed-form func (collect args)))))

(define (re-collect-args args)
  ;; Collect arguments to `APPLY` and `FUNCALL` into a list single.
  (match args
    (() args)
    (((,args ...)) args)
    ((,arg (,args ...)) (cons arg args))
    ((,arg ,args ...) (cons arg (re-collect-args args)))
    ))

(define (elisp-funcall . args) (eval-apply (lambda (id) id) args))

(define (elisp-apply . args) (eval-apply re-collect-args args))

(define elisp-function
  (make<macro>
   (lambda args
     (match args
       ((,arg)
        (let ((is-lambda? (lambda (o) (and (pair? o) (symbol? (car o)) (eq? 'lambda (car o)))))
              (make-lambda (lambda (o) (apply (macro-procedure elisp-lambda) o)))
              )
          (cond
           ((symbol? arg)
            (let ((result (eval-sym-lookup (symbol->string arg))))
              (cond
               ((sym-type? result) (sym-function result))
               ((lambda-type? result) result)
               ((is-lambda? result) (make-lambda result))
               (else (eval-error "not a function type" "function" arg))
               )))
           ((is-lambda? arg) (make-lambda arg))
           (else
            (eval-error "wrong type argument" "function" arg)
            ))))
       (,any (eval-error "wrong number of arguments" "function" #:expecting 1 #:value any))
       ))))


(define (elisp-symbol-op name type? op)
  (lambda args
    (match args
      ((,sym) (guard (type? sym))
       (scheme->elisp (op (*the-environment*) sym)))
      ((,sym)
       (eval-error
        "wrong type argument" name
        sym #:expecting "symbol"))
      (,any
       (eval-error
        "wrong number of arguments" name
        (length any) #:expecting 1))
      )))

(define (elisp-symbol-op2 name type? op)
  (lambda args
    (match args
      ((,sym ,val)
       (guard (type? sym))
       (scheme->elisp (op (*the-environment*) sym (scheme->elisp val)))
       )
      ((,sym ,val)
       (eval-error
        "wrong type argument" name
        sym #:expecting "symbol")
       )
      (,any
       (eval-error
        "wrong number of arguments" name
        (length any) #:expecting 2)
       ))))

(define (elisp-make-symbol . args)
  (match args
    ((,name)
     (guard (symbol/string? name))
     (eval-make-symbol (ensure-string name)))
    ((,name)
     (eval-error "wrong type argument" name #:expecting "symbol or string"))
    (,any (eval-error "wrong number of arguments" (length any) #:expecting 1))
    ))

(define elisp-symbol-name
  (elisp-symbol-op "symbol-name" any-symbol? eval-symbol-name))

(define elisp-boundp
  (elisp-symbol-op "unboundp" any-symbol? eval-boundp))

(define elisp-makunbound
  (elisp-symbol-op "makunbound" any-symbol? eval-makunbound))

(define elisp-intern
  (elisp-symbol-op "intern" symbol/string? eval-intern))

(define elisp-intern-soft
  (elisp-symbol-op "intern-soft" symbol/string? eval-intern-soft))

(define elisp-unintern
  (elisp-symbol-op "unintern" symbol/string? eval-unintern))

(define (elisp-mapatoms . args)
  (match args
    ((,func) (eval-mapatoms (eval-apply-as-proc func)))
    ((,func ,obarray)
     (eval-mapatoms (eval-apply-as-proc func) obarray))
    ((,any ...)
     (eval-error "wrong number of arguments" "mapatoms" (length any)))
    ))

(define elisp-symbol-plist
  (elisp-symbol-op "symbol-plist" any-symbol? eval-symbol-plist))

(define elisp-setplist
  (elisp-symbol-op2 "setplist" any-symbol? eval-setplist))

(define elisp-set
  (elisp-symbol-op2 "set" any-symbol? eval-set))

(define elisp-get
  (elisp-symbol-op2 "get" symbol? eval-get))

(define (elisp-put . args)
  (match args
    ((,sym ,prop ,val)
     (guard (symbol? sym))
     (if (symbol? prop)
         (eval-put
          (*the-environment*)
          (symbol->string sym)
          (symbol->string prop)
          val)
         val))
    (,args
     (eval-error
      "wrong number of arguments" "put"
      (length args) #:expecting 3))
    ))

(define elisp-symbol-function
  (elisp-symbol-op "symbol-function" symbol? eval-symbol-function))

(define elisp-fboundp
  (elisp-symbol-op "fboundp" symbol? eval-fboundp))

(define elisp-fmakunbound
  (elisp-symbol-op "fmakunbound" symbol? eval-fmakunbound))

(define elisp-fset
  (elisp-symbol-op2 "fset" symbol? eval-fset))


(define (elisp-list . args) (map scheme->elisp args))

(define (elisp-car lst)
  (cond
   ((eq? lst nil) '())
   ((null? lst) '())
   ((pair? lst) (car lst))
   (else (eval-error "wrong type argument" "car" lst))
   ))

(define (elisp-cdr lst)
  (cond
   ((eq? lst nil) '())
   ((null? lst) '())
   ((pair? lst) (cdr lst))
   (else (eval-error "wrong type argument" "cdr" lst))
   ))


(define elisp-quote
  (make<macro>
   (lambda args
     (match (cdr args)
       ((,expr) expr)
       ((,expr ,extra ...) (eval-error "wrong number of arguments" "quote" extra))
       (,any any)
       ))))


(define elisp-backquote
  (make<macro>
   (lambda exprs
     (match (cdr exprs)
       ((,exprs)
        (let expr-loop ((exprs exprs))
          (match exprs
            (() '())
            (((|,| ,unq) ,exprs ...)
             (cons (eval-form unq) (expr-loop exprs))
             )
            (((|,@| ,splice) ,exprs ...)
             (let ((elems (eval-form splice)))
               (let elem-loop ((elems elems))
                 (cond
                  ((null? elems) (expr-loop exprs))
                  ((pair? elems) (cons (car elems) (elem-loop (cdr elems))))
                  (else (cons elems (expr-loop exprs)))
                  )))
             )
            (((,sub-exprs ...) ,exprs ...)
             (cons (expr-loop sub-exprs) (expr-loop exprs))
             )
            ((,elem ,exprs ...) (cons elem (expr-loop exprs)))
            (,elem elem)
            )))
       (,any (eval-error "wrong number of arguments" "backquote" any))
       ))))


(define *macroexpand-max-depth* 16)

(define (eval-macroexpander all depth fail-depth)
  (case-lambda
    ((expr) ((eval-macroexpander all depth fail-depth) expr (*the-environment*)))
    ((expr st)
     (let loop ((depth depth) (expr expr))
       (match expr
         (() '())
         ((,label ,args ...)
          (let ((args
                 (if all
                     (map (lambda (expr) (loop depth expr)) args)
                     args)))
            (cond
             ((symbol? label)
              (let ((func (env-resolve-function st label)))
                (cond
                 ((and (lambda-type? func)
                       (eq? 'macro (lambda-kind func)))
                  (let ((result (eval-apply-lambda func args)))
                    (if (> depth 0)
                        (loop (- depth 1) result)
                        (if fail-depth
                            (eval-error "macro expansion exceeds recursion limit" label args)
                            result
                            ))))
                 (else (cons label args))
                 )))
             (else (cons label args)))))
         (,any any)
         )))))

(define (elisp-macroexpander all depth fail-depth)
  (lambda expr
    (match expr
      ((,expr) ((eval-macroexpander all depth fail-depth) expr))
      ((,expr ,env) ((eval-macroexpander all depth fail-depth) expr env))
      (,any (eval-error "wrong number of arguments" "macroexpand" any))
      )))

(define elisp-macroexpand
  (elisp-macroexpander #f *macroexpand-max-depth* #t))

(define elisp-macroexpand-1
  (elisp-macroexpander #f 1 #f))

(define elisp-macroexpand-all
  (elisp-macroexpander #t *macroexpand-max-depth* #t))


(define (elisp-prin1 . args)
  (match args
    ((,val) ((*impl/prin1*) val) val)
    ((,val ,port) ((*impl/prin1*) val port) val)
    ((,val ,port ,overrides) ((*impl/prin1*) val port overrides) val)
    (,any
     (eval-error
      "wrong number of arguments" "prin1"
      (length any) #:min 1 #:max 3))
    ))

(define (elisp-princ . args)
  (match args
    ((,val) ((*impl/princ*) val) val)
    ((,val ,port) ((*impl/princ*) val port) val)
    (,any (eval-error "wrong number of arguments" "princ" #:min 1 #:max 2))
    ))

(define eval-print
  (case-lambda
    ((val) (eval-print val *impl/elisp-output-port*))
    ((val port)
     (newline port)
     ((*impl/prin1*) val port)
     (newline port))
    ))

(define (elisp-print . args)
  (match args
    ((,val) (eval-print val))
    ((,val ,port) (eval-print val port))
    (,any (eval-error "wrong number of arguments" "print" #:min 1 #:max 2))
    ))

(define (elisp-message . args)
  (match args
    (() (eval-error "wrong number of arguments" "message" #:min 1))
    ((,format-str ,args ...)
     ;; TODO: actually format the string with arguments. Right now
     ;; this only outputs the format string and ignores the arguments.
     ((*impl/princ*) format-str (*impl/elisp-error-port*))
     )))


(define *elisp-init-env*
  ;; A parameter containing the default Emacs Lisp evaluation
  ;; environment. This environment is an ordinary association list
  ;; mapping strings (or symbols) to values. Any values satisfying the
  ;; predicate `LAMBDA-TYPE?` are automatically interned as functions
  ;; rather than ordinary values.
   ;;------------------------------------------------------------------
  (make-parameter
   `(;; ---- beginning of association list ----

     (nil . ,nil)
     (t   . ,t)

     (lambda    . ,elisp-lambda)
     (apply    . ,elisp-apply)
     (funcall  . ,elisp-funcall)
     (defun    . ,elisp-defun-defmacro)
     (defmacro . ,elisp-defun-defmacro)
     (function . ,elisp-function)
     (progn    . ,elisp-progn)
     (prog1    . ,elisp-prog1)
     (prog2    . ,elisp-prog2)
     (setq     . ,elisp-setq)
     (let      . ,elisp-let)
     (let*     . ,elisp-let*)

     (cond     . ,elisp-cond)
     (if       . ,elisp-if)
     (or       . ,elisp-or)
     (and      . ,elisp-and)
     (while    . ,elisp-while)
     (dotimes  . ,elisp-dotimes)
     (dolist   . ,elisp-dolist)

     (eq     . ,(pure 2 "eq" eq?))
     (equal  . ,(pure 2 "equal" equal?))
     (concat . ,(pure* string-append))

     (1+ . ,(pure 1 "1+" 1+))
     (1- . ,(pure 1 "1-" 1-))
     (+  . ,(pure*-numbers "+" +))
     (-  . ,(pure*-numbers "-" -))
     (*  . ,(pure*-numbers "*" *))
     (=  . ,(pure*-numbers "=" =))
     (<  . ,(pure*-numbers "<" <))
     (<= . ,(pure*-numbers "<=" <=))
     (>  . ,(pure*-numbers ">" >))
     (>= . ,(pure*-numbers ">=" >=))

     (cons  . ,(pure 2 'cons cons))
     (car   . ,(pure 1 'car car))
     (cdr   . ,(pure 1 'cdr cdr))
     (list  . ,elisp-list)
     (null  . ,(pure 1 "null" elisp-null?))

     (quote     . ,elisp-quote)
     (backquote . ,elisp-backquote)
     (|`|       . ,elisp-backquote)

     (make-symbol      . ,elisp-make-symbol)
     (symbol-name      . ,elisp-symbol-name)
     (boundp           . ,elisp-boundp)
     (makunbound       . ,elisp-makunbound)
     (intern           . ,elisp-intern)
     (intern-soft      . ,elisp-intern-soft)
     (unintern         . ,elisp-unintern)
     (mapatoms         . ,elisp-mapatoms)
     (symbol-plist     . ,elisp-symbol-plist)
     (setplist         . ,elisp-setplist)
     (set              . ,elisp-set)
     (get              . ,elisp-get)
     (put              . ,elisp-put)
     (symbol-function  . ,elisp-symbol-function)
     (fboundp          . ,elisp-fboundp)
     (fmakunbound      . ,elisp-fmakunbound)
     (fset             . ,elisp-fset)
     (declare          . ,elisp-void-macro) ;; pattern matcher special symbol
     (interactive      . ,elisp-void-macro) ;; pattern matcher special symbol

     (macroexpand      . ,elisp-macroexpand)
     (macroexpand-1    . ,elisp-macroexpand-1)
     (macroexpand-all  . ,elisp-macroexpand-all)

     (message          . ,elisp-message)
     (prin1            . ,elisp-prin1)
     (princ            . ,elisp-princ)
     (print            . ,elisp-print)

     ;; ------- end of assocaition list -------
     )))


(define elisp-reset-init-env!
  (case-lambda
    (() (elisp-reset-init-env! (*elisp-init-env*)))
    ((init-env) (elisp-reset-init-env! init-env (*the-environment*)))
    ((init-env env)
     (let loop ((init-env init-env) (errors '()))
       (cond
        ((null? init-env) errors)
        (else
         (let ((assoc (car init-env)))
           (cond
            ((pair? assoc)
             (let*((val (cdr assoc))
                   (val (if (or (not val) (null? val)) '() val)))
               (lens-set val env (=>env-obarray-key! (ensure-string (car assoc))))
               (loop (cdr init-env) errors)
               ))
            (else
             (loop (cdr init-env) (cons assoc errors)))
            ))))))))


(define new-environment
  ;; Construct a new Emacs Lisp environment object, which is a bit
  ;; like an obarray.
  (case-lambda
    (() (new-environment #f))
    ((inits) (new-environment inits #f))
    ((inits size)
     (let*((size (if (integer? size) size *default-obarray-size*))
           (inits (if (and inits (pair? inits)) inits (*elisp-init-env*)))
           (env (new-empty-env size))
           )
       (elisp-reset-init-env! inits env)
       env))))

(elisp-reset-init-env!)

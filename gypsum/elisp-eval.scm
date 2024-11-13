
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

(define nil (make<sym> "nil" #f #f #f))
(define t   (make<sym> "t"   #t #f #f))


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


(define (new-symbol name)
  (cond
   ((string? name) (make<sym> name #f #f #f))
   (else (error "not a string" name))))


(define (blank-symbol? sym)
  (not (or (sym-value sym)
           (sym-function sym)
           (sym-plist sym)))
  )

(define (canon-sym unit) (=>canonical unit (new-symbol "") blank-symbol?))

(define =>sym-value! (canon-sym =>sym-value*!))
(define =>sym-function! (canon-sym =>sym-function*!))
(define =>sym-plist! (canon-sym =>sym-plist*!))

;;--------------------------------------------------------------------
;; Lambdas of all kinds, including macros and built-ins

(define-record-type <lambda-type>
  (make<lambda> kind args optargs rest doc decls lexenv body)
  lambda-type?
  (kind     lambda-kind      set!lambda-kind)
  (args     lambda-args      set!lambda-args)
  (optargs  lambda-optargs   set!lambda-optargs)
  (rest     lambda-rest      set!lambda-rest)
  (doc      lambda-docstring set!lambda-docs)
  (decls    lambda-declares  set!lambda-declares)
  (lexenv   lambda-lexenv    set!lambda-lexenv)
  (body     lambda-body      set!lambda-body)
  )

(define =>lambda-kind*! (record-unit-lens lambda-kind set!lambda-kind '=>lambda-kind*!))
(define =>lambda-args*! (record-unit-lens lambda-args set!lambda-args '=>lambda-args*!))
(define =>lambda-optargs*! (record-unit-lens lambda-optargs set!lambda-optargs '=>lambda-optargs*!))
(define =>lambda-rest*! (record-unit-lens lambda-rest set!lambda-rest '=>lambda-rest*!))
(define =>lambda-docstring*! (record-unit-lens lambda-docstring set!lambda-docs '=>lambda-docstring*!))
(define =>lambda-declares*! (record-unit-lens lambda-declares set!lambda-declares '=>lambda-declares*!))
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
    (() (make<lambda> #f '() '() #f #f #f #f #f))
    ((args) (new-lambda args '() #f #f #f))
    ((args opts) (new-lambda args opts #f #f #f))
    ((args opts rest) (new-lambda args opts rest #f #f))
    ((args opts rest body) (new-lambda args opts rest body #f))
    ((args opts rest body docstr)
     (make<lambda> (guess-function-kind body) args opts rest docstr #f #f body))
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


(define (guess-function-kind body)
  (cond
   ((procedure? body) 'built-in)
   ((command-type? body) 'command)
   ((matcher-monad-type? body) 'macro)
   ((pair? body) 'function)
   (else #f)
   ))

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

(define (new-elstkfrm size)
  (make-hash-table string=? string-hash #:weak #f size))


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
      (list (alist->hash-table assocs))))
    ))

;;--------------------------------------------------------------------

(define-record-type <elisp-environment-type>
  (make<elisp-environment> cur env dyn lex mode)
  elisp-environment-type?
  (cur  elisp-env-progn     set!elisp-env-progn)    ;;cursor inspecting current form
  (env  elisp-env-obarray   set!elisp-env-obarray)  ;;environment (obarray)
  (dyn  elisp-env-dynstack  set!elisp-env-dynstack) ;;dynamically bound variable stack frames
  (lex  elisp-env-lexstack  set!elisp-env-lexstack) ;;lexically bound variable stack frames
  (mode elisp-env-lxmode    set!elisp-env-lxmode)   ;;lexical binding mode
  )


(define new-env
  (case-lambda
    (() (new-env #f))
    ((size)
     (let ((size (if (integer? size) size *default-obarray-size*)))
       (make-hash-table string=? string-hash #:weak #f size)))))


(define =>elisp-env-progn*!
  (record-unit-lens elisp-env-progn set!elisp-env-progn '=>elisp-env-progn*!))

(define =>elisp-env-dynstack*!
  (record-unit-lens elisp-env-dynstack set!elisp-env-dynstack '=>elisp-env-dynstack*!))

(define =>elisp-env-lexstack*!
  (record-unit-lens elisp-env-lexstack set!elisp-env-lexstack '=>elisp-env-lexstack*!))

(define =>elisp-env-obarray*!
  (record-unit-lens elisp-env-obarray set!elisp-env-obarray '=>elisp-env-obarray*!))

(define (=>elisp-env-obarray-key! name)
  (lens =>elisp-env-obarray*!
        (=>canonical (=>hash-key*! name) new-env hash-table-empty?)))

(define =>elisp-env-lexical-mode?!
  (record-unit-lens elisp-env-lxmode set!elisp-env-lxmode '=>elisp-env-lexical-mode?!))


(define (=>elisp-env-stack-lens*! st)
  ;; Select a lens for the lexical or dynamic variable stack depending
  ;; on the current lexical binding mode.
  (if (elisp-env-lxmode st) =>elisp-env-lexstack*! =>elisp-env-dynstack*!))


(define (elisp-env-push-empty-elstkfrm! st size)
  ;; Inspect the lexical binding mode and push a new stack frame on
  ;; the appropriate stack (lexical or dynamic stack). Return the
  ;; empty stack frame that was pushed so it can be updated by the
  ;; calling procedure.
  (let ((elstkfrm (new-elstkfrm size)))
    (update
     (lambda (stack) (cons elstkfrm stack)) st
     (=>elisp-env-stack-lens*! st))
    elstkfrm))


(define (elisp-env-pop-elstkfrm! st)
  (update
   (lambda (stack) (if (null? stack) '() (cdr stack))) st
   (=>elisp-env-stack-lens*! st)))


(define (elisp-sym-lookup st name)
  (or (view st =>elisp-env-lexstack*! (=>stack! name #f))
      (view st =>elisp-env-dynstack*! (=>stack! name #f))
      (view st (=>elisp-env-obarray-key! name))
      ))


(define (elisp-env-dynstack-update updater st name newsym)
  ;; Part of the Elisp "SETQ" semantics. This procedure tries to
  ;; update just the dynamic variable stack. If there is no variable
  ;; bound to `NAME` then apply `UPDATER`, `ST`, and `NAME` to the
  ;; `NEWSYM` procedure. `NEWSYM` must return two values, the updated
  ;; `ST` and an arbitrary return value for the `UPDATE&VIEW` lens.
  (update&view updater st
   =>elisp-env-dynstack*!
   (=>stack! name (lambda () (newsym updater st name)))))


(define (elisp-env-stack-update updater st name newsym)
  ;; Part of the Elisp "SETQ" semantics. This procedure updates the
  ;; lexical variable stack, or if in dynamic binding mode, updating
  ;; the dynamic variable stack. If there is no variable bound to
  ;; `NAME` then apply `UPDATER`, `ST`, and `NAME` to the `NEWSYM`
  ;; procedure. `NEWSYM` must return two values, the updated `ST` and
  ;; an arbitrary return value for the `UPDATE&VIEW` lens.
  (if (elisp-env-lxmode st)
      (update&view
       updater st =>elisp-env-lexstack*!
       (=>stack! name
        (lambda () (elisp-env-dynstack-update updater st name newsym))))
      (elisp-env-dynstack-update updater name st newsym)))


(define (elisp-env-intern! updater st name)
  ;; Part of the Elisp "SETQ" semantics. Interns a new symbol in the
  ;; obarray, replacing it if it already exists (although this
  ;; procedure is only called by procedures that have already checked
  ;; if the symbol exists and is called when it does not
  ;; exist). Before interning the new symbol, the symbol is applied to
  ;; the `UPDATER` procedure passed as an argument to this procedure.
  (let-values (((sym return) (updater (new-symbol name))))
    (values
     (lens-set sym st (=>elisp-env-obarray-key! name))
     return)))


(define (elisp-env-setq-bind! updater st name)
  ;; This procedure implements the `SETQ` semantics. It tries to
  ;; update an existing symbol bound to `NAME` anywhere in the lexical
  ;; stack, the dynamic stack, or the global "obarray", but if no such
  ;; `NAME` is bound anywhere, a new symbol is initerned in the global
  ;; obarray.
  (elisp-env-stack-update updater st name elisp-env-intern!))


(define (=>elisp-setq-bind! name)
  ;; This lens views or updates the `<ELISP-ENVIRONMENT-TYPE>`
  ;; object. A view finds the given `NAME` anywhere in the stack and
  ;; returns as `<SYM-TYPE>` object associated with the `NAME` it if
  ;; it exists. An update will update the `<SYM-TYPE>` object
  ;; associated with `NAME` anywhere in the stack if it eixsts. If a
  ;; `NAME` is not bound anywhere, this canonical lens will construct
  ;; a new `<SYM-TYPE>` object and apply it to `UPDATER` (which
  ;; returns an updated `<SYM-TYPE>` object and an arbitrary return
  ;; value), then bind the updated `<SYM-TYPE?>` object returned by
  ;; `UPDATER` to the `NAME` at the top-most layer of the stack. This
  ;; lens inspects the value of `=>ELISP-ENV-LEXICAL-MODE?!` and
  ;; determines whether to bind `NAME` in the lexical binding stack or
  ;; the dynamic binding stack.
  ;;------------------------------------------------------------------
  (let ((getter (lambda (st) (elisp-sym-lookup st name)))
        (updater (lambda (updater st) (elisp-env-setq-bind! updater st name)))
        )
    (unit-lens
     getter (default-unit-lens-setter updater) updater
     `(=>elisp-setq-bind! ,name))
    ))


(define %new-elisp-env
  (case-lambda
    (() (%new-elisp-env *default-obarray-size*))
    ((env) (%new-elisp-env env #t))
    ((env lexical-scoping)
     (define (make env) (make<elisp-environment> #f env '() '() lexical-scoping))
     (cond
      ((not (boolean? lexical-scoping)) (error "not a boolean" lexical-scoping))
      ((not env) (make (new-env *default-obarray-size*)))
      ((integer? env) (make (new-env env)))
      ((hash-table? env) (make env))
      (else (error "second argument not an <hash-table>" env))
      ))
    ))


(define (ensure-string name)
  (cond
   ((string? name) name)
   ((symbol? name) (symbol->string name))
   (else (error "not a symbol or string" name))
   ))


(define (hash-env-intern-soft hash name)
  (hash-table-ref/default hash (ensure-string name) #f))


(define *default-obarray-size* 32749)
  ;; ^ At the time of this writing, the size of the `OBARRAY` object
  ;; in my Emacs was 15121. I am choosing a prime-number size here
  ;; close to a power of 2, that is 2^15, which is roughly double that
  ;; which I would need for my Emacs. This will make one element per
  ;; cell highly likely, with plenty of room to spare for many more
  ;; symbols.


(define (env-intern-soft st name)
  (let ((name (ensure-string name)))
    (view st (=>elisp-env-obarray-key! name))))


(define (env-intern! st . assocs)
  ;; This procedure is mostly used for directly updating an Emacs Lisp
  ;; environment from within a Scheme procedure. It takes an arbitrary
  ;; number of pair arguments (cons cells) associating a symbol or
  ;; string to an arbitrary value. This procedure then creates a
  ;; symbol for each value and stores the value into the symbol. The
  ;; value will be stored into the `=>SYM-VALUE!` field of a
  ;; `<SYM-TYPE>` object unless it is a `<LAMBDA-TYPE>`, a Scheme
  ;; procedure, or a `<MATCHER-MONAD-TYPE>`, in which case it is
  ;; stored into the `=>SYM-FUNCTION!`
  ;;------------------------------------------------------------------
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
            st (=>elisp-env-obarray-key! name)))
          ((val) (cdr pair))
          )
       (cond
        ((or (lambda-type? val)
             (procedure? val)
             (matcher-monad-type? val))
         (set!sym-function sym val))
        (else (set!sym-value sym val)))
       ))
   assocs)
  st)


(define (=>env-intern! name)
  ;; This procedure is mostly used for directly updating an Emacs Lisp
  ;; environment from within a Scheme procedure. A unit lens that
  ;; focuses on the given symbol or string in the environment
  ;; obarray. This value always points to an existing symbol, a symbol
  ;; is created if it does not exist, so this lens is canonical.
  ;;------------------------------------------------------------------
  (let ((getter (lambda (st) (env-intern-soft st name)))
        (setter
         (lambda (st sym)
           (let*((name (ensure-string name))
                 (sym
                  (if (equal? name (sym-name sym))
                      sym ;; copy the symbol object if the name changed
                      (lens-set name sym =>sym-name)))
                 (st (lens-set sym st =>elisp-env-obarray*! (=>hash-key! name))))
             st)))
        )
    (unit-lens
     getter setter
     (default-unit-lens-updater getter setter)
     `(=>env-intern! ,name))
  ))


(define new-empty-elisp-env
  ;; Construct an empty Emacs Lisp evaluation environment that has no
  ;; symbols at all except the ones you optionally pass as an argument
  ;; to this constructor. The one argument you may pass must bt an
  ;; associative list or a hash table. Symbol objects are constructed
  ;; and interned for each `CAR` in the assocation list. If the `CDR`
  ;; of a pair satisfies the predicate `LAMBDA-TYPE?` it is
  ;; automatically interned in the `SYM-FUNCTION` field of the symbol
  ;; object, all other values are interned in the `SYM-VALUE` field of
  ;; the symbol object.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (new-empty-elisp-env #f))
    ((env)
     (cond
      ((or (not env) (hash-table? env)) (%new-elisp-env env))
      ((null? env) (new-empty-elisp-env #f))
      ((pair? env) (apply env-intern! (%new-elisp-env) env))
      (else (error "not a mapping from symbols to values" env))
      ))
    ))


(define (new-elisp-env . assocs)
  ;; Constrruct a new Emacs Lisp evaluation environment.
  (apply env-intern! (new-empty-elisp-env) (*elisp-init-env*)))


(define (elisp-eval! expr env)
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
  (run-matcher/cc env #f (elisp-eval-step-into-form expr)))


(define (elisp-eval->scheme env)
  ;; Return the value taken from an Emacs Lisp environment that was
  ;; the last value of the last expression returned by evaluation of
  ;; an Emacs Lisp statement. Apply this procedure to the first of the
  ;; two results returned by `ELISP-EVAL!`.
  ;;------------------------------------------------------------------
  (view env =>matcher-state-output!)
  )


;;--------------------------------------------------------------------
;; The interpreting evaluator. Matches patterns on the program and
;; immediately executes each form or symbol.


(define (expr-is . functors)
  ;; Compose a sequence functors against the current expression of the
  ;; interpreter state, assuming the current expression is a
  ;; <CURSOR-TYPE>, and assuming there is an element.
  (apply endo-view elisp-env-progn cursor-ref functors))


(define (has-next-form? st)
  (unless (elisp-environment-type? st)
    (error "expecting environment" st)
    )
  (let ((cur (elisp-env-progn st)))
    (and (cursor-type? cur)
         (not (cursor-end? cur)))
    ))


(define step-pc
  ;; Step the program counter
  (try
   (check elisp-env-progn cursor-end? not)
   (next (lambda (st) (cursor-step! (elisp-env-progn st)) st))
   ))


(define end-of-progn (check elisp-env-progn cursor-end?))


(define (elisp-error message . irritants)
  (pause (make<elisp-eval-error> message irritants)))


(define (elisp-get-form-args st)
  (cursor-collect-list (elisp-env-progn st)))


(define (elisp-eval-progn body) ;; evals the body of a `PROGN`, `LET`, or `LAMBDA`
  (into (lambda _ (new-cursor body)) (many elisp-eval-progn-step)))


(define (elisp-eval-step-into-list stmt monad)
  (/input
   (lambda (st)
     (let ((old-cursor (elisp-env-progn st)))
       (set!elisp-env-progn st (new-cursor stmt))
       (try
        monad
        (next
         (lambda (st)
           (display "elisp-eval-step-into-form: success\n")
           (set!elisp-env-progn st old-cursor)
           st)))
       ))))


(define (elisp-eval-step-into-form stmt) ;; the top-level
  (display "elisp-eval-step-into-form ")(write stmt)(newline)
  (cond
   ((symbol? stmt)
    (/input
     (lambda (st)
       (let ((sym (env-intern-soft st stmt)))
         (cond
          ((not sym) (elisp-error "void variable" stmt))
          ((sym-type? sym) (put (sym-value sym)))
          ((elisp-quote-scheme-type? sym) (put (elisp-unquote-scheme sym)))
          (else (put sym))))
       )))
   ((or (pair? stmt) ;; cursors can be constructed from one of these types
        (vector? stmt)
        (mutable-vector-type? stmt))
    (elisp-eval-step-into-list stmt elisp-eval-form))
   ((or (number? stmt)
        (string? stmt)
        (boolean? stmt)
        (char? stmt)) ;; simply return the value as it is
    (put stmt))
   ((elisp-quote-scheme-type? stmt)
    (put (elisp-unquote-scheme stmt)))
   (else (elisp-error "no semantics for statement" stmt))
   ))


(define (elisp-push-stack-frame st func args)
  ;; Bind argument values to a function's argument variables, push a
  ;; stack frame and evaluate a function body. The argument values
  ;; must already have been evaluated before applying this monad.
  (let*((elstkfrm (elstkfrm-from-args func args))
        (=>stack-lens
         (if (elisp-env-lxmode st)
             =>elisp-env-lexstack*!
             =>elisp-env-dynstack*!))
        (pop-stack
         (lambda ()
           (update cdr st =>stack-lens)
           skip))
        )
    (cond
     ((elisp-eval-error-type? elstkfrm) (pause elstkfrm))
     (else
      (update (lambda (stack) (cons elstkfrm stack)) st =>stack-lens)
      (either
       (try (elisp-eval-progn (lambda-body func)) (pop-stack))
       (pop-stack)))
     )))


(define elisp-re-eval-after-macro
  ;; After evaluating macro expansion, the macro expander should have
  ;; placed a new form into the pattern matcher output. Retrieve this
  ;; output and feed it back into the input and evaluate it.
  (/output elisp-eval-step-into-form))


(define (elisp-apply-elisp-macro func)
  ;; Needs to push a stack frame, bind elements in the current form to
  ;; the symbols in the macro arguments, and evaluate the body. It
  ;; takes no arguments because the arguments are taken from the
  ;; current form.
  (display "elisp-apply-elisp-macro ")(write func)(newline);;DEBUG
  (/input
   (lambda (st)
     (try
      (elisp-push-stack-frame st func (elisp-get-form-args st))
      elisp-re-eval-after-macro))))


(define (elisp-apply-macro func)
  ;; A built-in macro must be a monad, and in the evaluator it is
  ;; applied immediately. It may return any value at all.
  (display "elisp-apply built-in macro ")(write func)(newline);;DEBUG
  func)


(define (elisp-apply-elisp-function func)
  ;; This monad is evaluated when we are sure the `FUNC` argument is
  ;; an ordinary function defined in Emacs Lisp, and not a built-in
  ;; function.
  (display "elisp-apply-elisp-function ")(write func)(newline);;DEBUG
  (/input
   (lambda (st)
     (let ((args (elisp-get-form-args st)))
       (display "elisp-apply-elisp-function ")(write args)(newline);;DEBUG
       (apply
        monad-apply
        (lambda args (elisp-push-stack-frame st func args))
        (map elisp-eval-step-into-form args))))))


(define (elisp-apply-built-in func)
  ;; This monad checks if `FUNC` satisfies the `PROCEDURE?` or
  ;; `COMMAND-TYPE?` predicates. If so, it collects all arguments in
  ;; the current form, evaluates each one, then applies the result to
  ;; the given `FUNC` argument.
  (/input
   (lambda (st)
     (let ((args (elisp-get-form-args st)))
       (apply
        monad-apply
        (lambda args (apply func st args))
        (map elisp-eval-step-into-form args))))))


(define (elisp-apply depth sym func)
  ;; Determines if the `FUNC` argument is a symbol or lambda and tries
  ;; to somehow resolve it to a callable function. Pass the `SYM` in
  ;; case an error needs to be reported.
  (cond
   ((procedure? func) (elisp-apply-built-in func))
   ((command-type? func) (elisp-apply-built-in (command-procedure func)))
   ((matcher-monad-type? func) func) ;; <- a built-in macro is evaluated immediately
   ((lambda-type? func)
    (let ((kind (lambda-kind func)))
      (cond
       ((eq? kind 'macro) (elisp-apply-elisp-macro func))
       ((eq? kind 'function) (elisp-apply-elisp-function func))
       (else (elisp-error "unknown lambda type" kind)))))
   ((and (< depth 1)
         (or (pair? func)
             (vector? func)
             (mutable-vector-type? func)))
    (elisp-eval-step-into-form func)
    (/output (lambda (func) (elisp-apply (+ 1 depth) sym func))))
   (else (elisp-error "invalid function" sym))
   ))


(define (/current-expr on-expr)
  ;; This pattern matcher monad is "with current expression," which
  ;; fails if the current interpreter expression not is a
  ;; `<CURSOR-TYPE>` and or if `CURSOR-END?` is `#T`. Otherwise
  ;; applies the given function `ON-EXPR` is applied to the expression
  ;; under the cursor. The `ON-EXPR` is applied two arguments: (1) the
  ;; current matcher state, and (2) the current expression under the
  ;; cursor.
  (try
   (check has-next-form?)
   (/input
    (lambda (st)
      (let ((sym (cursor-ref (elisp-env-progn st))))
        (cursor-step! (elisp-env-progn st))
        (display "/current-expr: ")(write sym)(newline);;DEBUG
        (on-expr st sym))
      ))))


(define elisp-eval-form
  ;; Resolve a function from the head of the list, then call `ELISP-APPLY-FUNCTION`.
  (/current-expr
   (lambda (st sym)
     ;;----------------------------------------------------------------
     ;; First evaluate the head of the list, it must be either a
     ;; symbol or a lambda, otherwise this is an invalid form.
     (cond
      ((symbol? sym)
       (let*((func (env-intern-soft st (ensure-string sym))))
         (cond
          ((not func) (elisp-error "void variable" sym))
          ((sym-type? func) (elisp-apply 0 sym (sym-function func)))
          (else (elisp-apply 0 sym func))
          )))
      ((or (pair? sym) (vector? sym) (mutable-vector-type? sym))
       ;; Evaluate this form, it should produce a lambda.
       (elisp-eval-step-into-form sym)
       ;; Now apply the rest of this form to the lambda.
       (/output
        (lambda (out)
          (elisp-apply 0 sym out)
          )))
      (else (elisp-error "symbol at head of form is not a function or macro" sym))
      ))
   ))


(define elisp-eval-progn-step
  ;; Evaluate a single element of a "progn" form.
  (/current-expr ;; make sure the current input is a cursor
   (lambda (st stmt)
     (display "elisp-eval: ")(write stmt)(newline);;DEBUG
     (elisp-eval-step-into-form stmt)
     )))

;;--------------------------------------------------------------------------------------------------

(define (scheme->elisp val)
  (cond
   ((not val) nil)
   ((null? val) nil)
   ((pair? val)
    (cons
     (scheme->elisp (car val))
     (scheme->elisp (cdr val)))
    )
   ((char? val) (char->integer val))
   (else val)
   ))


(define (pure* proc)
  ;; Construct a procedure that always ignores it's first
  ;; argument. This is becuase whenever a built-in functions is
  ;; applied by the Emacs Lisp interpreter, the Emacs Lisp environment
  ;; object as the first argument. Use `PURE*` to wrap up a function
  ;; like + in a lambda that ignores that first argument, and takes an
  ;; arbitrary number of arguments.
  ;;------------------------------------------------------------------
  (lambda (st . args) (put (apply proc (map scheme->elisp args))))
  )


(define (pure n sym proc)
  ;; Like `PURE*`, but construct a procedure that takes exactly N+1
  ;; arguments and applies them all (except the first argument, which
  ;; is a reference to the environment) to `PROC`.
  (lambda (st . args)
    (let loop ((a args) (count n))
      (cond
       ((and (= n 0) (null? a))
        (apply proc (map scheme->elisp args)))
       ((and (> n 0) (null? a))
        (elisp-error "not enough arguments" sym n args))
       ((and (<= n 0) (pair? a))
        (elisp-error "too many arguments" sym n args))
       (else
        (elisp-error "wrong number of arguments" sym n args)
        ))
      )))


(define (check-pc . checks)
  ;; "Check program counter" monad: checks the item currently being
  ;; inspected by the cursor of the pattern matcher state.
  (apply
   check elisp-env-progn
   (lambda (cur) (and (not (cursor-end? cur)) (cursor-ref cur)))
   checks))


(define elisp-setq
  (/input
   (lambda (st)
     (let*((args (elisp-get-form-args st))
           (arg-count (length args)))
       (cond
        ((= 1 (modulo arg-count 2))
         (elisp-error "wrong number of arguments: setq" arg-count))
        (else
         (let loop ((args args))
           (cond
            ((null? args) skip) ;; no more symbols or values
            (else
             (try
              (elisp-eval-step-into-form (cadr args))
              (/output
               (lambda (val)
                 (lens-set val st
                  (=>elisp-setq-bind! (symbol->string (car args))))
                 (loop (cddr args)))))))
           )))))))


(define (elisp-def-local bind-semantics)
  ;; Note that the `BIND-SEMANTICS` **must** push it's own stack frame
  ;; because the semantics logic determines how many variables need to
  ;; be allocated. This function here will pop the stack frame when
  ;; the form body completes evaluation.
  (try
   (either
    (try
     (check-pc (lambda (binds) (null? binds) (pair? binds)))
     bind-semantics)
    (elisp-error "invalid let form, expecting bindings"))
   (many (try step-pc elisp-eval-progn-step))
   (/input elisp-env-pop-elstkfrm!)
   ))


(define (elisp-eval-let*-binding loop hash name . exprs)
  ;; Evaluate a single let* binding. There must be only one value in
  ;; the `EXPRS` list, if not an Elisp error is raised in the monad.
  ;; The `CAR` of the `EXPRS` argument is evaluated, the result is
  ;; bound to `NAME`.
  (cond
   ((not (symbol? name)) ;; ("non-symbol")
    (elisp-error "let* bindings: wrong type argument" name))
   ((null? exprs) ;; (symbol)
    (hash-table-set! hash (symbol->string name) nil)
    skip)
   ((not (null? (cdr exprs))) ;; (symbol expr something-else...)
    (elisp-error "let* bindings: can have only one value-form" name))
   (else ;; (symbol expr)
    (let ((name (symbol->string name))
          (expr (car exprs)))
      (try
       (elisp-eval-step-into-form expr)
       (/output
        (lambda (val)
          (hash-table-set! hash name val)
          skip))
       (loop)
       )))
   ))


(define let*-semantics
  ;; Called from within the `ELISP-DEF-LOCAL` monad. This procedure
  ;; pushes a new empty stack frame, then loops over each binding
  ;; expression in a LET* expression and evaluates each one in turn,
  ;; updating the current stack frame on each iteration.
  (/input
   (lambda (st)
     (let*((bindings (cursor-ref (elisp-env-progn st)))
           (size (length bindings))
           (hash (elisp-env-push-empty-elstkfrm! st size))
           )
       (let loop ((bindings bindings))
         (cond
          ((null? bindings) skip)
          ((pair? (car bindings))
           (try
            (apply elisp-eval-let*-binding
             (lambda () (loop (cdr bindings))) hash (car bindings))
            (loop (cdr bindings))
            ))
          ((symbol? (car bindings))
           (hash-table-set! hash (symbol->string (car bindings)) nil)
           (loop (cdr bindings))
           )
          (else
           (elisp-error "let* bindings: invalid form" bindings))
          ))))))


(define (elisp-zip-let-bindings st size syms vals)
  ;; Part of the "let" bindings syntax, called after evaluating all of
  ;; the values. This procedure pushes a new stack frame then binds
  ;; all of the symbols to the values at once.
  (let ((hash (elisp-env-push-empty-elstkfrm! st size)))
    (let loop ((syms syms) (vals vals))
      (cond
       ((and (null? syms) (null? vals)) skip)
       ((and (not (null? syms)) (not (null? vals)))
        (hash-table-set! hash (car syms) (car vals))
        (loop (cdr syms) (cdr vals)))
       (else
        (error "mismatched symbols and values lists" syms vals)
        )))))


(define let-semantics
  ;; Called from within the `ELISP-DEF-LOCAL` monad. This procedure
  ;; collects symbol-expression pairs. It then evaluates each
  ;; expression and collects the result of each expression evaluation.
  ;; It then pushes a new stack frame and associates all symbols with
  ;; collected values.
  (/input
   (lambda (st)
     (let*((bindings (cursor-ref (elisp-env-progn st)))
           (size (length bindings))
           )
       (let loop ((bindings bindings) (syms '()) (exprs '()))
         (cond
          ((null? bindings)
           (let loop ((exprs (reverse exprs)) (size 0) (results '()))
             (cond
              ((null? exprs)
               (elisp-zip-let-bindings st size syms results))
              (else
               (try
                (elisp-eval-step-into-form (car exprs))
                (/output
                 (lambda (val)
                   (loop (cdr exprs) (+ 1 size) (cons val results))))
                )))))
          ((pair? (car bindings))
           (let ((bind (car bindings)))
             (cond
              ((symbol? bind)
               (loop (cdr bindings) (cons (car bindings) syms) (cons #f exprs))
               )
              ((and (pair? bind) (symbol? (car bind)))
               (let*((name (car bind)) (expr (cdr bind)))
                 (cond
                  ((null? expr)
                   (loop (cdr bindings) (cons name syms) (cons #f exprs))
                   )
                  ((null? (cdr expr))
                   (loop (cdr bindings) (cons name syms) (cons (car expr) exprs)))
                  (else
                   (elisp-error "let bindings: can have only one value-form" bind)
                   ))))
              (else
               (elisp-error "let-bindings: wrong type argument" bind)
               ))))
          (else
           (elisp-error "let bindings: invalid form" bindings)
           )))
       ))))


(define elisp-let (elisp-def-local let-semantics))

(define elisp-let* (elisp-def-local let*-semantics))


(define (elisp-car lst)
  (cond
   ((eq? lst nil) (put nil))
   ((null? lst) (put nil))
   ((pair? lst) (put (car lst)))
   (else (elisp-error "car: wrong type argument" lst))
   ))


(define (elisp-cdr lst)
  (cond
   ((eq? lst nil) (put nil))
   ((null? lst) (put nil))
   ((pair? lst) (put (cdr lst)))
   (else (elisp-error "cdr: wrong type argument" lst))
   ))


(define *elisp-init-env*
  ;; A parameter containing the default Emacs Lisp evaluation
  ;; environment. This environment is an ordinary association list
  ;; mapping strings (or symbols) to values. Any values satisfying the
  ;; predicate `LAMBDA-TYPE?` are automatically interned as functions
  ;; rather than ordinary values.
  ;;------------------------------------------------------------------
  (make-parameter
   `(
    ;; ---- beginning of association list ----

     (+ . ,(pure* +))
     (- . ,(pure* -))
     (* . ,(pure* *))

     (cons . ,(pure 2 'cons cons))
     (car  . ,(pure 1 'car car))
     (cdr  . ,(pure 1 'cdr cdr))

     (progn . ,(many elisp-eval-progn-step))
     (setq . ,elisp-setq)
     (let  . ,elisp-let)
     (let* . ,elisp-let*)

    ;; ------- end of assocaition list -------
     )))


;;TODO: create an evaluation rule to handle Scheme literals.

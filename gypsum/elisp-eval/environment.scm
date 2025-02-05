
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

(define (symbol/string? o)
  ;; A `SYMBOL?`, `STRING?`, or `SYM-TYPE?`
  (or (symbol? o) (sym-type? o) (string? o)))

(define (any-symbol? o)
  ;; A `SYMBOL?` or `SYM-TYPE?`
  (or (symbol? o) (sym-type? o)))

(define (ensure-string name)
  ;; Convert a symbol to a string unless it is already a string and
  ;; return the string. Raise an exception on any other type.
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

(define (lambda-copy-into! to from)
  (set!lambda-kind        to (lambda-kind from))
  (set!lambda-args        to (lambda-args from))
  (set!lambda-optargs     to (lambda-optargs from))
  (set!lambda-rest        to (lambda-rest from))
  (set!lambda-docstring   to (lambda-docstring from))
  (set!lambda-declares    to (lambda-declares from))
  (set!lambda-interactive to (lambda-interactive from))
  (set!lambda-lexenv      to (lambda-lexenv from))
  (set!lambda-body        to (lambda-body from))
  )

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


(define default-raise-error-impl raise)

(define raise-error-impl* (make-parameter default-raise-error-impl))

(define (eval-raise err-obj) ((raise-error-impl*) err-obj))

(define (eval-error message . irritants)
  (eval-raise (make<elisp-eval-error> message irritants)))

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
  (make<elisp-environment> env dyn lex flag mode)
  elisp-environment-type?
  (env   env-obarray   set!env-obarray)  ;;environment (obarray)
  (dyn   env-dynstack  set!env-dynstack) ;;dynamically bound variable stack frames
  (lex   env-lexstack  set!env-lexstack) ;;lexically bound variable stack frames
  (flag  env-stkflags  set!env-stkflags) ;;bits indicating stack frame type
  (mode  env-lxmode    set!env-lxmode)   ;;lexical binding mode
  )

(define *elisp-input-port* (make-parameter (current-input-port)))
(define *elisp-output-port* (make-parameter (current-output-port)))
(define *elisp-error-port* (make-parameter (current-error-port)))

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


(define (print-stack-frames st)
  (define (print-pair pair)
    (let ((label (car pair))
          (symbol (cdr pair))
          )
      (print
       (line-break)
       (bracketed
        2 #\( #\)
        (qstr label) " . "
        (qstr
         (if (sym-type? symbol)
             (sym-value symbol)
             symbol)))
       )))
  (define (print-1frame frame)
    (apply print (map print-pair (hash-table->alist frame)))
    )
  (define (print-frames frames)
    (apply print
     (let loop ((n 0) (frames (reverse frames)))
       (cond
        ((pair? frames)
         (cons
          (print
           (bracketed 2 #\( #\) "frame " n (print-1frame (car frames)))
           (line-break))
          (loop (+ 1 n) (cdr frames))))
        ((null? frames) '())
        (else (error "not a frame" frames))
        ))))
  (print
   ";;---- dynstack ------------------" (line-break)
   (print-frames (env-dynstack st)) (line-break)
   ";;---- lexstack ------------------" (line-break)
   (print-frames (env-lexstack st))
   ))


(define (env-pop-elstkfrm! st)
  (let ((lxmode (bit-stack-pop! (env-stkflags st)))
        (pop (lambda (stack) (if (null? stack) '() (cdr stack))))
        )
    (unless lxmode (update pop st =>env-dynstack*!))
    (update pop st =>env-lexstack*!)
    ))


(define (env-push-new-elstkfrm! st size bindings)
  ;; Inspect the lexical binding mode and push a new stack frame on
  ;; the appropriate stack (lexical or dynamic stack). Return the
  ;; empty stack frame that was pushed so it can be updated by the
  ;; calling procedure.
  (let ((lxmode (env-lxmode st))
        (elstkfrm (new-elstkfrm size bindings)))
    (update (lambda (stack) (cons elstkfrm stack)) st =>env-lexstack*!)
    (unless lxmode
      (update (lambda (stack) (cons elstkfrm stack)) st =>env-dynstack*!))
    (bit-stack-push! (env-stkflags st) lxmode)
    elstkfrm))


(define (env-dynstack-update updater st name newsym)
  ;; Part of the Elisp "SETQ" semantics. This procedure tries to
  ;; update just the dynamic variable stack. If there is no variable
  ;; bound to `NAME` then apply `UPDATER`, `ST`, and `NAME` to the
  ;; `NEWSYM` procedure. `NEWSYM` must return two values, the updated
  ;; `ST` and an arbitrary return value for the `UPDATE&VIEW` lens.
  (update&view updater st
    =>env-dynstack*!
    (=>stack! name (lambda () (newsym updater st name)))))


(define (env-sym-update updater st name newsym)
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


(define (pdebug name where found)
  (display ";; found ")(write name)
  (display " in ")(display where)
  (display ": ")(write found)(newline)
  )

(define (env-sym-lookup st name)
  (or
   (view st =>env-lexstack*! (=>stack! name #f))
   (view st =>env-dynstack*! (=>stack! name #f))
   (view st (=>env-obarray-key! name))
   ))


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


(define (=>env-symbol! name)
  ;; A lens that looks up a symbol in the lexical stack, dynamic
  ;; stack, or global obarray, and if non exist, a new symbol may be
  ;; interned into the global obarray if the next composed lens
  ;; returns a non-empty symbol object.
  (let ((getter
         (lambda (st) (env-sym-lookup st name)))
        (updater
         (lambda (updater st)
           (env-sym-update updater st name env-intern!)))
        )
    (unit-lens
     getter
     (default-unit-lens-setter updater)
     updater
     `(=>env-symbol ,name))
    ))


(define (env-setq-bind! st updater name)
  ;; This procedure implements the `SETQ` semantics. It tries to
  ;; update an existing symbol bound to `NAME` anywhere in the lexical
  ;; stack, the dynamic stack, or the global "obarray", but if no such
  ;; `NAME` is bound anywhere, a new symbol is initerned in the global
  ;; obarray.
  (env-sym-update updater st name env-intern!))


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
    ))


(define new-empty-environment
  (case-lambda
    (() (new-empty-environment *default-obarray-size*))
    ((size)
     (make<elisp-environment>
      (new-empty-obarray size) '() '() (new-bit-stack) #t))))


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

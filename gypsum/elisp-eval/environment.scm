
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

(define (sym-name-equal? a b)
  (and
   (sym-type? a)
   (sym-type? b)
   (string=? (sym-name a) (sym-name b))
   ))

(define =>sym-value*! (record-unit-lens sym-value set!sym-value '=>sym-value*!))
(define =>sym-function*! (record-unit-lens sym-function set!sym-function '=>sym-function*!))
(define =>sym-plist*! (record-unit-lens sym-plist set!sym-plist '=>sym-plist*!))

(define (copy-symbol name sym)
  (make<sym> name (sym-value sym) (sym-function sym) (sym-plist sym)))


(define new-symbol
  ;; Construct a new uninterned symbol object. You can pass an
  ;; optional value, or a value and an optional function. This
  ;; procedure differs from `NEW-SYMBOL-VALUE` in that it does not
  ;; guess whether to put the given value into the function slot or
  ;; value slot.
  ;;------------------------------------------------------------------
  (case-lambda
    ((name) (new-symbol name #f #f))
    ((name val) (new-symbol name val #f))
    ((name val func)
     (cond
      ((string? name) (make<sym> name val func #f))
      (else (error "not a string" name))))))


(define (new-symbol-value name val)
  ;; Construct a new uninterned symbol object with a given value. The
  ;; value will be placed into the value slot or function slot of the
  ;; symbol object based on it's type. If `VAL` is already a symbol
  ;; object, a new symbol object is constructed and populated with the
  ;; fields of `VAL` except for `SYM-NAME` which is set to the `NAME`
  ;; argument instead. If you are certain which slot the value should
  ;; go into (`SYM-FUNCTION` or `SYM-VALUE`), use `NEW-SYMBOL` to
  ;; construct a symbol object instead as it is more efficient.
  ;;------------------------------------------------------------------
  (cond
   ((procedure? val) (new-symbol name #f val))
   ((macro-type? val) (new-symbol name #f val))
   ((syntax-type? val) (new-symbol name #f val))
   ((command-type? val) (new-symbol name #f val))
   ((sym-type? val)
    (if (string? name)
        (make<sym> name
         (sym-value val)
         (sym-function val)
         (sym-plist val))
        (error "not a string" name)
        ))
   (else (new-symbol name val))
   ))


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
  ;; This defines a macro, which contains a procedure for which it's
  ;; arguments are not evaluated, but passed as unevaluated
  ;; expressions, and must return a form. The returned form is
  ;; evaluated with a recursive call to `EVAL` to produce the result
  ;; value of the macro evaluation. In the Emacs Lisp documentation,
  ;; macros are documented as "Macros". Contrast this with
  ;; "<syntax-type>" values (described below) which are documented as
  ;; "Special Forms".
  ;;------------------------------------------------------------------
  (make<macro> proc)
  macro-type?
  (proc macro-procedure)
  )


(define-record-type <syntax-type>
  ;; This defines a syntactic form, which contains a procedure for
  ;; which it's arguments are not evaluated, like a macro, but unlike
  ;; a macro, returns a value that is not then applied to `EVAL`
  ;; recursively. In the Emacs Lisp documentation, these syntactic
  ;; forms are documented as "Special Forms". Contrast this with
  ;; "<macro-type>" values (described above) which are documented as
  ;; "Macros".
  ;;------------------------------------------------------------------
  (make<syntax> proc)
  syntax-type?
  (proc syntax-eval)
  )

;;--------------------------------------------------------------------
;; Lambdas of all kinds, including macros and built-ins

(define-record-type <lambda-type>
  (make<lambda> kind args optargs rest doc loc decls inter lexenv body)
  lambda-type?
  (kind     lambda-kind         set!lambda-kind)
  (args     lambda-args         set!lambda-args)
  (optargs  lambda-optargs      set!lambda-optargs)
  (rest     lambda-rest         set!lambda-rest)
  (doc      lambda-docstring    set!lambda-docstring)
  (loc      lambda-location     set!lambda-location)
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
(define =>lambda-location*! (record-unit-lens lambda-location set!lambda-location '=>lambda-location*!))
(define =>lambda-declares*! (record-unit-lens lambda-declares set!lambda-declares '=>lambda-declares*!))
(define =>lambda-interactive*! (record-unit-lens lambda-interactive  set!lambda-interactive '=>lambda-interactive*!))
(define =>lambda-lexenv*! (record-unit-lens lambda-lexenv set!lambda-lexenv '=>lambda-lexenv*!))
(define =>lambda-body*! (record-unit-lens lambda-body set!lambda-body '=>lambda-body*!))

(define (empty-lambda? o)
  (not (or (lambda-kind o)      
           (lambda-args o)      
           (lambda-optargs o)   
           (lambda-rest o)      
           (lambda-lexenv o)    
           (lambda-body o)      
           ))
  )

(define new-lambda
  (case-lambda
    (() (new-lambda 'lambda))
    ((kind) (make<lambda> kind '() '() #f #f #f #f #f #f #f))
    ((kind args) (new-lambda kind args '() #f #f #f))
    ((kind args opts) (new-lambda kind args opts #f #f #f))
    ((kind args opts rest) (new-lambda kind args opts rest #f #f))
    ((kind args opts rest body) (new-lambda kind args opts rest body #f))
    ((kind args opts rest body docstr)
     (make<lambda> kind
      (map ensure-string args)
      (map ensure-string opts)
      (if rest (ensure-string rest) #f)
      docstr #f #f #f #f body))
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
(define =>lambda-location! (canon-lambda =>lambda-location*!))
(define =>lambda-docstring! (canon-lambda =>lambda-docstring*!))
(define =>lambda-declares! (canon-lambda =>lambda-declares*!))
(define =>lambda-lexenv! (canon-lambda =>lambda-lexenv*!))
(define =>lambda-body! (canon-lambda =>lambda-body*!))

;;--------------------------------------------------------------------

(define-record-type <elisp-eval-error-type>
  (make<elisp-eval-error> message irritants trace)
  elisp-eval-error-type?
  (message    elisp-eval-error-message      set!elisp-eval-message)
  (irritants  elisp-eval-error-irritants    set!elisp-eval-irritants)
  (trace      elisp-eval-error-stack-trace  set!elisp-eval-error-stack-trace)
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

(define =>elisp-eval-error-stack-trace
  (record-unit-lens
   elisp-eval-error-stack-trace
   set!elisp-eval-error-stack-trace
   '=>elisp-eval-error-stack-trace))

(define (elisp-eval-error-equal? a b)
  (and
   (equal?
    (view a =>elisp-eval-error-message)
    (view b =>elisp-eval-error-message))
   (equal?
    (view a =>elisp-eval-error-irritants)
    (view b =>elisp-eval-error-irritants)
    )))

(define default-raise-error-impl raise)

(define raise-error-impl* (make-parameter default-raise-error-impl))

(define (eval-raise err-obj) ((raise-error-impl*) err-obj))

(define (eval-error message . irritants)
  (eval-raise
   (make<elisp-eval-error> message irritants #f)))

(define write-elisp-eval-error
  (case-lambda
    ((err-obj) (write-elisp-eval-error err-obj #f (current-output-port)))
    ((err-obj env/port)
     (cond
      ((port? env/port)
       (write-elisp-eval-error err-obj #f env/port)
       )
      (else
       (write-elisp-eval-error err-obj env/port (current-output-port))
       )))
    ((err-obj env port)
     (define (write-irritants irrs)
       (let loop ((irrs irrs) (i 0))
         (cond
          ((pair? irrs)
           (write-string (number->string i) port)
           (write-char #\space port)
           (write (car irrs) port)
           (newline port)
           (loop (cdr irrs) (+ 1 i))
           )
          (else #f)
          )))
     (cond
      ((elisp-eval-error-type? err-obj)
       (write-elisp-stack-trace
        (view err-obj =>elisp-eval-error-stack-trace)
        port)
       (newline port)
       (write-string
        (view err-obj =>elisp-eval-error-message)
        port)
       (newline port)
       (write-irritants (view err-obj =>elisp-eval-error-irritants))
       )
      ((error-object? err-obj)
       (when env
         (write-elisp-stack-trace (env-get-stack-trace env) port)
         )
       (newline port)
       (let ((msg (error-object-message err-obj)))
         (cond
          ((string? msg)
           (write-string msg port)
           (newline port)
           )
          (else
           (display "Error: " port)
           (write msg port)
           (newline port)
           )))
       (write-irritants (error-object-irritants err-obj))
       )
      (else #f))
     )))

;;--------------------------------------------------------------------
;; The stack

(define new-elstkfrm
  (case-lambda
    ((size) (new-elstkfrm size '()))
    ((size bindings)
     ;; TODO: make use of the `SIZE` argument
     (let ((elstkfrm (make-hash-table string=? string-hash)))
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

(define (assocs->elstkfrm size assocs)
  (let ((elstkfrm (new-elstkfrm size)))
    (for-each
     (lambda (pair)
       (elstkfrm-sym-intern! elstkfrm (car pair) (cdr pair)))
     assocs
     )
    elstkfrm
    ))


(define (elstkfrm-from-args func args)
  ;; Construct a stack frame by binding arguments values to the
  ;; argument symbol names in the givem `FUNC`, which must be a
  ;; `<LAMBDA-TYPE>`. The values are bound in the lexical (not
  ;; dynamic) scope.
  (let ((syms (lambda-args func))
        (opts (lambda-optargs func))
        (rest (lambda-rest func)))
    (define (bind-rest args count stack)
      (cond
       (rest
        (assocs->elstkfrm
         (+ 1 count)
         (cons (cons rest args) stack)))
       ((null? args) (assocs->elstkfrm count stack))
       (else (eval-error "too many arguments" func args))
       ))
    (define (bind-opts opts args count stack)
      (cond
       ((pair? opts)
        (cond
         ((pair? args)
          (bind-opts
           (cdr opts) (cdr args) (+ 1 count)
           (cons (cons (car opts) (car args)) stack))
          )
         (else
          (bind-opts
           (cdr opts) '() (+ 1 count)
           (cons (cons (car opts) '()) stack)
           ))
         ))
       (else (bind-rest args count stack))
       ))
    (define (bind-syms syms args count stack)
      (cond
       ((pair? syms)
        (cond
         ((pair? args)
          (bind-syms
           (cdr syms) (cdr args) (+ 1 count)
           (cons (cons (car syms) (car args)) stack))
          )
         (else (eval-error "not enough arguments" func args))
         ))
       (else (bind-opts opts args count stack))
       ))
    (bind-syms syms args 0 '())
    ))

;;--------------------------------------------------------------------

(define-record-type <stack-trace-frame-type>
  (make<stack-trace-frame> location symbol func frames)
  stack-trace-frame-type?
  (location   stack-trace-location  set!stack-trace-location)
  (symbol     stack-trace-symbol    set!stack-trace-symbol)
  (func       stack-trace-func      set!stack-trace-func)
  (frames     stack-trace-frames    set!stack-trace-frames)
  )

(define (new-stack-trace-frame location symbol func)
  ;; Construct a new object containing information for the current
  ;; stack trace. It takes three arguments:
  ;;
  ;;  1. location, as in the tokenizer or parser location indicating
  ;;     the source from where the currently evaluating code was read.
  ;;
  ;;  2. a symbol indicating which procedure is currently being
  ;;     evaluated, or `#T` for the top-level
  ;;
  ;;  3. an object of type `LAMBDA-TYPE?` containing the code being
  ;;     evaluated.
  ;;------------------------------------------------------------------
  (make<stack-trace-frame> location symbol func '())
  )

(define =>stack-trace-location*!
  (record-unit-lens
   stack-trace-location
   set!stack-trace-location
   '=>stack-trace-location*!
   ))

(define =>stack-trace-symbol*!
  (record-unit-lens
   stack-trace-symbol
   set!stack-trace-symbol
   '=>stack-trace-symbol*!
   ))

(define =>stack-trace-func*!
  (record-unit-lens
   stack-trace-func
   set!stack-trace-func
   '=>stack-trace-func*!
   ))

(define =>stack-trace-frames*!
  (record-unit-lens
   stack-trace-frames
   set!stack-trace-frames
   '=>stack-trace-frames*!
   ))

(define write-stack-trace-frame
  (case-lambda
    ((fr) (write-stack-trace-frame fr (current-output-port)))
    ((fr port)
     (let ((loc (stack-trace-location fr))
           (sym (stack-trace-symbol fr))
           )
       (when (source-file-location-type? loc)
         (write-parser-location loc port)
         (write-string ": " port)
         )
       (cond
        ((eq? #t sym) (write-string "#t" port))
        ((eq? #f sym) (write-string "#f" port))
        ((symbol? sym) (write-string (symbol->string sym) port))
        ((sym-type? sym) (write-string (sym-name sym) port))
        ((string? sym) (display sym port))
        (else (error "unknown type used for stack trace symbol" fr))
        )
       #t))))

;;--------------------------------------------------------------------

(define-record-type <elisp-stack-trace>
  (make<elisp-stack-trace> vec)
  elisp-stack-trace-type?
  (vec  elisp-stack-trace->vector)
  )

(define (elisp-stack-trace-ref stktrc n)
  (vector-ref (elisp-stack-trace->vector stktrc) n)
  )

(define spaces
  #("" " " "  " "   " "    " "     " "      " "       "))

(define (count-digits-b10 n)
  (cond
   ((not n) #f)
   ((< n 10) 1)
   ((< n 100) 2)
   ((< n 1000) 3)
   ((< n 10000) 4)
   ((< n 100000) 5)
   ((< n 1000000) 6)
   (else 7)
   ))

(define write-elisp-stack-trace
  (case-lambda
    ((tr) (write-elisp-stack-trace tr (current-output-port)))
    ((tr port)
     (let*((vec (elisp-stack-trace->vector tr))
           (len (and (vector? vec) (vector-length vec)))
           (maxdigits (count-digits-b10 len))
           )
       (cond
        (len
         (display ";;---------------------- stack trace -----------------------" port)
         (newline port)
         (let loop ((i 0) (pfxspace 1))
           (cond
            ((< i len)
             (let ((nspaces (- maxdigits pfxspace)))
               (when (> nspaces 0)
                 (write-string (vector-ref spaces nspaces))
                 ))
             (write-string (number->string i) port)
             (write-char #\space)
             (write-stack-trace-frame (vector-ref vec i) port)
             (newline port)
             (let ((i (+ 1 i)))
               (loop i (count-digits-b10 i))
               ))
            (else #t)
            )))
        (else #f)
        )))))

;;--------------------------------------------------------------------

(define-record-type <elisp-environment-type>
  ;; This is the environment object used for the Emacs Lisp evaluator.
  ;; Use `NEW-ENVIRONMENT` to construct an object of this type.
  ;;------------------------------------------------------------------
  (make<elisp-environment> env dyn lex flag trace mode)
  elisp-environment-type?
  (env   env-obarray   set!env-obarray)  ;;environment (obarray)
  (dyn   env-dynstack  set!env-dynstack) ;;dynamically bound variable stack frames
  (lex   env-lexstack  set!env-lexstack) ;;lexically bound variable stack frames
  (flag  env-stkflags  set!env-stkflags) ;;bits indicating stack frame type
  (trace env-trace     set!env-trace)    ;;stack trace
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

(define =>env-stack-trace*!
  (record-unit-lens env-trace set!env-trace '=>env-stack-trace*!))


(define (env-push-trace! st loc sym func)
  (update
   (lambda (stack) (cons (new-stack-trace-frame loc sym func) stack))
   st =>env-stack-trace*!))


(define (env-pop-trace! st)
  (update (lambda (stack) (cdr stack)) st =>env-stack-trace*!))


(define (env-trace! loc sym func st run)
  ;; Takes the same three arguments as `NEW-STACK-TRACE-FRAME`, a 4th
  ;; thunk procedure that performs evaluation of some Emacs Lisp code
  ;; and returns the resulting Emacs Lisp data. Also optionally takes
  ;; a 5th argument, the Emacs Lisp environment object, defaulting to
  ;; the content of `*THE-ENVIRONMENT*`.
  ;;------------------------------------------------------------------
  (env-push-trace! st loc sym func)
  (let ((result (run)))
    (env-pop-trace! st)
    result
    ))

(define (env-get-stack-trace env)
  (let*((trace (env-trace env))
        (size (and trace (length trace)))
        (vec (and size (make-vector size)))
        )
    (cond
     ((and size (> size 0))
      (let loop ((i size) (trace trace))
        (let ((i (- i 1)))
          (vector-set! vec i (car trace))
          (if (<= i 0)
              (make<elisp-stack-trace> vec)
              (loop i (cdr trace))
              ))))
     (else (make<elisp-stack-trace> #f))
     )))


(define =>env-trace-location*!
  (lens =>env-stack-trace*! =>car =>stack-trace-location*!))

(define =>env-trace-symbol*!
  (lens =>env-stack-trace*! =>car =>stack-trace-symbol*!))

(define =>env-trace-func*!
  (lens =>env-stack-trace*! =>car =>stack-trace-func*!))

(define =>env-trace-frames*!
  (lens =>env-stack-trace*! =>car =>stack-trace-frames*!))


(define (env-reset-stack! st)
  ;; Clear the stacks and stack traces, leave the rest of the
  ;; environment untouched.
  ;;------------------------------------------------------------------
  (set!env-dynstack st '())
  (set!env-lexstack st '())
  (set!env-stkflags st (new-bit-stack))
  (set!env-trace    st '())
  (set!env-lxmode   st #t)
  st)


(define (print-all-stack-frames st)
  ;; Shows all local variables in all stack frames.
  ;;------------------------------------------------------------------
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

(define (print-stack-frame st)
  (let ((len (length (env-trace st))))
    (let loop ((trace (reverse (env-trace st))) (i 0))
      (cond
       ((null? trace) #f)
       (else
        (let ((tr (car trace)))
          (print i ": " (if (sym-type? tr) (sym-name tr) tr) (line-break))
          (loop (cdr trace) (- i 1))
          ))))))



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
    ((size)
     ;; TODO: make use of the `SIZE` argument
     (make-hash-table string=? string-hash))
    ))


(define new-empty-environment
  (case-lambda
    (() (new-empty-environment *default-obarray-size*))
    ((size)
     (make<elisp-environment>
      (new-empty-obarray size) '() '() (new-bit-stack) '() #t))))


(define (env-alist-defines! env init-env)
  ;; This procedure updates the global state of an environment with a
  ;; list `INIT-ENV`. This `INIT-ENV` argument must be a list
  ;; containing one of two types, either:
  ;;
  ;;  1. a `SYM-TYPE?` symbol object, where the `SYM-NAME` of the
  ;;     symbol object is associated with the symbol object itself in
  ;;     the environment hash table,
  ;;
  ;;  ... or ...
  ;;
  ;;  2. a `PAIR?` type where `CAR` is a string name and `CDR` is the
  ;;     value. The `NEW-SYMBOL-VALUE` procedure is used to construct
  ;;     a new symbol object with the `CAR` as the name, and
  ;;     associating it with the new symbol in the environment hash
  ;;     table.
  ;;------------------------------------------------------------------
  (let loop ((init-env init-env) (errors '()))
    (cond
     ((null? init-env) errors)
     (else
      (let ((assoc (car init-env)))
        (cond
         ((pair? assoc)
          (let*((name (ensure-string (car assoc)))
                (val (cdr assoc))
                (obj (if (or (not val) (null? val)) #f
                         (new-symbol-value name val))))
            (cond
             (obj
              (lens-set obj env (=>env-obarray-key! name))
              (loop (cdr init-env) errors)
              )
             (else
              (loop (cdr init-env) (cons assoc errors))
              ))))
         ((sym-type? assoc)
          (lens-set assoc env (=>env-obarray-key! (sym-name assoc)))
          (loop (cdr init-env) errors)
          )
         (else
          (loop (cdr init-env) (cons assoc errors))
          )))))))

;;--------------------------------------------------------------------------------------------------
;; Interface between Scheme and Elisp

(define (elisp-null? val) (or (null? val) (not val)))

(define (scheme->elisp val)
  (define (replace-elem elem)
    (cond
     ((pair? elem) (scheme->elisp elem))
     (else elem)
     ))
  (define (replace-head head)
    (cond
     ((symbol? head)
      (case head
        ((unquote) '|,|)
        ((quasiquote) '|`|)
        ((unquote-splicing) '|,@|)
        (else head)
        ))
     (else (replace-elem head))
     ))
  (cond
   ((not   val) '())
   ((pair? val)
    (cons
     (replace-head (car val))
     (map replace-elem (cdr val))
     ))
   (else val)
   ))


(define (elisp->scheme val)
  (define (replace-elem elem)
    (cond
     ((pair? val)
      (let ((head (car val)) (tail (cdr val)))
        (case head
         ((|`|)  (cons 'quasiquote tail))
         ((|,|)  (cons 'unquote    tail))
         ((|,@|) (cons 'unquote-splicing tail))
         (else val)
         )))
     (else val)
     ))
  (define (replace-head head)
    (cond
     ((symbol? head)
      (case head
        ((|`|)  'quasiquote)
        ((|,|)  'unquote)
        ((|,@|) 'unquote-splicing)
        (else head)
        ))
     (else (replace-elem head))
     ))
  (let loop ((val val))
    (cond
     ((pair? val)
      (cons
       (replace-head (car val))
       (map replace-elem (cdr val))
       ))
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
          ((arg checking ...)
           (if (type-ok? arg)
               (loop checking)
               (eval-error "wrong type argument" sym arg 'expecting type-sym))
           ))))))

(define (pure*-numbers sym proc)
  (pure*-typed sym "number" number? proc))

(define (pure n sym proc)
  ;; Like `PURE*`, but construct a procedure that takes exactly N+1
  ;; arguments and applies them all (except the first argument, which
  ;; is a reference to the environment) to `PROC`.
  ;;------------------------------------------------------------------
  (pure-raw n sym
   (lambda args (scheme->elisp (apply proc (map scheme->elisp args))))))

(define (pure-raw n sym proc)
  ;; Like `PURE`, but does not convert the Elisp values passed as
  ;; arguments to Scheme values, you get the raw Elisp values. The
  ;; return values are not converted either.
  ;;------------------------------------------------------------------
  (lambda args
    (let ((count (length args)))
      (cond
       ((> count n) (eval-error "not enough arguments" sym n args))
       ((< count n) (eval-error "too many arguments" sym n args))
       (else (apply proc args))
       ))))

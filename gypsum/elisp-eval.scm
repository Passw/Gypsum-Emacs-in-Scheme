
(define *the-environment*
  (make-parameter (new-empty-environment *default-obarray-size*)))


(define (elisp-show-trace)
  (let ((st (*the-environment*)))
    (pretty
     (print
      (print-trace st)
      "==== stack frames ================"
      (line-break)
      (print-stack-frames st)))))


(define =>elisp-symbol!
  ;; This lens looks-up a symbol in the environment, including the
  ;; stack, so it can operate on an environment returned by a
  ;; breakpoint within a debugger and will resolve the same symbols
  ;; that would be resolved by the chain of function evaluation up to
  ;; the breakpoint.
  ;;------------------------------------------------------------------
  (let ((getter
         (lambda (sym)
           (view
            (*the-environment*)
            (=>env-symbol! (symbol->string sym)))))
        (updater
         (lambda (updater sym)
           (update&view
            updater
            (*the-environment*)
            (=>env-symbol! (symbol->string sym)))
           )))
    (unit-lens
     getter
     (default-unit-lens-setter updater)
     updater
     '=>elisp-symbol!))
  )


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
           (lens-set val sym =>sym-function*!))
          (else (lens-set val sym =>sym-value*!)))
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
    ((expr) (elisp-eval! expr #f))
    ((expr env)
     (call/cc
      (lambda (halt-eval)
        (let ((run
               (lambda ()
                 (parameterize ((raise-error-impl* halt-eval))
                   (eval-form (scheme->elisp expr)))
                 )))
          (if (not env) (run)
              (parameterize ((*the-environment* env)) (run))
              )))))))


(define (eval-iterate-forms st port use-form)
  (let-values
      (((port close-on-end)
        (cond
         ((input-port? port) (values port #f))
         ((string? port) (values (open-input-file port) #t))
         (else (error "not a filepath or input port" port))
         ))
       )
    (call-with-port port
      (lambda (port)
        (define (loop form)
          (cond
           ((eof-object? form)
            (when close-on-end (close-port port))
            '())
           (else
            (let ((result (use-form form)))
              (cond
               ((elisp-eval-error-type? result)
                (let ((stderr (current-error-port)))
                  (write form stderr) (newline stderr)
                  )
                result
                )
               (else (loop (read-elisp port)))
               ))
            )))
        ;; First handle the lexical binding mode, if it exists.
        (let ((form (read-elisp port)))
          (match form
            (`(%set-lexical-binding-mode ,on/off)
             (lens-set on/off st =>env-lexical-mode?!)
             (loop (read-elisp port))
             )
            (any (loop form))
            ))))))


(define elisp-load!
  (case-lambda
    ((filepath)
     (elisp-load! filepath (*the-environment*))
     )
    ((filepath st)
     (let ((name "load-file-name"))
       (define (setq-load-file-name val)
         (lens-set val st
          (=>env-obarray-key! name)
          (=>sym-value! name))
         )
       (setq-load-file-name filepath)
       (let ((result
              (eval-iterate-forms st filepath
               (lambda (form) (elisp-eval! form st))))
             )
         (exec-run-hooks (list name) (list filepath))
         (setq-load-file-name nil)
         result
         )))
    ))


(define exec-run-hooks
  (case-lambda
    ((hook-list args-list)
     (exec-run-hooks #f hook-list args-list)
     )
    ((until-val hook-list args-list)
     (define (until-failure result loop hook-list)
       (if (elisp-null? result) #f (loop hook-list))
       )
     (define (until-success result loop hook-list)
       (if (elisp-null? result) (loop hook-list) #f)
       )
     (define (until-end result loop hook-list)
       (loop hook-list)
       )
     (let ((st (*the-environment*))
           (recurse
            (cond
             ((not until-val) until-end)
             ((eq? until-val 'failure) until-failure)
             ((eq? until-val 'success) until-success)
             (else (error "loop control symbol" until-val))
             ))
           )
       (define (third hook)
         ;; Final level of indirection: `HOOK` must be a symbol
         ;; object, must contain a callable function.
         (let*((name (symbol->string hook))
               (func (view st
                      (=>env-symbol! name)
                      (=>sym-function! name)
                      ))
               )
           (cond
            (hook
             (eval-apply-lambda func args-list))
            (else (eval-error "void variable" hook)))
           ))
       (define (second hook)
         ;; Second level of indirection. A hook must be a symbol or a
         ;; list of symbols, each symbol must resolve to a function
         ;; that can be evaluated.
         (cond
          ((not   hook) #f)
          ((null? hook) #f)
          ((and (pair? hook) (symbol? (car hook)))
           (recurse (third (car hook)) second (cdr hook)))
          ((symbol? hook) (third hook))
          (else
           (eval-error "wrong type argument" hook #:expecting "symbol or list")
           )))
       (define (first hook-list)
         ;; First level of indirection. A hook must be a symbol that
         ;; resolves to a symbol or list of symbols.
         (cond
          ((pair? hook-list)
           (let*((hook-name (car hook-list))
                 (hook-list (cdr hook-list))
                 )
             (cond
              ((symbol? hook-name)
               (let*((name (symbol->string hook-name))
                     (hook
                      (view st
                       (=>env-symbol! name)
                       (=>sym-value! name)))
                     (result (second hook))
                     )
                 (recurse result first hook-list)
                 ))
              (else
               (eval-error "wrong type argument" hook-name #:expecting "symbol")
               ))
             ))
          (else #f)
          ))
       (cond
        ((null? hook-list) #f)
        ((pair? hook-list) (first hook-list))
        ((symbol? hook-list) (first (list hook-list)))
        (else
         (eval-error "wrong type argument" hook-list
          #:expecting "symbol or symbol list"))
        )))
    ))


(define (elisp-run-hooks . args) (exec-run-hooks args '()))

(define (elisp-hook-runner name control)
  (lambda args
    (match args
      (() (eval-error "wrong number of arguments" name 0 #:min 1))
      ((hook args ...) (exec-run-hooks control (list hook) args))
      ))
  )

(define elisp-run-hooks-with-args (elisp-hook-runner "run-hooks-with-args" #f))

(define elisp-run-hook-with-args-until-failure
  (elisp-hook-runner "run-hook-with-args-until-failure" 'failure))

(define elisp-run-hook-with-args-until-success
  (elisp-hook-runner "run-hook-with-args-until-success" 'success))

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
     (let ((elstkfrm (elstkfrm-from-args func args)))
       (cond
        ((not elstkfrm) (error "elstkfrm-from-args returned #f"))
        ((elisp-eval-error-type? elstkfrm) (eval-raise elstkfrm))
        (else
         (let*((st (*the-environment*))
               (old-stack (view st =>env-lexstack*!))
               (st (lens-set (list elstkfrm) st =>env-lexstack*!))
               (return (eval-progn-body (view func =>lambda-body*!))) ;; apply
               )
           (lens-set old-stack st =>env-lexstack*!)
           return
           )))))
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
  (let*((st (*the-environment*))
        (func (env-resolve-function st head)))
    (cond
     ((syntax-type?  func)
      (apply (syntax-eval func) head arg-exprs))
     ((macro-type?   func)
      (env-trace! st (cons head func)
       (lambda () (eval-form (apply (macro-procedure func) head arg-exprs)))))
     ((lambda-type?  func)
      (let ((return
             (env-trace! st (cons head func)
              (lambda () (eval-apply-lambda func arg-exprs)))))
        (cond ;; macro expand (if its a macro)
         ((eq? 'macro (view func =>lambda-kind*!)) (eval-form return))
         (else return)
         )))
     ((command-type? func)
      (env-trace! st (cons head func)
       (lambda () (eval-apply-proc (command-procedure func) arg-exprs))))
     ((procedure?    func)
      (env-trace! st (cons head func)
       (lambda () (eval-apply-proc func arg-exprs))))
     ((pair?         func)
      (match func
        (('lambda (args-exprs ...) body ...)
         (let ((func (apply (syntax-eval elisp-lambda) func)))
           (env-trace! st (cons head func)
            (lambda () (eval-apply-lambda func arg-exprs))
            )))
        (any (eval-error "invalid function" func))
        ))
     (else (eval-error "invalid function" head))
     )
    ))


(define (eval-form expr)
  ;; This is where evaluation begins. This is the actual `EVAL`
  ;; procedure for Emacs Lisp.
  (match expr
    (() '())
    ((head args ...) (eval-bracketed-form head args))
    (literal
     (cond
      ((symbol? literal)
       (let ((return (view literal =>elisp-symbol!)))
         (cond
          ((not return) (eval-error "void variable" literal))
          ((sym-type? return) (view return =>sym-value*!))
          (else return))))
      ((elisp-quote-scheme-type? literal) (elisp-unquote-scheme literal))
      (else literal)
      ))))


(define (eval-args-list arg-exprs)
  (let loop ((arg-exprs arg-exprs))
    (match arg-exprs
      (() '())
      ((expr more ...)
       (let ((result (eval-form expr)))
         (if (elisp-eval-error-type? result) result
             (cons result (loop more)))
         )))))


(define (eval-progn-body exprs)
  (let loop ((exprs exprs))
    (match exprs
      (() '())
      ((final) (eval-form final))
      ((head more ...) (eval-form head) (loop more))
      (exprs (error "no function body" exprs))
      )))

;;--------------------------------------------------------------------
;; Built-in macros
;;
;; Note that these macros always begin with a (next) statement. This
;; is because the macro evaluator for built-in macros does not skip
;; over the head of the form before evaluating the macro. This makes
;; built-in macro bahvior a bit more Scheme-like.

(define elisp-progn
  (make<syntax>
   (lambda expr
     (eval-progn-body (cdr expr)))))

(define elisp-prog1
  (make<syntax>
   (lambda expr
     (match (cdr expr)
       (() (eval-error "wrong number of arguments" "prog1" 0))
       ((first more ...)
        (let ((return (eval-form first)))
          (eval-progn-body more)
          return
          ))))))

(define elisp-prog2
  (make<syntax>
   (lambda expr
     (define (fail-nargs n)
       (eval-error "wrong number of arguments" "prog2" n))
     (match (cdr expr)
       (() (fail-nargs 0))
       ((any) (fail-nargs 1))
       ((first second more ...)
        (eval-form first)
        (let ((return (eval-form second)))
          (eval-progn-body more)
          return
          ))))))


(define (elisp-logic conjunction)
  (make<syntax>
   (lambda exprs
     (let loop ((exprs (cdr exprs)))
       (match exprs
         (() (if conjunction #t '()))
         ((final) (eval-form final))
         ((next more ...)
          (let ((success (eval-form next)))
            (if conjunction
                (if (elisp-null? success) '() (loop more))
                (if (elisp-null? success) (loop more) #t))
            )))))))

(define elisp-and (elisp-logic #t))
(define elisp-or  (elisp-logic #f))


(define elisp-if
  (make<syntax>
   (lambda exprs
     (match exprs
       (() (eval-error "wrong number of arguments" "if" 0))
       (('if cond-expr) (eval-error "wrong number of arguments" "if" 1))
       (('if cond-expr then-exprs else-exprs ...)
        (let ((result (eval-form cond-expr)))
          (if (not (elisp-null? result))
              (eval-form then-exprs)
              (eval-progn-body else-exprs)
              )))))))


(define elisp-when-unless
  (make<syntax>
   (lambda exprs
     (define (is   cond) (not (elisp-null? cond)))
     (define (isnt cond) (elisp-null? cond))
     (define (eval on-bool cond-expr body-expr)
       (let ((result (eval-form cond-expr)))
         (if (not (on-bool result)) #f
             (eval-progn-body body-expr))
         ))
     (match exprs
       (() (eval-error "wrong number of arguments" "when or unless" #:min 1))
       (('when   cond-expr body-expr ...)
        (eval is   cond-expr body-expr))
       (('unless cond-expr body-expr ...)
        (eval isnt cond-expr body-expr))
       (any (eval-error "wrong number of arguments" (car any) #:min 1))
       ))))


(define elisp-cond
  (make<syntax>
   (lambda exprs
    (define (eval-cond test body)
      (let ((result (eval-form test)))
        (if (not (elisp-null? result))
            (values #t (eval-progn-body body))
            (values #f #f))))
     (let loop ((exprs (cdr exprs)))
       (match exprs
         (() '())
         ((() more ...) (loop more))
         (((cond-expr body ...) more ...)
          (let-values
              (((success return) (eval-cond cond-expr body)))
            (if success return (loop more))
            )))))))


(define elisp-while
  (make<syntax>
   (lambda exprs
     (match (cdr exprs)
       (() (eval-error "wrong number of arguments" "while" 0))
       ((cond-expr body ...)
        (let loop ()
          (let ((success (eval-form cond-expr)))
            (if (not (elisp-null? success))
                (let () (eval-progn-body body) (loop))
                '() ;; while forms always evaluate to nil
                ))))))))


(define elisp-dotimes
  (make<syntax>
   (lambda exprs
     (define (fail-nargs n)
       (eval-error "wrong number of arguments" "dotimes" n))
     (match (cdr exprs)
       (() (fail-nargs 0))
       ((any) (fail-nargs 1))
       ((() any ...) (eval-error "expecting variable name" "dotimes" '()))
       (((var) any ...) (eval-error "expecting initial number" "dotimes" var))
       (((var limit-expr final-exprs ...) body ...)
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
  (make<syntax>
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
       (((var list-expr) body ...)
        (run var list-expr #f body))
       (((var list-expr result-expr) body ...)
        (run var list-expr result-expr body))
       (any (eval-error "wrong number of arguments" "dolist" any))
       ))))


(define elisp-setq
  (make<syntax>
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
             ((sym expr more ...)
              (let ((val (eval-form expr)))
                (cond
                 ((elisp-eval-error-type? val) val)
                 (else
                  (bind! sym val)
                  (loop more (+ 2 argc) val)
                  ))))))))))))


(define elisp-let
  (make<syntax>
   (lambda expr
     (let ((st (*the-environment*)))
       (match (cdr expr)
         (() '())
         (((bindings ...) progn-body ...)
          (let loop ((unbound bindings) (bound '()) (size 0))
            (match unbound
              (()
               (env-push-new-elstkfrm! st size (reverse bound))
               (let ((result (eval-progn-body progn-body)))
                 (env-pop-elstkfrm! st)
                 result))
              (((sym) more ...)
               (loop more (cons (new-symbol sym '()) bound) (+ 1 size))
               )
              (((sym expr) more ...)
               (let ((result (eval-form expr)))
                 (loop more
                       (cons (new-symbol (ensure-string sym) result) bound)
                       (+ 1 size))
                 ))
              (((sym expr extra ...) ,more ...)
               (eval-error "bindings can have only one value form" (car unbound))
               )
              ((sym more ...)
               (loop more (cons (new-symbol sym '()) bound) (+ 1 size))
               )
              (otherwise
               (eval-error "wrong type argument, expecting list" otherwise)
               ))))
         (,otherwise
          (eval-error "wrong type argument, expecting list" otherwise)
          ))))))


(define elisp-let*
  (make<syntax>
   (lambda expr
     (let ((st (*the-environment*)))
       (match (cdr expr)
         (() '())
         (((bindings ...) progn-body ...)
          (let ((elstkfrm (env-push-new-elstkfrm! st (length bindings) '())))
            (let loop ((unbound bindings))
              (match unbound
                (()
                 (let ((result (eval-progn-body progn-body)))
                   (env-pop-elstkfrm! st)
                   result))
                (((sym) more ...)
                 (elstkfrm-sym-intern! elstkfrm sym '())
                 (loop more)
                 )
                (((sym expr) more ...)
                 (let ((result (eval-form expr)))
                   (elstkfrm-sym-intern! elstkfrm sym result)
                   (loop more)
                   ))
                (((sym expr extra ...) ,more ...)
                 (eval-error "bindings can have only one value form" (car unbound))
                 )
                ((sym more ...)
                 (elstkfrm-sym-intern! elstkfrm sym '())
                 (loop more)
                 )
                (otherwise
                 (eval-error "wrong type argument, expecting list" otherwise)
                 )))))
         (otherwise
          (eval-error "wrong type argument, expecting list" otherwise)
          ))))))


(define elisp-lambda
  (make<syntax>
   (lambda expr
     (let ((expr (cdr expr)))
       (match expr
         (() (new-lambda))
         (((args ...) body ...)
          (eval-defun-args-body 'lambda args body))
         (args (eval-error "invalid lambda" args))
         )))))


(define variable-documentation "variable-documentation")


(define elisp-defvar
  (make<syntax>
   (lambda expr
     (let ((def (car expr))
           (expr (cdr expr)))
       (define (defvar sym-expr val-expr docstr)
         (let*((sym (eval-ensure-interned sym-expr))
               (val (if val-expr (eval-form val-expr) #f))
               )
           (lens-set val sym =>sym-value*!)
           (when docstr
             (lens-set docstr sym
              (=>sym-plist! (sym-name sym))
              (=>hash-key! variable-documentation)))
           val
           ))
       (match expr
         (() (eval-error "wrong number of arguments" def '()))
         ((sym-expr) (defvar sym-expr #f #f))
         ((sym-expr val-expr) (defvar sym-expr val-expr #f))
         ((sym-expr val-expr docstr) (defvar sym-expr val-expr docstr))
         (any (eval-error "wrong number of arguments" def any))
         )))))


(define elisp-defun-defmacro
  (make<syntax>
   (lambda expr
     (let ((def  (car expr))
           (expr (cdr expr))
           )
       (match expr
         (() (eval-error "wrong number of arguments" def '()))
         ((sym) (eval-error "wrong number of arguments" def sym))
         ((sym (args ...) body ...)
          (cond
           ((symbol? sym)
            (let*((func   (eval-defun-args-body def args body))
                  (name   (symbol->string sym))
                  (=>name (=>env-obarray-key! name))
                  (st     (*the-environment*))
                  (obj    (view st =>name))
                  (obj    (if obj obj
                              (let ((obj (new-symbol name)))
                                (lens-set! obj st =>name)
                                obj)))
                  )
              (cond
               ((or (eq? def 'defun) (eq? def 'defsubst)) func)
               ((eq? def 'defmacro) (lens-set 'macro func =>lambda-kind*!))
               (else (error "expecting function head to be 'defun or 'defmacro" def))
               )
              (cond
               ((sym-type? obj) ;; update and return the interned symbol object
                (let ((old-func (view obj =>sym-function*!)))
                  (cond
                   ((lambda-type? old-func)
                    (lambda-copy-into! old-func func))
                   (else
                    (lens-set func obj =>sym-function*!)))
                  obj))
               (else (eval-error "failed to intern symbol" sym obj)))
              ))
           (else (eval-error "wrong type argument" sym #:expecting "symbol"))
           ))
         ((name args body ...) (eval-error "malformed arglist" args))
         )))))


(define (eval-defalias sym-expr val-expr docstr)
  (let*((st (*the-environment*))
        (sym (eval-ensure-interned (eval-form sym-expr)))
        (val (eval-form val-expr))
        (func
         (let loop ((val val))
           (cond
            ((symbol? val)
             (loop (view st (=>env-obarray-key! (symbol->string val)))))
            ((sym-type? val) (view val =>sym-function*!))
            ((lambda-type? val) val)
            ((procedure? val) val)
            ((command-type? val) val)
            (else #f)
            )))
        )
    (cond
     ((not func) (eval-error "void function" val))
     ((not sym) (eval-error "wrong type argument" sym #:expecting "symbol"))
     (else
      (lens-set! func sym =>sym-function*!))
     )))


(define elisp-defalias
  (make<syntax>
   (lambda expr
     (match (cdr expr)
       ((sym-expr val-expr) (eval-defalias sym-expr val-expr #f))
       ((sym-expr val-expr docstr) (eval-defalias sym-expr val-expr docstr))
       (any
        (eval-error "wrong number of arguments" "defalias"
         (length any) #:min 2 #:max 3))
       ))))


(define (set-function-body! func body-exprs)
  ;; This procedure scans through a `BODY-EXPR` for `DECLARE` and
  ;; `INTERACTIVE` forms and both executes them, updating the given
  ;; `FUNC` in place, and also removes them from the `BODY-EXPRS` so
  ;; that the evaluator never executes them directly.
  (define (filter body-exprs)
    (match body-exprs
      (() '())
      ((('declare args ...) body-exprs ...)
       (let loop ((args args))
         (match args
           (() #t)
           (((sym vals ...) args ...)
            (cond
             ((symbol? sym)
              (lens-set vals
               func =>lambda-declares*! (=>hash-key! (symbol->string sym)))
              (loop args))
             (else
              (loop args) ;; ignored malformed declarations
              )))
           ))
       (filter body-exprs)
       )
      ((('interactive args ...) body-exprs ...)
       (lens-set args func =>lambda-interactive*!)
       (filter body-exprs)
       )
      ((any-expr body-exprs ...) ;; anything else, leave it alone
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
      ((docstr body-expr ...)
       (cond
        ((string? docstr)
         (lens-set docstr func =>lambda-docstring*!)
         (set-function-body! func body-expr)
         func)
        (else
         (set-function-body! func (cons docstr body-expr))
         func)))
      (body-expr
       (set-function-body! func body-expr)
       func)
      ))
  (define (get-rest-args arg-exprs args optargs)
    (match arg-exprs
      (() (eval-error "invalid function, no argument declaration after &rest delimiter"))
      ((sym)
       (cond
        ((symbol? sym)
         (get-body (new-lambda kind (reverse args) (reverse optargs) sym)))
        (else (non-symbol-error sym))
        ))
      ))
  (define (get-opt-args arg-exprs args optargs)
    (match arg-exprs
      (() (get-body (new-lambda kind (reverse args) (reverse optargs))))
      (('&optional more ...)
       (eval-error "invalid function, more than one &optional argument delimiter" arg-exprs))
      (('&rest arg-exprs ...)
       (get-rest-args arg-exprs args optargs))
      ((sym arg-exprs ...)
       (cond
        ((symbol? sym) (get-opt-args arg-exprs args (cons sym optargs)))
        (else (non-symbol-error sym))
        ))
      ))
  (define (get-args arg-exprs args)
    (match arg-exprs
      (() (get-body (new-lambda kind (reverse args))))
      (('&optional arg-exprs ...) (get-opt-args arg-exprs args '()))
      (('&rest arg-exprs ...) (get-rest-args arg-exprs args '()))
      ((sym arg-exprs ...)
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
    (let-values
        (((obj return)
          (update&view updater st
           (=>env-obarray-key! (ensure-string name/sym)))))
      return
      ))
   ))


(define (eval-ensure-interned sym)
  ;; A procedure that returns a <SYM-TYPE> object or creates a new
  ;; interned symbol and returns it.
  ;;------------------------------------------------------------------
  ;; TODO: place this API into (gypsum elisp-eval environment) instead?
  (let ((st (*the-environment*)))
    (cond
     ((symbol? sym)
      (let*((name (symbol->string sym))
            (obj (view st (=>env-obarray-key! name))))
        (cond
         (obj obj)
         (else
          (let ((obj (new-symbol name)))
            (lens-set obj st (=>env-obarray-key! name))
            obj
            )))))
     ((sym-type? sym) sym)
     (else (eval-error "wrong type argument" sym #:expecting "symbol"))
     )))


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
  (view-on-symbol st sym
   (lambda (obj)
     (view obj (=>sym-plist! sym) (=>hash-key! prop)))))

(define (eval-put st sym prop val)
  (update-on-symbol st sym
   (lambda (obj)
     (let ((prop (ensure-string prop)))
       (values (lens-set! val obj (=>sym-plist! sym) (=>hash-key! prop)) val)
       ))))

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
  (let ((sym (ensure-string sym)))
    (update-on-symbol st sym
     (lambda (obj)
       (values
        (lens-set (elisp->scheme func) obj (=>sym-function! sym))
        func
        )))))

(define (eval-fmakunbound st sym)
  (view-on-symbol st sym (lambda (obj) (view st (=>env-obarray-key! sym)))))

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;;--------------------------------------------------------------------------------------------------
;; Built-in functions

(define (eval-apply collect args)
  (match args
    (() (eval-error "wrong number of arguments" args))
    ((func args ...) (eval-bracketed-form func (collect args)))))

(define (re-collect-args args)
  ;; Collect arguments to `APPLY` and `FUNCALL` into a list single.
  (match args
    (() args)
    (((args ...)) args)
    ((arg (args ...)) (cons arg args))
    ((arg args ...) (cons arg (re-collect-args args)))
    ))

(define (elisp-funcall . args) (eval-apply (lambda (id) id) args))

(define (elisp-apply . args) (eval-apply re-collect-args args))

(define elisp-function
  (make<syntax>
   (lambda args
     (match args
       (('function arg)
        (let ((is-lambda? (lambda (o) (and (pair? o) (symbol? (car o)) (eq? 'lambda (car o)))))
              (make-lambda (lambda (o) (apply (macro-procedure elisp-lambda) o)))
              )
          (cond
           ((symbol? arg)
            (let ((result (view arg =>elisp-symbol!)))
              (cond
               ((sym-type? result) (view result =>sym-function*!))
               ((lambda-type? result) result)
               ((is-lambda? result) (make-lambda result))
               (else arg)
               )))
           ((is-lambda? arg) (make-lambda arg))
           (else
            (eval-error "wrong type argument" "function" arg)
            ))))
       (any (eval-error "wrong number of arguments" "function" #:expecting 1 #:value any))
       ))))


(define (elisp-symbol-op name type? op)
  (lambda args
    (match args
      ((sym)
       (cond
        ((type? sym)
         (scheme->elisp (op (*the-environment*) sym)))
        (else
         (eval-error
          "wrong type argument" name
          sym #:expecting "symbol")
         )))
      (any
       (eval-error
        "wrong number of arguments" name
        (length any) #:expecting 1))
      )))

(define (elisp-symbol-op2 name type? op)
  (lambda args
    (match args
      ((sym val)
       (cond
        ((type? sym)
         (scheme->elisp (op (*the-environment*) sym (scheme->elisp val))))
        (else
         (eval-error
          "wrong type argument" name
          sym #:expecting "symbol"))
        ))
      (any
       (eval-error
        "wrong number of arguments" name
        (length any) #:expecting 2)
       ))))

(define (elisp-make-symbol . args)
  (match args
    ((name)
     (cond
      ((symbol/string? name)
       (eval-make-symbol (ensure-string name)))
      (else
       (eval-error "wrong type argument" name #:expecting "symbol or string"))
      ))
    (any
     (eval-error "wrong number of arguments" "make-symbol"
      (length any) #:expecting 1))
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
    ((func) (eval-mapatoms (eval-apply-as-proc func)))
    ((func obarray)
     (eval-mapatoms (eval-apply-as-proc func) obarray))
    ((any ...)
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
    ((sym prop val)
     (eval-put
      (*the-environment*)
      (symbol->string sym)
      (symbol->string prop)
      val))
    (args
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
  (make<syntax>
   (lambda args
     (match (cdr args)
       ((expr) expr)
       ((expr extra ...) (eval-error "wrong number of arguments" "quote" extra))
       (any any)
       ))))


(define elisp-backquote
  (make<syntax>
   (lambda exprs
     (match (cdr exprs)
       ((exprs)
        (let expr-loop ((exprs exprs))
          (match exprs
            (() '())
            ((('|,| unq) exprs ...)
             (cons (eval-form unq) (expr-loop exprs))
             )
            ((('|,@| splice) exprs ...)
             (let ((elems (eval-form splice)))
               (let elem-loop ((elems elems))
                 (cond
                  ((null? elems) (expr-loop exprs))
                  ((pair? elems) (cons (car elems) (elem-loop (cdr elems))))
                  (else (cons elems (expr-loop exprs)))
                  )))
             )
            (((sub-exprs ...) exprs ...)
             (cons (expr-loop sub-exprs) (expr-loop exprs))
             )
            ((elem exprs ...) (cons elem (expr-loop exprs)))
            (elem elem)
            )))
       (any (eval-error "wrong number of arguments" "backquote" any))
       ))))


(define *macroexpand-max-depth* 16)

(define (eval-macroexpander all depth fail-depth)
  (case-lambda
    ((expr) ((eval-macroexpander all depth fail-depth) expr (*the-environment*)))
    ((expr st)
     (let loop ((depth depth) (expr expr))
       (match expr
         (() '())
         ((label args ...)
          (let ((args
                 (if all
                     (map (lambda (expr) (loop depth expr)) args)
                     args)))
            (cond
             ((symbol? label)
              (let ((func (env-resolve-function st label)))
                (cond
                 ((and (lambda-type? func)
                       (eq? 'macro (view func =>lambda-kind*!)))
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
         (any any)
         )))))

(define (elisp-macroexpander all depth fail-depth)
  (lambda expr
    (match expr
      ((expr) ((eval-macroexpander all depth fail-depth) expr))
      ((expr env) ((eval-macroexpander all depth fail-depth) expr env))
      (any (eval-error "wrong number of arguments" "macroexpand" any))
      )))

(define elisp-macroexpand
  (elisp-macroexpander #f *macroexpand-max-depth* #t))

(define elisp-macroexpand-1
  (elisp-macroexpander #f 1 #f))

(define elisp-macroexpand-all
  (elisp-macroexpander #t *macroexpand-max-depth* #t))


(define (elisp-format . args)
  (match args
    ((fstr args ...)
     (cond
      ((string? fstr) (scheme->elisp (apply format fstr args)))
      (else (eval-error "wrong type argument" fstr #:expecting "string"))
      ))
    ))

(define (elisp-prin1 . args)
  (match args
    ((val) ((*impl/prin1*) val) val)
    ((val port) ((*impl/prin1*) val port) val)
    ((val port overrides) ((*impl/prin1*) val port overrides) val)
    (any
     (eval-error
      "wrong number of arguments" "prin1"
      (length any) #:min 1 #:max 3))
    ))

(define (elisp-princ . args)
  (match args
    ((val) ((*impl/princ*) val) val)
    ((val port) ((*impl/princ*) val port) val)
    (any (eval-error "wrong number of arguments" "princ" #:min 1 #:max 2))
    ))

(define eval-print
  (case-lambda
    ((val) (eval-print val *elisp-output-port*))
    ((val port)
     (newline port)
     ((*impl/prin1*) val port)
     (newline port))
    ))

(define (elisp-print . args)
  (match args
    ((val) (eval-print val))
    ((val port) (eval-print val port))
    (any (eval-error "wrong number of arguments" "print" #:min 1 #:max 2))
    ))

(define (elisp-message . args)
  (match args
    (() (eval-error "wrong number of arguments" "message" #:min 1))
    ((format-str args ...)
     (let ((port (*elisp-error-port*)))
       (apply format-to-port port format-str args)
       (newline port)
       '()
       ))))


(define (elisp-load . args)
  (match args
    ((filepath)
     (cond
      ((string? filepath) (elisp-load! filepath (*the-environment*)))
      (else (eval-error "wrong type argument" filepath #:expecting "string"))
      ))
    (any
     (eval-error "wrong number of arguments" "load"
      (length any) #:min 1 #:max 2))
    ))


(define elisp-sxhash-equal (pure 1 "sxhash-equal" hash))


(define (elisp-make-keymap . args)
  (match args
    (() (keymap))
    ((name)
     (let ((km (keymap)))
       (lens-set name km =>keymap-label!) km))
    (any
     (eval-error
      "wrong number of arguments"
      "make-keymap" #:min 0 #:max 1))
    ))


(define (elisp-define-key . args)
  (define (define-key keymap key binding remove)
    (let ((binding (if binding binding nil))
          (=>lens (lens =>keymap-top-layer! (=>keymap-layer-index! key)))
          )
      (cond
       ((not (keymap-type? keymap))
        (eval-error "wrong type argument" keymap #:expecting "keymapp"))
       (remove (lens-set #f keymap =>lens))
       (else (lens-set binding keymap =>lens)))
      binding
      ))
  (match args
    ((keymap key binding)
     (define-key keymap key binding #f))
    ((keymap key binding remove)
     (define-key keymap key binding remove))
    (any
     (eval-error
      "wrong number of arguments"
      "define-key" #:min 3 #:max 4))
    ))


(define *elisp-init-env*
  ;; A parameter containing the default Emacs Lisp evaluation
  ;; environment. This environment is an ordinary association list
  ;; mapping strings (or symbols) to values. Any values satisfying the
  ;; predicate `LAMBDA-TYPE?` are automatically interned as functions
  ;; rather than ordinary values.
   ;;------------------------------------------------------------------
  (make-parameter
   `(;; ---- beginning of association list ----

     ,nil
     ,t
     ,(new-symbol "load-path" nil)
     ,(new-symbol "load-file-name")
     ,(new-symbol "noninteractive" t)
     ,(new-symbol "after-load-functions" nil)
     ,(new-symbol "features" nil)

     (lambda    . ,elisp-lambda)
     (apply    . ,elisp-apply)
     (funcall  . ,elisp-funcall)
     (defun    . ,elisp-defun-defmacro)
     (defsubst . ,elisp-defun-defmacro)
     (defmacro . ,elisp-defun-defmacro)
     (defalias . ,elisp-defalias)
     (defvar   . ,elisp-defvar)
     (function . ,elisp-function)
     (progn    . ,elisp-progn)
     (prog1    . ,elisp-prog1)
     (prog2    . ,elisp-prog2)
     (setq     . ,elisp-setq)
     (let      . ,elisp-let)
     (let*     . ,elisp-let*)

     (cond     . ,elisp-cond)
     (if       . ,elisp-if)
     (when     . ,elisp-when-unless)
     (unless   . ,elisp-when-unless)
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

     (macroexpand      . ,elisp-macroexpand)
     (macroexpand-1    . ,elisp-macroexpand-1)
     (macroexpand-all  . ,elisp-macroexpand-all)

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

     (format           . ,elisp-format)
     (message          . ,elisp-message)
     (prin1            . ,elisp-prin1)
     (princ            . ,elisp-princ)
     (print            . ,elisp-print)

     (load             . ,elisp-load)

     (sxhash-equal       . ,elisp-sxhash-equal)
     (make-keymap        . ,elisp-make-keymap)
     (make-sparse-keymap . ,elisp-make-keymap)
     (define-key         . ,elisp-define-key)

     (run-hooks                        . ,elisp-run-hooks)
     (run-hooks-with-args              . ,elisp-run-hooks-with-args)
     (run-hook-with-args-until-failure . ,elisp-run-hook-with-args-until-failure)
     (run-hook-with-args-until-success . ,elisp-run-hook-with-args-until-success)

     ;; ------- end of assocaition list -------
     )))


(define elisp-reset-init-env!
  (case-lambda
    (() (elisp-reset-init-env! (*elisp-init-env*)))
    ((init-env) (elisp-reset-init-env! init-env (*the-environment*)))
    ((init-env env) (env-alist-defines! env init-env))))


(define new-environment
  ;; Construct a new Emacs Lisp environment object, which is a bit
  ;; like an obarray.
  (case-lambda
    (() (new-environment #f))
    ((inits) (new-environment inits #f))
    ((inits size)
     (let*((size (if (integer? size) size *default-obarray-size*))
           (inits (if (and inits (pair? inits)) inits (*elisp-init-env*)))
           (env (new-empty-environment size))
           (errors (elisp-reset-init-env! inits env))
           )
       (for-each
        (lambda (err) (display ";;Warning, not a declaration: ") (write err) (newline))
        errors)
       env))))

(elisp-reset-init-env!)

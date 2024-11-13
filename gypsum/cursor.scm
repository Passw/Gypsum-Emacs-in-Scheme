
(define-record-type <cursor-type>
  ;; A cursor is useful for indexing through strings and vectors.
  ;;------------------------------------------------------------------
  (make<cursor> obj index iface)
  cursor-type?
  (obj    cursor-object  set!cursor-object) ;; <- the object being inspected
  (index  cursor-index   %set!cursor-index%)  ;; <- the current index
  (iface  cursor-interface) ;; <- pointer to a <cursor-interface>
  )

(define-record-type <cursor-interface>
  ;; It would be nice if R7RS had a standard mechanism for generic
  ;; methods, but it does not, so we declare a method dispatch by
  ;; hand for the object of inspection of cursor types.
  (make<cursor-interface> fend fref fstep fjmp)
  cursor-interface?
  (fend   cursor-report-end) ;; <- predicate returns `#t` when iteration is done
  (fref   cursor-referencer) ;; <- procedure used to access the current index
  (fstep  cursor-stepper)    ;; <- procedure to step to the next index (might be `#f`)
  (fjmp   cursor-jumper)     ;; <- procedure to move to some other index (might be `#f`)
  )

(define cursor-iface-listbox
  (let ((listbox '()))
    (case-lambda
      (() listbox)
      ((up) (set! listbox (up listbox)))
      )))

(define (declare-interface/cursor pred iface)
  (cond
   ((not (procedure? pred)) (error "not a procedure" pred))
   ((not (cursor-interface? iface)) (error "not a cursor interface" iface))
   (else (cursor-iface-listbox (endo-set iface (=>assoc-by =>bring eq? pred))))
   ))

(define (get-interface/cursor obj)
  (let*((found (view (cursor-iface-listbox) (=>find (lambda (item) ((car item) obj))))))
    (if (and found (not (null? found))) (cdr found) #f)
    ))


(define (set!cursor-index cur i)
  (let*((obj   (cursor-object cur))
        (iface (cursor-interface cur))
        (fjmp  (if iface (cursor-jumper iface) #f))
        )
    (cond
     (fjmp (fjmp obj i) (%set!cursor-index% cur i))
     (else
      (error
       "cursors have no random access on objects of this type"
       (cursor-object obj)))
     )))

(define =>cursor-object!
  (record-unit-lens cursor-object set!cursor-object '=>cursor-object!))

(define =>cursor-index!
  (record-unit-lens cursor-index set!cursor-index '=>cursor-index!))

(define (cursor-ref cur)
  ((cursor-referencer (cursor-interface cur)) (cursor-object cur) (cursor-index cur)))

(define (cursor-end? cur)
  ((cursor-report-end (cursor-interface cur)) (cursor-object cur) (cursor-index cur)))

(define (cursor-step! cur)
  (let ((fstep (cursor-stepper (cursor-interface cur)))
        (i (+ 1 (cursor-index cur)))
        )
    (when fstep
      (let ((new-obj (fstep (cursor-object cur) i)))
        (when new-obj
          (set!cursor-object cur new-obj))
        ))
    (%set!cursor-index% cur i)
    i))

(define (cursor-collect-list cursor)
  ;; Iterate through `CURSOR` until applying it to `CURSOR-END?`
  ;; evaluates to `#F`, collecting each elements and storing it into a
  ;; list. Return the list.
  (if (cursor-end? cursor) '()
      (cons
       (cursor-ref cursor)
       (cursor-collect-list (let () (cursor-step! cursor) cursor)))
      ))

(define *vector-cursor-interface*
  (make<cursor-interface>
   (lambda (obj i) (>= i (vector-length obj)))
   vector-ref #f (lambda (obj i) (values))
   ))

(define *list-cursor-interface*
  (make<cursor-interface>
   (lambda (obj i) (null? obj))
   (lambda (obj i) (car obj))
   (lambda (obj i) (cdr obj))
   #f))

(define *string-cursor-interface*
  (make<cursor-interface>
   (lambda (obj i) (>= i (string-length obj)))
   string-ref #f (lambda (obj i) (values)))
  )

(define new-cursor
  ;; Construct a <cursor-type> object with a vector, list, or string.
  ;; Optionally pass the initial index as the second argument.
  ;;
  ;;  1. The object over which to iterate
  ;;  2. The initial index (defaults to zero)
  ;;
  ;; If you want to iterate over other data types, you must pass four
  ;; additional arguments (the four iteration procedures), which are:
  ;;
  ;;  3. procedure to access the current index in the `CURSOR-OBJECT`
  ;;  4. predicate returns `#t` when iteration is done.
  ;;  5. procedure to step to the next index in the `CURSOR-OBJECT`
  ;;  6. optional `#f`, procedure to change the current index
  ;;
  ;; It is also possible to declare other types for which a cursor can
  ;; be constructed using `declare-rule/cursor-iteration`.
  ;;------------------------------------------------------------------
  (case-lambda
    ((obj) (new-cursor obj 0))
    ((obj i)
     (cond
      ((vector? obj)
       (make<cursor> obj i *vector-cursor-interface*))
      ((or (null? obj) (pair? obj))
       (make<cursor> obj i *list-cursor-interface*))
      ((hash-table? obj)
       (make<cursor> (hash-table->alist obj) i *list-cursor-interface*))
      ((string? obj)
       (make<cursor> obj i *string-cursor-interface*))
      (else
       (let ((iface (get-interface/cursor obj)))
         (cond
          (iface (make<cursor> obj i iface))
          (else (error "cannot create cursor for object of type" obj))
          ))))
     )))

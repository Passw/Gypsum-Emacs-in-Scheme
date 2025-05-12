
(define-record-type <bin-hash-table-type>
  ;; This exists because SRFI-69 does not provide any API for
  ;; discovering the number of bins in the vector backing store of the
  ;; hash table, even though it is an argument to the constructor.
  (make<bin-hash-table> store-size hash-table)
  bin-hash-table-type?
  (store-size get-bin-hash-table-store-size set!bin-hash-table-store-size)
  (hash-table get-bin-hash-table-hash set!bin-hash-table-hash))

(cond-expand
  (guile-3
   (set-record-type-printer!
    <bin-hash-table-type>
    (lambda (bht port)
      (display
       (format
        "(bin-hash-table ~s ~s)"
        (get-bin-hash-table-store-size bht)
        (let ((ht (get-bin-hash-table-hash bht)))
          (if (not ht) #f (hash-table->alist ht))))
       port)))

   (define (hash-table-empty? ht) (<= (hash-table-size ht) 0))
      ;; Guile does not yet provide SRFI-125
   )
  (else))

(define *bin-hash-table-init-size* (make-parameter 11))

(define *default-key-hash*
  (make-parameter (lambda (key size) (modulo (default-hash key) size))))

(define (bin-hash-table-size bht)
  (hash-table-size (get-bin-hash-table-hash bht)))

(define *default-make-hash-table*
  (make-parameter
   (lambda (size)
     ;; TODO: make use of the `SIZE` parameter.
     (cond-expand
       (gauche
        (make-hash-table equal-comparator))
       (else
        (make-hash-table equal? (*default-key-hash*))))
     )))

(define empty-bin-hash-table 
  ;; This lazy procedure defines a new constructor for a hash table
  ;; that will always use the given arguments to construct it. The
  ;; NEW-SIZED-HASH-TABLE argument must be a procedure that takes the
  ;; SIZE argument and constructs a new inner hash table, this
  ;; function will be called on demand, and you may specify the kind
  ;; of hash table you want to construct, for example whether you want
  ;; weak key references. If the `NEW-SIZED-HASH-TABLE` is not
  ;; specified, the `MAKE-HASH-TABLE` procedure is used. If the
  ;; initial size is not given, the `*BIN-HASH-TABLE-INIT-SIZE*`
  ;; parameter is used, and you can also parameterize this function.
  (case-lambda
    (() (empty-bin-hash-table (*bin-hash-table-init-size*) (*default-make-hash-table*)))
    ((size) (empty-bin-hash-table size (*default-make-hash-table*)))
    ((size new-sized-hash-table)
     (lambda () (make<bin-hash-table> size (new-sized-hash-table size))))))

(define (default-accum a b) b)

(define alist->bin-hash-table
  (case-lambda
    ((elems)
     (alist->bin-hash-table
      elems (*bin-hash-table-init-size*) default-accum (*default-make-hash-table*)))
    ((elems size)
     (alist->bin-hash-table elems size default-accum (*default-make-hash-table*)))
    ((elems size accum)
     (alist->bin-hash-table elems size accum (*default-make-hash-table*)))
    ((elems size accum new-sized-hash-table)
     (let*((bins (make<bin-hash-table> size (new-sized-hash-table size)))
           (table (get-bin-hash-table-hash bins))
           (put
            (cond
             ((eq? accum default-accum)
              (lambda (key val) (hash-table-set! table key val)))
             (else
              (lambda (key new)
                (hash-table-update!/default table key (lambda (old) (accum old new)) #f)))
             ))
           )
       (let loop ((elems elems))
         (cond
          ((null? elems) bins)
          (else
           (let ((pair (car elems))
                 (next (cdr elems)))
             (put (car pair) (cdr pair))
             (loop (cdr elems)))
           )))))))

(define (bin-hash-table->alist bht)
  (hash-table->alist (get-bin-hash-table-hash bht)))

(define (bin-hash-table-empty? bht)
  (or (not bht)
      (let ((ht (get-bin-hash-table-hash bht)))
        (or (not ht) (hash-table-empty? ht)))))

(define =>bin-hash-table-store-size!
  (record-unit-lens
   get-bin-hash-table-store-size
   set!bin-hash-table-store-size
   '=>bin-hash-table-store-size!))

(define =>bin-hash-table-hash*!
  (record-unit-lens
   get-bin-hash-table-hash
   set!bin-hash-table-hash
   '=>bin-hash-table-hash*!))

(define =>bin-hash-table-hash!
  ;; Access the srfi-69 or srfi-125 hash-table within the
  ;; <BIN-HASH-TABLE-TYPE>, both the inner hash table and the
  ;; <BIN-HASH-TABLE-TYPE> are canonicalized. The
  ;; <BIN-HASH-TABLE-TYPE> makes no assumptions about what kind of
  ;; hash-table is stored within it, just that the HASH-TABLE-EMPTY?
  ;; procedure is defined over it, so in order for the =>CANONICAL
  ;; lens to create a new hash table when a new one is necessary, you
  ;; may provide the NEW-SIZE and NEW-SIZED-HASH-TABLE procedure (a
  ;; procedure that takes NEW-SIZE as an argumetn) to this lens
  ;; constructor, these two arguments are passed to the
  ;; empty-bin-hash-table procedure. If the size and constructor
  ;; procedure arguments are not given, the defaults are used.
  ;;------------------------------------------------------------------
  (case-lambda
    (() (=>bin-hash-table-hash! (*bin-hash-table-init-size*) make-hash-table))
    ((new-size) (=>bin-hash-table-hash! new-size make-hash-table))
    ((new-size new-sized-hash-table)
     (=>canonical
      =>bin-hash-table-hash*!
      (lambda () (new-sized-hash-table new-size))
      bin-hash-table-empty?))))

(define =>bin-hash-key!
  ;; This is a canonicalized lens for a key into a SRFI-69 or SRFI-125
  ;; hash table which is constructed according to
  ;; MAKE-KEY-TABLE-INNER-HASH which is contained within the
  ;; KEY-TABLE-UTF-MAP and KEY-TABLE-ASCII-MAP fields of the
  ;; <KEY-TABLE-TYPE> data structure."
  ;;------------------------------------------------------------------
  (case-lambda
    ((key) (=>bin-hash-key! key (*bin-hash-table-init-size*) make-hash-table))
    ((key new-size) (=>bin-hash-key! key new-size make-hash-table))
    ((key new-size new-sized-hash-table)
     (=>encapsulate
      (=>canonical
       (lens
        (=>bin-hash-table-hash! new-size new-sized-hash-table)
        (=>canonical
         (=>hash-key! key)
         (lambda () (new-sized-hash-table (*bin-hash-table-init-size*)))
         hash-table-empty?))
       (empty-bin-hash-table)
       bin-hash-table-empty?)
      (list '=>bin-hash-key! key)
      ))))

(define hash-table-copy-with
  ;; deep-copy a hash table along with a `COPIER` for deep-copying keys.
  ;;------------------------------------------------------------------
  (case-lambda
    ((ht) (hash-table-copy-with (lambda (id) id)))
    ((ht copier)
     (cond
      ((not ht) #f)
      ((hash-table? ht)
       (let*((new-ht (hash-table-copy ht))
             (replace-w/copy
              (lambda (key val) (hash-table-set! new-ht key (copier val)))))
         (hash-table-walk ht replace-w/copy)
         new-ht))
      (else
       (display ";; deep-copy hash-table FAILED, not a hash-table: ")(write ht)(newline);;DEBUG
       ht)))))

(define bin-hash-table-copy
  (case-lambda
    ((bht) (bin-hash-table-copy bht (lambda (id) id)))
    ((bht copier)
     (cond
      ((not bht) #f)
      ((bin-hash-table-type? bht)
       (make<bin-hash-table>
        (get-bin-hash-table-store-size bht)
        (hash-table-copy-with (get-bin-hash-table-hash bht) copier)))
      (else
       (error ";; deep-copy bin-hash-table FAILED, not a bin-hash-table ")(write bht)(newline);;DEBUG
       bht)))))

(define (default-pair-printer pair)
  (print
   (bracketed 1 #\( #\) (print (car pair) " . " (cdr pair)))
   (newline-indent)))

(define bin-hash-table-print
  ;; A (GYPSUM PRETTY) pretty-printer for the `<BIN-HASH-TABLE-TYPE>`.
  ;; Takes a `PAIR-PRINT` argument to print each association in the
  ;; table.
  (case-lambda
    ((bht) (bin-hash-table-print bht default-pair-printer))
    ((bht pair-print)
     (let ((binsz (get-bin-hash-table-store-size bht)))
       (bracketed 1 #\( #\)
        "alist->bin-hash-table"
        (if (bin-hash-table-empty? bht) " '()"
            (print
             (newline-indent)
             (apply bracketed 2 "'(" ")"
              (map pair-print (hash-table->alist (get-bin-hash-table-hash bht))))
             (if (= binsz (*bin-hash-table-init-size*)) #f
                 (print #\space (number->string binsz))))
            ))))))

(define (bin-hash-table-fold bht fold init)
  (hash-table-fold (get-bin-hash-table-hash bht) fold init))

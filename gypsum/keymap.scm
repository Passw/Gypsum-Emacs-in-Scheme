;; (unless defining-library
;;   (include "keymap.sli"))

;; =================================================================================================

(define *unicode-max-code-point* (integer->char #x10FFFF)) ;; TODO: move these to (gypsum lexer)
(define *unicode-min-code-point* #\null)

(define (*->expr thing)
  (cond
   ((char-table-type? thing) (char-table->expr thing))
   ((keymap-layer-type? thing) (keymap-layer->expr thing))
   ((keymap-index-type? thing) (keymap-index->expr thing))
   ((hash-table? thing) (hash-table->expr *->expr thing))
   (else thing)))

(define (hash-table->expr *->expr thing)
  (hash-table-fold
   thing
   (lambda (key val head)
     (cons (cons (*->expr key) (*->expr val)) head))
   '()))

;; -------------------------------------------------------------------------------------------------

(define-record-type <char-table-type>
  ;; A char-table maps characters to values. The keys are split across
  ;; two tables. The first table handles all characters in the range
  ;; of ASCII code in the inclusive range ~#x20~ (~#\space~) to ~#x7F~
  ;; (~#\delete~), what is called the ASCII map. All other characters
  ;; (including ASCII control characters below code point ~#x20~) go
  ;; in the upper table, also called the UTF table. This is done
  ;; because most character keys are expected to be set in the ASCII
  ;; map.
  ;;------------------------------------------------------------------
  (make<char-table-type> ascii-map utf-map)
  char-table-type?
  (ascii-map  char-table-ascii-map  set!char-table-ascii-map)
  (utf-map    char-table-utf-map    set!char-table-utf-map)
  )

(define (empty-char-table)
  ;; Construct a completely key table.
  ;;------------------------------------------------------------------
  (make<char-table-type> #f #f))

(define (char-table-size kt)
  (if (not kt) 0
      (let ((size-of (lambda (bhm) (if bhm (bin-hash-table-size bhm) 0))))
        (+ (size-of (char-table-ascii-map kt))
           (size-of (char-table-utf-map kt))))))

(define char-table-copy
  ;; Deep-copy the given key table, that is, allocate a new key table
  ;; and copy the contents of the given key table `KT` into the new
  ;; key table. If the value of `KT` does not satisfy the
  ;; `CHAR-TABLE-TYPE?` predicate, the value of `KT` is returned as-is.
  ;;------------------------------------------------------------------
  (case-lambda
    ((kt) (char-table-copy kt (lambda (id) id)))
    ((kt copy-leaves)
     (cond
      ((char-table-type? kt)
       (let ((ascii-map (char-table-ascii-map kt))
             (utf-map   (char-table-utf-map kt))
             )
         (make<char-table-type>
          (if ascii-map (bin-hash-table-copy ascii-map copy-leaves) #f)
          (if utf-map (bin-hash-table-copy utf-map copy-leaves) #f)
          )))
      (else kt)))))

(define (char-table-empty? kt)
  (let ((empty? (lambda (bht) (or (not bht) (bin-hash-table-empty? bht)))))
    (or (not kt)
        (and (empty? (char-table-ascii-map kt))
             (empty? (char-table-utf-map kt))))))

(define (char-table-hash key table-size)
  ;; The hash function used to map characters to integers for key table indicies.
  ;;------------------------------------------------------------------
  (modulo
   (cond
    ((string? key) (string-hash key))
    ((char?   key) (char->integer key))
    (else (error "key must be a char or string value" key)))
   table-size))

(define =>ascii-map
  (record-unit-lens
   char-table-ascii-map
   set!char-table-ascii-map
   '=>ascii-map))

(define =>ascii-map?
  (=>canonical =>ascii-map empty-char-table char-table-empty?))

(define =>utf-map
  (record-unit-lens
   char-table-utf-map
   set!char-table-utf-map
   '=>utf-map))

(define =>utf-map?
  (=>canonical =>utf-map empty-char-table char-table-empty?))

(define *char-table-ascii-min-code-point* (char->integer #\space))
(define *char-table-ascii-max-code-point* (char->integer #\delete))

(define *lower-table-max-size*
  ;; It is expected that there will be a lot of character maps loaded
  ;; into memory at any given time, and so care is taken to minimize
  ;; the size of these objects.
  ;;
  ;; When creating a lower character table, initially the size is set
  ;; to 11 (a reasonable prime number, choosen by heuristic). However
  ;; if the weight of the table increases to roughly double this
  ;; size, the table is resized to this maximum size
  ;; *LOWER-TABLE-MAX-SIZE* which guarantees O(1) access to elements.
  ;;
  ;; See the "char-table-rebalance!" function.
  ;;------------------------------------------------------------------
  (+ 1 (- *char-table-ascii-max-code-point* *char-table-ascii-min-code-point*)))

(define (lower-char? char)
  ;; Pass a character, if the character is within the range of
  ;; printable ASCII characters, #t is returned.
  ;;------------------------------------------------------------------
  (<= *char-table-ascii-min-code-point*
      (char->integer char)
      *char-table-ascii-max-code-point*))

(define (char-table-weight kt)
  (+ (hash-table-size (char-table-ascii-map kt))
     (hash-table-size (char-table-utf-map kt))))

(define (char-table-hash->expr head hmap)
  (hash-table-fold
   hmap
   (lambda (key elem head)
     (cons (cons key (*->expr elem)) head))
   head))

(define (char-table->expr kt)
  (list
   'char-table
   (char-table-hash->expr
    (char-table-hash->expr '() (char-table-utf-map kt))
    (char-table-ascii-map kt))))

(define (char-table alist)
  ;; Construct a new key table. Returns a cons with the minimum
  ;; (lowest) and maximum (highest) key found.
  ;;------------------------------------------------------------------
  (let ((kt (empty-char-table)))
    (char-table-rebalance!
     (fold
      (lambda (pair kt)
        (char-table-set! #f (cdr pair) kt (car pair)))
      kt alist))))

(define (char-table-bin-rebalance! bin-hash-table)
  ;; This function rebalances a single hash table. Two values are
  ;; returned: the new BIN-COUNT and the updated hash table. Since
  ;; neither SRFI-69 nor SRFI-125 provide any way to retrieve the bin
  ;; count from the hash-table object, we need to track it ourselves
  ;; in our own data structure <BIN-HASH-TABLE-TYPE>.
  ;;------------------------------------------------------------------
  (if (not bin-hash-table) #f
      (let*((ht (view bin-hash-table =>bin-hash-table-hash*!))
            (bin-count (view bin-hash-table =>bin-hash-table-store-size!))
            (size (hash-table-size ht)))
        (if (<= (/ 1 2) (/ size bin-count) (/ 17 11))
            bin-hash-table
            (make<bin-hash-table>
             size
             (alist->hash-table
              (hash-table->alist ht)
              eqv? char-table-hash #:weak #f size))))))

(define (char-table-rebalance! kt)
  ;; It is expected that there will be a lot of character maps loaded
  ;; into memory at any given time, and so care is taken to minimize
  ;; the size of these objects.
  ;; 
  ;; When creating character tables, initially the size is set to 11
  ;; (a reasonable prime number, choosen by heuristic). However if the
  ;; weight of the table increases to roughly double this size, the
  ;; table is resized to to improve access time to all elements.
  ;; 
  ;; The rebalancing function is fairly simple: the HASH-TABLE-SIZE is
  ;; divided by the BIN-COUNT, if the resulting fraction is less than
  ;; 1/2 or approximately greater than 2, the table is 'unbalanced'
  ;; and so a new hash table is allocated with a bin count of exactly
  ;; the HASH-TABLE-SIZE, and then the elements are transferred to the
  ;; new table. If the table is not unbalanced, the given hash table
  ;; is returned unmodified.
  ;;------------------------------------------------------------------
  (let ((rebalance!
         (lambda (bin-hash-char-table)
           (char-table-bin-rebalance! bin-hash-char-table))))
    (let*((kt (update rebalance! kt =>ascii-map?))
          (kt (update rebalance! kt   =>utf-map?)))
      kt)))

;; (define kt (char-table #f '((#\a . "A") (#\b . "B") (#\c . "C") (#\d . "D"))))

(define *char-table-hash-init-size* (make-parameter 11))

(define (make-char-table-inner-hash-table size)
  ;; Constructs a new, correctly tuned SRFI-69 or SRFI-125 hash-table
  ;; for use within the <BIN-HASH-TABLE-TYPE> for either of the forks
  ;; of a <CHAR-TABLE-TYPE>, with the given number of bins.
  ;;------------------------------------------------------------------
  (make-hash-table eqv? char-table-hash #:weak #f size))

(define (=>char-table-char do-rebalance char)
  ;; Define a lens that selects an entry by the key CHAR from within a
  ;; <CHAR-TABLE-TYPE>. The \"CHAR\" argument may also be a string
  ;; representing a keyboard key such as an arrow key or function key.
  ;;------------------------------------------------------------------
  (let*((=>fork
         (cond
          ((string? char) =>utf-map?)
          ((char?   char)
           (cond
            ((char<=? char #\delete) =>ascii-map?)
            (else =>utf-map?)))
          (else (error "char argument is neither a character nor a string" char))))
        (=>key (=>bin-hash-key! char (*char-table-hash-init-size*) make-char-table-inner-hash-table)))
    (=>encapsulate
     (lens
      =>fork
      (if do-rebalance
          (=>on-update =>key char-table-bin-rebalance!)
          =>key))
     (list '=>char-table-char do-rebalance char))))

(define (=>char-table-char! do-rebalance char)
  (=>canonical (=>char-table-char do-rebalance char) empty-char-table char-table-empty?))

(define (char-table-set! do-rebalance elem kt char)
  (lens-set elem kt (=>char-table-char! do-rebalance char)))

(define (char-table-view kt char)
  ;; Lookup an element associated with a character from within a <char-table>.
  (view kt (=>char-table-char! #f char)))

(define (char-table-update! up kt char)
  (update up kt (=>char-table-char! #f char)))

(define (char-table->alist kt)
  (let ((ascii (char-table-ascii-map kt))
        (utf   (char-table-utf-map kt))
        )
    (apply append
     (map bin-hash-table->alist
          (cond
           ((and (not ascii) (not utf)) '())
           ((not utf) (cons ascii '()))
           ((not ascii) (cons utf '()))
           (else (list ascii utf))
           )))))

(define (char-table-print kt)
  (bracketed
   2 #\( #\) "char-table"
   (if (char-table-empty? kt) " '()"
       (apply print (newline-indent)
        (map
         (lambda (pair)
           (bracketed 1 #\( #\) (qstr (car pair)) " . "
            (let ((val (cdr pair)))
              (cond
               ((char-table-type? val) (char-table-print val))
               ((keymap-layer-type? val) (keymap-layer-print val))
               (else (print val)))
              )))
         (char-table->alist kt))))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <keymap-index-type>
  ;; This defines a data structure fore defining keymap indicies, that
  ;; is, the sequence of printable characters (keys on a keyboard,
  ;; with or without modifiers) that lead to an action. You must
  ;; construct a ~<KEYMAP-INDEX>~ in order to lookup or update a
  ;; ~<KEYMAP-LAYER>~.
  (make<keymap-index> modix charix nextix)
  keymap-index-type?
  (modix  mod-index) ; the modifiers (if any) key pressed while the printable character was pressed
  (charix char-index) ; the printable character
  (nextix next-index) ; the next item in the sequence
  )

(define ctrl-bit  #x01)
(define meta-bit  #x02)
(define super-bit #x04)
(define hyper-bit #x08)
(define alt-bit   #x10)

(define (ascii-modifier? mod)
  ;; #t if none of the super, hyper or alt-sc bits are present. This
  ;; is useful for deciding if the keymap index value maps to an ASCII
  ;; string using ordinary arithmetic. There are no mappings from
  ;; super, hyper, or alt-sc bits to ASCII characters.
  (= 0 (bitwise-and mod (bitwise-ior super-bit hyper-bit alt-bit))))

(define (keymap-index->ascii ix)
  ;; This function takes a  <keymap-index-type> and tries to construct
  ;; an ASCII character string from it, but may return #f. Control and
  ;; meta-modified characters can produce ASCII characters, but super,
  ;; hyper or Alt-SC modifiers cannot.
  (let*((ok #t)
        (fail (lambda () (set! ok #f) #f))
        (char-list
         (let loop ((ix ix))
           (cond
            ((keymap-index-type? ix)
             (let ((c (char-index ix)))
               (cond
                ((string? c)
                 (cons #\null (loop (next-index ix))))
                ((char? c)
                 (let*((i (char->integer c))
                       (mod (mod-index ix))
                       (has-ctrl-bit (= ctrl-bit (bitwise-and mod ctrl-bit)))
                       (prefix-mod ; if meta modifier, prefix an #\escape char
                        (lambda (next)
                          (cond
                           ((= meta-bit (bitwise-and mod meta-bit))
                            (cons #\escape next))
                           (else next))))
                       (ctrl-offset ; if ctrl modifier, subtract #x40 from the char index
                        (cond
                         (has-ctrl-bit (integer->char (- i #x40)))
                         (else c))))
                   (cond
                    ((and (ascii-modifier? mod) ; modifiers are any of meta, ctrl, or nothing
                          (or (not has-ctrl-bit) ; control bit is not set, or else
                              (<= #x40 i #x7F))) ; control bit is set and i is in range
                     (prefix-mod (cons ctrl-offset (loop (next-index ix)))))
                    ((not ix) '())
                    (else (fail)))))
                (else
                 (error "char-index field is not a char or string" c ix)))))
            ((not ix) '())
            (else (fail))))))
    (if ok (list->string char-list) #f)))

(define (keymap-index-head kmix)
  ;; Return just the head of a keymap index
  (make<keymap-index> (mod-index kmix) (char-index kmix) #f))

(define sym-lookup-table
  ;; The values of this table never change, so I hand-crafted a
  ;; perfect hash function for a table of 14 bins.
  (alist->hash-table
   `((C       . ,ctrl-bit)
     (ctrl    . ,ctrl-bit)
     (control . ,ctrl-bit)
     (M       . ,meta-bit)
     (meta    . ,meta-bit)
     (S       . ,super-bit)
     (super   . ,super-bit)
     (H       . ,hyper-bit)
     (hyper   . ,hyper-bit)
     (A       . ,alt-bit)
     (alt     . ,alt-bit))
   eqv?
   (lambda (sym len)
     (modulo
      (string-fold ; the hash performs a sum over the characters
       (lambda (c sum)
         (+ (* 6 sum) ; the 6 was the optimal value for this table
            (- (char->integer c) ; offset character to #\A before summing
               (char->integer #\A))))
       0
       (let ((key (symbol->string sym)))
         ;; Cut the key to 6 characters -- the smallest number that
         ;; cannot possibly match any strings in the table -- we want
         ;; keys that are too long to always be rejected.
         (substring key 0 (min 6 (string-length key)))))
      len))
   #:weak #f
   14))

(define (modifier->integer sym)
  ;; Lookup an integer value for a modifier symbol, return #f if the
  ;; symbol is not valid. Valid symbols are 'C, 'ctrl, 'M, 'meta,
  ;; 'alt, 'S, 'super."
  ;;------------------------------------------------------------------
  (hash-table-ref/default sym-lookup-table sym #f))

(define (keymap-index syms)
  ;; Construct a ~<KEYMAP-INDEX>~ from a symbolic representation. An
  ;; index is a sequence of keyboard key elements. Each element must
  ;; be symbolized by a typeable and printable UTF character (such as
  ;; a letter or number) or a string representing a keyboard key (such
  ;; as \"LEFT\" or \"RIGHT\" arrows), and may be preceded by zero or
  ;; more modifier symbols such as ~C~ or ~ctrl~, ~M~ or ~meta~, and
  ;; ~S~ or ~super~. The character or string terminates a key chord
  ;; with zero or more modifiers.  The keymap index may contain zero
  ;; or more keyboard key elements.  If there are zero elements ~#f~
  ;; is returned, otherwise a ~<KEYMAP-INDEX>~ is returned.
  ;; 
  ;; Use the ~MAP-KEY~ function to construct a list of associations
  ;; between a ~<KEYMAP-INDEX>~ structure and a procedure.
  ;;------------------------------------------------------------------
  (cond
   ((string? syms)
    (keymap-index (string->keymap-index syms)))
   ((pair? syms)
    (let loop
        ((mod-index 0)
         (syms syms))
      (cond
       ((null? syms) #f)
       (else
        (let ((sym (car syms))
              (next (cdr syms)))
          (cond
           ((or (char? sym) (string? sym))
            (make<keymap-index> mod-index sym (loop 0 next)))
           ((symbol? sym)
            (let ((mod (modifier->integer sym)))
              (cond
               (mod (loop (bitwise-ior mod mod-index) next))
               (else
                (error "unknown keymap-index modifier symbol" sym)))))
           (else
            (error "keymap index must be composed of symbols or characters" sym))))))))))


(define (string->keymap-index str)
  ;; Emacs uses the ASCII protocol to represent control characters and
  ;; non-control characters. It negates characters modulo 2^27 and
  ;; sets the upper bits on these integers to represent Meta, Super,
  ;; Hyper, and Alt modifiers.
  ;;------------------------------------------------------------------
  (let ((len (string-length str)))
    (let loop ((i 0) (stack '()))
      (cond
       ((>= i len) (reverse stack))
       (else
        (let*((ch (string-ref str i))
              (ci (char->integer ch))
              )
          (cond
           ((= ci 0)
            (loop (+ 1 i) (cons #\@ (cons 'ctrl stack))))
           ((< ci #x20)
            (loop (+ 1 i) (cons (integer->char (+ 96 ci)) (cons 'ctrl stack))))
           ;;TODO: there are a lot more things here that could be
           ;; decoded. For starters, the `STR` argument could be an
           ;; unboxed integer vector with the Meta, Super, Hyper, or Alt
           ;; bits set, none of that has been encoded here yet.
           (else
            (loop (+ 1 i) (cons ch stack))
            ))))))))


(define *mod-bit-alist*
  `((ctrl    . ,ctrl-bit)
    (meta    . ,meta-bit)
    (super   . ,super-bit)
    (hyper   . ,hyper-bit)
    (alt     . ,alt-bit)))

(define (keymap-index-append . items)
  (cond
   ((null? items) #f)
   (else
    (let ((head (car items))
          (tail (cdr items)))
      (cond
       ((not head)
        (apply keymap-index-append tail))
       ((keymap-index-type? head)
        (make<keymap-index>
         (mod-index head)
         (char-index head)
         (apply keymap-index-append (next-index head) tail)))
       (else
        (error "all arguments must be of <keymap-index-type>" head)))))))

(define (keymap-index->list km)
  ;; Convert a ~<KEYMAP-INDEX>~ to a list of sequences. This is the best
  ;; way to see a human-readable representation of a ~<KEYMAP-INDEX>~ value.
  (if (not km) '()
      (let ((modix (mod-index km)))
        (let loop ((mod-bit-alist *mod-bit-alist*))
          (cond
           ((null? mod-bit-alist)
            (cons (char-index km) (keymap-index->list (next-index km))))
           (else
            (let ((assoc (car mod-bit-alist)))
              (cond
               ((eqv? 0 (bitwise-and modix (cdr assoc)))
                (loop (cdr mod-bit-alist)))
               (else
                (cons (car assoc) (loop (cdr mod-bit-alist))))))))))))

(define (reverse-list->keymap-index nodes)
  (let loop
      ((nodes nodes)
       (key-path #f))
    (cond
     ((null? nodes) key-path)
     ((pair? nodes)
      (let ((head (car nodes))
            (tail (cdr nodes)))
        (cond
         ((not head)
          (loop tail key-path))
         ((keymap-index-type? head)
          (loop
           tail
           (make<keymap-index>
            (mod-index head)
            (char-index head)
            (keymap-index-append (next-index head) key-path))))
         (else
          (error "all list items must be of type <keymap-index-type>" head)))))
     (else
      (error "expecting list of <keymap-index-type> elements" nodes)))))


(define at-char (integer->char 64))
(define underscore (integer->char 95))

(define (keymap-index-to-char keyix allow-ctrl on-success on-fail)
  ;; Convert a keymap-index to a character, evaluate `ON-SUCCESS` with
  ;; the character if conversion succeeded, evaluate `ON-FAIL` if
  ;; conversion failed.
  ;;------------------------------------------------------------------
  (cond
   ((next-index keyix) (on-fail)) ;; if there is a next-index, return #f
   ((and allow-ctrl (= ctrl-bit (mod-index keyix)))
    (let ((char (char-upcase (char-index keyix))))
      (cond
       ((and (char>=? char at-char) (char<=? char underscore))
        ;; this equation is defined by ASCII standard for how to
        ;; convert letters to control characters.
        (on-success (integer->char (bitwise-and #x1F (char->integer char)))))
       (else (on-fail)))))
   ((not (= 0 (mod-index keyix)))
    (on-fail)) ;; ctrl chars not allows and mod-index is not zero
   (else (on-success (char-index keyix)))
   ))

(define keymap-index->expr keymap-index->list)

(define keymap-index-print
  (case-lambda
    ((kmix) (keymap-index-print #f kmix))
    ((label kmix)
     (cond
      ((keymap-index-type? kmix)
       (bracketed 1 #\( #\)
         (let ((content (apply join-by #\space (map qstr (keymap-index->list kmix)))))
           (if label (print label (bracketed 1 " '(" ")" content)) content))
         ))
      ((not kmix) (print "#f"))
      (else (error "not a <keymap-index-type>" kmix))
      ))))

;;(cond-expand
;;  (guile-3
;;   (set-record-type-printer!
;;    <keymap-index-type>
;;    (lambda (km port) (pretty port (keymap-index-print "keymap-index" km)))))
;;  (else))

;; -------------------------------------------------------------------------------------------------

(define-record-type <keymap-layer-type>
  (%make<keymap-layer> mod-table alt-action)
  keymap-layer-type?
  (mod-table keymap-layer-mod-table %set!keymap-layer-mod-table)
    ;; ^ an ordinary hash table (not a <bin-hash-table-type>)
    ;; containing keymaps selected by modifier key
  (alt-action keymap-layer-alt-action set!keymap-layer-alt-action)
    ;; ^ it is possible to merge keymap-layer tables such that mappings to
    ;; that terminate at the end of one key string (such as "C-c C-c")
    ;; is not a terminal string in the merged mapping (such as when
    ;; merging with "C-c C-c x"). This field keeps the action mapped
    ;; to the shorter string (e.g. "C-c C-c") available should the
    ;; longer string (e.g. "C-c C-c x") be deleted from the table.
  )

(define (make<keymap-layer> mod-table alt-action)
  (%make<keymap-layer>
   (cond
    ((not mod-table) #f)
    ((hash-table? mod-table) mod-table)
    (else (error "not a <hash-table-type>" mod-table)))
   alt-action))

(define (set!keymap-layer-mod-table kml mod-table)
  (%set!keymap-layer-mod-table kml
   (cond
    ((not mod-table) #f)
    ((hash-table? mod-table) mod-table)
    (else (error "not a <hash-table-type>" mod-table))
    )))

(define get-keymap-layer-mod-table keymap-layer-mod-table)

(define (keymap-layer-empty? km)
  (or (not km)
      (let ((ht (keymap-layer-mod-table km)))
        (and
         (not (keymap-layer-alt-action km))
         (or (not ht) (hash-table-empty? ht))))))

(define (keymap-layer . assocs) (alist->keymap-layer assocs))

(define (keymap-layer-action node)
  ;; This procedure checks if the given `LAYER` contains a value in
  ;; `KEYMAP-LAYER-ALT-ACTION` while also checking to make sure that
  ;; the `KEYMAP-LAYER-MOD-TABLE` is empty. If the action is defined
  ;; and the table is empty, the action is returned. If the table is
  ;; not empty, the table is always returned regardless of whether the
  ;; action is defined. If the table is empty and there is no action,
  ;; #f is returned.
  (let*((alt (keymap-layer-alt-action node))
        (mods (keymap-layer-mod-table node))
        (empty (or (not mods) (hash-table-empty? mods)))
        )
    (cond
     ((and empty (not alt)) #f)
     ((and empty alt) alt)
     (else node)
     )))

(define (alist->keymap-layer assocs)
  ;; Construct a new empty keymap-layer from an association list of
  ;; <KEYMAP-INDEX-TYPE> indicies paired with procedures to be
  ;; executed when the keymap index lookup occurs. The ALT-ACTION,
  ;; which is an action that may (or may not) be used if a lookup
  ;; fails, should be assigned to the null key, so the association
  ;; pair `('() . some-procedure)` will assign "some-procedure" to the
  ;; `KEYMAP-LAYER-ALT-ACTION` field of the constructed `<KEYMAP-LAYER-TYPE>`
  ;; value.
  ;; ------------------------------------------------------------------
  (keymap-layer-update! (lambda (_old new) new) (make<keymap-layer> #f #f) assocs))

(define keymap-layer-copy
  ;; Deep-copy the given key map object `KM`. If the object `KM` does
  ;; not satisfy the `KEYMAP-LAYER-TYPE?` predicate it is returned
  ;; as-is.  If two arguments are applied to this procedure, the
  ;; second must be a procedure that deep-copies the
  ;; `KEYMAP-LAYER-ALT-ACTION` fields as well.
  ;; ------------------------------------------------------------------
  (case-lambda
    ((km) (keymap-layer-copy km (lambda (id) id)))
    ((km alt-copy)
     (cond
      ((keymap-layer-type? km)
       (make<keymap-layer>
        (hash-table-copy-with
         (keymap-layer-mod-table km)
         (lambda (ct) (char-table-copy ct keymap-layer-copy)))
        (alt-copy (keymap-layer-alt-action km))
        ))
      (else km)))))

(define =>keymap-layer-mod-table
  (record-unit-lens keymap-layer-mod-table set!keymap-layer-mod-table '=>keymap-layer-mod-table))

(define =>keymap-layer-mod-table?
  (=>canonical =>keymap-layer-mod-table keymap-layer keymap-layer-empty?))

(define =>keymap-layer-alt-action
  (record-unit-lens keymap-layer-alt-action set!keymap-layer-alt-action '=>keymap-layer-alt-action))

(define =>keymap-layer-alt-action?
  (=>canonical =>keymap-layer-alt-action keymap-layer keymap-layer-empty?))

(define *keymap-layer-num-char-tables* 32
  ;; The number of key tables in a keymap-layer is defined by the number of
  ;; possible modifiers combinations that can be applied. There are
  ;; five modifiers: Control, Meta/Alt, Super, Hyper, and SC-Alt
  ;; ("Space-cadet Alt", different from IBM-PC "Alt"). Since these any
  ;; one of modifiers can be either on or off, we have 5 bits or 32
  ;; possible modifier values, each needing its own lookup table.
  )


(define (keymap-layer->expr km)
  (list
   'keymap-layer
   (*->expr (keymap-layer-alt-action km))
   (hash-table-fold
    (keymap-layer-mod-table km)
    (lambda (kmix kt head)
      (cons
       (keymap-index->expr (keymap-index-head kmix))
       (char-table->expr kt)))
    '())))


(define (map-key syms proc)
  "Construct an association between a list of symbols passed to
~KEYMAP-INDEX~ to construct a keymap index, and a procedure. A list of
these values is used to initialize or update a ~<KEYMAP-LAYER>~.

The ~PROC~ argument may be a procedure, or it may be another
~<KEYMAP-LAYER>~."
  (cons (keymap-index syms) proc))


(define (default-make-keymap-layer-mod-table)
  ;; This hash table has a size of 7. Any combination of the control
  ;; and meta bits (or none) exist tables in lower 4 bins (0, 1, 2, or
  ;; 3). All other modifiers are shoved up into upper 3 bins. I do
  ;; this because the use of super, hyper and alt modifiers are so
  ;; unusual in normal Emacs usage that I expect these upper 3 bins
  ;; will almost never be used.
  (make-hash-table
   eqv?
   (lambda (x size) (if (> x 3) (+ 4 (modulo x (- size 4))) x))
   #:weak #f 7
   ))


(define (=>keymap-layer-mod-table-key? key)
  ;; Define a lens accessing a key in the hash table of a
  ;; KEYMAP-LAYER-MOD-TABLE.
  (lens
   =>keymap-layer-mod-table?
   (=>canonical (=>hash-key! key) default-make-keymap-layer-mod-table hash-table-empty?)))


(define (=>keymap-layer-index! key-path)
  ;; Construct a lens to access a KEYMAP-LAYER-TYPE by the given
  ;; INDEX. The INDEX is of type <KEYMAP-INDEX-TYPE> or a list of
  ;; symbols and characters that can be passed to the KEYMAP-INDEX
  ;; function.
  ;;------------------------------------------------------------------
  (cond
   ((string? key-path)
    (=>keymap-layer-index! (keymap-index key-path)))
   ((keymap-index-type? key-path)
    (=>canonical
     (apply lens
      (let loop ((key-path key-path))
        (if (not key-path) '()
            (cons
             (=>keymap-layer-mod-table-key? (mod-index key-path))
             (cons
              (=>char-table-char! #t (char-index key-path))
              (loop (next-index key-path)))))))
     keymap-layer
     keymap-layer-empty?))
   ((pair? key-path) (=>keymap-layer-index! (keymap-index key-path)))
   ((null? key-path) =>self)
   (else
    (error "=>keymap-layer-index! lens, key-path not a list or a <KEYMAP-INDEX-TYPE>."
           key-path))
   ))


(define (=>kbd! . syms)
  ;; This function calls `=>KEYMAP-INDEX!` lens with an arbitrary
  ;; number of keyboard modifier and character symbols arguments, and
  ;; encapsulates the resulting `<COMPOUND-LENS-TYPE>` into a new
  ;; `<UNIT-LENS-TYPE>.` The `FINAL` argument to `=>KEYMAP-INDEX` is
  ;; `=>KEYMAP-LAYER-ALT-ACTION`, so the lens constructed by this
  ;; function will always operates on the leaf of the keymap-layer
  ;; node. The lens constructed in `=>CANONICAL`, so if the
  ;; `<KEYMAP-LAYER-TYPE>` is updated and becomes empty, the `UPDATE`
  ;; or `LENS-SET` function returns `#F` instead of an empty
  ;; `<KEYMAP-LAYER-TYPE>.`
  ;;------------------------------------------------------------------
  (let ((label (cons '=>kbd syms))
        (=>lens (lens (=>keymap-layer-index! syms)
                      =>keymap-layer-alt-action))
        )
    (unit-lens
     (lambda (km) (view km =>lens))
     (lambda (km val) (lens-set val km =>lens))
     (lambda (up km) (update&view up km =>lens))
     label
     )))

(define (prefer-new-bindings old new)
  ;; Use this as the first argument to the KEYMAP-LAYER-MERGE-ACTIONS
  ;; function, that is if you would like key new bindings that
  ;; conflict with old bindings to simply overwrite the old bindings
  ;; when two keymap-layers are merged
  ;;------------------------------------------------------------------
  new)

(define (prefer-old-bindings old new)
  ;; Use this as the first argument to the KEYMAP-LAYER-MERGE-ACTIONS
  ;; function, that is if you would like key old bindings that
  ;; conflict with new bindings to simply overwrite the old bindings
  ;; when two keymap-layers are merged.
  ;;------------------------------------------------------------------
  old)

(define (keymap-layer-merge-actions merge-actions km new-action)
  ;; This function evaluates a function MERGE-ACTIONS with two
  ;; arguments: the old value in the KEYMAP-LAYER-ALT-ACTION field of
  ;; the KM argument, and the NEW-ACTION argument given to this
  ;; functino. If the KM argument is not a <KEYMAP-LAYER-TYPE>, a new
  ;; keymap-layer is constructed and the KEYMAP-LAYER-ALT-ACTION of
  ;; this new structure is set to the result of calling MERGE-ACTIONS
  ;; with #f and the NEW-ACTION argument passed to this function.
  ;;------------------------------------------------------------------
  (cond
   ((keymap-layer-type? km)
    (let ((km (update
               (lambda (old-action)
                 (values (merge-actions old-action new-action) #f))
               km =>keymap-layer-alt-action?)))
      km))
   (else
    (keymap-layer (cons '() (merge-actions #f new-action))))))

(define (keymap-layer-update! merge-actions km alist)
  ;; Updates an existing <KEYMAP-LAYER> with an association
  ;; list. Provide a procedure MERGE-ACTIONS to combine an existing
  ;; action with a new action in the case the same key is set twice.
  ;; 
  ;; The MERGE-ACTIONS function must take 2 arguments, an OLD-ACTION
  ;; and a NEW-ACTION. The OLD-ACTION may satisfy either the
  ;; PROCEDURE? predicate or the `keymap-layer-type?` predicate. The
  ;; NEW-ACTION will only ever satisfy the PROCEDURE? predicate due to
  ;; the requirements on how ASSOCS are defined.
  ;; 
  ;; The MAP-KEY-ALIST argument should be an association list of cons
  ;; cells constructed by the MAP-KEY function.
  ;;------------------------------------------------------------------
  (let loop ((alist alist) (km km))
    (if (null? alist) km
        (let*((assoc  (car alist))
              (key    (car assoc))
              (=>key? (=>keymap-layer-index! key)))
          (loop
           (cdr alist)
           (let ((km
                  (update
                   (lambda (km)
                     (values
                      (keymap-layer-merge-actions merge-actions km (cdr assoc))
                      #f))
                   km =>key?)))
             km))))))

(define (keymap-layer->alist km)
  (let*((mods (keymap-layer-mod-table km))
        (alt  (keymap-layer-alt-action km))
        (ht
         (concatenate
          (map
           (lambda (pair)
             (let ((mod (car pair)) (ct (cdr pair)))
               (concatenate
                (map
                 (lambda (pair)
                   (let ((c (car pair)) (km (cdr pair)))
                     (cond
                      ((keymap-layer-type? km)
                       (map
                        (lambda (pair)
                          (let ((key (car pair)) (alt (cdr pair)))
                            (cons (make<keymap-index> mod c key) alt)))
                        (keymap-layer->alist km)))
                      (else (cons (make<keymap-index> mod c #f) km)))))
                 (char-table->alist ct)))))
           (if mods (hash-table->alist mods) '()))))
        )
    (if alt (cons (cons #f alt) ht) ht)))


(define (keymap-layer-print km)
  (cond
   ((keymap-layer-type? km)
    (let ((alt (keymap-layer-alt-action km))
          (mod (keymap-layer-mod-table km))
          )
      (bracketed 2 #\( #\)
       "alist->keymap-layer"
       (if (or (not mod) (hash-table-empty? mod)) '(" '()")
           (apply print
            (map
             (lambda (pair)
               (print (line-break)
                (bracketed 2 "'(" ")"
                 (keymap-index-print #f (car pair))
                 #\space
                 (print (cdr pair)))))
             (keymap-layer->alist km)))))
      ))
   ((keymap-index-predicate-type? km)
    (print (qstr km)))
   (else
    (error "not a <keymap-layer-type> or <keymap-index-predicate-type>" km)
    )))

;;(cond-expand
;;  (guile-3
;;   (set-record-type-printer!
;;    <keymap-layer-type>
;;    (lambda (km port) (pretty port (keymap-layer-print km)))))
;;  (else))

;; -------------------------------------------------------------------------------------------------

(define-record-type <keymap-index-predicate-type>
  ;; This is a procedure that can return a value for any
  ;; `<keymap-index-type>`. It is used to implement keymaps where the
  ;; majority of indicies all return the same value, such as the
  ;; self-insert-map, among others.
  (make<keymap-index-predicate> proc)
  keymap-index-predicate-type?
  (proc  keymap-index-predicate))

;;(cond-expand
;;  (guile-3
;;   (set-record-type-printer!
;;    <keymap-index-predicate-type>
;;    (lambda (km port)
;;      (pretty port
;;       "(make<keymap-index-predicate> "
;;       (keymap-index-predicate km) ")"))))
;;  (else))

(define (apply-keymap-index-predicate pred keyix)
  (cond
   ((not (keymap-index-type? keyix))
    (error "not a <keymap-index-type>" keyix))
   ((not (keymap-index-predicate-type? pred))
    (error "not a <keymap-index-predicate-type>" pred))
   (else
    ((keymap-index-predicate pred) keyix))
   ))


(define (new-self-insert-keymap-layer allow-ctrl on-success on-fail)
  ;; Construct a `<KEYMAP-INDEX-PREDICATE-TYPE>` that checks a given
  ;; `<KEYMAP-INDEX-TYPE>` value if it is any unmodified key (or, only
  ;; modified by a CTRL modifier if `ALLOW-CTRL` is #t), and also not
  ;; followed by any other key (`NEXT-INDEX` is #f), then the
  ;; `ON-SUCCESS` procedure is applied to the `CHAR-INDEX` of the
  ;; `<KEYMAP-INDEX-TYPE>` value. If lookup fails, the `ON-FAIL`
  ;; procedure is appled no arguments.
  (make<keymap-index-predicate>
   (lambda (keyix) (keymap-index-to-char keyix allow-ctrl on-success on-fail))))

;; -------------------------------------------------------------------------------------------------

(define-record-type <keymap-type>
  ;; A list of keymap layers. When looking-up an element with a
  ;; `<keymap-index-type>`, all layers are checked, the element
  ;; nearest the bottom of the list (nearer to `car` than to `cdr`) is
  ;; returned. Keymap operations like `KEYMAP-PUSH` treat
  ;; `<KEYMAP-TYPE>`s as immutable and always return newly constructed
  ;; `<KEYMAP-TYPE>` values.
  ;;------------------------------------------------------------------
  (make<keymap> layers label)
  keymap-type?
  (layers  keymap->layers-list  set!keymap-layers)
  (label   keymap-label         set!keymap-label)
  )


(define =>keymap-layers*!
  (record-unit-lens
   keymap->layers-list
   set!keymap-layers
   '=>keymap-layers*!))


(define (keymap . layers)
  ;; Construct a `<KEYMAP-TYPE>`. If the first argument is a string or
  ;; symbol, it is used as the label for this keymap.
  ;;------------------------------------------------------------------
  (define (to-list layer)
    (cond
     ((not layer) '())
     ((keymap-layer-type? layer) (list layer))
     ((keymap-type? layer) (keymap->layers-list layer))
     ((keymap-index-predicate-type? layer) (list layer))
     (else (error "not a <keymap-type> or <keymap-layer-type>" layer))
     ))
  (cond
   ((null? layers) (make<keymap> '() #f))
   ((or (string? (car layers)) (symbol? (car layers)))
    (make<keymap> (apply append (map to-list (cdr layers))) (car layers)))
   (else
    (make<keymap> (apply append (map to-list layers)) #f))))


(define =>keymap-label!
  (record-unit-lens keymap-label set!keymap-label '=>keymap-label!))


(define (keymap-layer-ref km key-path)
  ;; This is similar to `KEYMAP-LAYER-LOOKUP` except it always returns
  ;; a `KEYMAP-LAYER` node (or #f), rather than returning the
  ;; `KEYMAP-LAYER-ALT-ACTION` (if any).
  (cond
   ((keymap-layer-type? km)
    (view km (=>keymap-layer-index! key-path)))
   ((keymap-index-predicate-type? km)
    (apply-keymap-index-predicate km key-path))
   (else (error "not a <keymap-layer-type> or <keymap-index-predicate-type>" km))
   ))


(define =>keymap-top-layer!
  ;; A lens that focuses on the top-most layer of a
  ;; `<KEYMAP-TYPE>`. This lens is canonical, so if the keymap has no
  ;; layers, a new layer is created. If the top layer is empty, it is
  ;; removed.
  (=>encapsulate
   (lens
    =>keymap-layers*! =>head
    (=>canonical =>self keymap-layer keymap-layer-empty?)
    )
   '=>keymap-top-layer!
   ))


(define (keymap-layer-lookup km key-path)
  ;; Take a KEY-PATH that has been constructed by the KEYMAP-INDEX
  ;; procedure from a sequence of keyboard characters and keyboard
  ;; modifier symbols such as 'Control or 'Alt (see
  ;; MODIFIER->INTEGER), and determine if a procedure has been mapped
  ;; to `KM`, which may be a `<KEYMAP-LAYER-TYPE>` in which the
  ;; `KEY-PATH` is indexed, or a `<KEYMAP-INDEX-PREDICATE-TYPE>` to
  ;; which the `KEY-PATH` is applied. If the `KEY-PATH` leads to
  ;; another non-empty layer or predicate value, that value is
  ;; returned. But if the layer found contains a
  ;; `KEYMAP-LAYER-ALT-ACTION` and nothing else (the
  ;; `KEYMAP-LAYER-MOD-TABLE` is empty) the alt-action is returned.
  (cond
   ((keymap-layer-type? km)
    (view km (=>keymap-layer-index! key-path) =>keymap-layer-alt-action?))
   ((keymap-index-predicate-type? km)
    (apply-keymap-index-predicate km key-path))
   (else (error "not a <keymap-layer-type> or <keymap-index-predicate-type>" km))
   ))


(define (keymap-lookup km kmix)
  ;; This procedure performs a lookup in a `<KEYMAP-TYPE>` argument
  ;; `KM` with a `<KEYMAP-INDEX-TYPE>` argument `KMIX`. All layers in
  ;; the keymap are searched in order. The first time a lookup in a
  ;; `<KEYMAP-LAYER-TYPE>` or `<KEYMAP-INDEX-PREDICATE-TYPE>` results
  ;; in a value that is not another layer or predicate will cause this
  ;; procedure to immediately return that value. Otherwise, a new
  ;; <KEYMAP-TYPE> is returned containing only layers and predicates
  ;; that could be resolved by the key index. If all results are "#f",
  ;; then "#f" is returned.
  (cond
   ((not km) #f)
   ((not kmix) km)
   (else
    (letrec*
        ((found
          (call/cc
           (lambda (halt)
             (let loop ((layers (keymap->layers-list km)))
               (cond
                ((null? layers) '())
                (else
                 (let*((head (car layers))
                       (tail (cdr layers))
                       (layer (keymap-layer-ref head kmix))
                       )
                   (cond
                    ((not layer) (loop tail))
                    ((keymap-layer-type? layer)
                     (let ((action (keymap-layer-action layer)))
                       (cond
                        ((keymap-layer-type? action) (cons action (loop tail)))
                        ((keymap-type? action)
                         (error "keymap layer contains keymap" layer action))
                        ((not action) (loop tail))
                        (else (halt action))
                        )))
                    ((keymap-index-predicate-type? layer)
                     (error "keymap-layer-ref returned a <KEYMAP-INDEX-PREDICATE-TYPE>" layer))
                    (else (halt layer))
                    )))
                )))
           )))
      (cond
       ((not found) #f)
       ((null? found) #f)
       ((pair? found) (make<keymap> found #f))
       (else found)
       )))))

(define (keymap-print km)
  (cond
   ((not km) (print "#f"))
   (else
    (let*((separator (make-string 20 #\-))
           (items (keymap->layers-list km))
           (head (if (null? items) #f (car items))))
      (bracketed 2 "(keymap" ")"
        (apply join-by (line-break)
          (cond
           ((or (string? head) (symbol? head))
            (print #\space (qstr (if (symbol? head) (symbol->string head) head))))
           (else #f))
          (let loop ((count 0) (items items))
            (cond
             ((null? items) '())
             (else
              (cons
                (print
                  #\; separator " layer " count #\space separator
                  (line-break) (keymap-layer-print (car items))
                  )
                (loop (+ 1 count) (cdr items))
                )))
            )))))))

;;(cond-expand
;;  (guile-3
;;   (set-record-type-printer!
;;    <keymap-type>
;;    (lambda (km port) (pretty port (keymap-print km)))))
;;  (else))

;; -------------------------------------------------------------------------------------------------

(define-record-type <modal-lookup-state-type>
  ;; This is the state object updated by MODAL-STATE-LOOKUP-STEP!
  ;; function which implements the modal state key lookup
  ;; mechanism. This record type contains 2 fields:
  ;;
  ;;  1. KEYMAP is the current keymap in which the key-path will
  ;;     lookup the next action or keymap
  ;;
  ;;  2. STACK is a reversed list of key-path values that have been
  ;;     looked-up so far
  ;;------------------------------------------------------------------
  (make<modal-lookup-state-type> keymap stack)
  modal-lookup-state-type?
  (keymap  modal-lookup-state-keymap       set!modal-lookup-state-keymap)
  (stack   modal-lookup-state-index-stack  set!modal-lookup-state-index-stack))

(define (modal-lookup-state-print km)
  (bracketed 2 #\( #\)
    "make<modal-lookup-state-type>" (line-break)
    (join-by (line-break)
      ";modal-lookup-state-keymap"
      (keymap-print (modal-lookup-state-keymap km))
      ";modal-lookup-state-index-stack"
      (bracketed 2 #\( #\)
        "map keymap-index" (line-break)
        (bracketed 2 "'(" ")"
          (line-break)
          (apply join-by #\space
            (map keymap-index-print
                 (modal-lookup-state-index-stack km)))
          )))))

;;(cond-expand
;;  (guile-3
;;   (set-record-type-printer!
;;    <modal-lookup-state-type>
;;    (lambda (km port) (pretty port (modal-lookup-state-print km)))))
;;  (else))

(define (new-modal-lookup-state km)
  ;; Construct a new <modal-lookup-state-type> with either a
  ;; <keymap-type> argument or a list of <keymap-type> arguments.
  (cond
   ((keymap-type? km)
    (make<modal-lookup-state-type> km '()))
   ((or (keymap-layer-type? km)
        (keymap-index-predicate-type? km))
    (make<modal-lookup-state-type> (keymap km) '()))
   (else
    (error "argument must be a <keymap-type>" km))))

(define (modal-lookup-state-key-index state)
  (reverse-list->keymap-index (modal-lookup-state-index-stack state)))

(define (modal-lookup-state-lookup state key-path)
  ;; Looks-up a key-path in <modal-lookup-state-type> object, which
  ;; might contain a single keymap or a list of keymaps.
  (keymap-lookup (modal-lookup-state-keymap state) key-path))

(define (modal-lookup-state-step! state key-path do-action do-wait-next do-fail-lookup)
  ;; In Emacs, key lookup is actually a modal operation. Each key
  ;; chord is an index that looks up a keymap node. If the index
  ;; lookup returns another keymap node that is empty but has an
  ;; action, the action is executed. If the keymap node has an action
  ;; but the map is not empty, it displays the key chords that have
  ;; been pressed so far in the echo area and then waits for another
  ;; key chord to be pressed. This function takes the following
  ;; arguments:
  ;;
  ;;  1. an object of record type <MODAL-LOOKUP-STATE-TYPE>
  ;;
  ;;  2. a key-path from a keyboard event that recently occurred,
  ;;     which is used to lookup the next action or keymap in the the
  ;;     current keymap (the MODAL-LOOKUP-STATE-KEYMAP field) of the
  ;;     <MODAL-LOOKUP-STATE-TYPE> record.
  ;;
  ;;  3. DO-ACTION is a procedure called when the key-path lookup
  ;;     retrieved an action and not a keymap.
  ;;
  ;;  4. DO-WAIT-NEXT is a procedure called when the key-path lookup
  ;;     retrieved a keymap and not an action.
  ;;
  ;;  5. DO-FAIL-LOOKUP is a procedure called when the key-path lookup
  ;;     finds neither an action or a keymap.
  ;;
  ;; This function should be called every time a keyboard event
  ;; occurs. On every call the STATE argument is updated and then one
  ;; of three "DO-" procedures is called depending on the state
  ;; transition:
  ;;
  ;;   - If an alt-action procedure was found and there are no other
  ;;     possible lookups to be performed, evaluate the DO-ACTION
  ;;     procedure, which must take two arguments: 1. a key index that
  ;;     triggered it wrapped in a promise (must be forced in order to
  ;;     be used), and 2. a procedure to be evaluated. After
  ;;     evaluating DO-ACTION, return #f.
  ;;
  ;;   - If non-empty keymap is found, regardless of whether an
  ;;     alt-action is present, evaluate the DO-WAIT-NEXT procedure
  ;;     with a promise to the current key index (must be forced in
  ;;     order to be used) and the next keymap being awaited.  After
  ;;     evaluating DO-WAIT-NEXT, return #t.
  ;;
  ;;   - If an completely empty keymap is found, evaluate the
  ;;     DO-FAIL-LOOKUP procedure with a single argument: the key
  ;;     index up to this point (not a "promise" object wrapping the
  ;;     key index value, but the key index value itself). After
  ;;     evaluating DO-FAIL-LOOKUP, return #f.
  (let*((step (make<keymap-index> (mod-index key-path) (char-index key-path) #f))
        (next (next-index key-path))
        (stack (cons step (modal-lookup-state-index-stack state)))
        (full-key-path (delay (reverse-list->keymap-index stack)))
        (action-or-map (modal-lookup-state-lookup state step))
        )
    (set!modal-lookup-state-index-stack state stack)
    (cond
     ((and (not next) action-or-map (not (keymap-type? action-or-map)))
      (set!modal-lookup-state-keymap state #f)
      (do-action full-key-path action-or-map)
      #f)
     ((keymap-type? action-or-map)
      (set!modal-lookup-state-keymap state action-or-map)
      (do-wait-next full-key-path action-or-map) #t)
     (else
      (set!modal-lookup-state-keymap state #f)
      (do-fail-lookup (force full-key-path)) #f))
    ))

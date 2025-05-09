(cond-expand

  (mit

   (define loader load)

   (define (finish)
     (disk-save "./gypsum-mitscheme.com")
     (newline)
     (display ";;; Now run the command `mit-scheme --band ./gypsum-mitscheme.com`")
     (newline)
     #t)
   )

  (guile

   (define (loader file)
     (compile-file (string-append "./" file ".sld"))
     )

   (define (finish) #t)
   )

  (gauche

   (define (loader file)
     (load (string-append "./" file ".sld"))
     )
   (define (finish) #t)
   )

  (stklos

   (load-path
    (append
     '("."
       "./rapid"
       "./chibi"
       "./match"
       "./gypsum"
       "./gypsum/lens"
       "./gypsum/editor"
       "./gypsum/elisp-eval"
       )
     (filter
      (lambda (p) (not (string=? p ".")))
      (load-path)
      )))

   (define (loader file)
     (let ((src (string-append "./" file ".sld"))
           (obj (string-append "./" file ".ostk"))
           )
       (display "; compile ") (write src) (newline)
       (compile-file src obj)
       (load obj)
       ))
   (define (finish) #t)
   )

  (else
   (error "this Scheme implementation is not supported")
   ))

;;--------------------------------------------------------------------------------------------------

(define file-list
  (list
   "rapid/assume"
   "rapid/format"
   "rapid/test"
   "chibi/match"
   "slib/common"
   "slib/filename"
   "slib/directory"
   "gypsum/test"
   "gypsum/bitwise"
   "gypsum/string"
   "gypsum/vector"
   "gypsum/hash-table"
   "gypsum/lens"
   "gypsum/cursor"
   "gypsum/lens/vector"
   "gypsum/pretty"
   "gypsum/lens/bin-hash-table"
   "gypsum/lexer"
   "gypsum/editor/command"
   "gypsum/keymap"
   "gypsum/bit-stack"
   "gypsum/editor-impl"
   "gypsum/elisp-eval/pretty"
   "gypsum/elisp-eval/parser"
   "gypsum/elisp-eval/environment"
   "gypsum/elisp-eval/format"
   "gypsum/elisp-eval"
   "gypsum/elisp-load"))

(for-each
 (lambda (file) (loader file))
 file-list
 )

(finish)

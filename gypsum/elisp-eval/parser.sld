(define-library (gypsum elisp-eval parser)
  (import
    (scheme base)
    (scheme write);;DEBUG
    (scheme lazy);;old parser only
    (scheme case-lambda)
    (only (scheme char)
          char-numeric?  char-alphabetic?  digit-value
          )
    (only (scheme file) open-input-file)
    (only (gypsum lens vector)
          new-mutable-vector
          mutable-vector->vector
          mutable-vector-append!
          )
    (gypsum elisp-eval lexer)
    (only (gypsum lens)
          record-unit-lens  lens  =>view-only-lens
          view  update  lens-set
          )
    (only (gypsum lexer)
          run-lexer  lex-all
          any  char  take  eof
          alist->parse-table
          lex-table  parse-table-ref
          lex  look  lex/buffer
          many1  many1/buffer  either
          many   many/buffer   lex-const
          lex-fold  lex-fold-count    lex-fmap
          lex-brackets  lex-apply  lex-first
          scan-for-string  skip-to-next-line
          lex-trace
          new-lexer-error
          lexer-error-type?
          lexer-state  lexer-state-type?  lexer-look-ahead
          source-file-location-type?
          lexer-state-get-location
          write-lexer-location
          source-file-line  source-file-column
          *unicode-max-code-point*
          =>lexer-filepath*!
          )
    (only (gypsum elisp-eval environment)
          elisp-unquoted-form-equal?
          elisp-quote-scheme
          elisp-unquoted-form
          elisp-quote-scheme-equal?
          elisp-quote-scheme-type?
          elisp-backquoted-form?
          elisp-unquote-scheme
          elisp-unquoted-form-type?
          elisp-spliced-form?
          elisp-unquoted-get-form
          )
    )
  ;; -------- Hash Tables --------
  (cond-expand
    ((or guile gambit)
     (import
       (only (srfi 69)
             make-hash-table
             hash-table-ref
             hash-table-set!
             hash-table-ref/default
             )
       ))
    (mit)
    (else
     (import
       (only (srfi 125)
             make-hash-table
             hash-table-ref
             hash-table-set!
             hash-table-ref/default
             )
       )))
  ;; -------- File port extensions --------
  (cond-expand
    (guile
     (import
       (only (guile)
             port-filename
             port-line
             port-column
             unread-char
             set-source-properties!
             source-properties
             )
       )))
  (export
   read-elisp ;; this is the old API

   ;;----------------
   ;; The parsing API
   parse-state ;; this constructs a parser state, similar API to 
   elisp-parse-state-type?
   =>parse-state-filepath*! ;; lens to get or set the current filepath
   elisp-read  ;; read a single Elisp form
   elisp-read-all ;; read all forms into a vector
   elisp-read-file-for-each ;; loop on `ELISP-READ`, apply each parsed form to a procedure
   select-elisp-dialect! ;; check for "-*-lexical-binding:t-*-" magic comment 

   ;;----------------
   ;; Reporting
   write-parser-location ;; write the parser location to port
   write-location-form-newline ;; write a form's location, the form, and a newline

   ;;----------------
   ;; Working with parts of the abstract syntax tree (AST)
   get-location ;; get location information from various data types

   make<elisp-function-ref>
   elisp-function-ref-type?
   elisp-function-get-ref
   elisp-function-ref-loc

   elisp-form-type?
   elisp-form ;; construct an Elisp form
   elisp-form-tokens
   elisp-form-dot-element
   elisp-form-locations
   elisp-form-start-loc
   elisp-form-end-loc
   list->elisp-form ;; consruct an Elisp form from a Scheme list
   elisp-form->list
   elisp-form-equal? ;; compare Elisp form equality
   *elisp-form-sym-equal-test* ;; parameter that sets equality test for symbol objects
   elisp-form-length ;; return number of elements in a Elisp form
   write-elisp-form ;; write an Elisp form to a port

   ;;----------------
   ;; Selecting individual tokens
   lexer-state ;; re-export gypsum lexer state constructor
   run-elisp-tokenizer ;; lex the next token from a port
   whitespace?
   )
  (include "parser.scm"))

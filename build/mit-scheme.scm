(define file-list
  (list
   "./rapid/assume"
   "./rapid/format"
   "./rapid/test"
   "./chibi/match"
   "./slib/common"
   "./slib/filename"
   "./slib/directory"
   "./gypsum/test"
   "./gypsum/vector"
   "./gypsum/hash-table"
   "./gypsum/lens"
   "./gypsum/lens/vector"
   "./gypsum/lens/bin-hash-table"
   "./gypsum/pretty"
   "./gypsum/keymap"
   "./gypsum/bit-stack"
   "./gypsum/editor/command"
   "./gypsum/editor"
   "./gypsum/elisp-eval/environment"
   "./gypsum/elisp-eval/lexer"
   "./gypsum/elisp-eval/parser"
   "./gypsum/elisp-eval/pretty"
   "./gypsum/elisp-eval/format"
   "./gypsum/elisp-eval"
   "./gypsum/elisp-load"))

(for-each
 (lambda (file)
   (load (string-append file ".sld"))
   )
 file-list)

(disk-save "./gypsum.com")

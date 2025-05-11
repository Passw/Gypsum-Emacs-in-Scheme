(define-library (gypsum lens bin-hash-table)
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme write)
    (only (scheme write) display)
    (only (gypsum lens)
          lens record-unit-lens view lens-set
          =>self =>hash-key! =>on-update
          =>canonical =>encapsulate)
    (only (gypsum pretty)
           pretty print bracketed newline-indent line-break)
    (only (gypsum hash-table) ; Standard hash tables
          hash-table-empty?
          default-hash make-hash-table alist->hash-table hash-table->alist
          hash-table-size hash-table-copy hash-table-walk
          hash-table-update!/default hash-table-set!
          hash-table-fold hash-table?
          )
    )
  (cond-expand
    (gauche
     (import (only (srfi 114) equal-comparator))
     ))
  (cond-expand
    (guile-3
     (import
       (only (srfi 28) format)
       (only (srfi srfi-9 gnu) set-record-type-printer!)))
    (else)
    )
  (export
   *bin-hash-table-init-size*
   *default-make-hash-table*
   *default-key-hash*
   make<bin-hash-table>
   bin-hash-table-type?
   empty-bin-hash-table
   bin-hash-table
   alist->bin-hash-table
   bin-hash-table->alist
   bin-hash-table-empty?
   bin-hash-table-size
   =>bin-hash-table-store-size!
   =>bin-hash-table-hash*!
   =>bin-hash-table-hash!
   =>bin-hash-key!
   bin-hash-table-fold
   hash-table-copy-with
   bin-hash-table-copy
   bin-hash-table-print
   )

  (include "bin-hash-table.scm"))

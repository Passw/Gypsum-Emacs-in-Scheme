(define-library (gypsum cursor)
  (import
    (scheme base)
    (scheme case-lambda)
    (only (gypsum lens)
          record-unit-lens view endo-set
          =>assoc-by =>bring =>find
          )
    )
  (cond-expand
    ((or guile gambit stklos)
     (import
       (only (srfi 69) hash-table? hash-table->alist)
       ))
    )
  (export
   new-cursor  maybe-new-cursor  new-cursor-if-iterable
   cursor-type?  cursor-object
   cursor-index  set!cursor-index  =>cursor-index!
   cursor-ref  cursor-end?  cursor-step!
   cursor-collect-list

   cursor-interface
   make<cursor-interface>
   cursor-report-end
   cursor-reference
   cursor-stepper
   cursor-jumper
   declare-interface/cursor
   )
  (include "cursor.scm"))

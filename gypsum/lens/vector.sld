(define-library (gypsum lens vector)
  (import
    (scheme base)
    (scheme case-lambda)
    (scheme write)
    (srfi 1)
    (only (gypsum lens)
          record-unit-lens unit-lens lens
          view lens-set update&view
          =>canonical
          =>vector-index!
          declare-rule/index->lens)
    (only (gypsum cursor)
          make<cursor-interface>
          declare-interface/cursor
          )
    )
  (export
   make<mutable-vector>
   mutable-vector-type? new-mutable-vector mutable-vector-copy
   mutable-vector-empty? mutable-vector-length
   mutable-vector-store-size
   =>mutable-vector!
   =>mutable-vector-min-index
   =>mutable-vector-max-index
   =>mvector-index!
   =>mvector-index/default!
   mutable-vector-min-size
   *mutable-vector-grow*
   default-mutable-vector-grower
   mutable-vector-append!
   mutable-vector-shift!
   mutable-vector-fold
   mutable-vector-fold/index
   mutable-vector-for-each
   mutable-vector->list
   mutable-vector-fill!
   mutable-vector-clear!
   mutable-vector-iterate
   mutable-vector-delete-range!
   vector-copy-with)
  (include "vector.scm"))

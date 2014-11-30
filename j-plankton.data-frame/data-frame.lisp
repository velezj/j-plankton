
(in-package #:j-plankton.data-frame)


;;=========================================================================

;;;;
;;;; We defien a concept for getting a "slice" of a data-frame.
;;;; This returns a data-frame with a subset of the elements
(define-concept
    slice
    ( (data-source "The original object from which we want a slice.
                    Typically this is a data-frame but it can be other 
                    obsercts such as raw array and such")
      (slice-expression "A 'slice-expression' which determine
                         the inclusiong of certain objects into the slice.
                         Multiple slice expression can be concatenated using
                         the slice expression DSL")
      &key
      (drop-empty "do we drop empty dimensions in the resulting slice?
                   By default we do so that the resultign slice is as
                   'compact' as possible")
      (unused-value "The value for places in hte slice which have
                     no value. For example, if we sliced a normal 
                     dense 2D matrix to get a random subset, the
                     elements we did not 'get' in the slice would
                     be such 'unused' values")
      &allow-other-keys)
  "A 'slice' returns a data-frame with a subset of the data in the given
   object.  Usually, the given object is itself a data-frame, but it may also
   be any other type as long as a method is defiend to slice it.
   The subset elements are calculated using the slice-expression.
   Each slice-expression is a way to determine a set of elements to include.")


;;=========================================================================

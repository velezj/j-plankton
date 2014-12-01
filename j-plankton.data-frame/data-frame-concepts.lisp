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

;;;;
;;;; Define a concept for retrieving the set of 'labels' which are part
;;;; of *any* element of a data-frame (or object that behaves like one)
;;;; Labels are of any type (thought usually symbols).
;;;; Inherent integer indices are not consideres 'labels'
(define-concept
    label-set 
  ( (object "a data-frame of data-frame-like object which has a potential to
             have labeled elements inside it")
    &key
    (scope "What 'scope' of labels do be include.  This is a list which may
            include any of the following scopes:
               :global = global labels only (exist for logical sets of elements)
               :local  = local labels (exist for individual elements)
               :dimensional = labes exists for dimensions of the data-frame
               :all --> same as (:global :local :dimensional)") )
  "Returns a set of label objects of the given scope for the data-frame
   (or data-frame-like) object.  The ordering is non-specific (hence a set).
   Usually labels are symbols, but they can be any type.
   Inherent integer indices are not considered labels and are not retuned")

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
             


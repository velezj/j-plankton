(in-package #:j-plankton.data-frame)

;;============================================================================

;;;;
;;;; Here, we define the concept of creating a new sparse tensor.
(define-concept 
    make-sparse-tensor 
  ( (num-dimensions "The number of dimensions for this tensor")
    (default-value "The value for any non-set tensor element") )
  "Creates a new sparse tensor with the given number of dimensions.")

;;============================================================================

;;;;
;;;; Access an element of a sparse tensor using the integer indices
(define-concept
    tref
  ( (tensor "The sparse tensor object")
    &rest
    (indices "the indices for the dimensions of the tensor"))
  "Access a particular element of a tensor with N dimenensions by
   calling (tref <tensor> i0 i1 ... iN-1)")


;;============================================================================

;;;;
;;;; Set an element of a sparse tensor.
(define-concept
    set-tref
    ( (tensor "The sparse tensor object")
      (value "The value to set the lement to")
      &rest
      (indices "The indices for the element to set"))
  "Set an element of a sparse tensor of dimension N by calling
   (set-tref <tensor> <new-val> i0 i1 ... iN-1).
   This returns multiple values:
     first value is the old value of the element.
     second value is true iff the element was not a default element.
   So (values <old-value> <t iff element previously set>)")


;;============================================================================

;;;;
;;;; Is an element 'set' within a sparse tensor. Set elements have been
;;;; exlicitly (set-tref...) for the tensor (even if to the default value!)
;;;; and have not been (delete-tref...) (which un-sets them)
(define-concept
    is-tref-set
  ( (tensor "the tensor object")
    &rest
    (indices "the indices for thelement"))
  "Is an element 'set' within a sparse tensor. Set elements have been
   exlicitly (set-tref...) for the tensor (even if to the default value!)
   and have not been (delete-tref...) (which un-sets them)")

;;============================================================================


;;;;
;;;; Delete/clear an element of a sparse tensor.
;;;; such element are not considered 'set' anymore and revert back
;;;; to the sparse tensor default value
(define-concept
    delete-tref
    ( (tensor "The sparse tensor object")
      &rest
      (indices "The indices for the element to set"))
  "delete an element of a sparse tensor of dimension N by calling
   (delete-tref <tensor> i0 i1 ... iN-1).
   A delete element revarts back to the default tensor value and is
   no longer considered 'set'.
   This returns multiple values:
     first value is the old value of the element.
     second value is true iff the element was not a default element.
   So (values <old-value> <t iff element previously set>)")

;;============================================================================

;;;;
;;;; A concept which details how to loop over all of the *set* elements
;;;; in a sparse tensor. This means we will never iterate over an element
;;;; which has not been previously (set-tref ....). Also, we will skip
;;;; any (delete-tref...) elemetns.
;;;; The result is *alos a sparse tensor* with the element at <i0...iN-1>
;;;; being that returnd by calling (func <i-...iN-1> <element-value>).
(define-concept
    tensor-map-set-elements
  ( (tensor "the sparse tensor object")
    (func "the function to call for each set element of the form:
           (func indices value)
           where indices = (list i0 i1 .... iN-1).")
    &key
    (default-value "The default value for the retuerned sparse tensor"))
  "A concept which details how to loop over all of the *set* elements
   in a sparse tensor. This means we will never iterate over an element
   which has not been previously (set-tref ....). Also, we will skip
   any (delete-tref...) elemetns.
   The result is *alos a sparse tensor* with the element at <i0...iN-1>
   being that returnd by calling (func <i-...iN-1> <element-value>).")
	  

;;============================================================================

;;;;
;;;; Convert a sparse tensor to a flat index association list.
;;;; The keys are themselves the list of the indices for a set value
;;;; and the value is the value. Alos a key :default-value also exists
;;;; with the default value of the tensor and a key :num-dimensions with
;;;; the number of dimensions of the tensor
(define-concept 
    tensor->alist
  ( (tensor "the tensor object") )
  "Convert a sparse tensor to a flat index association list.
   The keys are themselves the list of the indices for a set value
   and the value is the value. Alos a key :default-value also exists
   with the default value of the tensor and a key :num-dimensions with
   the number of dimensions of the tensor.")


;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================


(implement-concept
    (tensor->alist generic 
     "The generic way of mapping a tensor to an alist simpy uses 
      tensor-map-set-elements and pushes the indices ignoring the resulting
      tensor."
     :default-implementation t)
    ( (tensor "the tensor object") )
  (let ((alist (list
		(cons :default-value
		      (default-value tensor))
		(cons :num-dimensions
		      (num-dimensions tensor)))))
    (tensor-map-set-elements tensor
			     #'(lambda (indices value)
				 (push (cons indices value) alist))
			     :default-value nil)
    alist))

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

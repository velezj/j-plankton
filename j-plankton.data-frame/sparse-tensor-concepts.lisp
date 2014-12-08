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
;;;; Access the number of dimension in a sparse tensor
(define-concept
    num-dimensions
    ( (object "the tensor object") )
  "Returns the number of dimensions for a sparse tensor")

;;============================================================================

;;;;
;;;; Access the default value of a sparse tensor
(define-concept
    default-value
    ( (object "the tensor object"))
  "Returns the default value for a sparse tensor")

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

;;;;
;;;; Set the valeus fo a tensor from another tensor.
;;;; Only the *set* values of the srouce tensor are used to
;;;; set the values of the given tensor.
(define-concept
    set-tensor
    ( (target-tensor "The tensor object whoose values are to 
                      be set and modified.")
      (source-tensor "The tensor object whoose values are the source
                      values"))
  "Sets teh values of the target sensor to those of teh source
   tensor.  Only the *set* values of the srouce sensor are used.
   In this way, the target tensor's *set* values will be a superset
   of the source tensors (and will be equal if no set values initially")

;;=========================================================================

;;;;
;;;; Set the valeus fo a tensor from another tensor.
;;;; Only the *set* values of the *target* tensor are maodfied
;;;; by the values from the source tensor (whetehr set of not)
(define-concept
    fill-tensor
    ( (target-tensor "The tensor object whoose values are to 
                      be set and modified.")
      (source-tensor "The tensor object whoose values are the source
                      values"))
  "Sets teh values of the target sensor to those of teh source
   tensor.  Only the *set* values of the *target* tensor are 
   modyfied by values from the srouce tensor")


;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

(implement-concept
    (num-dimensions generic
     "The generic way to query for number of dimensions:
      assume there is a method num-dimensions"
     :default-implementation t)
    ( (object "the sparse tensor"))
  (num-dimensions object))

;;=========================================================================

(implement-concept
    (default-value generic
     "The generic way to query for the default value:
      assume there is a method default-value"
      :default-implementation t)
    ( (object "the sparse tensor"))
  (default-value object))

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

;;;;
;;;; The genric version of set-tensor
(implement-concept
    (set-tensor generic
		 "Sets the values from soruce tensro to target tensor.
                  The indices from source are used for target, and only
                  *set* values are used from source"
		 :default-implementation t)
    ( (target-tensor "The target tensor which is modified")
      (source-tensor "Source tensor for values"))
  (tensor-map-set-elements
   source-tensor
   #'(lambda (idx value)
       (apply #'set-tref target-tensor value idx)))
  target-tensor)

;;=========================================================================

;;;;
;;;; The genric version of fill-tensor
(implement-concept
    (fill-tensor generic
		 "Sets the values from soruce tensro to target tensor.
                  The indices from source are used for target, and only
                  *set* values of *target* are modyfied."
		 :default-implementation t)
    ( (target-tensor "The target tensor which is modified")
      (source-tensor "Source tensor for values"))
  (tensor-map-set-elements
   target-tensor
   #'(lambda (idx value)
       (declare (ignore value))
       (apply #'set-tref target-tensor
	      (apply #'tref source-tensor idx)
	      idx)))
  target-tensor)

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
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

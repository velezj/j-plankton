
(in-package #:j-plankton.data-frame)

;;============================================================================

;;;;
;;;; A sparse tensor calss based upon hashtables
(defclass hash-tensor-t ()
  ((default-value 
    :initarg :default-value
    :initform nil
    :accessor default-value
    :documentation
    "The default value for unset elements (defaults to nil)")

   (num-dimensions
    :initarg :num-dimensions
    :initform 0
    :accessor num-dimensions
    :documentation
    "The number of dimensions in this tensor")
   
   (data-hashtable
    :initarg :data-hashtable
    :initform (make-hash-table :test 'equal)
    :accessor data-hashtable
    :documentation
    "The hashtable containing the set elements, mapping a list of indice
     to the value")))

;;============================================================================

(implement-concept
    (make-sparse-tensor flat-hash
     "The implementation of make-sparse-tensor which creates a 
      hash-tensor-t which uses a flat hashtable to store a mapping
      between indices and a value"
     :default-implementation t)
    ( (num-dimensions "the number of dimensions")
      (default-value "the default element") )
  (make-instance 'hash-tensor-t 
		 :num-dimensions num-dimensions
		 :default-value default-value))

;;============================================================================


(implement-concept
    (tref list-index-flat-hash 
     "The implementation of tref using a simpe flat hash of a list of indices"
     :default-implementation t)
    ( (tensor  hash-tensor-t "the tensor object, must be a hash-tensor-t")
      &rest
      (indices "the indices") )
  (gethash indices (data-hashtable tensor) (default-value tensor)))

;;============================================================================

(implement-concept
    (set-tref list-index-flat-hash 
     "The implementation of set-tref using a simpe flat hash 
      of a list of indices"
     :default-implementation t)
    ( (tensor  hash-tensor-t "the tensor object, must be a hash-tensor-t")
      (new-value "the new value")
      &rest
      (indices "the indices") )
  (multiple-value-bind (old-value set-p)
      (gethash indices (data-hashtable tensor) (default-value tensor))
    (setf (gethash indices (data-hashtable tensor)) new-value)
    (values old-value set-p)))


;;============================================================================

(implement-concept
    (is-tref-set list-index-flat-hash 
     "The implementation of is-tref-set using a simpe flat hash 
      of a list of indices"
     :default-implementation t)
    ( (tensor  hash-tensor-t "the tensor object, must be a hash-tensor-t")
      &rest
      (indices "the indices") )
  (multiple-value-bind (dummy set-p)
      (gethash indices (data-hashtable tensor))
    set-p))
  
;;============================================================================

(implement-concept
    (delete-tref list-index-flat-hash
     "delete a given element using a flat hashtable implementation")
    ( (tensor hash-tensor-t "the tensor, must be a hash-tensor-t")
      &rest
      (indices "the indices"))
  (let ((old-value
	  (gethash indices (data-hashtable tensor) (default-value tensor))))
    (values
     old-value
     (remhash indices (data-hashtable tensor)))))

;;============================================================================

(implement-concept
    (tensor-map-set-elements list-index-flat-hash
     "map tensor element which are set using a flat hashtable indexed
      by a list of indices"
     :default-implementation t)
    ( (tensor hash-tensor-t "The tensor, must be a hash-tensor-t")
      (func "the function, callabel as (func <list-of-indices> <value>)")
      &key
      (default-value "the default value in resulting tensor"))
  (let ((res-tensor (make-sparse-tensor (num-dimensions tensor)
					(default-value tensor))))
    (maphash
     #'(lambda (indices value)
	 (let ((res-value
		 (funcall func indices value)))
	   (apply #'set-tref res-tensor res-value indices)))
     (data-hashtable tensor))
    res-tensor))

;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================

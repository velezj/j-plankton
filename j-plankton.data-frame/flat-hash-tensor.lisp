
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

;;;;
;;;; Drop any empty dimensions for this sparse tensor.
;;;; (empty dimensions are only ever 'at the end' of the dimensoions list.
(defmethod drop-empty-dimensions ( (tensor hash-tensor-t) )
  (let ((max-index-dims
	 (loop for idx being the hash-keys of (data-hashtable tensor)
	    maximize (length idx))))
    (setf (num-dimensions tensor)
	  max-index-dims)))
  
;;============================================================================
;;============================================================================
;;============================================================================


;;;;
;;;; Implement the slice concept for sparse tensor.
;;;; This is the generic implementation
(implement-concept (slice generic
			  "Retrieve subset of elements from a tensor
                           using a cursor"
			  :default-implementation t)
    ( ((tensor hash-tensor-t)
               "The tensor from which we want a slice.
                Typically this is a data-frame but it can be other 
                obsercts such as raw array and such")
      (cursor-expression "A cursor-expression which determine
                         the inclusiong of certain objects into the slice.
                         The cursor value are the indices.")
      &key
      (drop-empty t "do we drop empty dimensions in the resulting slice?
                       By default we do so that the resultign slice is as
                       'compact' as possible")
      (unused-value nil "The value for places in hte slice which have
                           no value. For example, if we sliced a normal 
                           dense 2D matrix to get a random subset, the
                           elements we did not 'get' in the slice would
                           be such 'unused' values")
      (flatten nil "Do we return a fla tensor (1-dimensional) with the
                    valeus we asked for ratehr than a structured tensor")
      &allow-other-keys)
  
  (let ((cursor (j-plankton.cursor:cursor-expression->cursor cursor-expression))
	(flatten nil))
    (if (not flatten)

	;; keep structuer since flatten is nil
	(let ((result-tensor (make-sparse-tensor
			      (num-dimensions tensor)
			      unused-value)))
	  (j-plankton.cursor:cursor/loop (idx cursor)
	    (let ((val (apply #'tref tensor idx)))
	      (apply #'set-tref result-tensor val idx)))
	  (when drop-empty
	    (drop-empty-dimensions result-tensor))
	  result-tensor)
	
	;; else we are flattening
	(let ((result-tensor (make-sparse-tensor
			      1 unused-value))
	      (flat-idx 0))
	  (format t "faltten is true ...~%")
	  (j-plankton.cursor:cursor/loop (idx cursor)
	    (let ((val (apply #'tref tensor idx)))
	      (set-tref result-tensor val flat-idx)
	      (incf flat-idx)))
	  result-tensor))))

;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================
;;============================================================================

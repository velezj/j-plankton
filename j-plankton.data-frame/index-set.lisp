
(in-package #:j-plankton.data-frame)

;;=========================================================================

;;;;
;;;; An index-set-t is an *ordered* set of indices.
;;;; These are generalyl not materialized as a list but ratehr
;;;; can be map'd trhough to use.  index-set-t can include
;;;; multiple instances of the same index.
;;;; Also, and index is in fact a set of labels for multi-dimensional
;;;; index-set-t
(defclass index-set-t ()
  ((index-set-labels
    :type 'list
    :initarg :labels
    :initform nil
    :accessor index-set-labels
    :documentation
    "A list of all the possible index labels in a single index for this
     index set. Not all index will have all labels, but each label
     is contained in at least one index")

   (index-set-map
    :type 'function
    :initarg :map
    :accessor index-set-map
    :documentation
    "A function wich maps all the indices in an index set.
     This is the main way of iterating over the indices in an
     index set.  This function must take the following args:
         <func>        = a fucntion
         :skip-nil <s> = a &key arg used to determine whether to call
                         <func> with nil indices or not")

   ;; (index-set-in
   ;;  :type 'function
   ;;  :initarg :in
   ;;  :accessor index-set-in
   ;;  :documentation
   ;;  "A function which returns whether a given index is part
   ;;   of this index set")

   ))

;;=========================================================================

;;;;
;;;; Maps the indices in an index-set-t.
;;;; The function takes as arguments an association list of
;;;; (label value) with the labesl for a paricular index
;;;; By default, we skip nil elements (indexes).
;;;; The results of the function are thrown away
(defmethod map-index-set ( func (index-set index-set-t) &key (skip-nil t) )
  (funcall (index-set-map index-set)
	   func
	   :skip-nil skip-nil))

;;=========================================================================

;;;;
;;;; Make an index set which has a constant index value for the given
;;;; labels, repeated the given number of times
(defun index/constant (index-labels value &key (count 1))
  (make-instance 'index-set-t
		 :labels index-labels
		 :map #'(lambda (func &key skip-nil)
			  (let ((index-assoc
				 (mapcar #'(lambda (idx-label)
					     (cons idx-label
						   value))
					 index-labels)))
			    (dotimes (iter count)
			      (when (or (not skip-nil)
					index-assoc)
				(funcall func index-assoc)))))))

;;=========================================================================

;;;;
;;;; Make an index set where each dimension, in parralel, starts at
;;;; a given value and increases ot hte given end value.
;;;; Start and end are *inclusive*!
;;;; Steps must be positive
(defun index/parallel-sweep ( index-labels &key (start 0) end (step 1) )
  (let ((mapper
	 #'(lambda (func &key skip-nil)
	     (if (and skip-nil
		      (null index-labels))
		 (values) ;; nothing to do since eveyrthing will be nil!
		 (loop for v from start to end by step
		    do (let ((assoc
			      (mapcar #'(lambda (idx-label)
					  (cons idx-label v))
				      index-labels)))
			 (funcall func assoc)))))))
    (make-instance 'index-set-t
		   :labels index-labels
		   :map mapper)))

;;=========================================================================

;;;;
;;;; Create and index-set which sweeps through each dimension from
;;;; left-to-right and for each set's it's value from start to end
;;;; (inclusive!) incrmenting by given step
(defun index/sweep (index-labels &key (start 0) end (step 1))

  (labels
      ((sweeper (func sweep-labels start end step set-labels)
	 (if (null sweep-labels)
	     (funcall func set-labels)
	     (loop for i from start to end by step
		do
		  (sweeper func
			   (cdr sweep-labels)
			   start
			   end
			   step
			   (append (list (cons (car sweep-labels)
					       i))
				   set-labels))))))
    (make-instance 'index-set-t
		   :labels index-labels
		   :map #'(lambda (func &key skip-nil)
			    (if (null index-labels)
				(values)
				(sweeper func
					 (reverse index-labels)
					 start
					 end
					 step
					 nil))))))
				
    
			  

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

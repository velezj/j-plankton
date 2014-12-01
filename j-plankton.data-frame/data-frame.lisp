
(in-package #:j-plankton.data-frame)



;;=========================================================================

;;;;
;;;; This is the data-frame-t class which stores the data and labels.
;;;; Internally, the data itself is store in a spartns sparse tebnsor.
(defclass data-frame-t ()
  ((data-tensor
    :initarg :data-tensor
    :accessor data-tensor
    :documentation "the spartns sparse tensor which has as values the data
                    indexed by the inherent integer indices" )
   
   (global-labels
    :type 'list
    :initarg :global-labels
    :initform nil
    :accessor global-labels
    :documentation "A list of global labels which have values for all data")

   (global-labels-tensor
    :initarg :global-labels-tensor
    :accessor global-labels-tensor
    :documentation "A tensor with the mapping for global labels to a value 
                    for all the data in the data-tensor")

   (dimension-labels
    :type 'hash-table
    :initarg :dimension-labels
    :initform (make-hash-table)
    :accessor dimension-labels
    :documentation "A hashtable mapping a dimension to a list of
                    labes which are all applicable and have values for
                    that dimension")

   (dimension-labels-data-map
    :type 'hash-table
    :initform :dimension-labels-data-map
    :accessor dimension-labels-data-map
    :documentation "a hashtable mapping from a dimension label to a 
                    tensor with the label value for that data cell")

   (local-labels
    :type 'list
    :initarg :local-labels
    :initform nil
    :accessor local-labels
    :documentation "A list of any local labels (labels that are per-cell)
                    that this dataframe contains")

   (local-labels-tensor
    :initform :local-labels-tensor
    :accessor local-labels-tensor
    :documentation "A tensor whoose elements are association lists with
                    (local-label value). The tensor is indexed by the inherent
                    integer indices just like the data-tensor.")))
   

;;=========================================================================

;;;;
;;;; A function to make a data-frame-t object.
;;;; It takes thw following keyword arguments:
;;;;    :data-tensor = the underlying sparse tensor holding data
;;;;    :dimensional-extents = a lsit of extents dterminig the subset of the
;;;;                           sparse tensor this data-frame is 'viewing'
;;;;                           Each element must be an dimensional-extent-t
;;;;    :

;;=========================================================================
;;=========================================================================
;;=========================================================================

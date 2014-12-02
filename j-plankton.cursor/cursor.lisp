
(in-package #:j-plankton.cursor)

;;=========================================================================

;;;;
;;;; The generic protocol method which returns the current
;;;; value of a cursor
(defgeneric value (cursor)
  (:documentation
   "Returns the current value of the cursor"))

;;=========================================================================

;;;;
;;;; Protocol method which returns if the cursor is done,
;;;; so no (next ...) or (value ...)
(defgeneric done-p (cursor)
  (:documentation
   "Returns if wew can call (next...) or (value...)
    on cursor to get a next/current element"))

;;=========================================================================

;;;;
;;;; Protocol method to reset the cursor to the starting value
(defgeneric reset (cursor)
  (:documentation
   "reset cursor to starting value"))

;;=========================================================================

;;;;
;;;; Protocol method which moves the cursor to the next value,
;;;; returning it. raises CURSOR-FINISHED-CONDITION condition if
;;;; there is no next element
(defgeneric next (cursor)
  (:documentation
   "Moves cursor to next element and returns
    (values <previous-value> <done-p>)
   Raises cursor-finished-condition is no next element."))
  
;;=========================================================================

;;;;
;;;; Condition raised when calling (next...) on a cursor with no next
;;;; element
(define-condition cursor-finished-condition ()
  ((cursor
    :initarg :cursor
    :initform nil
    :reader cursor))
  (:report
   (lambda (condition stream)
     (format stream "cursor ~A finished but you tries to access it without (reset..)!"))))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; the type for a ranged cursor with start/end and step
(defclass range-cursor-t ()
  ((value
    :type 'number
    :initarg :value
    :accessor value
    :documentation 
    "The curent value of this cursor")
   
   (start
    :initarg :start
    :initform 0
    :reader start
    :documentation
    "The start of the range, inclusive")

   (end
    :initarg :end
    :reader end
    :documentation
    "The end of the range, inclusive, may not actually ever be a value
     depending on the step")

   (step-f
    :initarg :step
    :initform 1
    :reader step-f
    :documentation
    "The step to get to a next value.
     If this is a function, it must return two values:
        (values <next-value> <done-p>) where if done-p is t
     then the value can be anything since we are done.")))

(defmethod initialize-instance :after ( (range range-cursor-t) &key )
  (when (not (slot-boundp range 'value))
    (setf (value range) (start range))))


(defmethod print-object ( (range range-cursor-t) stream )
  (format stream "#<range ~A [~A,~A,~A]~A>"
	  (value range)
	  (start range)
	  (end range)
	  (step-f range)
	  (if (done-p range) "*" "")))

;;=========================================================================

(defmethod done-p ( (range range-cursor-t) )
  (not 
   (and
    (>= (value range) (start range))
    (<= (value range) (end range)))))
  
;;=========================================================================

(defmethod next ( (range range-cursor-t) )
  (when (done-p range)
    (error 'cursor-finished-condition
	   :cursor range))
  (let ((prev-value (value range))
	(next-value
	  (if (functionp (step-f range))
	      (+ (value range)
		 (funcall (step-f range) range))
	      (+ (value range)
		 (step-f range)))))
    (setf (value range)
	  next-value)
    (values 
     prev-value
     (done-p range))))

;;=========================================================================

(defmethod reset ( (range range-cursor-t) )
  (setf (value range)
	(start range)))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; A transform-cursor-t is a cursor whch takes the output values
;;;; of an original set of cursors and returns a transformed version
;;;; of theyr values by applying a function to them.
;;;; We optionally allow to iterator to the longest or shortest of the
;;;; cursors given, and can include a default value to use per cursor
;;;; for when transofmring based onlongest cursor
(defclass transform-cursor-t ()
  ((original-cursors
    :initarg :cursors
    :reader original-cursors
    :documentation
    "a list of cursors we will use")

   (termination
    :initarg :termination
    :initform :shortest
    :reader termination
    :documentation
    "Do we terminate this cursor when the first orignal finishes (:first)
     or when the last orginal cursor finished (:last)")

   (default-values
    :initarg :default-values
    :accessor default-values
    :documentation
    "a list of default values, one per original cursor")

   (transform-f
    :initarg :f
    :reader transform-f
    :documentation
    "The function to apply to the values of hte cursors to get the
     value of this cursor.
     The function must take as many arguments as there are cursors and 
     must return a single value")))

(defmethod initialize-instance :after ( (tc transform-cursor-t) &key )
  (when (not (slot-boundp tc 'default-values))
    (setf (default-values tc)
	  (make-list (length (original-cursors tc)) 
		     :initial-element '+unset+))))

;;=========================================================================

(defmethod value ( (tc transform-cursor-t))
  (when (done-p tc)
    (error 'cursor-finished-condition
	   "Cursor ~A is finished but you tried to access it's (value...)"
	   :cursor tc))
  (let ((original-values
	  (mapcar #'(lambda (cursor default)
		      (if (done-p cursor)
			  default
			  (value cursor)))
		  (original-cursors tc)
		  (default-values tc))))
    (apply (transform-f tc) original-values)))


;;=========================================================================

(defmethod done-p ( (tc transform-cursor-t) )
  (case (termination tc)
    (:first
     (some #'done-p (original-cursors tc)))
    (:last
     (every #'done-p (original-cursors tc)))))

;;=========================================================================

(defmethod next ( (tc transform-cursor-t) )
  (when (done-p tc)
    (error 'cursor-finished-condition
	   "Cursor ~A tried to be (next...) when done"))
  (let ((prev-value (value tc)))
    (mapcar #'(lambda (c)
		(if (not (done-p c) )
		    (next c)
		    c))
	    (original-cursors tc))
    (values 
     prev-value
     (done-p tc))))

;;=========================================================================

(defmethod reset ( (tc transform-cursor-t) )
  (mapcar #'reset (original-cursors tc))
  (values))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; A cursor type which sweeps a given a pair (2)  cursors from left-ro-right
;;;; resetting each until at the ned all are done
(defclass %sweep-2-cursor-t ()
  ((cursor-pair
    :initarg :cursors
    :reader cursor-pair)))

(defmethod print-object ( (sweep %sweep-2-cursor-t) stream )
  (format stream "#<%sweep-2 ~A , ~A ~A>"
	  (first (cursor-pair sweep))
	  (second (cursor-pair sweep))
	  (if (done-p sweep) "*" "")))

;;=========================================================================

(defmethod value ( (sweep %sweep-2-cursor-t) )
  (destructuring-bind (a b) (cursor-pair sweep)					
    (append
     (typecase a
       (%sweep-2-cursor-t (value a))
       (t (list (value a))))
     (typecase b
       (%sweep-2-cursor-t (value b))
       (t (list (value b)))))))
			  

;;=========================================================================

(defmethod done-p ( (sweep %sweep-2-cursor-t ))
  (every #'done-p (cursor-pair sweep)))

;;=========================================================================

(defmethod reset ( (sweep %sweep-2-cursor-t) )
  (mapcar #'reset (cursor-pair sweep)))


;;=========================================================================

(defmethod next ( (sweep %sweep-2-cursor-t) )

  ;; if we are done, error out
  (when (done-p sweep)
    (error 'cursor-finished-condition
	   :cursor sweep))

  ;; always call next on the first cursor
  ;; if this causes it to be done, call next on the second cursor.
  ;; if it is not done, reset first cursor to keep sweeping
  (let ((prev-value (value sweep))
	(a (first (cursor-pair sweep)))
	(b (second (cursor-pair sweep))))
    (next a)
    (when (done-p a)
      (next b)
      (when (not (done-p b)) 
	(reset a)))
    (values
     prev-value
     (done-p sweep))))

;;=========================================================================


;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================


;;;;
;;;; Create a range cursor with start/end and step.
;;;; step may be a function which takes the a range-cursor-t object
;;;; and must return the next step to apply as well as if the 
;;;; cursor is done
;;;;
;;;; if only one argument is supplied we start a 0 until the given arg
(defun cursor/range (start/end &optional (end nil) (step 1) (init nil) )
  (if end
    (if init
      (make-instance
       'range-cursor-t
       :start start/end
       :end end
       :step step
       :value init)
      (make-instance
       'range-cursor-t
       :start start/end
       :end end
       :step step))
    (make-instance
     'range-cursor-t
     :start 0
     :end start/end
     :step step)))
      

;;=========================================================================

;;;;
;;;; Creat a transforming cursor.
;;;; This cursor will stop as soon as the first of the gievn cursors
(defun cursor/transform (func &rest cursors)
  (make-instance
   'transform-cursor-t
   :cursors cursors
   :f func
   :termination :first))
   

;;=========================================================================

;;;;
;;;; Create a tnrasofmring cursor of given cursors.
;;;; This will stop when the last of the cursors stops.
;;;; Each <cursor> element is a pair (cursor default-value) which
;;;; detemines both the curost and the default value to use
;;;; if it finishes
(defun cursor/transform-to-last (func &rest cursor-and-defaults)
  (make-instance
   'transform-cursor-t
   :cursors (mapcar #'first cursor-and-defaults)
   :default-values (mapcar #'second cursor-and-defaults)
   :termination :last
   :f func))

;;=========================================================================

;;;;
;;;; Make a labeled cursor, which labels the results of a cursor
;;;; with a set of labels, returning and association list with
;;;; (label value) pairs.
;;;;
;;;; If the label is not a list of labels, we return a single
;;;; pairing of (label value) rather than a ssociation lisst
(defun cursor/label (label-list cursor)
  (if (listp label-list)
      (cursor/transform
       #'(lambda (val)
	   (mapcar #'(lambda (label)
		       (cons label val))
		   label-list))
       cursor)
      (cursor/transform
       #'(lambda (val)
	   (list label-list val))
       cursor)))
  

;;=========================================================================

;;;;
;;;; Define a cursorwhich returns a parrallel sweep of the given
;;;; cursors (their values as a list of values)
(defun cursor/parallel-sweep (&rest cursors)
  (apply
   #'cursor/transform 
   #'list
   cursors))

;;=========================================================================

;;;;
;;;; Define a cursor which sweeps , from left ro right, the values of the
;;;; given cursors, retunring the values as a list. 
(defun cursor/sweep (&rest cursors)
  (when (= 1 (length cursors))
    (return-from cursor/sweep (car cursors)))
  
  ;; ok, build up a chain of %sweep-2 cursors 
  (reduce #'(lambda (cursor sweep-2)
	      (make-instance '%sweep-2-cursor-t
			     :cursors (list cursor sweep-2)))
	  cursors
	  :from-end t))
      

;;=========================================================================



;;;;
;;;; Returns a list of the values of a cursor
(defun cursor/materialize (cursor)
  (loop while (not (done-p cursor))
	collect (next cursor)))

;;=========================================================================

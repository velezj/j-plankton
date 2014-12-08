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
;;;; Protocol method which clones a cursor so that calling next/reset
;;;; change the returned cursor not hte original
(defgeneric clone (cursor)
  (:documentation
   "Make a copy of a cursor so that future next/reset calls on the 
    copy do not change the original"))

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
     (format stream "cursor ~A finished but you tries to access it without (reset..)!" (cursor condition)))))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; the base cursor type. Any cursors should substype from this
(defclass cursor-t ()
  ((properties
    :initarg :properties
    :initform nil
    :accessor properties
    :documentation
    "a properties-plist with properties and their values")))
   

;;;;
;;;; base cursor type fo compositional cursors
(defclass composing-cursor-t (cursor-t)
  ((original-cursors
    :initarg :cursors
    :reader original-cursors)))

;;=========================================================================

;;;;
;;;; Add a property to a cursor.
;;;; If the property already existed, update it's value
(defun cursor/add-property (cursor key value)
  (multiple-value-bind (found-prop old-value tail)
      (get-properties (properties cursor) (list key))
    (setf (properties cursor)
	  (append (list key value)
		  (alexandria:remove-from-plist
		   (properties cursor)
		   key)))
    (values
     (not found-prop)
     old-value)))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; the type for a ranged cursor with start/end and step
(defclass range-cursor-t (cursor-t)
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

(defmethod clone ((range range-cursor-t))
  (make-instance 'range-cursor-t
		 :value (value range)
		 :start (start range)
		 :end (end range)
		 :step (step-f range)))

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
(defclass transform-cursor-t (composing-cursor-t)
  ((termination
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

(defmethod print-object ( (tc transform-cursor-t) stream )
  (format stream "#<trans ~A ~S defs=~A F=~A ~A>"
	  (original-cursors tc)
	  (termination tc)
	  (if (and (slot-boundp tc 'default-values)
		   (default-values tc))
	      (default-values tc)
	      nil)
	  (transform-f tc)
	  (if (done-p tc) "*" "")))

;;=========================================================================

(defmethod clone ((tc transform-cursor-t))
  (make-instance 'transform-cursor-t
		 :cursors (mapcar #'clone (original-cursors tc))
		 :default-values (default-values tc)
		 :termination (termination tc)
		 :f (transform-f tc)))

;;=========================================================================

(defun %transform/raw-value ( tc )  
  (let ((original-values
	 (mapcar #'(lambda (cursor default)
		     (if (done-p cursor)
			 default
			 (value cursor)))
		 (original-cursors tc)
		 (default-values tc))))
    (apply (transform-f tc) original-values)))


(defmethod value ( (tc transform-cursor-t))
  (when (done-p tc)
    (error 'cursor-finished-condition
	   "Cursor ~A is finished but you tried to access it's (value...)"
	   :cursor tc))
  (%transform/raw-value tc))

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
(defclass %sweep-2-cursor-t (composing-cursor-t)
  ())

(defmethod print-object ( (sweep %sweep-2-cursor-t) stream )
  (format stream "#<%sweep-2 ~A , ~A ~A>"
	  (first (original-cursors sweep))
	  (second (original-cursors sweep))
	  (if (done-p sweep) "*" "")))

;;=========================================================================

(defmethod clone ( (sweep %sweep-2-cursor-t))
  (make-instance '%sweep-2-cursor-t
		 :cursors (mapcar #'clone (original-cursors sweep))))

;;=========================================================================


(defmethod value ( (sweep %sweep-2-cursor-t) )
  (destructuring-bind (a b) (original-cursors sweep)					
    (append
     (typecase a
       (%sweep-2-cursor-t (value a))
       (t (list (value a))))
     (typecase b
       (%sweep-2-cursor-t (value b))
       (t (list (value b)))))))
			  

;;=========================================================================

(defmethod done-p ( (sweep %sweep-2-cursor-t ))
  (every #'done-p (original-cursors sweep)))

;;=========================================================================

(defmethod reset ( (sweep %sweep-2-cursor-t) )
  (mapcar #'reset (original-cursors sweep)))


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
	(a (first (original-cursors sweep)))
	(b (second (original-cursors sweep))))
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

;;;;
;;;; A cursor which goes through elements of a sequence
(defclass seq-cursor-t (cursor-t)
  ((seq
    :type 'sequence
    :initarg :seq
    :reader seq
    :documentation
    "The seuence backing this cursor")
   (seq-length
    :reader seq-length)
   (seq-value
    :accessor seq-value)
   (index
    :initarg :index
    :accessor index
    :documentation
    "The current elemetn's index")))


(defmethod initialize-instance :after ( (seq seq-cursor-t) &key )
  (when (not (slot-boundp seq 'index))
    (setf (slot-value seq 'index) 0))
  (setf (slot-value seq 'seq-length)
	(length (seq seq)))
  (setf (seq-value seq)
	(elt (seq seq) (index seq))))


(defmethod print-object ( (seq seq-cursor-t) stream )
  (format stream "#<seq ~A@~A #~A ~A>"
	  (value seq)
	  (index seq)
	  (length (seq seq))
	  (if (done-p seq) "*" "")))
	      

;;=========================================================================

(defmethod clone ( (seq seq-cursor-t) )
  (make-instance 'seq-cursor-t
		 :seq (seq seq)
		 :index (index seq)))

;;=========================================================================

(defmethod done-p ( (seq seq-cursor-t) )
  (>= (index seq)
      (seq-length seq)))

;;=========================================================================

(defmethod value ( (seq seq-cursor-t) )
  (when (done-p seq)
    (error 'cursor-finished-condition
	   :cursor seq))
  (seq-value seq))

;;=========================================================================

(defmethod reset ( (seq seq-cursor-t) )
  (setf (index seq) 0)
  (setf (seq-value seq)
	(elt (seq seq) 0)))

;;=========================================================================

(defmethod next ( (seq seq-cursor-t) )
  (when (done-p seq)
    (error 'cursor-finished-condition
	   :cursor seq))
  (let ((prev-val (value seq)))
    (incf (index seq))
    (when (< (index seq)
	     (seq-length seq))
      (setf (seq-value seq)
	    (elt (seq seq)
		 (index seq))))
    (values
     prev-val
     (done-p seq))))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; A cursor which concatenates a set of cursor in order
(defclass cat-cursor-t (composing-cursor-t)
  ((index
    :initarg :index
    :accessor index)))


(defmethod initialize-instance :after ( (cat cat-cursor-t) &key )
  (when 
      (not (slot-boundp cat 'index))
    (setf (index cat) 0)))

;;=========================================================================

(defmethod clone ( (cat cat-cursor-t) )
  (make-instance
   'cat-cursor-t
   :cursors
   (mapcar #'clone (original-cursors cat))
   :index 
   (index cat)))
  

;;=========================================================================


(defmethod print-object ( (cat cat-cursor-t) stream )
  (format stream "#<cat ~{~A~^, ~} ~A>"
	  (original-cursors cat)
	  (if (done-p cat) "*" "")))

;;=========================================================================

(defmethod done-p ( (cat cat-cursor-t) )
  (every #'done-p (original-cursors cat)))

;;=========================================================================

(defmethod value ( (cat cat-cursor-t) )
  (when (done-p cat)
    (error 'cursor-finished-condition
	   :cursor cat))
  (value
   (elt (original-cursors cat)
	(index cat))))

;;=========================================================================

(defmethod reset ( (cat cat-cursor-t) )
  (mapcar #'reset (original-cursors cat))
  (setf (index cat) 0))

;;=========================================================================

(defmethod next ( (cat cat-cursor-t) )
  (when (done-p cat)
    (error 'cursor-finished-condition
	   :cursor cat))

  (let ((prev-value (value cat))
	(c (elt (original-cursors cat)
		(index cat))))
    (next c)
    (when (done-p c)
      (incf (index cat)))
    (values
     prev-value
     (done-p cat))))
	
	     
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; Create a repeating cursor which just call next on the given
;;;; cursor until it is done then resets it and continues
;;;; This cursor is *never* done by default
(defclass repeat-cursor-t (composing-cursor-t)
  ((max-count
    :initarg :max-count
    :accessor max-count)
   (current-count
    :initarg :count
    :accessor current-count)))

(defmethod initialize-instance :after ( (rep repeat-cursor-t) &key)
  (when (not (slot-boundp rep 'max-count))
    (setf (slot-value rep 'max-count) nil))
  (when (not (slot-boundp rep 'current-count))
    (setf (current-count rep) 0 )))

(defmethod print-object ( (rep repeat-cursor-t) stream )
  (format stream "#<repeat ~A ~A ~A>"
	  (first (original-cursors rep))
	  (if (max-count rep)
	      (format nil "~A/~A"
		      (current-count rep)
		      (max-count rep))
	      "FOREVER")
	  (if (done-p rep) "*" "")))
	      
	   

;;=========================================================================

(defmethod clone ( (rep repeat-cursor-t) )
  (make-instance 'repeat-cursor-t
		 :cursors (mapcar #'clone (original-cursors rep))
		 :max-count (max-count rep)
		 :count (current-count rep)))
   

;;=========================================================================

(defmethod done-p ( (rep repeat-cursor-t) )
  (if (max-count rep)
      (>= (current-count rep)
	 (max-count rep))
      nil))

;;=========================================================================

(defmethod reset ( (rep repeat-cursor-t) )
  (mapcar #'reset (original-cursors rep))
  (setf (current-count rep) 0))

;;=========================================================================

(defmethod value ( (rep repeat-cursor-t) )
  (value (first (original-cursors rep))))

;;=========================================================================

(defmethod next ( (rep repeat-cursor-t) )
  (let ((old-value (value rep)))
    (next (first (original-cursors rep)))
    (incf (current-count rep))
    (when (done-p (first (original-cursors rep)))
      (reset (first (original-cursors rep))))
    (values
     old-value
     (done-p rep))))

;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

;;;;
;;;; a filtering cursor which applies a function to a set of cursor 
;;;; an olny returns those elements for which it returns true
;;;;
;;;; This is a subclass of transform-cursor-t, so it has a
;;;; transform function which is applied *before* the filter
;;;; function, then the filter function is called with thre
;;;; resulting transformed cursor value
(defclass filter-cursor-t (transform-cursor-t)
  ((filter-f
    :initarg :filter-f
    :reader filter-f)))

(defmethod print-object ( (filter filter-cursor-t) stream )
  (format stream "#<filter ~A Trans-F=~A Filter=~A ~A>"
	  (original-cursors filter)
	  (transform-f filter)
	  (filter-f filter)
	  (if (done-p filter) "*" "")))

;;=========================================================================

(defmethod clone ( (filter filter-cursor-t) )
  (let ((copy (call-next-method)))
    (setf (slot-value copy 'filter-f)
	  (filter-f filter))
    copy))   

;;=========================================================================

(defmethod done-p ( (filter filter-cursor-t) )
  (if (call-next-method)
      (return-from done-p t))
  (do
   ()
   ((or
     (call-next-method filter)
     (funcall (filter-f filter) (%transform/raw-value filter)))
    (call-next-method))
    (mapcar #'next
	    (original-cursors filter))))

    


;;=========================================================================

(defmethod value ( (filter filter-cursor-t) )
  (do
   ()
   ((or
     (done-p filter)
     (funcall (filter-f filter) (call-next-method)))
    (call-next-method))
    (next filter)))

;;=========================================================================


(defmethod next ( (filter filter-cursor-t) )
  (when (done-p filter)
    (error 'cursor-finished-condition
	   :cursor filter))
  
  ;; ok, call next on all of the inner cursors until
  ;; one of two thigs happens:
  ;;   filter-f of values returns true
  ;;   one of the cursors is done
  (let ((old-value (value filter)))     ; safe to call (value ) since (done-p)
					; was called first which means we will
					; no go into infinite loop here
    (call-next-method)
    (do
     ()
     ((or
       (done-p filter)
       (funcall (filter-f filter) (%transform/raw-value filter)))
      nil)
      (call-next-method))
    (values
     old-value
     (done-p filter))))
    

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
;;;; Create a cursor from a sequence that iterates over it
(defun cursor/seq ( seq )
  (make-instance 'seq-cursor-t
		 :seq seq))

;;=========================================================================

;;;;
;;;; Create a cursor which just concatenates the given cursors in order
(defun cursor/cat (&rest cursors)
  (make-instance
   'cat-cursor-t
   :cursors cursors))

;;=========================================================================

;;;;
;;;; Create a cursor which just repeats the elemetns from the given
;;;; cursor. Optionally, a maximum count of element can be given
(defun cursor/repeat ( cursor &key (max-count nil) )
  (make-instance 'repeat-cursor-t
		 :cursors (list cursor)
		 :max-count max-count))

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
;;;; Make a filtering cursor.
;;;; Notes: 
;;;;     the transform function is applied *before* the filter
;;;;     function.
(defun remove-key (list key)
  (let ((key-pos
	  (position key list)))
    (if key-pos
	(append
	 (subseq list
		 0 key-pos)
	 (subseq list
		 (+ key-pos 2)))
	list)))

(defun cursor/filter (&key
			filter-func 
			cursors 
			(trans-func #'list))
  (make-instance 'filter-cursor-t
		 :cursors cursors
		 :f trans-func
		 :filter-f filter-func
		 :termination :first))


;;=========================================================================


;;;;
;;;; Make a labeled cursor, which labels the results of a cursor
;;;; with a set of labels, returning and association list with
;;;; (label value) pairs.
;;;;
;;;; If the label is not a list of labels, we return a single
;;;; pairing of (label value) rather than a ssociation lisst
(defun cursor/label (label-list cursor)
  (let ((c
	 (if (listp label-list)
	     (cursor/transform
	      #'(lambda (val)
		  (mapcar #'(lambda (label)
			      (list label val))
			  label-list))
	      cursor)
	     (cursor/transform
	      #'(lambda (val)
		  (list label-list val))
	      cursor))))
    (cursor/add-property c prop/is-labeled t)
    c))
  

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

;;=========================================================================

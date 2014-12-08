(in-package #:j-plankton.cursor)

;;=========================================================================

;;;;
;;;; Packing a cursor tree means that we take a cursor object and
;;;; to to remove redundancies which may have happened because of
;;;; the simple parser we have for cursor expressions.
;;;; This is a method based upoin the cursor type.
;;;; by default we do nothing and just reutnr the original cursor
;;;;
;;;; rturns (values <packed-tree> <changed-p>)
;;;; where changed-p is true iff an actual change (or pakcing) ocurred
(defmethod %pack-cursor-tree ( cursor &key &allow-other-keys )
  cursor)

;;=========================================================================

;;;;
;;;; returns the largest number of adjacent elements in sequence for
;;;; which the given predicate is true (a streak).
;;;; Also returns the start and end positions of the streak in the 
;;;; next two values 
;;;;  returns: (values <n> <start> <end> )
(defun largest-streak (pred seq)
  (let ((streak)
	(max-streak)
	(in-streak nil))
    (setf (getf streak :n) 0)
    (setf (getf streak :start) nil)
    (setf (getf streak :end) nil)
    (setf (getf max-streak :n) 0)
    (setf (getf max-streak :start) nil)
    (setf (getf max-streak :end) nil)
    (loop for x in seq
	  for i upto (length seq)
	  do
	     (let ((found (funcall pred x)))
	       (if (not found)
		   (when in-streak
		     (progn
		       (setf in-streak nil)
		       (when (> (getf streak :n)
				(getf max-streak :n))
			 (setf max-streak
			       (copy-seq streak)))
		       (setf (getf streak :n) 0)
		       (setf (getf streak :start) nil)
		       (setf (getf streak :end) nil)))
		   (if in-streak
		       (progn
			 (incf (getf streak :n))
			 (incf (getf streak :end)))
		       (progn
			 (setf in-streak t)
			 (setf (getf streak :start) i)
			 (setf (getf streak :end) i)
			 (setf (getf streak :n) 1))))))
    (when (> (getf streak :n)
	     (getf max-streak :n))
      (setf max-streak streak))
    (values
     (getf max-streak :n)
     (getf max-streak :start)
     (getf max-streak :end))))

;;=========================================================================

;;;;
;;;; Compsing cursors should pack their inner cursors
;;;; This should be chained in a call to call-next-method
(defmethod %pack-cursor-tree ( (comp composing-cursor-t) &key )
  (format t "composing pack called: ~A~%" comp)
  (let ((any-changes nil))
    (replace 
     (original-cursors comp)
     (mapcar #'(lambda (x)
		 (multiple-value-bind (packed changed-p)
		     (%pack-cursor-tree x)
		   (when changed-p
		     (setf any-changes t))
		   packed))
	     (original-cursors comp)))
    (if any-changes
	(%pack-cursor-tree comp)
	(values
	 comp
	 nil))))

;;=========================================================================


;;;;
;;;; Pack concatenation cursors
(defmethod %pack-cursor-tree ( (cat cat-cursor-t) &key (phase 0) )

  ;; only do so much packing
  (when (> phase 100)
    (format t "limiting phase reached: ~A~%" phase)
    (return-from %pack-cursor-tree (values cat nil)))

  ;; check if we only have a single inner cursor, in which case
  ;; we are redundant
  (when (= (length (original-cursors cat)) 1)
    (return-from %pack-cursor-tree
      (%pack-cursor-tree (first (original-cursors cat)))))

  ;; ok, now condense sequence cursors
  (multiple-value-bind
	(n start end)
      (largest-streak #'(lambda (x)
			  (typep x 'seq-cursor-t))
		      (original-cursors cat))
    (format t "largest seq-cursor streak ~A (~A ~A)~%"
	    n start end)
    (if (> n 1)
	(let* ((new-seq
		 (alexandria:mappend 
		  #'seq
		  (subseq (original-cursors cat)
			  start
			  (1+ end))))
	       (new-cursors (copy-seq (original-cursors cat)))
	       (sub-cursors
		 (append
		  (subseq new-cursors
			  0
			  start)
		  (list (cursor/seq new-seq))
		  (subseq new-cursors
			  (1+ end)))))
	  (format t "new seq: ~A~%" new-seq)
	  (format t "copied cursors: ~A~%" new-cursors)
	  (format t "substituted cursor: ~A~%" sub-cursors)
	  (values
	   (%pack-cursor-tree
	    (apply #'cursor/cat sub-cursors)
	    :phase (1+ phase))
	   t))
	(if (next-method-p)
	    (call-next-method cat)
	    (values cat nil)))))



;;=========================================================================

;;;;
;;;; Autodetect properties for a cursor-tree
;;;; Generally this is done for leaf nodes, and tehn we "cascade"
;;;; the properties of the leaf nodes upwars usign cascade-properties
;;;;
;;;; returns (values cursor detected-p)
(defgeneric auto-detect-properties (cursor))

;;=========================================================================

;;;;
;;;; Defautl autodetect does nothing
(defmethod auto-detect-properties (cursor)
  (values cursor nil))

;;=========================================================================

;;;;
;;;; auto-detect range-cursor-t properties
(defmethod auto-detect-properties ( (range range-cursor-t) )
  (let ((detected nil))

    ;; dense ranges
    (when (eql 1 (step-f range))
      (let ((new-p
	     (cursor/add-property range prop/is-dense t)))
	(setf detected
	      (or detected new-p))))

    ;; ranges are by default uniqeu
    (let ((new-p
	   (cursor/add-property range prop/is-unique-valued t)))
      (setf detected
	    (or detected new-p)))

    ;; return
    (values range detected)))


;;=========================================================================

;;;;
;;;; auto-detect properties for seq-cursor-t
(defmethod auto-detect-properties ( (seq seq-cursor-t) )
  (let ((detected nil))

    ;; check uniqeueness of sequence
    (when (= (length (seq seq))
	     (length (remove-duplicates (seq seq))))
      (setf detected
	    (or detected
		(cursor/add-property seq prop/is-unique-valued t))))

    (values seq detected)))

;;=========================================================================

;;;;
;;;; autodetect for composing cusrots detects inner
(defmethod auto-detect-properties ( (cursor composing-cursor-t) )
  (values
   cursor
   (some #'identity
	 (mapcar #'(lambda (c)
		     (multiple-value-bind (c new-p)
			 (auto-detect-properties c)
		       new-p))
		 (original-cursors cursor)))))

;;=========================================================================

;;=========================================================================





;;;;
;;;; We can also cascade properties for a cursro-tree, which
;;;; computes and sets the properties of the nodes (cursors)
;;;; of the tree given those already set so far (usually by leaft nodes)
(defgeneric cascade-properties (cursor))

;;=========================================================================

;;;;
;;;; By default, no properties cascade so nothing to do
(defmethod cascade-properties (cursor)
  (values cursor nil))

;;=========================================================================

;;;;
;;;; The interface to calculate the properties in a property tree
;;;; gnereally this will jsut be a call to auto-detect-properties
;;;; then cascade-properties
(defgeneric materialzie-properties (cursor))

(defmethod materialzie-properties (cursor)
  (cascade-properties
   (auto-detect-properties
    cursor)))

;;=========================================================================

;;;;
;;;; simplified cascading of properties be defninng
;;;; "rules" of when a particular property cascades given
;;;; a cursor  and a property
(defmethod cascade-property-rule (cursor property)
  nil)
	     

;;=========================================================================

;;;;
;;;; compsite cursor by default use the cascade property rules defined
(defmethod cascade-properties ( (cursor composing-cursor-t) )

  ;; fetch all properties from cascaded inner cursor
  (let ((props
	 (remove-duplicates
	  (alexandria:flatten
	   (mapcar #'(lambda (c)
		       (mapcar #'first
			       (alexandria:plist-alist
				(properties
				 (cascade-properties c)))))
		   (original-cursors cursor))))))

    ;; ok, now go trough each property and it's rule to see if
    ;; we cascade
    (let ((cascaded nil))
      (dolist (prop props)
	(when (cascade-property-rule cursor prop)
	  (setf cascaded
		(or cascaded
		    (cursor/add-property cursor prop t)))))
		    
      ;; return
      (values cursor cascaded))))
	    

;;=========================================================================

;;;;
;;;; repeat-cursor-t never has prop/is-unique-valued
(defmethod cascade-property-rule ( (rep repeat-cursor-t)
				  (p (eql :prop/is-unique-valued)))
  nil)

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

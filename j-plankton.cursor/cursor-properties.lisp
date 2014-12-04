
(in-package #:j-plankton.cursor)


;;=========================================================================

;;;;
;;;; The base class for cursor properties
(defclass cursor-properties-t ()
  ((properties 
    :initarg :properties
    :reader properties)))

;;=========================================================================

;;;;
;;;; A global mapping between cursor-properties-list and
;;;; a cursor-properties-t subclass
(defvar *properties-list->class* (make-hash-table :test #'equal))


;;=========================================================================


;;=========================================================================


;;;;
;;;; Create and cache a new class for given property
(defun %create-property-class (prop)
  (let* ((prop-name (string prop))
	 (prop-symbol (intern prop-name *package*))
	 (class-name (jpu:intern+ *package* 
				  :cursor-prop-t/
				  prop-name))
	 (class-def
	   `(defclass ,class-name ()
	      ((,prop-symbol
		:accessor ,prop-symbol)))))
    (format t "making individual prop-class~%")
    (format t "name=~S, prop-name=~S, def=~S~%"
	    class-name
	    prop-name
	    class-def)
    (eval class-def)))

;;=========================================================================

;;;;
;;;; create a new mixture class with given properties
(defun %create-property-mixture-class (sorted-props)
  (let* ((class-name
	   (apply #'jpu:intern+ *package*
		  :cursor-prop-t/
		  (alexandria:mappend
		   #'(lambda (x)
		       (list (string x) "/"))
		   sorted-props)))
	 (individual-classes
	   (mapcar #'(lambda (x)
		       (cursor-properties-class (list x)))
		   sorted-props))
	 (class-def
	   `(defclass ,class-name ,individual-classes
	      ())))
    (format t "making composite prop-class~%")
    (format t "name=~S, subpers=~S, def=~S~%"
	    class-name
	    individual-classes
	    class-def)
    (eval class-def)))

;;=========================================================================


;;;;
;;;; Returns the clas object for a given proerties-list.
;;;; This will create a new class if none exists and store this
;;;; in the global cache
(defun cursor-properties-class (properties-list)
  (let ((sorted-props (sort properties-list #'string<)))
    
    ;; ok, first check if there is already a mapping in our cache
    (alexandria:when-let 
	(class (gethash sorted-props *properties-list->class* nil))
      (return-from cursor-properties-class (values class nil)))
    
    ;; now we want to create a new class with all the property classes
    ;; as super-classes (in the sorted order), 
    ;; cache this new class,
    ;; and return it.
    ;; individual property classes are special, they actually
    ;; contain slots for the property value
    (let ((class
	    (if (= (length sorted-props) 1)
		(%create-property-class (first sorted-props))
		(%create-property-mixture-class sorted-props))))
      		
      (setf (gethash sorted-props *properties-list->class*)
	    class)
      (values 
       class
       t))))


;;=========================================================================

;;;;
;;;; Returns a properties object for hte given properties-plist
;;;; ( which is a list of properties and their values).
;;;; The object will be of the right properties class given the properties
;;;; in the list
(defun cursor-properties-class-object (props-plist)
  (let* ((class
	   (cursor-properties-class
	    (mapcar #'car
		    (alexandria:plist-alist 
		     props-plist))))
	 (obj
	   (make-instance class)))
    ;; set the properties
    (alexandria:doplist (prop value props-plist)
			(setf (slot-value obj (intern (string prop))) value))
    ;; return new object
    obj))
    

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
;;=========================================================================
;;=========================================================================
;;=========================================================================
;;=========================================================================

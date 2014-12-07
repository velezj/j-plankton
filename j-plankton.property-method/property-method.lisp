

(in-package #:j-plankton.property-method)


;;=========================================================================

;;;;
;;;; Rertieve the propertis for an object
(defgeneric properties (obj) )

;;;;
;;;; standard symbol properties query
(defmethod properties ( (sym symbol) )
  (symbol-plist sym))

;;=========================================================================

;;;;
;;;; Retrieves the properties-class for an object
(defgeneric properties-class (obj))
(defmethod properties-class (obj)
  (%properties-class
   (mapcar #'first (alexandria:plist-alist (properties obj)))))

;;=========================================================================

;;;;
;;;; Retrieves the properties class object for an object
(defgeneric properties-class-object (obj))
(defmethod properties-class-object (obj)
  (%properties-class-object
   (properties obj)))

;;=========================================================================
;;=========================================================================


;;;;
;;;; The base class for properties
(defclass properties-t ()
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
				  :property-type/
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
		  :property-type/
		  (alexandria:mappend
		   #'(lambda (x)
		       (list (string x) "+"))
		   sorted-props)))
	 (super-classes
	   (alexandria:flatten
	    (let ((classes nil))
	      (alexandria:map-combinations 
	       #'(lambda (comb)
		   (push (%properties-class (sort (alexandria:ensure-list comb) #'string<)) classes))
	       sorted-props
	       :length (1- (length sorted-props)))
	      classes)))
	 (class-def
	   `(defclass ,class-name ,super-classes
	      ())))
    (format t "making composite prop-class~%")
    (format t "name=~S, subpers=~S, def=~S~%"
	    class-name
	    super-classes
	    class-def)
    (eval class-def)))

;;=========================================================================


;;;;
;;;; Returns the clas object for a given proerties-list.
;;;; This will create a new class if none exists and store this
;;;; in the global cache
(defun %properties-class (properties-list)
  (let ((sorted-props (sort properties-list #'string<)))
    
    ;; ok, first check if there is already a mapping in our cache
    (alexandria:when-let 
	(class (gethash sorted-props *properties-list->class* nil))
      (return-from %properties-class (values class nil)))
    
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
(defun %properties-class-object (props-plist)
  (let* ((class
	   (%properties-class
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

;;;;
;;;; Craete a new method based on the properties of
;;;; an object given to it
(defmacro define-property-method (name
				  var-props
				  lambda-list
				  &body body)
  (let* ((props-classes
	  (mapcar #'(lambda (var-props)
		      (destructuring-bind (var props) var-props
			(declare (ignore var))
			(if (null props)
			    't
			    (%properties-class props))))
		  var-props))
	 (props-vars
	  (mapcar #'(lambda (var-prop)
		      (jpu:intern+ *package* (first var-prop) :/properties))
		  var-props))
	 (modified-lambda-list
	  (append
	   (mapcar #'list props-vars props-classes)
	   lambda-list))
	 (impl-method-symbol
	  (jpu:intern+ *package* name :/property-impl))
	 (impl-fowarding-arguments
	  (handler-bind ((warning #'muffle-warning))
	    (jp.doc-ll:foward-arguments-from-documented-lambda-list
	     lambda-list :error-on-missing-doc nil)))
	 (interface-method-def
	  `(defmethod ,name ,lambda-list
	     (,impl-method-symbol
	      ,@(mapcar #'(lambda (var)
			    `(properties-class-object ,var))
			props-vars)
	      ,@impl-fowarding-arguments)))
	 (impl-method-def
	  `(defmethod ,impl-method-symbol ,modified-lambda-list
	     ,@body)))
    `(progn
       ,interface-method-def
       ,impl-method-def)))


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

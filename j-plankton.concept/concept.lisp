(in-package #:j-plankton.concept)


;;;;
;;;; What, exactly is a "concept" ? Read on....
;;;;
;;;; Ok, so say that there is a particular computation that is *known*
;;;; (whether by a standard API, a set of test cases that define it,
;;;;  or some other definition (perhaps a reference implemntation).
;;;; Now, we would like to have several differnet implementations of the
;;;; same concept.  An we would like to choose which implementation to
;;;; use easily and at run-time (perhaps even using some form of 
;;;;  machine learning)
;;;;
;;;; Which brings us to the "concept".
;;;; A concept for use will be a generic method with a fixed input,
;;;; so all implementations of a concept *must* take the same arguments.
;;;; However, the concept method delegates to an implementation method at 
;;;; runtime which is based on the value of a special variable for the concept.
;;;;
;;;; These "concept" variables are created in their own package and are 
;;;; made using the (define-concept ...) construction.
;;;; This ensure that there is only one concept per concept package, and 
;;;; that the binding of this concept symbol to an implementation tag
;;;; determines the runtime method actually called.
;;;;
;;;; Implementations of concepts are done using the (implement-concept ....)
;;;; Macro, which takes the concept name, implementation tag, and documentation
;;;; along with the lambda form and the body of the lambda.
;;;; A new implementation tag in implicitly created in the concepts package
;;;; if not already there. The documentation for the concept and the tag in 
;;;; the concept package is also updated to reflect the implementation.
;;;;
;;;; Most importantly, documentation is kept and updated, so we can do
;;;; the following:
;;;;    (define-concept BAR (x y) "Documentation ofr BAR as generioc concept")
;;;;    .... some otehr part of the code
;;;;    (implement-concept (bar fast-impl "Doc of FAST version of bar concept)
;;;;       ( x y )
;;;;      ... something super fast here)
;;;;    ..... in hte repl
;;;;    (describe (find-concept-symbol 'bar))
;;;;    and we will see that FAST-IMPL is an implementation, along
;;;;    with it's docum entation as well as the generic concept doc.


;;;;
;;;; This is the currently bound concept's package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *concepts-package* (make-package (symbol-name :default-concepts))))


;;;;
;;;; clears the concepts package
(defun clear-concepts-package (&optional (package *concepts-package*))
  (do-symbols (s package)
    (unintern s package)))




;;;;
;;;; Rebind the concepts package to use
(defmacro with-concepts-package (package-designator &rest body)
  `(let ((*concepts-package* (find-package ,package-designator)))
     ,@body))



	    
	
	

;;;;
;;;; Creates a symbol based on a concept and the typed arguments given 
;;;; to it.
;;;; If the symbol already exists, return it instead of creating it.
;;;; If created, sets the value ot +UNSET-IMPLEMENTATION-TAG+
;;;; Returns (value symbol second-intern-value)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %create-concept-symbol (concept-name arg-types &optional (package *concepts-package*) )
    (multiple-value-bind (s present-already) 
	(intern (apply #'concatenate 
		       'string 
		       (symbol-name concept-name)
		       (if (every #'(lambda (type) (eq type 't)) arg-types)
			   nil
			   (mapcar #'(lambda (type)
				       (concatenate 'string
						    "["
						    (format nil "~S" type)
						    "]"))
				   arg-types)))
		package)
      (when (not present-already)
	(setf (symbol-value s) +UNSET-IMPLEMENTATION-TAG+))
    s)))
		       
		       

	
;;;; 
;;;; A special symbol meaning that a concept has an unset implementation tag
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (boundp '+UNSET-IMPLEMENTATION-TAG+))
    (defconstant +UNSET-IMPLEMENTATION-TAG+ (gensym "unset-impl-")
      "Special symbol denoting an unset implementation tag.
   This is the default value for all defined concepts initially
   unless changed by a call to implement-concept")))
  
;;;;
;;;; Define a particular concept
;;;; This just makes it's name available in the current concepts package
;;;; (or the given concepts package, defautls ot current)
(defmacro define-concept (concept-name concept-args doc &key (package *concepts-package*) (error-when-duplicate-concept t))
  (let ((concept-method (intern (concatenate 'string (symbol-name concept-name) "")))
	(concept-method-impl (intern (concatenate 'string (symbol-name concept-name) "-IMPL")))
	(concept-implementation-tag (intern (concatenate 'string (symbol-name concept-name) "-IMPLEMENTATON-TAG")))
	(concept-implementation-tag-setter (intern (concatenate 'string (symbol-name concept-name) "-IMPLEMENTATON-TAG-SETTER"))))
    (with-concepts-package package
      (if (and (find-symbol (symbol-name concept-name) *concepts-package*)
	       error-when-duplicate-concept)
	  (error "The symbol ~A is already bound in the concepts package ~A" concept-name *concepts-package*)
	  (progn
	    ;; intern the symbol and declaim it special
	    (let ((concept-symbol (intern (symbol-name concept-name) *concepts-package*)))
	      (eval-when (:compile-toplevel)
		(proclaim `(special ,concept-symbol)))
	      (eval-when (:load-toplevel :execute)
		(proclaim `(special ,concept-symbol)))
	      (setf (documentation concept-symbol 'VARIABLE) doc)
	      (setf (symbol-value concept-symbol) +UNSET-IMPLEMENTATION-TAG+)
	      ;; create the concept list in package if needed
	      (if (not (find-symbol (symbol-name '+concept-list+) *concepts-package*))
		  (let ((concept-list (intern (symbol-name '+concept-list+) *concepts-package*)))
		    (setf (symbol-value concept-list) nil)))
	      ;; add new symbol (concept) to the concept list
	      (pushnew concept-symbol (symbol-value (find-symbol (symbol-name '+concept-list+) *concepts-package*)))
	      ;; create ne generic method for this concept
	      `(progn
		 (defgeneric ,concept-method ( ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) ) 
		   (:documentation ,doc))
		 (defgeneric ,concept-method-impl ( +concept-impl+ ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) )
		   (:documentation ,doc))
		 (defgeneric ,concept-implementation-tag ( ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) )
		   (:documentation "retrieves the current implementation tag for a concept (based on types used to callthe concept!)"))
		 (defgeneric ,concept-implementation-tag-setter ( new-tag ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) )
		   (:documentation "sets the current implementation tag for a concept (based on types used to call the concept!)"))
		 (defmethod ,concept-implementation-tag ( ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) )
		   (let ((concept-symbol
			   (find-symbol 
			    (symbol-name 
			     (%create-concept-symbol ',concept-name
						     (mapcar #'type-of (list ,@(jp.doc-ll:documented-lambda-list-argument-symbols concept-args)))))
			     *concepts-package*)))
		     (if (and concept-symbol (boundp concept-symbol))
			 (values (symbol-value concept-symbol)
				 concept-symbol)
			 (if (next-method-p)
			     (call-next-method)
			     (values (format nil "** ~A" ,concept-symbol)
				     ,concept-symbol)))))
		 (defmethod ,concept-implementation-tag-setter  ( new-tag ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) )
		   (let ((concept-symbol
			   (find-symbol 
			    (symbol-name 
			     (%create-concept-symbol ',concept-name
						     (mapcar #'type-of (list ,@(jp.doc-ll:documented-lambda-list-argument-symbols concept-args)))))
			    *concepts-package*)))
		     (setf (symbol-value concept-symbol) new-tag)))
		 (defmethod ,concept-method ( ,@(jp.doc-ll:documented-lambda-list->lambda-list concept-args) )
		   (,concept-method-impl (,concept-implementation-tag ,@(jp.doc-ll:foward-arguments-from-documented-lambda-list concept-args)) ,@(jp.doc-ll:foward-arguments-from-documented-lambda-list concept-args))))))))))
  
  

;;;;
;;;; Some test defined concepts!       
(define-concept foobar ( (x "") 
			 (y "")) 
  "The FOOBAR concept")
(define-concept foobar ( (x "Some X argument")
			 (y "Some Y argument, >0")) 
  "The NEW foobar concept" 
  :error-when-duplicate-concept nil)


;;;;
;;;; Returns the concept symbol for a concept, or NIL of not found
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-concept-symbol (concept-name &optional (package *concepts-package*))
    (etypecase concept-name
      (symbol (find-symbol (symbol-name concept-name) package))
      (string (find-symbol concept-name package)))))

;;;;
;;;; Returns the implementation tag symbol.
;;;; If not there, creates a new symbol in the current concepts package
;;;; and attached documentation to concept and tag
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-or-make-implementation-tag (implementation-tag concept-name doc &optional (package *concepts-package*))
    (let ((s (find-symbol implementation-tag *concepts-package*)))
      (if (not (null s))
	  s
	  ;; ok, we want to intern the tag symbol and attach docs
	  (let ((s (intern implementation-tag package))
		(concept (find-concept-symbol concept-name package)))
	    (if (null concept)
		(error "Cannot make implementation tag ~A inside concepts package ~A: concept ~A for tag cannot be found!" implementation-tag package concept-name)
		(progn
		  (setf (symbol-value s) implementation-tag)
					;(format t "set value of ~A to ~A~%" s (symbol-value s))
		  (setf (documentation s 'VARIABLE) doc)
					;(format t "set doc of ~A (VARIABLE) to ~A~%" s (documentation s 'VARIABLE))
		  (setf (documentation concept 'VARIABLE)
			(concatenate 'string 
				     (documentation concept 'VARIABLE)
				     (list #\Newline)
				     (format nil "Implementation ~A: " implementation-tag)
				     doc))
					;(format t "set doc of ~A (VARIABLE) to ~A~%" concept (documentation concept 'VARIABLE))
		  (let ((tags (intern (symbol-name '+implementations-tags+) *concepts-package*)))
		    (if (boundp tags)
			(setf (symbol-value tags)
			      (pushnew s (symbol-value tags)))
			(setf (symbol-value tags)
			      (list s))))
		  
		  s)))))))
	    
  

;;;;
;;;; Thin wrapper around defmthod
(defmacro define-concept-method-impl (name lambda-args &rest body)
  `(defmethod ,name ,lambda-args ,@body))


;;;;
;;;; Defines a new implementation for a concept.
;;;; This includes the actual implementation function,
;;;; but also the concept tag (as per define-concept) *and*
;;;; the implementation tag.
;;;; The implementation tag will be added to the current *concepts-package*
;;;; if not already there (with + around it)
(defmacro implement-concept ( (concept-name implementation-tag doc &key (default-implementation nil)) documented-lambda-list &body body )
  (let ((method-name (intern (concatenate 'string (symbol-name concept-name) "-IMPL")))
	(normal-lambda-args (jp.doc-ll:documented-lambda-list->lambda-list documented-lambda-list :error-on-missing-doc nil))
	(concept-symbol (%create-concept-symbol concept-name 
						(jp.doc-ll:documented-lambda-list-argument-types documented-lambda-list :error-on-missing-doc nil)))
	(concept-implementation-tag (intern (concatenate 'string (symbol-name concept-name) "-IMPLEMENTATON-TAG")))
	(concept-implementation-tag-setter (intern (concatenate 'string (symbol-name concept-name) "-IMPLEMENTATON-TAG-SETTER"))))
    (let* ((implementation-tag-+ (concatenate 'string "+" (symbol-name implementation-tag) "+"))
	   (implementation-tag-symbol 
	    (find-or-make-implementation-tag implementation-tag-+ (symbol-name concept-name) doc))
	   (concept-check (list (gensym "concept-var-") `(eql ',implementation-tag-symbol))))
      (when (and 
	     (eq (symbol-value concept-symbol)
		 +UNSET-IMPLEMENTATION-TAG+)
	     default-implementation)
	(setf (symbol-value concept-symbol)
	      implementation-tag-symbol))
      `(progn
	 (defmethod ,concept-implementation-tag ( ,@(jp.doc-ll:documented-lambda-list->lambda-list documented-lambda-list) )
	   (let ((concept-symbol
		  (find-symbol 
		   (symbol-name 
		    (%create-concept-symbol
		     ',concept-name
		     ',(jp.doc-ll:documented-lambda-list-argument-types documented-lambda-list)))
		   *concepts-package*)))
	     (if (and concept-symbol
		      (boundp concept-symbol)
		      (not (eq (symbol-value concept-symbol) +UNSET-IMPLEMENTATION-TAG+)))
		 (values (symbol-value concept-symbol)
			 concept-symbol
			 (eq (symbol-value concept-symbol) +UNSET-IMPLEMENTATION-TAG+))
		 (if (next-method-p)
		     (call-next-method)
		     (values (format nil "** ~A" ,concept-symbol)
			     ,concept-symbol)))))
	 (defmethod ,concept-implementation-tag-setter  ( new-tag ,@(jp.doc-ll:documented-lambda-list->lambda-list documented-lambda-list) )
	   (let ((concept-symbol
		   (find-symbol 
		    (symbol-name 
		     (%create-concept-symbol ',concept-name
					     (mapcar #'type-of (list ,@(jp.doc-ll:documented-lambda-list-argument-symbols documented-lambda-list)))))
		    *concepts-package*)))
	     (setf (symbol-value concept-symbol) new-tag)))
	 (define-concept-method-impl ,method-name (,concept-check ,@normal-lambda-args) ,@body)))))


;;;;
;;;; Some test concept implementations
(implement-concept 
    (foobar impl-integer "FAST foobar implementation")
  ( ((x integer) "") 
    ((y integer) "") ) 
  (+ x y))

(implement-concept 
    (foobar impl-generic "GENERIC foobar implementation" :default-implementation t)
    ( (x "")
      (y "") )
  (- x y))



;;;;
;;;; Returns the currently set implementation tag for a concept when called
;;;; with the givne arguments
(defun concept-implementation-tag (concept-symbol 
				   &rest concept-args)
  (let* ((method-name
	  (concatenate 'string
		       (symbol-name concept-symbol)
		       "-IMPLEMENTATON-TAG"))
	 (method-symbol (find-symbol method-name))
	 (method (if (and method-symbol (fboundp method-symbol))
		     (symbol-function method-symbol)
		     nil)))
    (unless method
      (error "Cannot find concept-specific implementation tag method for concept  ~A.  Symbol named ~S either not found or not fboundp!"
	     concept-symbol
	     method-name))
    (apply method concept-args)))


;;;;
;;;; Set the implementation tag for a concept when called with given
;;;; arguments.
(defun %set-concept-implementation-tag (concept-symbol 
					&rest concept-args-and-new-tag)
  (let* ((method-name
	   (concatenate 'string
			(symbol-name concept-symbol)
			"-IMPLEMENTATON-TAG-SETTER"))
	 (method-symbol (find-symbol method-name))
	 (method (if (and method-symbol (fboundp method-symbol))
		     (symbol-function method-symbol)
		     nil)))
    (unless method
      (error "Cannot find concept-specific implementation tag setter method for concept  ~A.  Symbol named ~S either not found or not fboundp!"
	     concept-symbol
	     method-name))
    (apply method concept-args-and-new-tag)))


;;;;
;;;; a setf expander for:
;;;; (setf (concept-implementation-tag <concept> <args> ) <tag> )
(defsetf concept-implementation-tag %set-concept-implementation-tag)
    


;;;;
;;;; Removes a concept from the concepts package as well as
;;;; from the current package usign unintern
(defun purge-concept (concept-name)
  (do-symbols (s *concepts-package*)
    (let ((pos (search (concatenate 'string (symbol-name concept-name) "[")
		      (symbol-name s))))
      (if (or (and pos (= 0 pos))
	      (equal (symbol-name concept-name)
		     (symbol-name s)))
	  (unintern s *concepts-package*))))
  (let ((concept-list (find-symbol (symbol-name '+CONCEPT-LIST+) *concepts-package*)))
    (when concept-list
      (setf (symbol-value concept-list)
	    (remove-if #'(lambda (c)
			   (equal (symbol-name c)
				  (symbol-name concept-name)))
		       (symbol-value concept-list)))))
  (unintern (intern (symbol-name concept-name)))
  (unintern (intern (concatenate 'string
				 (symbol-name concept-name)
				 "-IMPL")))
  (unintern (intern (concatenate 'string
				 (symbol-name concept-name)
				 "-IMPLEMENTATON-TAG"))))
  

;;;;
;;;; purge all the known concepts (those stored in +CONCEPT-LIST+)
(defun purge-all-concepts ()
  (dolist (s (symbol-value (find-symbol (symbol-name '+CONCEPT-LIST+) *concepts-package*)))
    (purge-concept s)))


;;;;
;;;; purge all symbols in the concepts package
(defun purge-concepts-package ()
  (do-symbols (s *concepts-package*)
    (unintern s *concepts-package*)))


;;;;
;;;; Print out a list of current concepts known about
(defun print-concepts (&key (print-doc nil))
  (let ((tags (find-symbol (symbol-name '+concept-list+) *concepts-package*)))
    (if (not tags)
	nil
	(dolist (s (symbol-value tags))
	  (format t "~A::~A  set to ~A~%" (package-name *concepts-package*) s (symbol-value s))
	  (when print-doc
	    (format t "     ~A~%" (documentation s 'VARIABLE)))))))

  
;;;;
;;;; Print out a list of current implementation tags known about
(defun print-implementation-tags (&key (print-doc nil))
  (let ((tags (find-symbol (symbol-name '+implementations-tags+) *concepts-package*)))
    (if (not tags)
	nil
	(dolist (s (symbol-value tags))
	  (format t "~A::~A~%" (package-name *concepts-package*) s)
	  (when print-doc
	    (format t "     ~A~%" (documentation s 'VARIABLE)))))))

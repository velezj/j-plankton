(in-package #:j-plankton)


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
;;;; Checks that a given specification for a documented lambda parameter
;;;; is comforming to the syntax:
;;;; it is a list, with the last element a non-empty string
;;;; Returns a continuable error otherwise
;; (defun %check-documented-lambda-arg-conformant (arg)
  
	


;;;;
;;;; Ok, we want ot parse a "documented lambda list", which is
;;;; a modified lambda list where each parameter *must* include a
;;;; documentation string.  In essence, each parameter is itself 
;;;; a list with the firt element being hte normal parameter syntax
;;;; (including keyword/optional/aux syntax) and the second element
;;;; is a documentation string for that parameter
;;;;
;;;; Given such a documented lambda list, split it into
;;;; the known types of parameters, retunring
;;;; a plist with:
;;;; ( :positional ( ordinary-param-spec ... )
;;;;   :named ( keyword-param-specs ... aux-param-specs .... )
;;;;   :rest ( rest-param-specs ) )
;;;;
;;;; ordinary-param-spec = ( name doc-string )
;;;; keyword-param-specs = ( bind-name keyword given-p lambda-spec doc-string)
;;;; aux-param-specs = ( bind-name keyword given-p lambda-spec doc-string)
;;;; rest-param-specs = ( name doc-string ) | nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-documented-lambda-list (lambda-list)
    
    ;; find the locations of each of the lambda keywords
    ;; and grab the ordinary, special (key and aux) and rest args
    (metabang-bind:bind 
	(((:assoc (ordinary-args &ordinary)
		  (key-args &key)
		  (rest-args &rest)
		  (aux-args &aux)
		  (optional-args &optional))
	  (jpu:make-sassoc 
	   lambda-list 
	   :is-key 
	   #'(lambda (k) 
	       (and (symbolp k) 
		    (equal #\& (elt (symbol-name k) 0)))) 
	   :default-key '&ordinary)))
      (let ((special-args (append key-args aux-args)))
	
	;; make sure that each element is a list and that
	;; the last element is a string
	(mapcar #'(lambda (e)
		    (check-type e list)
		    (assert (> (length e) 1) (e) "You *must* supply a documentation string")
		    (check-type (car (last e)) string))
		(append ordinary-args optional-args rest-args special-args))
	
	(list :positional (append ordinary-args optional-args)
	      :named special-args
	      :rest rest-args)))))
    

;;;;
;;;; "normalizes" arguments from the result of parsing a documented lambda list
;;;; so that each arugment is in the cacnonical form
;;;; and in hte right order for forwarding arguments
;;;; ( :var-forward forward :doc doc :original original-form )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %normalize-parsed-documented-lambda-list (parsed-doc-ll)
    (append
     (mapcar #'(lambda (ordinary-arg)
		 (list :var-forward (list (first ordinary-arg))
		       :doc (second ordinary-arg)
		       :original ordinary-arg))
	     (getf parsed-doc-ll :positional))
     (mapcar #'(lambda (named-arg)
		 (let ((name
			(if (> (length named-arg) 2)
			    (elt named-arg 1)
			    (intern (symbol-name (elt named-arg 0)) 
				    (find-package "KEYWORD")))))
		   (list :var-forward (list name (elt named-arg 0))
			 :doc (car (last named-arg))
			 :original named-arg)))
	     (getf parsed-doc-ll :named))
     (mapcar #'(lambda (rest-arg)
		 (list :var-forward (list (first rest-arg))
		       :doc (second rest-arg)
		       :original rest-arg))
	     (getf parsed-doc-ll :rest)))))
  

;;;;
;;;; Given a documented lambda list, returns a list of how one should
;;;; forward the arguments of the lambda list
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %extract-arguments-from-documented-lambda-list (doc-lambda-list)
    (alexandria:flatten 
     (mapcar #'(lambda (norm-arg)
		 (getf norm-arg :var-forward))
	     (%normalize-parsed-documented-lambda-list
	      (%parse-documented-lambda-list 
	       doc-lambda-list))))))


;;;;
;;;; Given a documetned lambda list, returns a plain lambda list for it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %documented-lambda-list-to-lambda-list (doc-lambda-list)
    (mapcar #'(lambda (doc-elt)
		(if (listp doc-elt)
		    (if (> (length doc-elt) 2)
			(subseq doc-elt 0 (1- (length doc-elt)))
			(first doc-elt))
		    doc-elt))
	    doc-lambda-list)))

;;;;
;;;; This is a tricky function:
;;;; Given a lambda args defintiion ( like (a b &rest args &key (c 1) d) )
;;;; we return a list of how one would forward these arguments
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %extract-arguments-from-lambda-args (lambda-args)
    (let ((all-args
	   (mapcar (lambda (arg-spec)
		     (if (listp arg-spec)
			 (if (=  (length arg-spec) 3)
			     (list (nth 2 arg-spec) (nth 0 arg-spec))
			     (car arg-spec))
			 arg-spec))
		   (remove-if (lambda (arg-spec)
				(and (not (listp arg-spec))
				     (equal (elt (symbol-name arg-spec) 0) #\&)))
			      lambda-args))))
      ;; ok, now we need to push keyword args to the back and
      ;; move &rest arg all the way to the end
      (let* ((rest-pos (position '&rest lambda-args))
	     (rest-arg (if rest-pos (list (elt all-args rest-pos)) nil))
	     (args-minus-rest (if rest-pos (append (subseq all-args 0 rest-pos)
						   (subseq all-args (1+ rest-pos)))
				  all-args))
	     (key-pos-temp (position '&key lambda-args))
	     (key-pos (if (and rest-pos (< rest-pos key-pos-temp))
			  (1- key-pos-temp)
			  key-pos-temp))
	     (args-minus-rest-up-keys
	      (if key-pos
		  (append (subseq args-minus-rest 0 key-pos )
			  (mapcar (lambda (k)
				    (if (not (listp k))
					(list (intern (symbol-name k) (find-package 'keyword))
					      k)
					k))
				  (subseq args-minus-rest key-pos)))
		  args-minus-rest))
	     (normal-args (remove-if #'listp args-minus-rest-up-keys))
	     (key-args (remove-if-not #'listp args-minus-rest-up-keys)))
	(append normal-args (alexandria:flatten key-args) rest-arg)))))
  


;;;;
;;;; Returns the default concepts package which is based on the current
;;;; package.  This will create a default concepts package in the current
;;;; package if not there, and returns the package
(defvar *default-concept-package-name* (symbol-name :default-concepts)
  "The name of the defaultconcepts package created insude current package")

(defun get-default-concepts-package ()
  (if (find-symbol *default-concept-package-name* *package*)
      (find-symbol *default-concept-package-name* *package*)
      (progn
	(intern *default-concept-package-name* *package*)
	(let ((s (find-symbol *default-concept-package-name* *package*)))
	  (setf (symbol-value s)
		(make-package *default-concept-package-name*))
	  s))))
  
	

;;;;
;;;; Define a particular concept
;;;; This just makes it's name available in the current concepts package
;;;; (or the given concepts package, defautls ot current)
(defmacro define-concept (concept-name concept-args doc &key (package *concepts-package*) (error-when-duplicate-concept t))
  (let ((concept-method (intern (concatenate 'string (symbol-name concept-name) "-METHOD")))
	(concept-method-impl (intern (concatenate 'string (symbol-name concept-name) "-METHOD-IMPL"))))
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
	      (setf (symbol-value concept-symbol) nil)
	      ;; create the concept list in package if needed
	      (if (not (find-symbol (symbol-name '+concept-list+) *concepts-package*))
		  (let ((concept-list (intern (symbol-name '+concept-list+) *concepts-package*)))
		    (setf (symbol-value concept-list) nil)))
	      ;; add new symbol (concept) to the concept list
	      (pushnew concept-symbol (symbol-value (find-symbol (symbol-name '+concept-list+) *concepts-package*)))
	      ;; create ne generic method for this concept
	      `(progn
		 (defgeneric ,concept-method ( ,@(%documented-lambda-list-to-lambda-list concept-args) ) 
		   (:documentation ,doc))
		 (defgeneric ,concept-method-impl ( +concept-impl+ ,@(%documented-lambda-list-to-lambda-list concept-args) )
		   (:documentation ,doc))
		 (defmethod ,concept-method ( ,@(%documented-lambda-list-to-lambda-list concept-args) )
		   (,concept-method-impl ,concept-symbol ,@(%extract-arguments-from-documented-lambda-list concept-args)))
		 ;; print to the user
		 (format t "created new concept ~A~%" ',concept-symbol))))))))
  
  
       
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
    (find-symbol concept-name package)))


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
(defmacro implement-concept ( (concept-name implementation-tag doc) &body body )
  (let ((method-name (intern (concatenate 'string (symbol-name concept-name) "-METHOD-IMPL")))
	(body-lambda-args (car body))
	(body-body (cdr body)))
    (let* ((implementation-tag-+ (concatenate 'string "+" (symbol-name implementation-tag) "+"))
	   (implementation-tag-symbol 
	    (find-or-make-implementation-tag implementation-tag-+ (symbol-name concept-name) doc))
	   (concept-check (list (gensym "concept-var-") (list 'eql implementation-tag-symbol))))
      `(define-concept-method-impl ,method-name (,concept-check ,@body-lambda-args) ,@body-body))))


(implement-concept 
    (foobar impl-fast "FAST foobar implementation")
  (x y)
  (+ x y))

(implement-concept 
    (foobar impl-generic "GENERIC foobar implementation")
  (x y)
  (- x y))



;;;;
;;;; Print out a list of current concepts known about
(defun print-concepts (&key (package *concepts-package*) (print-doc nil))
  (do-symbols (s package)
    (unless (equal (elt (symbol-name s) 0) #\+)
      (format t "~A::~A  set to ~A~%" (package-name package) s (symbol-value s))
      (when print-doc
	(format t "     ~A~%" (documentation s 'VARIABLE))))))

;;;;
;;;; Print out a list of current implementation tags known about
(defun print-implementation-tags (&key (package *concepts-package*) (print-doc nil))
  (do-symbols (s package)
    (when (equal (elt (symbol-name s) 0) #\+)
      (format t "~A::~A~%" (package-name package) s)
      (when print-doc
	(format t "     ~A~%" (documentation s 'VARIABLE))))))
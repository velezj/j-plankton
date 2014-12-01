
(in-package #:j-plankton.documented-lambda-list)

;;=======================================================================
;;=======================================================================



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

(defun %parse-documented-lambda-list (lambda-list &key (error-on-missing-doc t))
    
  ;; find the locations of each of the lambda keywords
  ;; and grab the ordinary, special (key and aux) and rest args
  (metabang-bind:bind 
      (((:assoc (ordinary-args &ordinary)
		(key-args &key)
		(rest-args &rest)
		(aux-args &aux)
		(optional-args &optional))
	(jpu:parse-sassoc-to-alist
	 lambda-list 
	 :is-key 
	 #'(lambda (k) 
	     (and (symbolp k) 
		  (equal #\& (elt (symbol-name k) 0)))) 
	 :default-key '&ordinary)))
    (let* ((special-args (append key-args aux-args))
	   (ll-alist
	     (jpu:parse-sassoc-to-alist
	      lambda-list 
	      :is-key 
	      #'(lambda (k) 
		  (and (symbolp k) 
		       (equal #\& (elt (symbol-name k) 0)))) 
	      :default-key '&ordinary))
	   (other-keys
	     (remove-if 
	      #'(lambda (key)
		  (member key 
			  '(&ordinary &rest &key &aux &optional)))
	      (mapcar #'car ll-alist)))
	   (other-all
	     (alexandria:mappend
	      #'(lambda (k)
		  (let ((a (assoc k ll-alist)))
		    (append
		     (list (car a))
		     (cdr a))))
	      other-keys)))
      
      ;; make sure that each element is a list and that
      ;; the last element is a string
      (when error-on-missing-doc
	(mapcar #'(lambda (e)
		    (check-type e list)
		    (assert (> (length e) 1) (e) "You *must* supply a documentation string")
		    (check-type (car (last e)) string))
		(append ordinary-args optional-args rest-args special-args)))
      ;; ensure formatted even if missing docs, but warn
      (flet ((fix-broken-arg (arg)
	       (let ((a (alexandria:ensure-list arg)))
		 (when (not (listp arg))
		   (warn "non-list element in documented-lamba list, ensuring list: ~S -> ~S" arg a))
		 (when (< (length a) 2)
		   (setf a (append a (list "")))
		   (warn "doc-less element in documented-lambda list, adding empty doc string: ~S -> ~S" arg a))
		 (when (not (stringp (car (last a))))
		   (let ((b a))
		     (setf a (append a (list "")))
		     (warn "final element of documented-lambda list argumet not a doc string, adding empty doc string: ~S -> ~S" b a)))
		 a)))
	(setf 
	 ordinary-args (mapcar #'fix-broken-arg ordinary-args)
	 optional-args (mapcar #'fix-broken-arg optional-args)
	 rest-args (mapcar #'fix-broken-arg rest-args)
	 special-args (mapcar #'fix-broken-arg special-args)))
      
      
      (list :positional (append ordinary-args optional-args)
	    :named special-args
	    :rest rest-args
	    :others other-all))))


;;=======================================================================

;;;;
;;;; Returns the head of hte list, all but the last element
(defun all-but-last (list)
  (subseq list 0 (1- (length list))))


;;=======================================================================

;;;;
;;;; "normalizes" arguments from the result of parsing a documented lambda list
;;;; so that each arugment is in the cacnonical form
;;;; and in hte right order for forwarding arguments
;;;; ( :var-forward forward 
;;;;   :lambda-list-foward foward 
;;;;   :doc doc 
;;;;   :original original-form
;;;;   :type [:positional | :named | :rest | :other ] )
(defun %normalize-parsed-documented-lambda-list (parsed-doc-ll)
  (append
   (mapcar #'(lambda (ordinary-arg)
	       (list :var-forward (list (if (listp (first ordinary-arg))
					    (elt (first ordinary-arg) 0)
					    (first ordinary-arg)))
		     :lambda-list-foward (list (first ordinary-arg))
		     :doc (car (last ordinary-arg))
		     :original ordinary-arg
		     :type :positional))
	   (getf parsed-doc-ll :positional))
   (mapcar #'(lambda (named-arg)
	       (let ((name
		       (intern (symbol-name (elt named-arg 0)) 
			       (find-package "KEYWORD"))))
		 (list :var-forward (list name (elt named-arg 0))
		       :lambda-list-foward (list (all-but-last named-arg))
		       :doc (car (last named-arg))
		       :original named-arg
		       :type :named)))
	   (getf parsed-doc-ll :named))
   (mapcar #'(lambda (other-arg)
	       (list :var-forward nil
		     :lambda-list-foward (list other-arg)
		     :doc nil
		     :original other-arg
		     :type :other))
	   (getf parsed-doc-ll :others))
   (mapcar #'(lambda (rest-arg)
	       (list :var-forward (all-but-last rest-arg)
		     :lambda-list-foward (all-but-last rest-arg)
		     :doc (car (last rest-arg))
		     :original rest-arg
		     :type :rest))
	   (getf parsed-doc-ll :rest))))


;;=======================================================================

;;;; "normalizes" arguments from a documented lambda list
;;;; so that each arugment is in the cacnonical form
;;;; and in hte right order for forwarding arguments
;;;; ( :var-forward forward 
;;;;   :lambda-list-foward foward 
;;;;   :doc doc 
;;;;   :original original-form
;;;;   :type [:positional | :named | :rest | :other ] )
(defun normalize-documented-lambda-list (documented-lambda-list 
					 &key (error-on-missing-doc t) )
  (%normalize-parsed-documented-lambda-list
   (%parse-documented-lambda-list 
    documented-lambda-list
    :error-on-missing-doc error-on-missing-doc)))


;;=======================================================================

;;;;
;;;; Given a documented lambda list, returns a list of how one should
;;;; forward the arguments of the lambda list
(defun foward-arguments-from-documented-lambda-list (doc-lambda-list 
						     &key 
						       (error-on-missing-doc t))
  (alexandria:flatten 
   (mapcar #'(lambda (norm-arg)
	       (getf norm-arg :var-forward))
	   (normalize-documented-lambda-list
	    doc-lambda-list
	    :error-on-missing-doc error-on-missing-doc))))


;;=======================================================================

;;;;
;;;; Given a documetned lambda list, returns a plain lambda list for it
(defun documented-lambda-list->lambda-list (doc-lambda-list 
					    &key 
					      (error-on-missing-doc t))
  (let* ((norm-args
	   (normalize-documented-lambda-list
	    doc-lambda-list
	    :error-on-missing-doc error-on-missing-doc))
	 (positional 
	   (alexandria:mappend 
	    #'(lambda (norm-arg)
		(getf norm-arg :lambda-list-foward))
	    (remove-if #'(lambda (norm-arg)
			   (not (eq :positional
				    (getf norm-arg :type))))
		       norm-args)))
	 (named 
	   (alexandria:mappend 
	    #'(lambda (norm-arg)
		(getf norm-arg :lambda-list-foward))
	    (remove-if #'(lambda (norm-arg)
			   (not (eq :named
				    (getf norm-arg :type))))
		       norm-args)))
	 (rest 
	   (alexandria:mappend 
	    #'(lambda (norm-arg)
		(getf norm-arg :lambda-list-foward))
	    (remove-if #'(lambda (norm-arg)
			   (not (eq :rest
				    (getf norm-arg :type))))
		       norm-args)))
	 (others
	   (alexandria:mappend
	    #'(lambda (norm-arg)
		(getf norm-arg :lambda-list-foward))
	    (remove-if #'(lambda (norm-arg)
			   (not (eq :other
				    (getf norm-arg :type))))
		       norm-args))))
    (append positional 
	    (when rest (list '&rest)) rest
	    (when named (list '&key)) named
	    others)))
  

;;=======================================================================
  
;;;;
;;;; This is a tricky function:
;;;; Given a lambda args defintiion ( like (a b &rest args &key (c 1) d) )
;;;; we return a list of how one would forward these arguments
(defun %foward-arguments-from-lambda-args (lambda-args)
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
      (append normal-args (alexandria:flatten key-args) rest-arg))))


;;=======================================================================

;;;;
;;;; Returns the type specifications in a documented-lambda list.
;;;; This is returned for all the arguments.
;;;; By type specifications this means any spewcialized types such as used 
;;;; in method specialization lambda lists.
;;;; non-specialized argument default to t
(defun documented-lambda-list-argument-types (doc-lambda-list 
					      &key 
						(error-on-missing-doc t))
  (let ((norm-args
	  (normalize-documented-lambda-list
	   doc-lambda-list
	   :error-on-missing-doc error-on-missing-doc)))
    (mapcar #'(lambda (norm-arg)
		(if (eq :positional (getf norm-arg :type))
		    (if (listp (car (getf norm-arg :lambda-list-foward)))
			(elt (car (getf norm-arg :lambda-list-foward)) 1)
			't)
		    't))
	    (remove-if #'(lambda (norm-arg)
			   (eq :other (getf norm-arg :type)))
		       norm-args))))


;;=======================================================================

;;;;
;;;; Returns the argument symbols in a documented-lambda list.
;;;; This is different than the fowarding arguments since we do not
;;;; what hte keywords/specialization-types, just the argument symbols
(defun documented-lambda-list-argument-symbols (doc-lambda-list 
						&key 
						  (error-on-missing-doc t))
  (let ((norm-args
	  (normalize-documented-lambda-list
	   doc-lambda-list
	   :error-on-missing-doc error-on-missing-doc)))
    (mapcar #'(lambda (norm-arg)
		(elt 
		 (alexandria:ensure-list
		  (car
		   (getf norm-arg :lambda-list-foward)))
		 0))
	    (remove-if #'(lambda (norm-arg)
			   (eq :other (getf norm-arg :type)))
		       norm-args))))

;;=======================================================================
;;=======================================================================



(in-package #:j-plankton.cursor)

;;=========================================================================

;;;;
;;;; A generic function to parse a cursor expression into a
;;;; cursor tree given a cursor expression dialect
(defgeneric %parse-cursor-expression ( expr &key dialect ))

;;;;
;;;; The implementation version of parsing allowing us to
;;;; specialize on dialect
(defgeneric %parse-cursor-expression/impl ( dialect expr ))

;;=========================================================================

;;;;
;;;; The currently select cursor expression dialect
(defvar *cursor-expression-dialect* :cursor-dsl)

;;=========================================================================

;;;;
;;;; a condition when we fail to pars a cursor expression
(define-condition cursor-expression-parse-failed ()
  ((dialect
    :initarg :dialect
    :reader dialect)
   (expr
    :initarg :expr
    :reader expr)))
   

;;=========================================================================


;;;;
;;;; foward %parse-cursor-expression keyword argument onto
;;;; and implementation version so that we can specialize
;;;; on those
(defmethod %parse-cursor-expression ( expr
				     &key
				       (dialect *cursor-expression-dialect*) )
  (%parse-cursor-expression/impl dialect expr))

;;=========================================================================

;;;;
;;;; go from a cursto expression to a cursor
;;;; (parse then pack)
(defun cursor-expression->cursor (expr
				  &key
				    (dialect *cursor-expression-dialect*))
  (%pack-cursor-tree
   (%parse-cursor-expression
    expr
    :dialect dialect)))

;;=========================================================================


;;;;
;;;; Given a cursor-expression, returns the resulting cursor
;;;; This is the :dsl1 static cursor dialect
(defun %parse-cursor-expression-dsl1 (expr)
  (when (null expr)
    (return-from %parse-cursor-expression-dsl1
      (cursor/range 0 -1 0 -1)))
  (etypecase expr
    (cursor-t
     expr)
    (symbol
     (if (and (boundp expr)
	      (not (keywordp expr)))
	 (%parse-cursor-expression (symbol-value expr))
	 (if (fboundp expr)
	     (%parse-cursor-expression (funcall (symbol-function expr) nil))
	     (cursor/seq (list expr)))))
    (list
     (let ((head (first expr)))
       (case head
	 ((#\: :range :|:| \:) (apply #'cursor/range (cdr expr)))
	 ((#\x :sweep :x :#) (apply #'cursor/sweep (mapcar #'%parse-cursor-expression (cdr expr))))
	 ((#\| :parsweep :par :||) (apply #'cursor/parallel-sweep (mapcar #'%parse-cursor-expression (cdr expr))))
	 ((:repeat) (if (and (> (length (cdr expr)) 2)
			     (eql (cadr expr) :max))
			(cursor/repeat (%parse-cursor-expression (cdddr expr))
				       :max-count (caddr expr) )
			(cursor/repeat (%parse-cursor-expression (cdr expr)))))
	 ((:f :trans) (apply #'cursor/transform (cadr expr)
			     (mapcar #'%parse-cursor-expression (cddr expr))))
	 ('quote (cursor/seq expr))
	 (t
	  (if (functionp head)
	      (%parse-cursor-expression
	       (apply head (eval `(list ,@(cdr expr)))))
	      (if (and (symbolp head) (fboundp head))
		  (%parse-cursor-expression
		   (apply (symbol-function head) (eval `(list ,@(cdr expr)))))
		  (apply #'cursor/cat 
			 (mapcar #'%parse-cursor-expression
				 expr))))))))
    (t
     (cursor/seq (list expr)))))

;;=========================================================================

;;;;
;;;; Specilized parsing for :dsl1 dialect
(defmethod %parse-cursor-expression/impl ( (dialect (eql :dsl1)) expr )
  (%parse-cursor-expression-dsl1 expr))

;;=========================================================================

;;;;
;;;; A general dialect parser which looks up 'rules' for the dialect
;;;; and simply keeps applying them until we get a parse.
;;;;
;;;; A ruler is a property list with
;;;;    :name <rule name as string>
;;;;    :ruler <function returning (values cursor parsed-p)>
;;;;
;;;; The cursor from the first rule whoose parsed-p is true is used

(defvar *dialect-rule-map* (make-hash-table)
  "a mapping between dialect to an ordered list of rules")
    
(defmethod %parse-cursor-expression/impl ( dialect expr )
  (let ((rule-workspace (make-hash-table))
	(rules (append
		(first (gethash dialect *dialect-rule-map* nil))
		(second (gethash dialect *dialect-rule-map* nil))))
	(cursor nil)
	(parsed-p nil))
    
    ;; loop over rules in order, seeing which one parses
    ;; th expression first and setting cursor and parsed-p
    (dolist (rule rules)
      (format t "parse, trying rule ~A~%"
	      (getf rule :name))
      (multiple-value-bind (r-cursor r-parsed-p)
	  (handler-case
	      (funcall (getf rule :rule)
		       expr rule-workspace)
	    (t (c) (progn
		     (warn "Cursor parsing rule ~A in dialect ~A raised condition ~A, ignoring"
			   (getf rule :name)
			   dialect
			   c))))
	(when r-parsed-p
	  (setf cursor r-cursor
		parsed-p r-parsed-p)
	  (return))))
    
    ;; see if we have a parse or raise condition
    (unless parsed-p
      (error 'cursor-expression-parse-failed
	     :dialect dialect
	     :expr expr))
    cursor))

;;=========================================================================

;;;;
;;;; Add a rule for a given dialect (defaults to current dialect)
;;;;
;;;; The position is one of
;;;;     :end (default) at the ned of normal rules
;;;;     :start at the beginning of normal ruler
;;;;     :last-rule at end of last rules
(defun parse/add-rule (name rule-f
		       &key
			 (dialect *cursor-expression-dialect*)
			 (position :end) )
  (let ((rule
	 (list :name name
	       :rule rule-f)))
    (ecase position
      (:end
       (setf (gethash dialect *dialect-rule-map*)
	     (list
	      (append (first (gethash dialect *dialect-rule-map*))
		      (list rule))
	      (second (gethash dialect *dialect-rule-map*)))))
      (:start
       (setf (gethash dialect *dialect-rule-map*)
	     (list
	      (append (list rule)
		      (first (gethash dialect *dialect-rule-map*)))
	      (second (gethash dialect *dialect-rule-map*)))))
      (:last-rule
       (setf (gethash dialect *dialect-rule-map*)
	     (list
	      (first (gethash dialect *dialect-rule-map*))
	      (append
	       (second (gethash dialect *dialect-rule-map*))
	       (list rule))))))))

;;=========================================================================

;;;;
;;;; A generic function to add a parse rule based on the type of
;;;; cursor
(defgeneric parse/add-rule-for (cursor-type-or-name dialect))

;;=========================================================================

;;;;
;;;; Add a default rule for an object by fowarding to
;;;; adding by it's type
(defmethod parse/add-rule-for ( object dialect )
  (parse/add-rule-for (type-of object) dialect))

;;=========================================================================

;;;;
;;;; Add default parse rule for range-cursor-t
(defmethod parse/add-rule-for ( (type (eql 'range-cursor-t)) dialect )
  (parse/add-rule-for :range dialect))
(defmethod parse/add-rule-for ( (name (eql :range)) dialect )
  (parse/add-rule
   :range
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (member (first expr) '(:range #\: \: :|:|)))
	   (return-from rule (values nil nil)))
	 (values
	  (apply #'cursor/range (cdr expr))
	  t)))))

;;=========================================================================

;;;;
;;;; Add default parse rule for sweeps
(defmethod parse/add-rule-for ( (name (eql ':sweep)) dialect )
  (parse/add-rule
   :sweep
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (member (first expr) '(:sweep #\x :x :#)))
	   (return-from rule (values nil nil)))
	 (values
	  (apply #'cursor/sweep
		 (mapcar #'(lambda (x)
			     (%parse-cursor-expression x :dialect dialect))
			 (cdr expr)))
	  t)))))

;;=========================================================================

;;;;
;;;; Add default parse rule for parallel sweeps
(defmethod parse/add-rule-for ( (name (eql ':parsweep)) dialect )
  (parse/add-rule
   :parsweep
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (member (first expr) '(:parsweep :par #\| :||)))
	   (return-from rule (values nil nil)))
	 (values
	  (apply #'cursor/parallel-sweep
		 (mapcar #'(lambda (x)
			     (%parse-cursor-expression x :dialect dialect))
			 (cdr expr)))
	  t)))))


;;=========================================================================

;;;;
;;;; Add default parse rule for repeat
(defmethod parse/add-rule-for ( (type (eql 'repeat-cursor-t)) dialect )
  (parse/add-rule-for :repeat dialect))
(defmethod parse/add-rule-for ( (name (eql ':repeat)) dialect )
  (parse/add-rule
   :repeat
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (member (first expr) '(:repeat)))
	   (return-from rule (values nil nil)))
	 (values
	  (if (and (> (length (cdr expr)) 2)
		   (eql (cadr expr) :max))
	      (cursor/repeat (%parse-cursor-expression (cdddr expr)
						       :dialect dialect)
			     :max-count (caddr expr) )
	      (cursor/repeat (%parse-cursor-expression (cdr expr)
						       :dialect dialect)))
	  t)))))


;;=========================================================================

;;;;
;;;; Add parse rules for seq cursors
;;;; This will add two rules, a normal and a last rule
(defmethod parse/add-rule-for ( (type (eql 'seq-cursor-t)) dialect )
  (parse/add-rule-for :seq dialect))
(defmethod parse/add-rule-for ( (name (eql :seq)) dialect )

  ;; normal rule
  (parse/add-rule
   :seq/seq
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (member (first expr) '(:seq)))
	   (return-from rule (values nil nil)))
	 (values
	  (cursor/seq (cdr expr))
	  t))))

  ;; normal rule
  (parse/add-rule
   :seq/terminal
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (or
		  (and (symbolp expr)
		       (not (boundp expr))
		       (not (fboundp expr)))
		  (numberp expr)
		  (characterp expr)
		  (stringp expr))
	   (return-from rule (values nil nil)))
	 (values
	  (cursor/seq (list expr))
	  t)))))

;;=========================================================================

;;;;
;;;; pass-trhough ruler
(defmethod parse/add-rule-for ( (name (eql :cursor)) dialect )
  (parse/add-rule
   :cursor/passthrough
   #'(lambda (expr rules-workspace)
       (block rule
	 (format t "cursor/passthrough expr=~S~%" expr)
	 (unless (and (typep expr 'cursor-t))
	   (return-from rule (values nil nil)))
	 (format t "cursor/passthrough passing untouched ~S~%"
		 (values expr t))
	 (values
	  expr
	  t)))))

;;=========================================================================

;;;;
;;;; rule for symbols which are boundp
(defmethod parse/add-rule-for ( (name (eql :bound-var)) dialect )
  (parse/add-rule
   :bound-var
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and
		  (not (keywordp expr))
		  (symbolp expr)
		  (boundp expr))
	   (return-from rule (values nil nil)))
	 (values
	  (%parse-cursor-expression
	   (symbol-value expr)
	   :dialect dialect)
	  t)))))
	 

;;=========================================================================


;;;;
;;;; Add parse rules for concatenation (cat) cursors
(defmethod parse/add-rule-for ( (type (eql 'cat-cursor-t)) dialect )
  (parse/add-rule-for :cat dialect))
(defmethod parse/add-rule-for ( (name (eql :cat)) dialect )
  
  ;; normal rule
  (parse/add-rule
   :cat/cat
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (member (first expr) '(:cat)))
	   (return-from rule (values nil nil)))
	 (values
	  (apply #'cursor/cat 
		 (mapcar #'(lambda (x)
			     (%parse-cursor-expression 
			      x
			      :dialect dialect))
			 (cdr expr)))
	  t))))

  ;; normal ruler
  (parse/add-rule
   :cat/inherent-list
   #'(lambda (expr rules-workspace)
       (block rule
	 (unless (and (listp expr)
		      (not (and
			    (consp (first expr))
			    (symbolp  (first expr))
			    (fboundp (first expr)))))
	   (return-from rule
	     (values nil nil)))
	 (values
	  (apply #'cursor/cat
		 (mapcar #'(lambda (x)
			     (%parse-cursor-expression
			      x :dialect dialect))
			 expr))
	  t)))))
  


;;=========================================================================

(defun %add-default-rules ()
  (parse/add-rule-for :cursor *cursor-expression-dialect*)
  (parse/add-rule-for :range *cursor-expression-dialect*)
  (parse/add-rule-for :sweep *cursor-expression-dialect*)
  (parse/add-rule-for :parsweep *cursor-expression-dialect*)
  (parse/add-rule-for :repeat *cursor-expression-dialect*)
  (parse/add-rule-for :seq *cursor-expression-dialect*)
  (parse/add-rule-for :cat *cursor-expression-dialect*)
  (parse/add-rule-for :bound-var *cursor-expression-dialect*))
  

;;=========================================================================

;;;;
;;;; reset rules to default parse rules
(defun parse/reset-rules ()
  (setf *dialect-rule-map* (make-hash-table))
  (%add-default-rules))

;;=========================================================================
;;=========================================================================

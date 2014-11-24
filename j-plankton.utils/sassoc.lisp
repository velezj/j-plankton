
(in-package #:j-plankton.util)


;;;;
;;;; This file contains the code for "statefull associations lists"
;;;; or sassoc for short
;;;;
;;;; Unlike a normal porperty list which must be of length a multiuple of
;;;; 2, a sassoc has a set of things which can/are keys and will treat 
;;;; all non-key items after a key until another key as belonging to the
;;;; first key.
;;;;
;;;; For Example:
;;;; (make-sassoc '(:a 1 :b 2 :c 3 4)) => ( (:a (1)) (:b (2)) (:c (3 4)) )



;;;;
;;;; Default key predicate which just returns whether this is a symbol in
;;;; the kewrod package or not
(defun is-in-keyword-package (k)
  (and (symbolp k)
       (find-symbol (symbol-name k) (find-package "KEYWORD")))) 


;;;;
;;;; Creates a 'standard' assoc from a list using the sassoc concept 
;;;; of keys.
;;;; By default, keywords are the only keys, but this can be changed
;;;; by giving the key predicate function
(defun make-sassoc (property-list &key (is-key #'is-in-keyword-package) (default-key nil) (test #'eql))
  (let ((current-key default-key)
	(alist nil))
    (dolist (e property-list)
      (if (funcall is-key e)
	  (progn
	    (setf current-key e)
	    (push (cons e nil) alist))
	  (if (null alist)
	      (push (cons default-key (list e)) alist)
	      (setf alist 
		    (mapcar 
		     #'(lambda (ale)
			 (destructuring-bind (k . v) ale
			   (if (funcall test k current-key)
			       (if (null v)
				   (cons k (list e))
				   (cons k (cons e v)))
			       ale)))
		     alist)))))
    (mapcar #'(lambda (ale)
		(destructuring-bind (k . v) ale
		  (cons k (reverse v))))
	    (reverse alist))))






(in-package #:j-plankton.utils)


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
;;;; (parse-sassoc-to-alist '(:a 1 :b 2 :c 3 4)) => ( (:a (1)) (:b (2)) (:c (3 4)) )



;;;;
;;;; Default key predicate which just returns whether this is a symbol in
;;;; the kewrod package or not
(defun is-in-keyword-package (k)
  (and (symbolp k)
       (equal
	(symbol-package k)
	(symbol-package ':a))))

;;;;
;;;; Creates a 'standard' assoc from a list using the sassoc concept 
;;;; of keys.
;;;; By default, keywords are the only keys, but this can be changed
;;;; by giving the key predicate function
(defun parse-sassoc-to-alist (property-list 
			      &key 
				(is-key #'is-in-keyword-package) 
				(default-key nil))
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
		    (cons
		     (cons
		      (car (car alist))
		      (cons e
			    (cdr (car alist))))
		     (cdr alist))))))
    ;; rever both the associations and the alist itself
    (mapcar #'(lambda (ale)
		(destructuring-bind (k . v) ale
		  (cons k (reverse v))))
	    (reverse alist))))





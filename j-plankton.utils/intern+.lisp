
(in-package #:j-plankton.utils)


;;=======================================================================

;;;;
;;;; interns a symbol in the given package with a name
;;;; which is the concatenation of the given argument.
;;;; symbol arguments are used as (symbol-name ...)
(defun intern+ (package &rest args)
  (let ((name
	 (apply 'concatenate
		'string
		(mapcar #'(lambda (x)
			    (typecase x
			      (string x)
			      (symbol (symbol-name x))
			      (t (format nil "~A" x))))
			args))))
    (intern name package)))

;;=======================================================================
;;=======================================================================

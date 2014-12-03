(in-package #:j-plankton.cursor)

;;=========================================================================


;;;;
;;;; macro which binds the value of a cursor elements and calls body forms
(defmacro cursor/loop ((var cursor) &body body)
  (when (not (typep cursor 'cursor-t))
    (setf cursor
	  (cursor-expression->cursor cursor)))
  `(do ((,var))
       ((done-p ,cursor) nil)
     (setf ,var (value ,cursor))
     ,@body
     (when (not (done-p ,cursor))
       (next ,cursor))))

;;=========================================================================

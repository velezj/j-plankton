
(in-package #:j-plankton.utils)


;;;; A generic euality predicate which can be instantiated for different
;;;; objecvts (unlike (equal ... ) which calls back to (eq ...) for 
;;;; object isntances
(defgeneric object-equal-p (x y))


;;;; By default, object-equal-p acts like equal
(defmethod object-equal-p (x y)
  (equal x y))

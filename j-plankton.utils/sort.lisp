
(in-package #:j-plankton.utils)

;;;;
;;;; This file contains simple sorting utilities for sequences


;;;;
;;;; Given a list and a "sorting list", returns the
;;;; list sorted by the ordering of the "sorting list"
(defun sort-by ( sequence sorting-sequence predicate &key (key #'identity))
  (mapcar #'car
	  (sort (mapcar #'cons sequence sorting-sequence)
		predicate
		:key #'(lambda (x) (funcall key (cdr x))))))

;;;;
;;;; Given a sequence, returns the indices to the sorted sequence
;;;; rather htan the soprted sequence itself
(defun sort-indices (sequence predicate &key (key #'identity))
  (sort-by (alexandria:iota (length sequence)) sequence predicate :key key))



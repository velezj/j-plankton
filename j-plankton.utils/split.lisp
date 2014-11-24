
(in-package #:j-plankton.utils)


;;;;
;;;; This file includes utilities to split sequences based on elements
;;;; in them



;;;;
;;;; A less than operator fir integers nad nil which places nil at
;;;; the end (so max element)
(defun <-nil-at-end (a b)
  (cond 
    ( (and (null a) (null b)) t)
    ( (null a) nil )
    ( (null b) t )
    ( t (< a b))))
      

;;;;
;;;; Returns all but the last part of a list
(defun all-but-last (sequence)
  (let ((len (length sequence)))
    (if (> len 1)
	(subseq sequence 0 (- len 1))
	nil)))


;;;;
;;;; Given an item and a sequence, returns three values:
;;;; The things in the sequence before the item, the item, and those
;;;; after the item.
;;;;
;;;; If the item is not found in the sequence , it is treated as being 
;;;; at the end of the sequence, hence the entire saequence it returned
;;;; in the first value, and nil is returned in the third value
(defun split (item-spec sequence)
  (let ((pos (position item-spec sequence)))
    (if (null pos)
	(values sequence item-spec nil)
	(values (subseq sequence 0 pos)
		item-spec
		(subseq sequence (1+ pos))))))

;;;;
;;;; Given a list of items, recursively call (split ... sub-sequence) 
;;;; with the subsequence being the things after the previous split.
;;;;
;;;; So, (recursive-split '( 2 4 ) '( 1 2 3 4 5 )) =>
;;;;     ( (split 2 '( 1 2 3 4 5) ) 2 (split 4 '( 3 4 5 ) ) ) =>
;;;;     ( (1) 2 (3) 4 (5) )    
(defun recursive-split (item-specs sequence)
  (if (null item-specs)
      (list sequence)
      (multiple-value-bind (beg item end)
	  (split (car item-specs) sequence)
	(cons
	 beg
	 (cons
	  item
	  (recursive-split (cdr item-specs) end))))))


;;;;
;;;; Returns the positions of the given list of items in the
;;;; sequence.  Right now this is implemented as a simple mapcar
;;;; so no real performance benefit, but maybe someday....
(defun multiple-position (item-specs sequence)
  (mapcar #'(lambda (item-spec)
	      (position item-spec sequence))
	  item-specs))



;;;; 
;;;; split a sequence into sections between given items.
;;;; The items are aranges in a strucutre which determines precedence,
;;;; so the second items will only be split off from the second part of
;;;; the first split.
;;;;
;;;; The items to split are thmeselves list with same-precedence items to
;;;; split on, in which case the first to appear is split first
;;;;
;;;; Items not found are ignored, but nil is returned in their split point
;;;; the split continues on as if the item was not even supplued to split on.
;;;;
;;;; Example:
;;;; (multiple-split '( (2) (1) (5 6) ) '( 1 2 3 4 5 1 2 4 6 3 5 ) )
;;;; => ((1) 2 (3 4 5) 1 (2 4) 6 (3) 5 NIL)
;;;; Note how the ( 5 6 ) split actually happens in hte ( 6 5 ) order since
;;;; the precedence is the same and 6 appeared before 5.
(defun multiple-split (item-specs sequence &key (ignore-not-found nil))
  (if (null item-specs)
      (list sequence)
      (let* ((positions 
	      (multiple-position (first item-specs) sequence))
	     (items-ordered
	      (sort-by (first item-specs)
		       positions 
		       #'<-nil-at-end))
	     (splits (recursive-split items-ordered sequence)))
	(if (null ignore-not-found)
	    (append (all-but-last splits)
		    (multiple-split (rest item-specs) (car (last splits)) 
				    :ignore-not-found ignore-not-found))
	    (error "This is not quite implemented yet, just use the :ignore-not-found = nil (the default)")))))
	    

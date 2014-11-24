
(in-package #:j-plankton.utils)

;;;;
;;;; classic with-gensyms macro from "On Lisp" by Paul Graham
;; (defmacro with-gensyms (syms &body body)
;;   `(let ,(mapcar #'(lambda (s)
;; 		     `(,s (gensym)))
;; 		 syms)
;;      ,@body))

;;;;
;;;; Not so clkassic with-unique-names which extends with-gensyms
;; (defmacro with-unique-names (symbols &body body)
;;   `(let ,(mapcar (lambda (symbol)
;;                    (let* ((symbol-name (symbol-name symbol))
;;                           (stem (if (every #'alpha-char-p symbol-name)
;;                                     symbol-name
;;                                     (concatenate 'string symbol-name "-"))))
;;                      `(,symbol (gensym ,stem))))
;;                  symbols)
;;      ,@body))


;;;;
;;;; Define a globale lexical (hence can be shadwed by let!)
;;;; This code if from a post from Rob Warnock, 2006 on comp.lang.lisp
;;;; http://xach.com/rpw3/articles/UNOdnYQFX9CyzR_YnZ2dnUVZ_ompnZ2d%40speakeasy.net.html
(defmacro deflex (var val &optional (doc nil docp))    
  (let ((backing-var (intern (concatenate 'string
					  (symbol-name '#:*deflex-var-)
					  (symbol-name var)
					  (symbol-name '#:*))
	                     (symbol-package var)))) ; <== Important!
    `(progn
       (defparameter ,backing-var ,val ,doc)
       ,@(when docp `((setf (documentation ',var 'variable) ,doc)))
       (define-symbol-macro ,var ,backing-var))))


;;;; package.lisp

(defpackage #:j-plankton.concept
  (:nicknames #:jp)
  (:use #:cl)
  (:export
   #:*concepts-package*
   #:clear-concepts-package
   #:with-concepts-package
   #:define-concept
   #:implement-concept
   #:concept-implementation-tag
   #:purge-concept
   #:purge-all-concepts
   #:purge-concepts-package
   #:find-concept-symbol
   #:print-concepts
   #:print-implementation-tags))


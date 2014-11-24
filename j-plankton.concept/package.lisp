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
   #:print-concepts
   #:print-implementation-tags))


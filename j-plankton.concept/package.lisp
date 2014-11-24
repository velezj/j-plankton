;;;; package.lisp

(defpackage #:j-plankton
  (:nicknames #:jp)
  (:use #:cl #:alexandria)
  (:export
   #:*concepts-package*
   #:clear-concepts-package
   #:with-concepts-package
   #:define-concept
   #:implement-concept
   #:print-concepts
   #:print-implementation-tags))


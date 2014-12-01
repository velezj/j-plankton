;;;; package.lisp

(defpackage #:j-plankton.documented-lambda-list
  (:nicknames #:jp.doc-ll)
  (:use #:cl)
  (:export
   #:normalize-documented-lambda-list
   #:documented-lambda-list-has-&rest
   #:documented-lambda-list-argument-types
   #:documented-lambda-list-argument-symbols
   #:foward-arguments-from-documented-lambda-list
   #:documented-lambda-list->lambda-list
   ))


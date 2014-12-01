;;;; package.lisp

(defpackage #:j-plankton.cursor
  (:use #:cl)
  (:export
   #:has-next-p
   #:next
   #:value
   #:reset
   #:cursor-finished-condition
   ))


;;;; package.lisp

(defpackage #:j-plankton.cursor
  (:use #:cl)
  (:export

   ;; the protocol
   #:done-p
   #:next
   #:value
   #:reset
   #:clone
   #:cursor-finished-condition

   ;; creators
   #:cursor/range
   #:cursor/seq

   ;; transformers
   #:cursor/label
   #:cursor/tranform
   #:cursor/tranform-to-last

   ;; sweeps
   #:cursor/parallel-sweep
   #:cursor/sweep

   ;; utility
   #:cursor/materialize
   ))


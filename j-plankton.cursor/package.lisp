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
   #:cursor/cat
   #:cursor/label
   #:cursor/transform
   #:cursor/transform-to-last
   #:repeat
   #:cursor/filter

   ;; sweeps
   #:cursor/parallel-sweep
   #:cursor/sweep

   ;; utility / iteration
   #:cursor/materialize
   #:cursor/loop

   ;; cursor expressions
   #:cursor-expression->cursor
   ))



(in-package #:j-plankton.util)


;;;; create a new custom hash table constructor which uses the object-equal-p
;;;; test function and default hash function
(cl-custom-hash-table:define-custom-hash-table-constructor make-object-equal-hash-table
    :test object-equal-p
    :hash-function sxhash)


;;;; push onto a list which is the value of a particular hashtable
;;;; (Create new list of key has no value)
(defun ensure-push-hash (obj key hash)
  (let ((storage (alexandria:ensure-gethash key hash nil)))
    (push obj storage)
    (setf (gethash key hash) storage)))


;;;; pushnew onto a list which is the value of a particular hashtable
;;;; (Create new list of key has no value)
(defun ensure-pushnew-hash (obj key hash &key (pushnew-key #'identity) (test #'object-equal-p))
  (let ((storage (alexandria:ensure-gethash key hash nil)))
    (pushnew obj storage :test test :key pushnew-key)
    (setf (gethash key hash) storage)))




;;;; Redefine hte standard hash table methods to use the custom hashtable
;;;; interface (adds a with-custom-hash-table around calls)
(defun gethash (key hash &optional default )
  (cl-custom-hash-table:with-custom-hash-table 
    (cl:gethash key hash default)))
(defun (setf gethash) (val key hash)
  (cl-custom-hash-table:with-custom-hash-table 
    (setf (cl:gethash key hash) val)))
(defun remhash (key hash)
  (cl-custom-hash-table:with-custom-hash-table
    (cl:remhash key hash)))
(defun clrhash (hash)
  (cl-custom-hash-table:with-custom-hash-table
    (cl:clrhash hash)))
(defmacro with-hash-table-iterator ((name hash) &body body )
  (cl:with-hash-table-iterator (name hash)
    `(cl-custom-hash-table:with-custom-hash-table
       ,@body)))
(defmacro maphash (function-designator hash) 
  `(cl-custom-hash-table:with-custom-hash-table
     (cl:maphash ,function-designator ,hash)))
(defun hash-table-count (hash)
  (cl-custom-hash-table:with-custom-hash-table
    (cl:hash-table-count hash)))
(defun hash-table-rehash-size (hash)
  (cl-custom-hash-table:with-custom-hash-table
    (cl:hash-table-rehash-size hash)))
(defun hash-table-rehash-threshold (hash)
  (cl-custom-hash-table:with-custom-hash-table
    (cl:hash-table-rehash-threshold hash)))
(defun hash-table-size (hash)
  (cl-custom-hash-table:with-custom-hash-table
    (cl:hash-table-size hash)))

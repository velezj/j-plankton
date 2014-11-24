;;;; package.lisp

(defpackage #:j-plankton.util
  (:nicknames #:jpu)
  (:use #:cl)
  (:shadow #:gethash
	   #:clrhash
	   #:remhash
	   #:with-hash-table-iterator
	   #:maphash
	   #:hash-table-count
	   #:hash-table-rehash-size
	   #:hash-table-rehash-threshold
	   #:hash-table-size)
  (:export
   #:object-equal-p
   #:make-object-equal-hash-table
   #:gethash
   #:clrhash
   #:remhash
   #:with-hash-table-iterator
   #:maphash
   #:hash-table-count
   #:hash-table-rehash-size
   #:hash-table-rehash-threshold
   #:hash-table-size
   #:ensure-push-hash
   #:ensure-pushnew-hash
   
   #:with-gensyms
   #:with-unique-names
   #:deflex
   
   #:sort-by
   #:sort-indices
   
   #:split
   #:recursive-split
   #:multiple-split

   #:make-sassoc
   ))


(asdf:defsystem #:j-plankton.utils
  :depends-on ( #:alexandria #:cl-custom-hash-table )
  :serial t
  :components ((:file "package")
	       (:file "macros")
	       (:file "object")
	       (:file "hash")
	       (:file "sort")
	       (:file "split")
	       (:file "sassoc")
	       (:file "intern+")))
	       

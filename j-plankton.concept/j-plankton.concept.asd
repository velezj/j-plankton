
(asdf:defsystem #:j-plankton
  :depends-on ( #:alexandria #:cl-custom-hash-table #:metabang-bind )
  :serial t
  :components ((:file "package")
	       
	       (:module "util"
			:serial t
			:components ((:file "package")
				     (:file "macros")
				     (:file "object")
				     (:file "hash")
				     (:file "sort")
				     (:file "split")
				     (:file "sassoc")))
	       
	       (:file "concept")))


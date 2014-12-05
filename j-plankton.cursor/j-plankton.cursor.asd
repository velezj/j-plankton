
(asdf:defsystem #:j-plankton.cursor
  :depends-on ( #:alexandria
		#:j-plankton.utils
		#:j-plankton.documented-lambda-list)
  :serial t
  :components ((:file "package")
	       (:file "cursor-properties")
	       (:file "cursor")
	       (:file "common-properties")
	       (:file "properties-method")
	       (:file "cursor-tree")
	       (:file "dsl")
	       (:file "macros")))


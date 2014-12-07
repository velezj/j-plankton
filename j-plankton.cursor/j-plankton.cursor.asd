
(asdf:defsystem #:j-plankton.cursor
  :depends-on ( #:alexandria
		#:j-plankton.utils
		#:j-plankton.property-method)
  :serial t
  :components ((:file "package")
	       (:file "cursor")
	       (:file "common-properties")
	       (:file "cursor-tree")
	       (:file "dsl")
	       (:file "macros")))


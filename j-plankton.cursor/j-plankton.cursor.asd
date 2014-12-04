
(asdf:defsystem #:j-plankton.cursor
  :depends-on ( #:alexandria #:j-plankton.utils)
  :serial t
  :components ((:file "package")
	       (:file "cursor-properties")
	       (:file "cursor")
	       (:file "cursor-tree")
	       (:file "dsl")
	       (:file "macros")))


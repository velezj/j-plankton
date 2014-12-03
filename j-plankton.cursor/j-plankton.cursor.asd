
(asdf:defsystem #:j-plankton.cursor
  :depends-on ( #:alexandria )
  :serial t
  :components ((:file "package")
	       (:file "cursor")
	       (:file "cursor-tree")
	       (:file "dsl")
	       (:file "macros")))


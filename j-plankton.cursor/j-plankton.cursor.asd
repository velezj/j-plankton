
(asdf:defsystem #:j-plankton.cursor
  :depends-on ( #:alexandria )
  :serial t
  :components ((:file "package")
	       (:file "cursor")
	       (:file "dsl")
	       (:file "macros")))



(asdf:defsystem #:j-plankton.documented-lambda-list
  :depends-on ( #:alexandria #:j-plankton.utils #:metabang-bind)
  :serial t
  :components ((:file "package")
	       (:file "documented-lambda-list")))


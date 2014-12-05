
(asdf:defsystem #:j-plankton.property-method
  :depends-on ( #:alexandria
		#:j-plankton.utils
		#:j-plankton.documented-lambda-list)
  :serial t
  :components ((:file "package")
	       (:file "property-method")))


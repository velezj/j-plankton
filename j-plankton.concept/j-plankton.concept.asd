
(asdf:defsystem #:j-plankton.concept
  :depends-on ( #:j-plankton.documented-lambda-list )
  :serial t
  :components ((:file "package")
	       (:file "concept")))


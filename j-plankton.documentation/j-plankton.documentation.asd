;;;; jplankton.documentation.asd

(asdf:defsystem #:j-plankton.documentation
  :description "Describe jplankton.documentation here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
	       #:uiop
	       #:cl-ppcre
	       #:optima)
  :serial t
  :components ((:file "package")
               (:file "j-plankton.documentation")
	       (:file "pandoc")))


(asdf:defsystem #:j-plankton.documentation/test
  :description "Testing of jplankton.documentation using fiveam"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:j-plankton.documentation
	       #:fiveam)
  :serial t
  :components ((:file "j-plankton.documentation.test")))

  

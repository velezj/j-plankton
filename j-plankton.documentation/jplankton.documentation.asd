;;;; jplankton.documentation.asd

(asdf:defsystem #:jplankton.documentation
  :description "Describe jplankton.documentation here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
	       #:uiop
	       #:cl-ppcre
	       #:optima)
  :serial t
  :components ((:file "package")
               (:file "jplankton.documentation")
	       (:file "pandoc")))


(asdf:defsystem #:jplankton.documentation/test
  :description "Testing of jplankton.documentation using fiveam"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:jplankton.documentation
	       #:fiveam)
  :serial t
  :components ((:file "jplankton.documentation.test")))

  

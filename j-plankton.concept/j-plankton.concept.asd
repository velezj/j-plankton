
(asdf:defsystem #:j-plankton.concept
  :depends-on ( #:j-plankton.utils #:metabang-bind )
  :serial t
  :components ((:file "package")
	       (:file "concept")))


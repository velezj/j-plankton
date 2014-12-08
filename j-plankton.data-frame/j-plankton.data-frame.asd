
(asdf:defsystem #:j-plankton.data-frame
  :depends-on ( #:j-plankton.concept
		#:j-plankton.cursor
		#:j-plankton.property-method)
  :serial t
  :components ((:file "package")
	       (:file "data-frame-concepts")
	       (:file "sparse-tensor-concepts")
	       (:file "flat-hash-tensor")
	       (:file "data-frame")))


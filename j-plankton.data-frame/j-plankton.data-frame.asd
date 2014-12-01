
(asdf:defsystem #:j-plankton.data-frame
  :depends-on ( #:j-plankton.concept #:spartns )
  :serial t
  :components ((:file "package")
	       (:file "data-frame")
	       (:file "flat-sparse-tensor")))


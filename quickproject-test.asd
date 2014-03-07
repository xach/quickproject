;;;; quickproject.asd

(asdf:defsystem #:quickproject-test
  :description "Unit tests for Quickproject"
  :version "1.2"
  :depends-on (#:quickproject
	       #:cl-fad
	       #:cl-ppcre
               #:lisp-unit)
  :serial t
  :components
  ((:module :test
	    :components ((:file "package")
			 (:file "quickproject-test")))))

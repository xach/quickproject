;;;; quickproject.asd

(asdf:defsystem #:quickproject
  :description "Creates the skeleton of a new Common Lisp project"
  :serial t
  :components ((:file "package")
               (:file "quickproject")))

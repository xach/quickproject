;;;; (#| TMPL_VAR name |#).asd(#| TMPL_IF copyright |#)
;;
;;;; (#| TMPL_VAR copyright |#)(#| /TMPL_IF |#)

(asdf:defsystem #:(#| TMPL_VAR name |#)
  :description "Describe (#| TMPL_VAR name |#) here"
  :author  "(#| TMPL_VAR author |#)"
  :license "(#| TMPL_VAR license |#)"
  :version "0.0.1"
  :serial t(#| TMPL_IF depends-on |#)
  :depends-on (#| TMPL_VAR dependencies-string |#)(#| /TMPL_IF |#)
  :components ((:module
		src :components
		((:file "package")
		 (:file "(#| TMPL_VAR name |#)")))))

(asdf:defsystem #:(#| TMPL_VAR name |#)/test
  :description "Test suite for :(#| TMPL_VAR name |#)"
  :author "(#| TMPL_VAR author |#)"
  :license "(#| TMPL_VAR license |#)"
  :serial t
  :depends-on (#:(#| TMPL_VAR name |#) #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "(#| TMPL_VAR name |#)"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))

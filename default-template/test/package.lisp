;;;; test/package.lisp(#| TMPL_IF copyright |#)
;;
;;;; (#| TMPL_VAR copyright |#)(#| /TMPL_IF |#)

(defpackage #:(#| TMPL_VAR name |#)/test
  (:use #:cl #:(#| TMPL_VAR name |#) #:test-utils))

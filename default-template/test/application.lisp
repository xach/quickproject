;;;; test/(#| TMPL_VAR name |#).lisp(#| TMPL_IF copyright |#)
;;
;;;; (#| TMPL_VAR copyright |#)(#| /TMPL_IF |#)

(in-package #:(#| TMPL_VAR name |#)/test)

(tests
 (is (+ 2 3) 5 "Addition works")
 (is (+ 2 3) 6 "Intentionally fails")

 (for-all ((a a-number) (b a-number))
	  (is= (+ a b) (+ b a))
	  "Addition is commutative")
 (for-all ((a a-number) (b a-number))
	  (is= (- a b) (- b a))
	  "Subtraction is not, so this should fail"))

;;
;; Continuous integration init
;;

(load "./quicklisp.lisp")

(handler-case
    (quicklisp-quickstart:install :path "./.quicklisp")
  (simple-error (e)
    (format t "Warning: ~s" e)))

(load "./.quicklisp/setup.lisp")

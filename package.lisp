;;;; package.lisp

(defpackage #:quickproject
  (:use #:cl)
  (:export #:make-project
           #:*after-make-project-hooks*))

(in-package #:quickproject)


;;;; package.lisp

(defpackage #:quickproject
  (:docuemntation "The Quickproject package.")
  (:use #:cl)
  (:export #:make-project
           #:*after-make-project-hooks*
           #:*author*
           #:*license*))

(in-package #:quickproject)


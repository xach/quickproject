;;;; package.lisp

(defpackage #:quickproject
  (:documentation "The Quickproject package.")
  (:use #:cl)
  (:export #:make-project
           #:*after-make-project-hooks*
           #:*author*
           #:*license*)
  (:shadowing-import-from #:html-template
                          #:*warn-on-creation*
                          #:fill-and-print-template
                          #:*template-start-marker*
                          #:*template-end-marker*)
  (:shadowing-import-from #:cl-fad
                          #:pathname-as-directory
                          #:walk-directory))

(in-package #:quickproject)

(macrolet ((foo ()
             bar))
  (foo))

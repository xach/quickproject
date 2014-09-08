;;;; package.lisp

(defpackage #:quickproject
  (:documentation "The Quickproject package.")
  (:use #:cl)
  (:export #:make-project
           #:*after-make-project-hooks*
           #:*author*
           #:*license*
           #:*template-directory*
           #:*include-copyright*
           #:default-template-parameters
           #:*template-parameter-functions*)
  (:shadowing-import-from #:html-template
                          #:fill-and-print-template
                          #:*template-start-marker*
                          #:*template-end-marker*)
  (:shadowing-import-from #:cl-fad
                          #:pathname-as-directory
                          #:walk-directory))

(in-package #:quickproject)


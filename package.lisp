;;;; package.lisp

(defpackage #:quickproject
  (:documentation "The Quickproject package.")
  (:use #:cl)
  (:export #:make-project
           #:*after-make-project-hooks*
           #:*author*
           #:*license*
           #:*template-directory*
           #:*include-test*
           #:*include-copyright*
           #:default-template-parameters
           #:*template-parameter-functions*
           #:target-exists)
  (:shadowing-import-from #:html-template
                          #:fill-and-print-template
                          #:*template-start-marker*
                          #:*template-end-marker*)
  (:shadowing-import-from #:cl-fad
                          #:pathname-as-directory
                          #:walk-directory
                          #:file-exists-p
                          #:delete-directory-and-files))

(in-package #:quickproject)

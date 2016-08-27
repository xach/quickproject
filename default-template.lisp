(in-package #:quickproject)

(defun default-template-parameters ()
  "Return a plist of :NAME, :LICENSE, and :AUTHOR parameters."
  (list :name *name*
        :license *license*
        :author *author*))

(defun uninterned-symbolize (name)
  "Return an uninterned symbol named after NAME, which is treated as a
string designator and upcased."
  (make-symbol (string-upcase name)))

(defun write-system-form (name &key depends-on (stream *standard-output*))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((*print-case* :downcase))
    (format stream "(asdf:defsystem ~S~%" (uninterned-symbolize name))
    (format stream "  :description \"Describe ~A here\"~%"
            name)
    (format stream "  :author ~S~%" *author*)
    (format stream "  :license ~S~%" *license*)
    (when depends-on
      (format stream "  :depends-on (~{~S~^~%~15T~})~%"
              (mapcar #'uninterned-symbolize depends-on)))
    (format stream "  :serial t~%")
    (format stream "  :components ((:file \"package\")~%")
    (format stream "               (:file ~S)))~%" (string-downcase name))))

(defmacro with-new-file ((stream file) &body body)
  "Like WITH-OPEN-FILE, but specialized for output to a file that must
not already exist."
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :error)
     (let ((*print-case* :downcase))
       ,@body)))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun file-comment-header (stream)
  (format stream ";;;; ~A~%" (file-namestring stream))
  (when *include-copyright*
    (format stream ";;;;~%")
    (format stream ";;;; Copyright (c) ~D ~A~%" (current-year) *author*))
  (terpri stream))

(defun write-system-file (name file &key depends-on)
  (with-new-file (stream file)
    (file-comment-header stream)
    (write-system-form name
                       :depends-on depends-on
                       :stream stream)
    (terpri stream)))

(defun write-readme-file (name file)
  (with-new-file (stream file)
    (format stream "This is the stub ~A for the ~S project.~%"
            (file-namestring file)
            name)))

(defun write-package-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(defpackage ~S~%" (uninterned-symbolize name))
    (format stream "  (:use #:cl))~%~%")))

(defun write-application-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(in-package ~S)~%~%" (uninterned-symbolize name))
    (format stream ";;; ~S goes here. Hacks and glory await!~%~%" name)))

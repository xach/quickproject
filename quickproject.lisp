;;;; quickproject.lisp

(in-package #:quickproject)

(defun uninterned-symbolize (name)
  "Return an uninterned symbol named after NAME, which is treated as a
string designator and upcased."
  (make-symbol (string-upcase name)))

(defun write-system-form (name &key depends-on (stream *standard-output*))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((*print-case* :downcase))
    (format stream "(asdf:defsystem ~S~%" (uninterned-symbolize name))
    (format stream "  :serial t~%")
    (when depends-on
      (format stream "  :depends-on (~{~S~^~%~15T~})~%"
              (mapcar #'uninterned-symbolize depends-on)))
    (format stream "  :components ((:file \"package\")~%")
    (format stream "               (:file ~S)))~%" (string-downcase name))))

(defun pathname-project-name (pathname)
  "Return a project name based on PATHNAME by taking the last element
in the pathname-directory list. E.g. returns \"awesome-project\" for
#p\"src/awesome-project/\"."
  (first (last (pathname-directory pathname))))

(defmacro with-new-file ((stream file) &body body)
  "Like WITH-OPEN-FILE, but specialized for output to a file that must
not already exist."
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :error)
     (let ((*print-case* :downcase))
       ,@body)))

(defun file-comment-header (stream)
  (format stream ";;;; ~A~%~%" (file-namestring stream)))

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

(defvar *after-make-project-hooks* nil
  "A list of functions to call after MAKE-PROJECT is finished making a
  project. It is called with the same arguments passed to
  MAKE-PROJECT, except that NAME is canonicalized if necessary.")

(defun make-project (pathname &key
                     depends-on
                     (name (pathname-project-name pathname)))
  "Create a project skeleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (labels ((relative (file)
             (merge-pathnames file pathname))
           (nametype (type)
             (relative (make-pathname :name name :type type))))
    (ensure-directories-exist pathname)
    (write-readme-file name (relative "README.txt"))
    (write-system-file name (nametype "asd") :depends-on depends-on)
    (write-package-file name (relative "package.lisp"))
    (write-application-file name (nametype "lisp"))
    (pushnew (truename pathname) asdf:*central-registry*
             :test 'equal)
    (dolist (hook *after-make-project-hooks*)
      (funcall hook pathname :depends-on depends-on :name name))
    name))

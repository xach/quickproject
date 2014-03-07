;;;; quickproject.lisp

(in-package #:quickproject)

(defvar *name*)
(setf (documentation '*name* 'variable)
      "The name of the project currently being created.")

(defvar *template-directory* nil
  "A directory to use as a source of template files.")

(defvar *author*
  "Your Name <your.name@example.com>"
  "Set this variable to your contact information.")

(defvar *license*
  "Specify license here")

(defun uninterned-symbolize (name)
  "Return an uninterned symbol named after NAME, which is treated as a
string designator and upcased."
  (make-symbol (string-upcase name)))

(defun write-system-form (name &key depends-on (stream *standard-output*))
  "Write an asdf defsystem form for NAME to STREAM."
  (let ((*print-case* :downcase))
    (format stream "(asdf:defsystem ~S~%" (uninterned-symbolize name))
    (format stream "  :serial t~%")
    (format stream "  :description \"Describe ~A here\"~%"
            name)
    (format stream "  :author ~S~%" *author*)
    (format stream "  :license ~S~%" *license*)
    (when depends-on
      (format stream "  :depends-on (~{~S~^~%~15T~})~%"
              (mapcar #'uninterned-symbolize depends-on)))
    (format stream "  :components~%")
    (format stream "  ((:module :src~%")
    (format stream "            :components ((:file \"package\")~%")
    (format stream "                         (:file ~S)))~%" (string-downcase name))))

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
    (format stream "# ~A~%~%This is the stub ~A for the ~S project.~%"
	    (string-upcase name)
            (file-namestring file)
            name)
    (format stream "~%In a Common Lisp REPL :~%~%")
    (format stream "    CL-USER> (push #p\"/projects/~A/\" asdf:*central-registry*)~%"
	    name)
    (format stream "    CL-USER> (ql:quickload \"~A\")~%" name)))

(defun write-package-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(defpackage ~S~%" (uninterned-symbolize name))
    (format stream "  (:use #:cl))~%~%")))

(defun write-package-test-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(defpackage ~S-test~%" (uninterned-symbolize name))
    (format stream "  (:use #:cl))~%~%")))

(defun write-application-test-file (name file)
  (with-new-file (stream file)
    (file-comment-header stream)
    (format stream "(in-package ~S-test)~%~%" (uninterned-symbolize name))
    (format stream ";;; ~S goes here. Hacks and glory await!~%~%" name)))



(defvar *after-make-project-hooks* nil
  "A list of functions to call after MAKE-PROJECT is finished making a
project. Each function is called with the same arguments passed to
MAKE-PROJECT, except that NAME is canonicalized if
necessary. *DEFAULT-PATHNAME-DEFAULTS* bound to the newly created
project directory.")

(defun rewrite-templates (template-directory target-directory parameters)
  "Treat every file in TEMPLATE-DIRECTORY as a template file; fill it
out using PARAMETERS into a corresponding file in
TARGET-DIRECTORY. The rewriting uses HTML-TEMPLATE. The template start
marker is the string \"\(#|\" and the template end marker is the string
\"|#)\". Template vars are not modified or escaped when written."
  (let ((*template-start-marker* "(#|")
        (*template-end-marker* "|#)")
        (html-template:*warn-on-creation* nil)
        (html-template:*string-modifier* 'identity))
    (setf template-directory (truename template-directory)
          target-directory (truename target-directory))
    (flet ((rewrite-template (pathname)
             (let* ((relative-namestring
                     (enough-namestring pathname template-directory))
                    (target-pathname (merge-pathnames relative-namestring
                                                      target-directory)))
               (ensure-directories-exist target-pathname)
               (with-open-file (stream
                                target-pathname
                                :direction :output
                                :if-exists :rename-and-delete)
                 (fill-and-print-template pathname
                                          parameters
                                          :stream stream)))))
      (walk-directory template-directory #'rewrite-template))))

(defun default-template-parameters ()
  "Return a plist of :NAME, :LICENSE, and :AUTHOR parameters."
  (list :name *name*
        :license *license*
        :author *author*))

(defvar *template-parameter-functions* (list 'default-template-parameters)
  "A list of functions that return plists for use when rewriting
  template files. The results of calling each function are appended
  together to pass to FILL-AND-PRINT-TEMPLATE.")

(defun template-parameters (initial-parameters)
  "Return all template parameters returned by calling each element in
*TEMPLATE-PARAMETER-FUNCTIONS*, appended together as a single plist."
  (apply 'append initial-parameters
         (mapcar 'funcall *template-parameter-functions*)))

(defun make-project (pathname &key
                     depends-on
                     template-parameters
                     ((:template-directory *template-directory*)
                      *template-directory*)
                     ((:author *author*) *author*)
                     ((:license *license*) *license*)
                     (name (pathname-project-name pathname) name-provided-p))
  "Create a project skeleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (when (pathname-name pathname)
    ;;(warn "Coercing ~S to directory" pathname)
    (setf pathname (pathname-as-directory pathname))
    (unless name-provided-p
      (setf name (pathname-project-name pathname))))
  (let ((sources-pathname (pathname-as-directory (merge-pathnames pathname "src")))
	(tests-pathname (pathname-as-directory (merge-pathnames pathname "test"))))
    (labels ((relative (directory file)
	       (merge-pathnames file directory))
	     (nametype (title directory type)
	       (relative directory (make-pathname :name title :type type))))
      (ensure-directories-exist pathname)
      (ensure-directories-exist sources-pathname)
      (ensure-directories-exist tests-pathname)
      (write-readme-file name (relative pathname "README.md"))
      (write-system-file name (nametype name pathname "asd") :depends-on depends-on)
      (write-system-file (format nil "~A-test" name)
			 (nametype (format nil "~A-test" name) pathname "asd")
			 :depends-on depends-on)
      (ensure-directories-exist pathname)
      (write-package-file name (relative sources-pathname "package.lisp"))
      (write-application-file name (nametype name sources-pathname "lisp"))
      (write-package-test-file name (relative tests-pathname "package.lisp"))
      (write-application-test-file name (nametype name tests-pathname "lisp"))
      (let ((*default-pathname-defaults* (truename pathname))
	    (*name* name))
	(when *template-directory*
	  (rewrite-templates *template-directory* *default-pathname-defaults*
			     (template-parameters template-parameters)))
	(pushnew *default-pathname-defaults* asdf:*central-registry*
		 :test 'equal)
	(dolist (hook *after-make-project-hooks*)
	  (funcall hook pathname :depends-on depends-on :name name
		   :allow-other-keys t)))
      name)))

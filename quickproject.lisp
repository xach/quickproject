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

(defvar *include-copyright* nil         ; This gives default behavior.
  "Include a copyright notice at the top of files.")

(defvar *after-make-project-hooks* nil
  "A list of functions to call after MAKE-PROJECT is finished making a
project. Each function is called with the same arguments passed to
MAKE-PROJECT, except that NAME is canonicalized if
necessary. *DEFAULT-PATHNAME-DEFAULTS* bound to the newly created
project directory.")

(defun pathname-project-name (pathname)
  "Return a project name based on PATHNAME by taking the last element
in the pathname-directory list. E.g. returns \"awesome-project\" for
#p\"src/awesome-project/\"."
  (first (last (pathname-directory pathname))))

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
                     (name (pathname-project-name pathname) name-provided-p)
                     ((:include-copyright *include-copyright*) *include-copyright*))
  "Create a project skeleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (when (pathname-name pathname)
    (warn "Coercing ~S to directory"
          pathname)
    (setf pathname (pathname-as-directory pathname))
    (unless name-provided-p
      (setf name (pathname-project-name pathname))))
  (labels ((relative (file)
             (merge-pathnames file pathname))
           (nametype (type)
             (relative (make-pathname :name name :type type))))
    (ensure-directories-exist pathname)
    (write-readme-file name (relative "README.txt"))
    (write-system-file name (nametype "asd") :depends-on depends-on)
    (write-package-file name (relative "package.lisp"))
    (write-application-file name (nametype "lisp"))
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
    name))

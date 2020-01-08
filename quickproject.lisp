;;;; quickproject.lisp

(in-package #:quickproject)

(defvar *name*)
(setf (documentation '*name* 'variable)
      "The name of the project currently being created.")

(defvar *template-directory* (asdf:system-relative-pathname :quickproject "default-template")
  "A directory to use as a source of template files.")

(defvar *depends-on* nil
  "Dependencies specified at project creation.")

(defvar *author*
  "Your Name <your.name@example.com>"
  "Set this variable to your contact information.")

(defvar *license* "Specify license here"
  "Name of the license.")

(defvar *include-copyright* nil         ; This gives default behavior.
  "Include a copyright notice at the top of files.")

(defun pathname-project-name (pathname)
  "Return a project name based on PATHNAME by taking the last element
in the pathname-directory list. E.g. returns \"awesome-project\" for
#p\"src/awesome-project/\"."
  (first (last (pathname-directory pathname))))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defvar *after-make-project-hooks* nil
  "A list of functions to call after MAKE-PROJECT is finished making a
project. Each function is called with the same arguments passed to
MAKE-PROJECT, except that NAME is canonicalized if
necessary. *DEFAULT-PATHNAME-DEFAULTS* bound to the newly created
project directory.")

(defun matches-template-p (pathname template)
  "Returns true if PATHNAME matches the pathname TEMPLATE in both name and type."
  (and (equal (pathname-name pathname) (pathname-name template))
       (equal (pathname-type pathname) (pathname-type template))))

(defun template-pathname->output-name (path)
  "Convert a template file name to a project-specific name."
  (if (or (matches-template-p path "system.asd")
          (matches-template-p path "application.lisp"))
      (make-pathname :name *name* :defaults path)
    path))

(define-condition target-exists (file-error)
  ((template-directory :initarg :template-directory :reader template-directory)
   (target-directory :initarg :target-directory :reader target-directory)
   (parameters :initarg :parameters :reader parameters)
   (if-exists :initarg :if-exists :reader if-exists))
  (:report "The target directory has files in it.")
  (:documentation "When a target directory has files in it, throw this."))

(defun rewrite-templates (template-directory target-directory parameters
                          &optional if-exists)
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
                    (target-pathname (template-pathname->output-name
                                      (merge-pathnames relative-namestring
                                                       target-directory)))
                    (if-exists if-exists))
               (ensure-directories-exist target-pathname)
               (when (and (null if-exists)
                          (file-exists-p target-pathname))
                 (error 'target-exists
                        :template-directory template-directory
                        :target-directory target-directory
                        :parameters parameters
                        :if-exists if-exists))
               (when (not (and (eql if-exists :preserve-readme)
                               (string-equal (pathname-name target-pathname)
                                             "README")))
                 (when (eql if-exists :preserve-readme)
                   (setf if-exists :rename-and-delete))
                 (with-open-file (stream
                                  target-pathname
                                  :direction :output
                                  :if-exists if-exists)
                   (fill-and-print-template pathname
                                            parameters
                                            :stream stream))))))
      (walk-directory template-directory #'rewrite-template))))

(defun default-template-parameters ()
  "Return a plist of :NAME, :LICENSE, and :AUTHOR parameters."
  (list :name *name*
        :license *license*
        :author *author*
        :depends-on (mapcar
                     (lambda (sym)
                       (list :symbol sym :uninterned (format nil "#:~(~a~)" sym)))
                     *depends-on*)
        :dependencies-string (format nil "(~{#:~(~a~)~^ ~})" *depends-on*)
        :copyright (when *include-copyright*
                     (format nil "Copyright (c) ~D ~A~%" (current-year) *author*))))

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
                     template-parameters
                     ((:template-directory *template-directory*)
                      *template-directory*)
                     ((:depends-on *depends-on*) *depends-on*)
                     ((:author *author*) *author*)
                     ((:license *license*) *license*)
                     (name (pathname-project-name pathname) name-provided-p)
                     ((:include-copyright *include-copyright*) *include-copyright*))
  "Create a project skeleton for NAME in PATHNAME. If DEPENDS-ON is provided,
it is used as the asdf defsystem depends-on list."
  (check-type *depends-on* list)
  (when (pathname-name pathname)
    (warn "Coercing ~S to directory"
          pathname)
    (setf pathname (pathname-as-directory pathname))
    (unless name-provided-p
      (setf name (pathname-project-name pathname))))
  (ensure-directories-exist pathname)
  (let ((*default-pathname-defaults* (truename pathname))
        (*name* name))
    (let (c)
      (restart-case (handler-case
                        (rewrite-templates *template-directory*
                                           *default-pathname-defaults*
                                           (template-parameters
                                            template-parameters))
                      (target-exists (condition)
                        (setf c condition)
                        (error condition)))
        (overwrite-everything ()
          :report "Overwrite everything."
          (delete-directory-and-files (target-directory c))
          (ensure-directories-exist (target-directory c))
          (rewrite-templates (template-directory c)
                             (target-directory c)
                             (parameters c)
                             :rename-and-delete))))
    (pushnew *default-pathname-defaults* asdf:*central-registry*
             :test 'equal)
    (dolist (hook *after-make-project-hooks*)
      (funcall hook pathname :depends-on *depends-on* :name name
                             :allow-other-keys t)))
  name)

;;; Unit tests for QuickProject

(in-package #:quickproject-test)



(defun search-token-into-file (token filename)
  (let ((scanner (cl-ppcre:create-scanner token))
	(find-token nil))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil nil)
	 while line
	 when (cl-ppcre:scan scanner line)
	 do (setf find-token t)))
    find-token))

(defun get-random-name ()
  (format nil "qp-tests-~A~A" (get-universal-time) (random 1000)))

(defun make-project-file (path filename)
  (merge-pathnames path filename))

(defun make-source-file (path filename)
  (merge-pathnames
   (cl-fad:pathname-as-directory (merge-pathnames path "src"))
   filename))

(defun make-test-file (path filename)
  (merge-pathnames
   (cl-fad:pathname-as-directory (merge-pathnames path "test"))
   filename))

(define-test cant-create-project-directory-with-invalid-path
  (let ((path #p"/foo/bar/"))
    (assert-error 'file-error (make-project path))
    (assert-false (cl-fad:directory-exists-p path))))


(define-test cant-create-project-with-directory-existing
  (let ((path #p"/tmp/bar/"))
    (ensure-directories-exist path)
    (assert-error 'file-error (make-project path))))


(defmacro with-project ((name path) &body body)
  `(let* ((,name (get-random-name))
	  (,path (pathname (format nil "/tmp/~A/" ,name))))
     (make-project ,path)
     ,@body))


(define-test can-create-project
  (with-project (name path)
      (assert-true (cl-fad:directory-exists-p path))
      (assert-equal 5 (list-length (cl-fad:list-directory path)))
      (mapc #'(lambda (filename)
		(assert-true (cl-fad:file-exists-p
			      (make-project-file path filename))))
	    (list (format nil "~A.asd" name)
		  (format nil "~A-test.asd" name)
		  "README.md"))
      (assert-true (cl-fad:directory-exists-p
		    (merge-pathnames path "src")))
      (assert-true (cl-fad:directory-exists-p
		    (merge-pathnames path "test")))
      (assert-true (cl-fad:file-exists-p
		    (make-source-file path "package.lisp")))
      (assert-true (cl-fad:file-exists-p
		    (make-source-file path (format nil "~A.lisp" name))))
      (assert-true (cl-fad:file-exists-p
		    (make-test-file path "package.lisp")))
      (assert-true (cl-fad:file-exists-p
		    (make-test-file path (format nil "~A.lisp" name))))
      ))


(define-test check-default-description-header
  (with-project (name path)
    (assert-true (search-token-into-file
  		  (format nil ":description \"Describe ~A here\"" name)
  		  (make-project-file path (format nil "~A.asd" name))))))

(define-test check-default-author-header
  (with-project (name path)
    (assert-true (search-token-into-file
		  (format nil ":author \"Your Name <your.name@example.com>\"")
		  (make-project-file path (format nil "~A.asd" name))))))

(define-test check-default-license-header
  (with-project (name path)
    (assert-true (search-token-into-file
		  (format nil ":license \"Specify license here\"")
		  (make-project-file path (format nil "~A.asd" name))))))

(define-test check-customize-author-header
  (let* ((author "Foo Bar <foo.bar@gmail.com>")
	 (quickproject:*author* author))
    (with-project (name path)
      (assert-true (search-token-into-file
		    (format nil ":author \"~A\"" author)
		    (make-project-file path (format nil "~A.asd" name)))))))

(define-test check-customize-license-header
  (let* ((license "DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE")
	 (quickproject:*license* license))
    (with-project (name path)
      (assert-true (search-token-into-file
		    (format nil ":license \"~A\"" license)
		    (make-project-file path (format nil "~A.asd" name)))))))

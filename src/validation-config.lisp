(defpackage :agent-code/src/validation-config
  (:use :cl)
  (:nicknames :validation-config)
  (:export #:*validation-rules*
           #:*test-commands*
           #:*linter-configs*
           #:get-validation-rule
           #:file-type-from-extension
           #:project-type-from-directory
           #:test-files-exist-p))

(in-package :agent-code/src/validation-config)

(defparameter *validation-rules*
  '((:common-lisp . ((:syntax-check . t)
                     (:compile-check . t)
                     (:lint-check . :warning-only)
                     (:test-run . t)))
    (:java . ((:syntax-check . t)
              (:compile-check . t)
              (:lint-check . :warning-only)
              (:test-run . t)))
    (:python . ((:syntax-check . t)
                (:lint-check . t)
                (:test-run . :if-tests-exist)))
    (:javascript . ((:syntax-check . t)
                    (:lint-check . t)))
    (:default . ((:syntax-check . t)))))

(defparameter *test-commands*
  '((:common-lisp . "sbcl --noinform --non-interactive --eval \"(asdf:test-system :~A)\"")
    (:java . "mvn clean verify")
    (:python . "python -m pytest ~A")
    (:javascript . "npm test")))

(defparameter *linter-configs*
  '((:common-lisp . "cl-lint")
    (:python . "pylint")
    (:javascript . "eslint")))

(defun get-validation-rule (file-type rule)
  (let ((rules (cdr (assoc file-type *validation-rules*))))
    (if rules
        (cdr (assoc rule rules))
        (cdr (assoc rule (cdr (assoc :default *validation-rules*)))))))

(defun project-type-from-directory (path)
  (let ((files (uiop:directory-files path)))
    (dolist (file files)
      (let* ((ext (pathname-type file))
             (name (if ext
                       (format nil "~A.~A" (pathname-name file) ext)
                       (pathname-name file))))
        (cond ((string-equal ext "asd")
               (return :common-lisp))
              ((string-equal name "pom.xml")
               (return :java))
              ((or (string-equal ext "py"))
               :python)
              ((or (string-equal ext "js") (string-equal ext "ts"))
               :javascript)
              (t :default))))))

(defun test-files-exist-p (path)
  "Check if test files exist for the given path."
  (let* ((dir (directory-namestring path))
         (test-patterns (list (format nil "~Atests/" dir)
                              (format nil "~Asrc/test/" dir))))
    (some #'uiop:directory-exists-p test-patterns)))

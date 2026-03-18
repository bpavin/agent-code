(defpackage :agent-code/src/validation-config
  (:use :cl)
  (:nicknames :validation-config)
  (:export #:*validation-rules*
           #:*test-commands*
           #:*linter-configs*
           #:get-validation-rule
           #:file-type-from-extension
           #:test-files-exist-p))

(in-package :agent-code/src/validation-config)

(defparameter *validation-rules*
  '((:common-lisp . ((:syntax-check . t)
                     (:compile-check . t)
                     (:lint-check . :warning-only)
                     (:test-run . :if-tests-exist)))
    (:python . ((:syntax-check . t)
                (:lint-check . t)
                (:test-run . :if-tests-exist)))
    (:javascript . ((:syntax-check . t)
                    (:lint-check . t)))
    (:default . ((:syntax-check . t)))))

(defparameter *test-commands*
  '((:common-lisp . "sbcl --noinform --non-interactive --eval \"(asdf:test-system :~A)\"")
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

(defun file-type-from-extension (path)
  (let ((ext (pathname-type path)))
    (cond ((or (string-equal ext "lisp") (string-equal ext "lsp") (string-equal ext "cl"))
           :common-lisp)
          ((or (string-equal ext "py"))
           :python)
          ((or (string-equal ext "js") (string-equal ext "ts"))
           :javascript)
          (t :default))))

(defun test-files-exist-p (path)
  "Check if test files exist for the given path."
  (let* ((dir (directory-namestring path))
         (name (pathname-name path))
         (test-patterns (list (format nil "~A/test-~A.*" dir name)
                              (format nil "~A/~A-test.*" dir name)
                              (format nil "~A/tests/*" dir))))
    (some #'probe-file test-patterns)))

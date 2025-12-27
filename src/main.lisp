(defpackage :agent-code/src/main
  (:use #:cl)
  (:import-from :log4cl)
  (:import-from :dexador)
  (:import-from :cl-json)
  (:import-from :rutils)
  (:import-from :serapeum)
  (:import-from :agent-code/src/llm)
  (:import-from :agent-code/src/tool)
  (:export
   #:main
   #:ask))

(in-package :agent-code/src/main)

(defparameter *ctx* nil)

(defun main ()
  (init))

(defun init (&key (force-init-p t))
  (if (or force-init-p (null *ctx*))
      (let* ((this (make-instance 'llm:llm)))
        (setf *ctx* this))))

(defun ask (query)
  (init :force-init-p nil)

  (let ((response (send-chat *ctx* query)))
    (log:info "~%~A" response)))

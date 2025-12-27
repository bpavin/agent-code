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

(defun main (project-path)
  (init :project-path project-path))

(defun init (&key (project-path nil) (force-init-p t))
  (if (or force-init-p (null *ctx*))
      (let* ((this (make-instance 'llm:llm
                    :project-path project-path
                    :tools (list (make-instance 'tool:read-tool)
                                 (make-instance 'tool:write-tool)
                                 (make-instance 'tool:delete-tool)))))
        (setf *ctx* this))))

(defun ask (query)

  (let ((response (llm:send-query *ctx* query)))
    (log:info "~%~A" response)))

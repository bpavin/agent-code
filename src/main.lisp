(defpackage :agent-code/src/main
  (:use #:cl)
  (:import-from :log4cl)
  (:import-from :dexador)
  (:import-from :rutils)
  (:import-from :serapeum)
  (:import-from :agent-code/src/llm)
  (:import-from :agent-code/src/api-provider)
  (:import-from :agent-code/src/tool)
  (:import-from :agent-code/src/llm-response)
  (:export
   #:main
   #:ask
   #:initial-analysis
   #:ask-analysis))

(in-package :agent-code/src/main)

(defparameter *ctx* nil)

(defun main (project-path)
  (log:config :debug)

  (if (null lparallel:*kernel*)
      (setf lparallel:*kernel* (lparallel:make-kernel 12)))

  (init :project-path project-path))

(defun init (&key (project-path nil) (force-init-p t))
  (if (or force-init-p (null *ctx*))
      (let* ((this (make-instance 'llm:llm
                    :project-path project-path
                    :project-summary (if (probe-file (format nil "~A/agent-code.md" project-path))
                                         (alexandria:read-file-into-string
                                          (format nil "~A/agent-code.md" project-path)))
                    ;; :api-provider (make-instance 'api-provider:chat-completion-api-provider)
                    :api-provider (make-instance 'api-provider:responses-api-provider)
                    :tools-enabled-p t
                    :deep-thinking-p t)))
        (setf *ctx* this))))

(defun initial-analysis ()
  (let* ((list-of-files (uiop:run-program (format nil "cd ~A && git ls-files --others --cached --exclude-standard"
                                                  (llm:project-path *ctx*))
                                          :output :string))
         (response (llm:send-query *ctx* persona:analyzing-persona
                                   (format nil "Analyse the project ~A. These are the files in the project: ~A"
                                           (llm:project-path *ctx*)
                                           list-of-files)
                                   nil)))
    (log:info "~%~A" response)))

(defun ask-analysis (query)
  (handler-bind ((conditions:llm-condition
                   (lambda (e)
                     (log:info "~A" e))))
      (llm:send-query *ctx* persona:analyzing-persona query nil)))

(defun ask (query)
  (setf query (append-file-content query))

  (let* ((persona persona:coordinator-persona))

    (handler-bind ((conditions:llm-condition
                     (lambda (e)
                       (conditions:print-log e))))
      (llm:send-query *ctx* persona query nil))))

(defun append-file-content (query)
  (if query
      (cl-ppcre:do-register-groups (file-path) ("@([^\\s]+)" query)
        (if (probe-file file-path)
            (setf query
                  (format nil "~A~% ----- ~A -----~%~A~%~%"
                          query file-path (alexandria:read-file-into-string file-path))))))

  query)

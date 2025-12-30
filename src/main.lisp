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
   #:ask
   #:initial-analysis
   #:ask-analysis))

(in-package :agent-code/src/main)

(defparameter *ctx* nil)

(defun main (project-path)
  (log:config :debug)
  (init :project-path project-path))

(defun init (&key (project-path nil) (force-init-p t))
  (if (or force-init-p (null *ctx*))
      (let* ((this (make-instance 'llm:llm
                    :project-path project-path
                    :project-summary (alexandria:read-file-into-string
                                      (format nil "~A/agent-code.md" project-path))
                    :tools (list (make-instance 'tool:read-many-files-tool)
                                 (make-instance 'tool:write-tool)
                                 (make-instance 'tool:git-tool)
                                 (make-instance 'tool:grep-tool)
                                 ;; (make-instance 'tool:delete-tool)
                                 ;; (make-instance 'tool:bash-tool)
                                 ))))
        (setf *ctx* this))))

(defun initial-analysis ()
  (let* ((list-of-files (uiop:run-program (format nil "cd ~A && git ls-files --others --cached --exclude-standard"
                                                  (llm:project-path *ctx*))
                                          :output :string))
         (response (llm:send-query *ctx* persona:analyzing-persona
                                   (format nil "Analyse the project ~A. These are the files in the project: ~A"
                                           (llm:project-path *ctx*)
                                           list-of-files))))
    (log:info "~%~A" response)))

(defun ask-analysis (query)
  (let ((response (llm:send-query *ctx* persona:analyzing-persona query)))
    (log:info "~%~A" response)))

(defun ask (query)
  (let ((response (llm:send-query *ctx* persona:planning-persona query)))
    (log:info "~%~A" response)))

(defpackage :agent-code/src/main
  (:use #:cl)
  (:import-from :log4cl)
  (:import-from :dexador)
  (:import-from :cl-json)
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
  (init :project-path project-path))

(defun init (&key (project-path nil) (force-init-p t))
  (if (or force-init-p (null *ctx*))
      (let* ((this (make-instance 'llm:llm
                    :project-path project-path
                    :project-summary (if (probe-file (format nil "~A/agent-code.md" project-path))
                                         (alexandria:read-file-into-string
                                          (format nil "~A/agent-code.md" project-path)))
                    :api-provider (make-instance 'api-provider:responses-api-provider)
                    :tools (list (make-instance 'tool:read-many-files-tool)
                                 ;; (make-instance 'tool:write-tool)
                                 ;; (make-instance 'tool:git-appy-patch-tool :project-directory project-path)
                                 ;; (make-instance 'tool:edit-file-tool)
                                 ;; (make-instance 'tool:git-tool)
                                 ;; (make-instance 'tool:grep-tool)
                                 ;; (make-instance 'tool:delete-tool)
                                 (make-instance 'tool:bash-tool)
                                 ))))
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
  (let ((response (llm:send-query *ctx* persona:analyzing-persona query nil)))
    (log:info "~%~A" response)))

(defun ask (query &key (mode :plan))
  (setf query (append-file-content query))

  (let* (history
         (persona (case mode
                    (:base persona:base-persona)
                    (:plan persona:planning-persona)
                    (:implement (progn
                                  (if (not (eq mode (llm:mode *ctx*)))
                                      (let* ((request (llm:last-in-history *ctx*))
                                             (funcalls
                                               (mapcan (lambda (response)
                                                         (if (or (string-equal "function_call" (llm-response:output-type response))
                                                                 (string-equal "function_call_output" (llm-response:output-type response)))
                                                             (list response)))
                                                       (llm:history *ctx*)))
                                             (tmp (llm:clear-history *ctx*)))
                                        (setf query request)
                                        (setf history funcalls)))
                                  persona:coding-persona))
                    (:analyze persona:analyzing-persona)))
         (tmp (progn (setf (llm:mode *ctx*) mode)))
         (response (llm:send-query *ctx* persona query history)))
    (log:info "~%~A" response)))

(defun append-file-content (query)
  (if query
      (cl-ppcre:do-register-groups (file-path) ("@([^\\s]+)" query)
        (if (probe-file file-path)
            (setf query
                  (format nil "~A~% ----- ~A -----~%~A~%~%"
                          query file-path (alexandria:read-file-into-string file-path))))))

  query)

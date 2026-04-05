(defpackage :agent-code/src/main
  (:use #:cl)
  (:import-from :log4cl)
  (:import-from :dexador)
  (:import-from :rutils)
  (:import-from :serapeum)
  (:import-from :agent-code/src/llm)
  (:import-from :agent-code/src/api-provider)
  (:import-from :agent-code/src/tool)
  (:import-from :agent-code/src/mcp)
  (:import-from :agent-code/src/personas)
  (:import-from :agent-code/src/llm-response)
  (:export
   #:main
   #:ask
   #:initial-analysis
   #:ask-analysis
   #:ask-implement))

(in-package :agent-code/src/main)

(defparameter *ctx* nil)

(defun main (project-path)
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
                    :mcps (list
                           ;; (make-instance 'mcp:mcp
                           ;;                     :name "deepwiki"
                           ;;                     :url "https://mcp.deepwiki.com/mcp")
                                ;; (make-instance 'mcp:mcp
                                ;;                :name "sequentialthinking"
                                ;;                :url "https://remote.mcpservers.org/sequentialthinking/mcp")
                                )
                    :tools-enabled-p t
                    :deep-thinking-p t)))
        (setf *ctx* this))))

(defun initial-analysis ()
  (let* ((list-of-files (uiop:run-program (format nil "cd ~A && git ls-files --others --cached --exclude-standard"
                                                  (llm:project-path *ctx*))
                                          :output :string))
         (response (llm:send-query *ctx* personas:analyzing-persona
                                   (format nil "Analyse the project ~A. These are the files in the project: ~A"
                                           (llm:project-path *ctx*)
                                           list-of-files)
                                   nil)))
    (log:info "~%~A" response)))

(defun ask-analysis (query)
  (handler-bind ((conditions:llm-condition
                   (lambda (e)
                     (log:info "~A" e))))
      (llm:send-query *ctx* personas:analyzing-persona query nil)))

(defun ask-implement (query)
  (when *ctx*
    (init :project-path (llm:project-path *ctx*)))
  (handler-bind ((conditions:llm-condition
                   (lambda (e)
                     (conditions:print-log e))))
    (iterative-code-validation *ctx* query)))

(defun ask (query &key (mode :coordinator))
  (setf query (append-file-content query))

  (handler-bind ((conditions:llm-condition
                   (lambda (e)
                     (conditions:print-log e))))
    (case mode
      (:base
       (let* ((persona personas:base-persona))
         (llm:send-query *ctx* persona query nil)))

      (:write
       (let* ((persona personas:writing-persona))
         (llm:send-query *ctx* persona query nil)))

      (:plan
       (let* ((persona personas:planning-persona))
         (llm:send-query *ctx* persona query nil)))

      (:implement
       (let* ((persona personas:coding-persona))
         (llm:send-query *ctx* persona query nil)))

      (:coordinator
       (let* ((persona personas:coordinator-persona))
         (llm:send-query *ctx* persona query nil))))))

(defun append-file-content (query)
  (if query
      (cl-ppcre:do-register-groups (file-path) ("@([^\\s]+)" query)
        (if (probe-file file-path)
            (setf query
                  (format nil "~A~% ----- ~A -----~%~A~%~%"
                          query file-path (alexandria:read-file-into-string file-path))))))
  query)

(defun iterative-code-validation (llm prompt &key (max-iterations 5))
  "Orchestrates coding-validation loop with failure recovery."
  (block validation-loop
    (do ((i 1 (1+ i))
         (current-prompt prompt))
        ((> i max-iterations)
         (error "Validation failed after ~D iterations" max-iterations))

      (let* ((coder personas:coding-persona)
             (coder-response (llm:send-query (llm:create-subagent llm coder) coder current-prompt nil))
             (validation-response (call-validator llm prompt coder-response)))

        (when (eq (getf validation-response :status) :success)
          (return-from validation-loop (values coder-response validation-response)))

        (setf current-prompt
              (format nil "Validation failed (~A): ~A. Fix errors and retry."
                      i
                      (getf validation-response :details)))))))

(defun call-validator (llm prompt coder-response)
  (handler-bind ((conditions:tool-response
                   (lambda (e)
                     (log:info "Validation executed with result: ~A" (conditions:result e))
                     (return-from call-validator (conditions:result e)))))

    (let ((validator personas:validator-persona))
      (llm:send-query (llm:create-subagent llm validator)
                     validator
                     (format nil "Validate this implementation: ~A" prompt)
                     nil))))

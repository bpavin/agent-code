(defpackage :agent-code/src/llm
	(:use :cl)
    (:nicknames :llm)
    (:import-from :cl-ppcre)
    (:import-from :cl-json)
    (:import-from :lparallel)
    (:import-from :defclass-std)
    (:import-from :agent-code/src/conditions)
    (:import-from :agent-code/src/api-provider)
    (:import-from :agent-code/src/persona)
    (:import-from :agent-code/src/tool)
    (:import-from :agent-code/src/mcp)
    (:import-from :agent-code/src/llm-response)
	(:export
     #:llm
     #:send-query
     #:project-path
     #:project-summary
     #:history
     #:api-provider
     #:last-subagent-response
     #:last-in-history
     #:clear-history
     #:mode
     #:iterative-code-validation
     #:subagent-tool
     ))

(in-package :agent-code/src/llm)

(defclass llm ()
  ((host :initform "http://localhost:11434"
         :accessor host)
   (model :initform "qwen3:8b"
          :accessor model)
   (weaker-model :initform nil
                 :accessor weaker-model
                 :documentation "Weaker model that can be used for a little less important tasks.")
   (api-key :initform nil
            :accessor api-key)
   (tools-enabled-p :initarg :tools-enabled-p
                    :initform t
                    :accessor tools-enabled-p)
   (mode :initarg :mode
         :initform :plan
         :accessor mode)
   (project-path :initform nil
                 :initarg :project-path
                 :accessor project-path)
   (project-summary :initform nil
                 :initarg :project-summary
                 :accessor project-summary)
   (api-provider :initform (make-instance 'api-provider:responses-api-provider)
                 :initarg :api-provider
                 :accessor api-provider)
   (history :initform nil
            :accessor history)
   (mcps :initform nil
         :initarg :mcps
         :accessor mcps)
   (tools :initform nil
          :initarg :tools
          :accessor tools)
   (tools-history :initform (make-hash-table :synchronized t)
                  :initarg :tools-history
                  :accessor tools-history
                  :documentation "Map of previous tool calls. Key is tool name+args used.")
   (deep-thinking-p :initform nil
                    :initarg :deep-thinking-p
                    :accessor deep-thinking-p)
   (last-subagent-response :initform (make-hash-table)
                           :accessor last-subagent-response)))

(defparameter loop-detector-persona
  (make-instance 'persona:persona
                 :name "loop-detector"
                 :description "Assistant that detects if the current AI conversation is stuck in a loop."
                 :use-weaker-model-p t
                 :tools (list (make-instance 'tool:loop-detection-tool))
                 :system
                 "Check the current conversation and determine if there is a duplication in the last 10 messages or tool calls.
You must use loop_detection tool as notify the user."
                 :parallel-p t))

(defmethod get-tools ((this llm) persona)
  (append (or (persona:tools persona) (tools this))))

(defmethod get-all-tools ((this llm) persona)
  (append (or (persona:tools persona) (tools this))
          (mapcan (lambda (mcp)
                    (mcp:tools mcp))
                  (mcps this))))

(defmethod send-query ((this llm) persona query history)

  (if (and (history this)
           (= 0 (mod (length (history this)) 10)))
      (detect-loop-in-conversation this))

  (if history
      (setf (history this) history))

  (if query
      (add-history this (llm-response:create-message :user query)))

  (let* ((api-response (send-request this persona query))
         (api-response-alist (cl-json:decode-json-from-string api-response)))

    (signal 'conditions:llm-response
            :text "LLM response" :json api-response
            :total-tokens (api-provider:get-total-tokens (api-provider this) api-response-alist))

    (handler-case
        (let ((llm-responses (api-provider:handle-response (api-provider this) api-response-alist)))
          (act-on-llm-response this persona llm-responses))
      (error (e)
        (send-query this persona
                    (format nil "Response was invalid: ~A" e)
                    history)))))

(defun detect-loop-in-conversation (llm)
  (signal 'conditions:llm-condition :text "Running loop detection.")
  (let* ((sub (create-subagent llm loop-detector-persona)))
    (handler-bind ((tool:no-loop (lambda (e)
                                   (declare (ignore e))
                                   (return-from detect-loop-in-conversation))))
      (send-query sub
                  loop-detector-persona
                  (persona:system loop-detector-persona)
                  (copy-list (history llm))))))

(defmethod send-request ((this llm) persona query)
  (let* ((conversation (get-history this persona))
         (model (resolve-model this persona))
         (content (api-provider:create-request
                   (api-provider this)
                   model
                   conversation
                   (if (tools-enabled-p this)
                       (get-all-tools this persona)))))

    (request-post this model content)))

(defmethod resolve-model ((this llm) persona)
  (if (persona:use-weaker-model-p persona)
      (or (weaker-model this) (model this))
      (model this)))

(defun request-post (this model content)
  (let (result
        (url (format nil "~A~A"
                     (host this)
                     (api-provider:url (api-provider this)))))

    (signal 'conditions:llm-request
            :text "LLM request" :model model :json content)

    (do ((retry t)
         (retry-count 0))
        ((null retry)
         result)
      (handler-case
          (progn
            (setf result
                  (dex:post url
                            :insecure t
                            :headers `(("Content-type" . "application/json")
                                       ,(if (api-key this)
                                            `("Authorization" . ,(format nil "Bearer ~A" (api-key this)))))
                            :content content))
            (setf retry nil))
        (dex:http-request-too-many-requests (e)
          (log:warn "~A" e)
          (if (> (incf retry-count) 10)
              (error e))
          (sleep 1))
        (dex:http-request-bad-request (e)
          (setf retry nil)
          (log:warn "request: ~A~%response: ~A" content e)
          (error e))))))

(defmethod compress-history ((this llm))
  "Remove all history entries that are considered old."
  (let* ((last-user-input (position-if (lambda (lr)
                                         (eq :user (llm-response:role lr)))
                                       (history this))))
    (if last-user-input
        (setf (history this)
              (subseq (history this) 0 last-user-input)))))

(defmethod clear-history ((this llm))
  (setf (history this) nil))

(defmethod last-in-history ((this llm))
  (car (history this)))

(defmethod add-history ((this llm) llm-response)
  (if llm-response
      (push llm-response (history this))))

(defmethod get-history ((this llm) persona)
  "Returns conversation history. History is first copied, before appending custom history items."
  (let* ((conversations (mapcar (alexandria:curry #'api-provider:create-response (api-provider this))
                                (reverse (history this)))))

    (if (project-path this)
        (push (api-provider:create-response
               (api-provider this)
               (llm-response:create-message
                :assistant (format nil "Project directory is ~A" (project-path this))))
              conversations))
    (if (project-summary this)
        (push (api-provider:create-response
               (api-provider this)
               (llm-response:create-message
                :assistant (project-summary this)))
              conversations))

    (if (not (tools-enabled-p this))
        (push (api-provider:create-response
               (api-provider this)
               (llm-response:create-message
                :assistant (format nil "Available tools.
Tool calls must be the only message, surrounded by JSON fences, but no other explanations.
Tools must be called in this format: {\"type\":\"function\",\"name\":\"replace-with-tool-name\",\"parameters\": <replace with parameters>}.
These are tool descriptions:~%~%~A"
                                   (responses-tools-as-json this persona))))
              conversations))

    (if (persona:user persona)
        (push `((:role . :user) (:content . ,(persona:get-user-prompt
                                              persona
                                              (get-tools this persona))))
              conversations))
    (if (persona:system persona)
        (push `((:role . :system) (:content . ,(persona:system persona)))
              conversations))
    (to-json-array conversations)))

(defun to-json-array (lst)
  (serapeum:string-join
   (mapcar #'cl-json:encode-json-alist-to-string lst)
   ","))

(defmethod responses-tools-as-json ((this llm) persona)
  (to-json-array
   (mapcar (lambda (tool)
             (tool:to-alist tool))
           (get-tools this persona))))

(defmethod act-on-llm-response ((this llm) persona llm-responses)
  "Convert raw LLM response to internal structures. Execute function calls if any."

  (let ((funcalls-p nil))
    (dolist (llm-response llm-responses)
      (cond ((or (string-equal "function_call" (llm-response:output-type llm-response))
                 (string-equal "function" (llm-response:output-type llm-response)))
             (setf funcalls-p T)
             (let (result (success "completed"))

               (handler-case
                   (setf result
                         (handle-function-call
                          this persona
                          (llm-response:name llm-response)
                          (llm-response:arguments llm-response)))
                 (error (e)
                   (log:warn "Tool call failed: [tool=~A, msg=~A]" (llm-response:name llm-response) e)
                   (setf result (format nil "~A" e)
                         success "incomplete")))

               (add-history this llm-response)
               (add-history this
                            (llm-response:create-function-output
                             llm-response success result))))))

    (dolist (llm-response llm-responses)
      (cond ((string-equal "message" (llm-response:output-type llm-response))
             (if (not funcalls-p)
                 (add-history this llm-response))
             (signal 'conditions:llm-response
                     :text (llm-response:text llm-response)))))

    (if funcalls-p
        (send-query this persona nil nil)
        (llm-response:text (car (history this))))))

(defmethod handle-function-call ((this llm) persona tool-name args)
  (let ((tool-called-p nil))
    (dolist (tool (get-tools this persona))
      (when (string-equal tool-name (tool:name tool))
        (setf tool-called-p t)

        (signal 'conditions:tool-call
                :text "Executing tool" :name tool-name :args args)

        (cond (T
               (let ((tool-result (tool:tool-execute tool args :llm this)))

                 (signal 'conditions:tool-response
                         :text "Tool executed successfully"
                         :name tool-name :args args :result tool-result)

                 (return-from handle-function-call tool-result))))))

    (if (null tool-called-p)
        (dolist (mcp (mcps this))
          (dolist (mcp-tool (mcp:tools mcp))
            (when (string-equal tool-name (tool:name mcp-tool))
              (setf tool-called-p t)

              (signal 'conditions:tool-call
                      :text "Executing MCP tool" :name tool-name :args args)

              (cond (T
                     (let ((tool-result (mcp:tool-execute mcp mcp-tool args)))

                       (signal 'conditions:tool-response
                               :text "MCP tool executed successfully"
                               :name tool-name :args args :result tool-result)

                       (return-from handle-function-call tool-result))))))))

    (when (null tool-called-p)
      (error "Tool was not found: ~A" tool-name))))

(defclass subagent-tool (tool:tool)
  ((tool:name :initform "execute_subagent")
   (tool:description :initform "Run standalone subagent to complete specific task.")
   (personas :initform nil
             :initarg :personas
             :accessor personas)
   (tool:properties)
   (tool:required :initform '(:name :prompt))
   (deep-thinking-p :initform nil
                    :accessor deep-thinking-p)))

(defmethod initialize-instance :after ((this subagent-tool) &rest args)
  (setf (tool:properties this)
        `((:name . ((:type . :string)
                    (:description . ,(format nil "Name of the subagent. Available subagents: ~A~%"
                                             (reduce (lambda (sum p)
                                                       (format nil "~AName: ~A~%Description: ~A~%"
                                                               sum
                                                               (persona:name p)
                                                               (persona:description p)))
                                                     (personas this)
                                                     :initial-value "")))))
          (:prompt . ((:type . :string)
                      (:description . "Instructions for the subagent."))))))

(defmethod tool:tool-execute ((tool subagent-tool) args &rest options)
  (if (null args)
      (error "No arguments specified."))

  (let* ((name (tool:aget args :name))
         (prompt (tool:aget args :prompt)))
    (if (null name)
        (error "Name not specified."))
    (if (null prompt)
        (error "Prompt not specified."))

    (destructuring-bind (&key (llm nil)) options
      (let* ((persona
               (find-if (lambda (p) (string-equal name (persona:name p)))
                        (personas tool))))

        (cond ((and (deep-thinking-p tool) (persona:parallel-p persona))
               (let* ((count 3)
                      (subs (create-subagents llm persona count))
                      (history (create-subagent-history llm persona)))

                 (signal 'conditions:tool-call
                         :text (format nil "Starting ~A subagent" count) :name name)

                 (let* ((results (lparallel:pmapcar
                                  (lambda (sub-llm)
                                    (llm:send-query sub-llm persona prompt history))
                                  subs))
                        (response (format nil "~{---- subagent response: -----~%~A~^~%~}" results)))

                   (put-last-subagent-response llm name response)

                   response)))

              (t
               (let* ((subagent (create-subagent llm persona))
                      (history (create-subagent-history llm persona))
                      (response (send-query subagent persona
                                            prompt history)))

                 (put-last-subagent-response llm name response)

                 response)))))))

(defun create-subagent-history (llm persona)
  (append (get-last-subagent-response llm persona)
          (if (not (string-equal (persona:before-in-chain persona) (persona:name persona)))
              (get-last-subagent-response llm nil :name (persona:name persona)))))

(defun get-last-subagent-response (llm persona &key name)
  (if (or persona name)
      (multiple-value-bind (i existsp)
          (gethash (sxhash (if name name (persona:before-in-chain persona)))
                   (llm:last-subagent-response llm))
        (if existsp
            (list (llm-response:create-message :assistant i))))))

(defun put-last-subagent-response (llm name response)
  (setf (gethash (sxhash name) (llm:last-subagent-response llm))
        response))

(defun create-subagents (llm persona count)
  (let ((subagents)
        (temps '(0.4 0.6 1.0)))
   (dotimes (i count subagents)
     (let ((sub (create-subagent llm persona)))
       (setf (api-provider:temperature (llm:api-provider sub))
             (nth i temps))
       (push sub subagents)))))

(defun create-subagent (llm persona)
  (make-instance 'llm:llm
                 :project-path (project-path llm)
                 :project-summary (project-summary llm)
                 :tools (persona:tools persona)))

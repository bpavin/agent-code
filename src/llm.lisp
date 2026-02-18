(defpackage :agent-code/src/llm
	(:use :cl)
    (:nicknames :llm)
    (:import-from :cl-ppcre)
    (:import-from :lparallel)
    (:import-from :defclass-std)
    (:import-from :agent-code/src/api-provider)
    (:import-from :agent-code/src/persona)
    (:import-from :agent-code/src/tool)
    (:import-from :agent-code/src/llm-response)
	(:export
     #:llm
     #:send-query
     #:project-path
     #:project-summary
     #:history
     #:memory
     #:last-in-history
     #:clear-history
     #:mode))

(in-package :agent-code/src/llm)

(defclass llm ()
  ((host :initform "http://localhost:11434"
         :accessor host)
   (model :initform "qwen3:8b"
          :accessor model)
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
   (memory-enabled-p :initform nil
                     :initarg :memory-enabled-p
                     :accessor memory-enabled-p)
   (memory :initform nil
           :initarg :memory
           :accessor memory)
   (memory-tool :initform (make-instance 'memory-tool)
                :accessor memory-tool)
   (tools :initform (list (make-instance 'subagent-tool))
          :initarg :tools
          :accessor tools)
   (deep-thinking-p :initform t
                    :initarg :deep-thinking-p
                    :accessor deep-thinking-p)))

(defmethod get-tools ((this llm) persona)
  (append (or (persona:tools persona) (tools this))
          (if (memory-enabled-p this)
              (list (memory-tool this)))))

(defmethod send-query ((this llm) persona query history)
  (if history
      (setf (history this) history))
  (if query
      (add-history this (llm-response:create-message :user query)))

  (let* ((api-response (call-chat-completion this persona query)))

    (when (log:debug)
      (log:debug "LLM response: ~A" (cl-ppcre:regex-replace-all "\\s+" api-response " ")))

    (let ((llm-responses (api-provider:handle-response (api-provider this) api-response)))

      (act-on-llm-response this persona llm-responses))))

(defmethod compress-history ((this llm))
  "Remove all history entries that are considered old."
  (let* ((last-user-input (position-if (lambda (lr)
                                         (eq :user (llm-response:role lr)))
                                       (history this))))
    (if last-user-input
        (setf (history this)
              (subseq (history this) 0 last-user-input)))))

(defmethod call-chat-completion ((this llm) persona query)
  (let* ((conversation (get-history this persona))
         (content (api-provider:create-request
                   (api-provider this)
                   (model this)
                   conversation
                   (if (tools-enabled-p this)
                       (get-tools this persona)))))

    (request-post this content)))

(defun request-post (this content)
  (let ((url (format nil "~A~A" (host this) (api-provider:url (api-provider this)))))
    (when (log:debug)
      (log:debug "~A~%~A" url (cl-ppcre:regex-replace-all "\\s+" content " ")))

    (dex:post url
              :insecure t
              :read-timeout 60000
              :headers `(("Content-type" . "application/json")
                         ,(if (api-key this)
                              `("Authorization" . ,(format nil "Bearer ~A" (api-key this)))))
              :content content)))

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

    (if (memory-enabled-p this)
        (push (api-provider:create-response
               (api-provider this)
               (llm-response:create-message
                :assistant (format nil "CRITICAL:
Use ~A tool to keep track of important details about conversation.
Memory list will be always visibile to you.
Keep memory list concise, up to date, and use it often.
Use memory list as a longterm memory.
Using this tool will remove past conversation and only the last 5 messages will be visible to you.

==== MEMORY LIST ====
~A
"
                                   +memory-tool-name+
                                   (forge-memory-list (memory this)))))
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

(defun forge-memory-list (memory)
  (if (null memory)
      "Memory list is empty."
      (let ((count 0))
        (serapeum:string-join
         (mapcan (lambda (i)
                   (list (format nil "~A. --- note start ---
~A
--- note end ---"
                                 (incf count) i)))
                 memory)
         #\NewLine))))

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
             (log:info (llm-response:text llm-response)))))

    (if funcalls-p
        (send-query this persona nil nil)
        (llm-response:text (car (history this))))))

(defmethod handle-function-call ((this llm) persona tool-name args)
  (let ((tool-called-p nil))
   (dolist (tool (get-tools this persona))
     (when (string-equal tool-name (tool:name tool))
       (setf tool-called-p t)
       (log:debug "Executing tool [name=~A, args=~A]" tool-name args)
       (cond ((string-equal tool-name +memory-tool-name+)
              (let ((memory-result (update-memory tool this args)))
                (compress-history this)
                (return-from handle-function-call memory-result)))

             (T
              (let ((tool-result (tool:tool-execute tool this args)))

                (log-tool-result tool-name args tool-result)

                (return-from handle-function-call tool-result))))))

    (when (null tool-called-p)
      (error "Tool was not found: ~A" tool-name))))

(defun log-tool-result (tool-name args tool-result)
  (if (log:trace)
      (log:trace "Tool executed [name=~A, args=~A, result=~A]" tool-name args tool-result)
      (log:debug "Tool executed successfully [name=~A]" tool-name)))

(defparameter +memory-tool-name+ "update_memory")

(defclass memory-tool (tool:tool)
  ((tool:name :initform +memory-tool-name+)
   (tool:description
    :initform "Tool used for retaining key information. Use this often to store key information about previous conversation.
Critical: Calling this tool will remove previous conversations, so make sure to include all information that is important for context.")
   (tool:properties :initform '((:operation . ((:type . :string)
                                          (:description . "Operation for updating memory. Must be one of: INSERT, UPDATE, REMOVE. You can insert new memory item or update/delete existing.")))
                           (:index . ((:type . :integer)
                                      (:description . "Memory is kept in a list. Each memory item is prefixed with index.
Use this index to specify which memory item you want to update. Index is mandatory for update and delete.")))
                           (:content . ((:type . :string)
                                        (:description . "Information you want to update memory with. Content is mandatory for insert and update.")))))
   (tool:required :initform '(:operation))))

(defmethod update-memory ((tool memory-tool) llm args)
  (if (null args)
      (error "No arguments specified."))

  (let* ((operation (tool:aget args :operation)))
    (if (null operation)
        (error "Operation is not specified."))

    (alexandria:switch (operation :test #'string-equal)
      ("insert"
       (setf (llm:memory llm)
             (append (llm:memory llm)
                     (list (tool:aget args :content)))))
      ("update"
       (let ((i (- (parse-integer (tool:aget args :index)) 1)))
         (setf (nth i (llm:memory llm))
               (tool:aget args :content))))
      ("remove"
       (setf (llm:memory llm)
             (delete (nth (tool:aget args :index) (llm:memory llm))
                     (llm:memory llm))))
      (t
       (error "Invalid operation: ~A" operation)))
    "Memory successfully updated."))

(defclass subagent-tool (tool:tool)
  ((tool:name :initform "execute_subagent")
   (tool:description :initform "Run standalone subagent to complete specific task.")
   (personas :initform (list ;persona:analyzing-persona
                             ;persona:explore-persona
                             persona:planning-persona
                             persona:coding-persona)
             :accessor personas)
   (tool:properties)
   (tool:required :initform '(:name :prompt))))

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

(defmethod tool:tool-execute ((tool subagent-tool) llm args)
  (if (null args)
      (error "No arguments specified."))

  (let* ((name (tool:aget args :name))
         (prompt (tool:aget args :prompt)))
    (if (null name)
        (error "Name not specified."))
    (if (null prompt)
        (error "Prompt not specified."))

    (let* ((persona
             (find-if (lambda (p) (string-equal name (persona:name p)))
                      (personas tool))))

      (cond ((and (deep-thinking-p llm) (persona:parallel-p persona))
             (let* ((count 3)
                    (subs (create-subagents llm persona count))
                    (response (last-in-history llm))
                    (history (if (llm-response:role response)
                                 (list response))))

               (log:info "Starting ~A subagents ~A" count name)
               (let ((results (lparallel:pmapcar
                               (lambda (sub-llm)
                                 (llm:send-query sub-llm persona prompt history))
                               subs)))
                 (format nil "~{---- subagent response: -----~%~A~^~%~%~}" results))))

            (t
             (let ((subagent (make-instance 'llm:llm
                                            :project-path (project-path llm)
                                            :project-summary (project-summary llm)
                                            :tools (persona:tools persona))))
                   (log:info "Starting subagent ~A" name)
                   (llm:send-query subagent persona prompt nil)))))))

(defun create-subagents (llm persona count)
  (let ((subagents)
        (temps '(0.4 0.6 1.0)))
   (dotimes (i count subagents)
     (let ((sub (make-instance 'llm:llm
                               :project-path (project-path llm)
                               :project-summary (project-summary llm)
                               :tools (persona:tools persona))))
       (setf (api-provider:temperature (api-provider sub))
             (nth i temps))
      (push sub subagents)))))

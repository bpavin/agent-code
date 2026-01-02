(defpackage :agent-code/src/llm
	(:use :cl)
    (:nicknames :llm)
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
     #:last-in-history
     #:clear-history))

(in-package :agent-code/src/llm)

(defclass llm ()
  ((url :initform "http://localhost:11434"
        :accessor url)
   (model :initform "qwen3:8b"
          :accessor model)
   (chat-completion :initform "/v1/chat/completions"
                    :accessor chat-completion)
   (responses :initform "/v1/responses"
                    :accessor responses)
   (project-path :initform nil
                 :initarg :project-path
                 :accessor project-path)
   (project-summary :initform nil
                 :initarg :project-summary
                 :accessor project-summary)
   (api-provider :initarg :api-provider
                 :accessor api-provider)
   (history :initform nil
            :accessor history)
   (tools :initarg :tools
          :accessor tools)))

(defmethod call-chat-completion ((this llm) persona query)
  (let* ((conversation (get-history this persona))
         (content (api-provider:create-request
                   (api-provider this)
                   (model this)
                   conversation
                   (or (persona:tools persona) (tools this)))))

    (request-post this content)))

(defmethod call-responses ((this llm) persona query)
  (let* ((conversation (get-history this persona))
         (content (format nil
                          "{
                            \"model\": ~A,
                            \"input\": [
                                ~A
                            ],
                            \"tools\": [
                                ~A
                            ],
                            \"summary\": \"concise\",
                            \"stream\": false,
                            \"temperature\": 0.6,
                            \"max_tokens\": 2000
                          }"
                          (cl-json:encode-json-to-string (model this))
                          conversation
                          (responses-tools-as-json this persona))))

    (request-post this content)))

(defmethod responses-tools-as-json ((this llm) persona)
  (to-json-array
   (mapcar (lambda (tool)
             (tool:to-alist tool))
           (or (persona:tools persona)
               (tools this)))))

(defun request-post (this content)
    (when (log:debug)
      (log:debug content))

  (dex:post (format nil "~A~A" (url this) (chat-completion this))
              :insecure t
              :read-timeout 60000
              :headers '(("Content-type" . "application/json"))
              :content content))



(defmethod clear-history ((this llm))
  (setf (history this) nil))

(defmethod last-in-history ((this llm))
  (let ((msg (pop (history this))))
    (alexandria:assoc-value msg :content)))

(defmethod add-history ((this llm) role content)
  (if (and role content)
      (push `((:role . ,role) (:content . ,content)) (history this))))

(defmethod push-history ((this llm) alist)
  (if alist
      (push alist (history this))))

(defmethod get-history ((this llm) persona)
  "Returns conversation history. History is first copied, before appending custom history items."
  (let* ((conversations (reverse (history this))))
    (if (persona:user persona)
        (push `((:role . :user) (:content . ,(persona:get-user-prompt persona
                                                                      (or (persona:tools persona) (tools this)))))
              conversations))
    (if (persona:system persona)
        (push `((:role . :system) (:content . ,(persona:system persona)))
              conversations))
    (to-json-array conversations)))

(defun to-json-array (lst)
  (serapeum:string-join
   (mapcar #'cl-json:encode-json-alist-to-string lst)
   ","))

(defmethod send-query ((this llm) persona query)
  (when (null (history this))
    (add-history this :assistant (format nil "Project directory is ~A" (project-path this)))
    (add-history this :assistant (project-summary this))
    (add-history this :assistant (format nil "Available tools:~%~%~A" (responses-tools-as-json this persona))))

  (when query
    (add-history this :user query))

  (let* ((api-response (call-chat-completion this persona query)))

    (when (log:debug)
      (log:debug "LLM response: ~A" api-response))

    (let ((llm-responses (api-provider:handle-response (api-provider this) api-response)))

      (act-on-llm-response this persona llm-responses))))

(defmethod handle-response ((this llm) persona api-response)
  (let* ((api-alist (cl-json:decode-json-from-string api-response))
         (llm-responses))

    (dolist (output (alexandria:assoc-value api-alist :output))
      (let* ((result (rutils:-> (alexandria:assoc-value output :content)
                         car
                         (alexandria:assoc-value rutils:% :text)
                         sanitize)))

        (add-history this (alexandria:assoc-value output :role) result)

        (push (make-instance 'llm-response:llm-response
                             :output-type (alexandria:assoc-value output :type)
                             :call-id (alexandria:assoc-value output :call--id)
                             :name (alexandria:assoc-value output :name)
                             :arguments (alexandria:assoc-value output :arguments)
                             :role (alexandria:assoc-value output :role)
                             :text result)
              llm-responses)))

    (act-on-llm-response this persona llm-responses)))

(defun sanitize (str)
  (cl-ppcre:regex-replace-all "```(json)?" str ""))

(defmethod act-on-llm-response ((this llm) persona llm-responses)
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

               ;; (push-history this `((:type . :function--call)
               ;;                      (:call--id . ,(llm-response:call-id llm-response))
               ;;                      (:name . ,(llm-response:name llm-response))
               ;;                      (:arguments . ,(llm-response:arguments llm-response))))
               ;; (push-history this `((:type . :function--call--output)
               ;;                      (:call--id . ,(llm-response:call-id llm-response))
               ;;                      (:output . ,result)
               ;;                      (:status . ,success)))

               (push-history this (api-provider:create-response (api-provider this) llm-response))
               (push-history this (api-provider:create-response
                                   (api-provider this)
                                   (make-instance 'llm-response:llm-response
                                                  :output-type :function--call--output
                                                  :name (llm-response:name llm-response)
                                                  :arguments (llm-response:arguments llm-response)
                                                  :status success
                                                  :text result)))
               ))

            ((string-equal "message" (llm-response:output-type llm-response))
             (add-history this
                          (llm-response:role llm-response)
                          (llm-response:text llm-response))
             (log:info (llm-response:text llm-response)))))

    (if funcalls-p
        (send-query this persona nil))))

(defmethod handle-function-call ((this llm) persona tool-name args)
  (dolist (tool (or (persona:tools persona) (tools this)))
    (when (string-equal tool-name (tool:name tool))
      (log:debug "Executing tool [name=~A, args=~A]" tool-name args)
      (let ((tool-result (tool:tool-execute tool args)))
        (log:debug "Tool executed successfully [name=~A, args=~A]" tool-name args)
        (if (log:trace)
            (log:trace "Tool executed [name=~A, args=~A, result=~A]" tool-name args tool-result))
        (return-from handle-function-call tool-result)))))

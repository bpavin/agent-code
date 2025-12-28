(defpackage :agent-code/src/llm
	(:use :cl)
    (:nicknames :llm)
    (:import-from :defclass-std)
    (:import-from :agent-code/src/persona)
    (:import-from :agent-code/src/tool)
	(:export
     #:llm
     #:send-query
     #:project-path
     #:project-summary))

(in-package :agent-code/src/llm)

(defclass llm ()
  ((url :initform "http://localhost:11434"
        :accessor url)
   (chat-completion :initform "/v1/responses"
                    :accessor chat-completion)
   (model :initform "qwen2.5-coder:7b"
          :accessor model)
   (project-path :initform nil
                 :initarg :project-path
                 :accessor project-path)
   (project-summary :initform nil
                 :initarg :project-summary
                 :accessor project-summary)
   (history :initform nil
            :accessor history)
   (tools :initarg :tools
          :accessor tools)))

(defmethod call-chat-completion ((this llm) persona query)
  (let* ((conversation (get-history this (persona:system persona)))
         (content (format nil
                          "{
                            \"model\": ~A,
                            \"input\": [
                                ~A
                            ],
                            \"tools\": [
                                ~A
                            ],
                            \"stream\": false,
                            \"max_tokens\": 1000
                          }"
                          (cl-json:encode-json-to-string (model this))
                          conversation
                          (include-tools-as-json this))))
    (when (log:debug)
      (log:debug content))

    (dex:post (format nil "~A~A" (url this) (chat-completion this))
              :insecure t
              :read-timeout 60000
              :headers '(("Content-type" . "application/json"))
              :content content)))

(defmethod include-tools-as-json ((this llm))
  (to-json-array
   (mapcar (lambda (tool)
             (tool:to-alist tool))
           (tools this))))

(defmethod add-history ((this llm) role content)
  (push `((:role . ,role) (:content . ,content)) (history this)))

(defmethod get-history ((this llm) system-prompt)
  (let* ((conversations (reverse (history this))))
    (push `((:role . :system) (:content . ,system-prompt))
          conversations)
    (to-json-array conversations)))

(defun to-json-array (lst)
  (serapeum:string-join
   (mapcar #'cl-json:encode-json-alist-to-string lst)
   ","))

(defmethod send-query ((this llm) persona query)
  (when (null (history this))
    (add-history this :assistant (format nil "Project directory is ~A" (project-path this)))
    (add-history this :assistant (project-summary this)))

  (when query
    (add-history this :user query))

  (let* ((api-response (call-chat-completion this persona query)))

    (when (log:debug)
      (log:debug "LLM response: ~A" api-response))

    (handle-response this persona api-response)))

(defmethod handle-response ((this llm) persona api-response)
  (let* ((api-alist (cl-json:decode-json-from-string api-response))
         (llm-response))

    (dolist (output (alexandria:assoc-value api-alist :output) llm-response)
      (let* ((result (rutils:-> (alexandria:assoc-value output :content)
                         car
                         (alexandria:assoc-value rutils:% :text)
                         sanitize)))

        (add-history this (alexandria:assoc-value output :role) result)
        (setf llm-response result)))

    (act-on-response this persona llm-response)))

(defun sanitize (str)
  (cl-ppcre:regex-replace-all "```(json)?" str ""))

(defmethod act-on-response ((this llm) persona result)
  (let ((result-alist (handler-case
                          (cl-json:decode-json-from-string result)
                        (error (e)
                          (declare (ignore e))))))

    (if (or (null result-alist)
            (not (listp result-alist)))
        (return-from act-on-response result))

   (let ((explanation (alexandria:assoc-value result-alist :explanation)))
     (when explanation
       (return-from act-on-response explanation)))

    (let ((tool-name (alexandria:assoc-value result-alist :name)))
     (when tool-name
       (let ((args (alexandria:assoc-value result-alist :arguments)))

         (dolist (tool (tools this))
           (when (string-equal tool-name (tool:name tool))
             (log:debug "Executing tool [name=~A, args=~A]" tool-name args)
             (add-history this :assistant (tool:tool-execute tool args))))

         (send-query this persona nil))))))

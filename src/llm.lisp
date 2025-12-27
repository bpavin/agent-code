(defpackage :agent-code/src/llm
	(:use :cl)
    (:nicknames :llm)
    (:import-from :agent-code/src/tool)
	(:export
     #:llm
     #:send-query))

(in-package :agent-code/src/llm)

(defclass llm ()
  ((url :initform "http://localhost:11434"
        :accessor url)
   (chat-completion :initform "/v1/chat/completions"
                    :accessor chat-completion)
   (model :initform "qwen2.5-coder:7b"
          :accessor model)
   (project-path :initform nil
                 :initarg :project-path
                 :accessor project-path)
   (history :initform nil
            :accessor history)
   (tools :initarg :tools
          :accessor tools)))

(defmethod call-chat-completion ((this llm) query)
  (let* ((system-prompt
           (format nil "You are an automated coding agent operating inside a real codebase.

Your job is to modify files to fulfill the user’s request.

Rules:
- Do not explain your reasoning unless explicitly asked
- Do not output prose or markdown
- Do not modify files unnecessarily
- Preserve formatting, comments, and style
- Prefer minimal diffs
- If the request is ambiguous or risky, ask a clarifying question
- Never invent files that do not exist unless instructed
- You are working in project ~A
- Assume changes will be applied directly to disk
- If your output is not valid JSON, you have failed the task
- Result must be disk interaction or asking user for more information

You must respond with valid JSON only.

Rules:
- Output must be valid, parseable JSON.
- Output can be JSON array including multiple JSON objects
- If information is missing, use null.
- Do not add any text outside the JSON.
"
                   (project-path this)))

         (assistant-prompt (get-history this))
         (content (format nil
                   "{
                        \"model\": ~A,
                        \"messages\": [
                            {\"role\": \"system\", \"content\": ~A},
                            {\"role\": \"assistant\", \"content\": ~A},
                            {\"role\": \"user\", \"content\": ~A}
                        ],
                        \"tools\": [
                            ~A
                        ],
                        \"stream\": false,
                        \"max_tokens\": 1000
                    }"
                   (cl-json:encode-json-to-string (model this))
                   (cl-json:encode-json-to-string system-prompt)
                   (cl-json:encode-json-to-string assistant-prompt)
                   (cl-json:encode-json-to-string query)
                   (include-tools-as-json this))))
    (when (log:debug)
      (log:debug content))

    (dex:post (format nil "~A~A" (url this) (chat-completion this))
              :insecure t
              :read-timeout 60000
              :headers '(("Content-type" . "application/json"))
              :content content)))

(defmethod include-tools-as-json ((this llm))
  (serapeum:string-join
   (mapcar (lambda (tool)
             (cl-json:encode-json-alist-to-string (tool:to-alist tool)))
           (tools this))
   ","))

(defmethod get-history ((this llm))
  (serapeum:string-join (reverse (history this)) ""))

(defmethod send-query ((this llm) query)
  (let* ((response (call-chat-completion this query))
         (alist (cl-json:decode-json-from-string response)))

    (when (log:debug)
      (log:debug "LLM response: ~A" alist))

    (push (sanitize query) (history this))

    (dolist (choice (alexandria:assoc-value alist :choices))
      (let ((result (rutils:-> (alexandria:assoc-value choice :message)
                        (alexandria:assoc-value rutils:% :content))))

        (push (sanitize result) (history this))
        (return result)))))

(defun sanitize (str)
  (cl-ppcre:regex-replace-all "\\r\\n|\\n|\\t|\\r" str ""))

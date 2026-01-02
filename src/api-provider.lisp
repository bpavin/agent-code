(defpackage :agent-code/src/api-provider
	(:use :cl)
    (:nicknames :api-provider)
    (:import-from :defclass-std)
    (:import-from :agent-code/src/tool)
    (:import-from :agent-code/src/llm-response)
	(:export
        #:api-provider
        #:create-request
        #:handle-response

        ;; impls
        #:chat-completion-api-provider
        #:responses-api-provider
        #:create-response))

(in-package :agent-code/src/api-provider)

(defclass-std:defclass/std api-provider () ())

(defgeneric create-request (this model conversation tools))

(defgeneric handle-response (this api-response))

(defgeneric create-response (this llm-response))

(defclass-std:defclass/std chat-completion-api-provider (api-provider)
  ())

(defmethod create-request ((this chat-completion-api-provider) model conversation tools)
  (format nil
          "{
                \"model\": ~A,
                \"messages\": [
                    ~A
                ],
                ~A
                \"summary\": \"concise\",
                \"stream\": false,
                \"temperature\": 0.6,
                \"max_tokens\": 2000
            }"
          (cl-json:encode-json-to-string model)
          conversation
          (if (null tools) ""
              (format nil
                      "\"tools\": [
                          ~A
                      ],"
                      (chat-completion-tools-as-json tools)))))

(defun chat-completion-tools-as-json (tools)
  (to-json-array
   (mapcar (lambda (tool)
             `((:type . :function)
               (:function . ((:name . ,(tool:name tool))
                             (:description . ,(tool:description tool))
                             (:parameters . ((:type . :object)
                                             (:properties . ,(tool:properties tool))
                                             (:required . ,(tool:required tool))))))))
           tools)))

(defun to-json-array (lst)
  (serapeum:string-join
   (mapcar #'cl-json:encode-json-alist-to-string lst)
   ","))

(defmethod handle-response ((this chat-completion-api-provider) api-response)
  (let* ((api-alist (cl-json:decode-json-from-string api-response))
         (llm-responses))

    (dolist (choice (alexandria:assoc-value api-alist :choices))

      (let* ((message-alist
               (alexandria:assoc-value choice :message))
             (result
               (alexandria:assoc-value message-alist :content)))

        (dolist (json-alist (extract-jsons result))
          (let ((json-type (alexandria:assoc-value json-alist :type)))
            (when (or (string-equal json-type "function")
                      (alexandria:assoc-value json-alist :parameters))
              (push (make-instance 'llm-response:llm-response
                                   :output-type "function"
                                   :name (alexandria:assoc-value json-alist :name)
                                   :arguments (alexandria:assoc-value json-alist :parameters))
                    llm-responses))))

        (push (make-instance 'llm-response:llm-response
                             :output-type "message"
                             :name "message"
                             :role (alexandria:assoc-value message-alist :role)
                             :text result)
              llm-responses)))

    llm-responses))

(defun extract-jsons (str)
  (let (results)
    (cl-ppcre:do-register-groups (raw-json) ("```json([^`]*)?```" str)
      (push (cl-json:decode-json-from-string raw-json) results))
    (nreverse results)))

(defmethod create-response ((this chat-completion-api-provider) llm-response)
  (case (llm-response:output-type llm-response)
    (:function--call--output
     `((:role . :assistant)
       (:content . ,(cl-json:encode-json-alist-to-string
                     `((:type . :function)
                       (:name . ,(llm-response:name llm-response))
                       (:parameters . ,(llm-response:arguments llm-response))
                       (:output . ,(llm-response:text llm-response))
                       (:success . ,(llm-response:status llm-response)))))))))

(defclass-std:defclass/std responses-api-provider (api-provider)
  ())

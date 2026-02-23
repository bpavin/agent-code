(defpackage :agent-code/src/api-provider
	(:use :cl)
    (:nicknames :api-provider)
    (:import-from :defclass-std)
    (:import-from :agent-code/src/tool)
    (:import-from :agent-code/src/llm-response)
	(:export
        #:api-provider
        #:url
        #:temperature
        #:create-request
        #:handle-response

        ;; impls
        #:chat-completion-api-provider
        #:responses-api-provider
        #:create-response))

(in-package :agent-code/src/api-provider)

(defclass-std:defclass/std api-provider ()
  ((url)
   (temperature :std 0.6)))

(defgeneric create-request (this model conversation tools))

(defgeneric handle-response (this api-response))

(defgeneric create-response (this llm-response))

(defclass-std:defclass/std chat-completion-api-provider (api-provider)
  ((url :std "/v1/chat/completions")))

(defmethod create-request ((this chat-completion-api-provider) model conversation tools)
  (format nil
          "{
                \"model\": ~A,
                \"messages\": [
                    ~A
                ],
                ~A
                \"stream\": false,
                \"temperature\": ~F,
                \"max_tokens\": 20000
            }"
          (cl-json:encode-json-to-string model)
          conversation
          (if (null tools) ""
              (format nil
                      "\"tools\": [
                          ~A
                      ],"
                      (chat-completion-tools-as-json tools)))
          (temperature this)))

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
               (alexandria:assoc-value message-alist :content))
             (tool-calls-p nil))

        (dolist (tool-alist (alexandria:assoc-value message-alist :tool--calls))
          (setf tool-calls-p t)
          (let ((fn-info (alexandria:assoc-value tool-alist :function)))
            (push (make-instance 'llm-response:llm-response
                                 :output-type "function_call"
                                 :call-id (format nil "call_~A" (serapeum:random-in-range 0 10000))
                                 :name (alexandria:assoc-value fn-info :name)
                                 :arguments (decode-json (alexandria:assoc-value fn-info :arguments)))
                  llm-responses)))

        (if (not tool-calls-p)
            (dolist (json-alist (extract-jsons result))
              (if (and json-alist (listp json-alist))
                  (let ((json-type (alexandria:assoc-value json-alist :type)))
                    (when (or (string-equal json-type "function")
                              (string-equal json-type "function_call")
                              (alexandria:assoc-value json-alist :parameters))
                      (push (make-instance 'llm-response:llm-response
                                           :output-type "function_call"
                                           :call-id (format nil "call_~A" (serapeum:random-in-range 0 10000))
                                           :name (alexandria:assoc-value json-alist :name)
                                           :arguments (alexandria:assoc-value json-alist :parameters))
                            llm-responses))))))

        (if (not (string-equal "" result))
            (push (llm-response:create-message (alexandria:assoc-value message-alist :role) result)
                  llm-responses))))

    llm-responses))

(defun extract-jsons (str)
  (if (not (string-equal "" str))
      (let (results)
        (cl-ppcre:do-register-groups (raw-json) ("```json([^`]*)?```" str)
          (push (decode-json raw-json) results))
        (if (null results)
            (list (decode-json str))
            (nreverse results)))))

(defmethod create-response ((this chat-completion-api-provider) llm-response)
  (alexandria:switch ((llm-response:output-type llm-response) :test #'string-equal)
    ("message"
     `((:role . ,(llm-response:role llm-response))
       (:content . ,(llm-response:text llm-response))))
    ("function_call"
     `((:role . :assistant)
       (:content . ,(cl-json:encode-json-alist-to-string
                     `((:type . :function--call)
                       (:call--id . ,(llm-response:call-id llm-response))
                       (:name . ,(llm-response:name llm-response))
                       (:parameters . ,(llm-response:arguments llm-response)))))))
    ("function_call_output"
     `((:role . :assistant)
       (:content . ,(cl-json:encode-json-alist-to-string
                     `((:type . :function--call--output)
                       (:call--id . ,(llm-response:call-id llm-response))
                       (:name . ,(llm-response:name llm-response))
                       (:parameters . ,(llm-response:arguments llm-response))
                       (:output . ,(llm-response:text llm-response))
                       (:success . ,(llm-response:status llm-response)))))))))

(defclass-std:defclass/std responses-api-provider (api-provider)
  ((url :std "/v1/responses")))

(defmethod create-request ((this responses-api-provider) model conversation tools)
  (format nil
          "{
                \"model\": ~A,
                \"input\": [
                    ~A
                ],
                ~A
                \"stream\": false,
                \"temperature\": ~F,
                \"max_output_tokens\": 20000
          }"
          (cl-json:encode-json-to-string model)
          conversation
          (if (null tools) ""
              (format nil
                      "\"tools\": [
                          ~A
                      ],"
                      (responses-tools-as-json this tools)))
          (temperature this)))

(defmethod responses-tools-as-json ((this responses-api-provider) tools)
  (to-json-array
   (mapcar (lambda (tool)
             (tool:to-alist tool))
           tools)))

(defmethod handle-response ((this responses-api-provider) api-response)
  (let* ((api-alist (cl-json:decode-json-from-string api-response))
         (llm-responses))

    (dolist (output (alexandria:assoc-value api-alist :output))
      (let* ((result (rutils:-> (alexandria:assoc-value output :content)
                         car
                         (alexandria:assoc-value rutils:% :text))))

        (push (make-instance 'llm-response:llm-response
                             :id (alexandria:assoc-value output :id)
                             :output-type (alexandria:assoc-value output :type)
                             :call-id (alexandria:assoc-value output :call--id)
                             :name (alexandria:assoc-value output :name)
                             :arguments (decode-json (alexandria:assoc-value output :arguments) :throwp t)
                             :role (alexandria:assoc-value output :role)
                             :text result)
              llm-responses)))

    (nreverse llm-responses)))

(defun sanitize (str)
  (cl-ppcre:regex-replace-all "```(json)?" str ""))

(defun decode-json (str &key (throwp nil))
  (if str
      (handler-case
          (cl-json:decode-json-from-string str)
        (error (e)
          (log:warn "Invalid JSON: ~A" e)
          (format nil "~A" e)
          (if throwp
              (error e))))))

(defmethod create-response ((this responses-api-provider) llm-response)
  (let ((out-type (llm-response:output-type llm-response)))
    (if (stringp out-type)
        (alexandria:switch (out-type :test #'string-equal)
          ("message"
           `((:role . ,(llm-response:role llm-response))
             (:content . ,(llm-response:text llm-response))))
          ("function_call"
           `((:id . ,(llm-response:id llm-response))
             (:type . :function--call)
             (:call--id . ,(llm-response:call-id llm-response))
             (:name . ,(llm-response:name llm-response))
             (:arguments . ,(cl-json:encode-json-alist-to-string (llm-response:arguments llm-response)))))
          ("function_call_output"
           `((:type . :function--call--output)
             (:call--id . ,(llm-response:call-id llm-response))
             (:output . ,(if (llm-response:text llm-response)
                             (llm-response:text llm-response)
                             ""))
             (:status . ,(llm-response:status llm-response))))))))

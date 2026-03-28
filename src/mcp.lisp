(defpackage :agent-code/src/mcp
	(:use :cl)
    (:nicknames :mcp)
    (:import-from :cl-json)
    (:import-from ::agent-code/src/tool)
	(:export
     #:mcp
     #:tools
     #:connect))

(in-package :agent-code/src/mcp)

(defclass-std:defclass/std mcp ()
    ((name :type string)
     (url :type string)
     (description :type string)
     (tools :type list)
     (session-id)))

(defmethod initialize-instance :after ((this mcp) &rest args)
  (connect this))

(defmethod connect ((this mcp))
  (initialize this)
  (setf (tools this) (get-tools this)))

(defmethod initialize ((this mcp))
  (multiple-value-bind (response headers)
      (call-mcp this "initialize"
                `((:protocol-version . "2024-11-05")
                  (:client-info . ((:name . "client")
                                   (:version . "1.1")))
                  (:capabilities . ((:tools . ((:list-changed . "true")))))))
    (declare (ignore response))
    (setf (session-id this) (gethash "mcp-session-id" headers))))

(defmethod get-tools ((this mcp))
  (let* ((raw-alist (call-mcp this "tools/list"))
         (result-alist (alexandria:assoc-value raw-alist :result))
         (tools-alist (alexandria:assoc-value result-alist :tools))
         (tools))

    (dolist (tool-alist tools-alist tools)
      (let* ((schema-alist (alexandria:assoc-value tool-alist :input-schema))
             (tool (make-instance 'tool:tool
                                  :name (alexandria:assoc-value tool-alist :name)
                                  :description (alexandria:assoc-value tool-alist :description)
                                  :properties (alexandria:assoc-value schema-alist :properties)
                                  :required (alexandria:assoc-value schema-alist :required))))
        (push tool tools)))))

(defmethod call-tool ((this mcp) args)
  (let* ((raw-alist (call-mcp this "tools/call" args))
         (result-alist (alexandria:assoc-value raw-alist :result))
         (contents-alist (alexandria:assoc-value result-alist :content))
         (results))

    (dolist (content-alist contents-alist results)
      (let* ((text (alexandria:assoc-value content-alist :text)))
        (push text results)))))

(defmethod call-mcp ((this mcp) (method string) &optional params &rest options)
  "It is possible to pass HTTP headers for this one call by giving an alist as :headers keyword argument."
  (destructuring-bind (&key
                         (timeout 5000)
                         (basic-auth nil)
                         (headers nil)) options
    (let* ((version "2.0")
           (request-headers
             (append
              (list (cons :content-type "application/json, text/event-stream")
                    (cons :accept "application/json, text/event-stream")
                    (if (session-id this) (cons "mcp-session-id" (session-id this))))
              headers))
           (json (cl-json:encode-json-alist-to-string
                  `((:jsonrpc . ,version)
                    (:id . "1")
                    (:method . ,method)
                    ,(if params `(:params . ,params))))))

      (multiple-value-bind (raw-response status response-headers) (dex:post (url this)
                                                                   :content json
                                                                   :basic-auth basic-auth
                                                                   :headers request-headers
                                                                   :connect-timeout timeout
                                                                   :read-timeout timeout)
        (declare (ignore status))
        (alexandria:switch ((gethash "content-type" response-headers) :test #'string-equal)
          ("text/event-stream"
           (dolist (line (cl-ppcre:split "\\r\\n|\\n" raw-response))
             (when (serapeum:string-prefix-p "data: " line)
               (let ((alist (cl-json:decode-json-from-string (subseq line 6))))
                 (return (values alist response-headers))))))

          (t
           (let ((alist (cl-json:decode-json-from-string raw-response)))
             (values alist response-headers))))))))

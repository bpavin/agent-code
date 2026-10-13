(defpackage :agent-code/src/mcp
	(:use :cl)
    (:nicknames :mcp)
    (:import-from :cl-json)
    (:import-from ::agent-code/src/tool)
	(:export
     #:mcp
     #:tools
     #:connect
     #:tool-execute))

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
  (log:info "Initializing connection to mcp server: ~A" (url this))
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

(defmethod tool-execute ((this mcp) tool args)
  (let* ((params `((:name . ,(tool:name tool))
                   (:arguments . ,args)))
         (raw-alist (call-mcp this "tools/call" params))
         (result-alist (alexandria:assoc-value raw-alist :result))
         (contents-alist (alexandria:assoc-value result-alist :content)))

    (dolist (content-alist contents-alist)
      (let* ((text (alexandria:assoc-value content-alist :text)))
        (return text)))))

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

      (handler-case
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
              (values alist response-headers)))))
        (dex:http-request-unauthorized (e)
          (log:info "MCP not authorized ~A" e)
          ; (authenticate-mcp (dex:response-headers e))
          )))))

(defun authenticate-mcp (response-headers)
  (let* ((www-auth (gethash "www-authenticate" response-headers)))
    (cl-ppcre:do-register-groups (raw-metadata-url) ("Bearer resource_metadata=\"(.*?)\"" www-auth)
      (log:info "got resource_metadata url: ~A" raw-metadata-url)
      (let* ((metadata-url (cl-ppcre:regex-replace "/mcp$" raw-metadata-url ""))
             (client-info (register-client metadata-url)))
        (dex:get metadata-url))
      )))

(defun register-client (url)
  (let* ((full-url (format nil "~Aregister" url))
         (body
           (cl-json:encode-json-alist-to-string
            `((:client--name . "MCPClient")
              (:redirect--uris "http://localhost:8080/callback")
              (:grant--types "authorization_code" "refresh_token")
              (:response--types "code")
              (:token--endpoint--auth--method . "none")))))

    (dex:post full-url :content body)))

(defun authorize (url client-info)
  (let* ((client-id (cdr (assoc :client--id client-info)))
         (grant-type "authorization_code")
         (response-type "code")
         (scope (cdr (assoc :scope client-info)))
         (redirect-uri (cadr (assoc :redirect--uris client-info)))

         (query-path (format nil "?~A=~A&~A=~A&~A=~A&~A=~A&~A=~A&~A=~A"
                             "client_id" client-id
                             "grant_type" grant-type
                             "response_type" response-type
                             "scope" scope
                             "redirect_uri" (quri:url-encode redirect-uri)
                             "code_challenge" "123"))
         (full-url (format nil "~Aauthorize~A" url query-path)))
    (log:info "Enter url to browser: ~A" full-url)))

(defun code-auth (url callback-url)

  (let* ((callback-params-full (subseq callback-url (search "?" callback-url)))
         (callback-params (quri:url-decode-params callback-params-full))
         (query-path (format nil "?~A=~A" ))
         (full-url (format nil "~Atoken" url))))

  )

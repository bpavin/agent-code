(defpackage :agent-code/src/llm-response
  (:use :cl)
  (:nicknames :llm-response)
  (:import-from :defclass-std)
  (:export
   #:llm-response
   #:id
   #:output-type
   #:role
   #:text
   #:call-id
   #:name
   #:arguments
   #:status
   #:request-id
   #:generate-request-id
   #:create-message
   #:create-function-output
   #:find-responses-by-request-id))

(in-package :agent-code/src/llm-response)

(defclass-std:defclass/std llm-response ()
  ((id)
   (output-type)
   (role)
   (text)
   (call-id)
   (name)
   (arguments)
   (status)
   (request-id)))

(defmethod print-object ((this llm-response) s)
  (print-unreadable-object (this s :type t :identity t)
    (format s "name=~A" (name this))))

(defun generate-request-id ()
  "Generate a unique request-id based on timestamp and random component."
  (format nil "req_~A_~A"
          (get-universal-time)
          (serapeum:random-in-range 0 10000)))

(defun create-message (role content &optional request-id)
  (if (and role content (not (string-equal "" content)))
      (make-instance 'llm-response:llm-response
                     :output-type "message"
                     :role role
                     :text content
                     :request-id request-id)))

(defun create-function-output (llm-response-function-call success result)
  (make-instance 'llm-response:llm-response
                 :output-type "function_call_output"
                 :call-id (llm-response:call-id llm-response-function-call)
                 :name (llm-response:name llm-response-function-call)
                 :arguments (llm-response:arguments llm-response-function-call)
                 :status success
                 :text result))

(defun find-responses-by-request-id (history request-id)
  "Find all llm-response objects in history with the given request-id."
  (loop for response in history
        when (string= (request-id response) request-id)
        collect response))

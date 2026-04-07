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
     #:create-message
     #:create-function-output))

(in-package :agent-code/src/llm-response)

(defclass-std:defclass/std llm-response ()
  ((id)
   (output-type)
   (role)
   (text)
   (call-id)
   (name)
   (arguments)
   (status)))

(defmethod print-object ((this llm-response) s)
  (print-unreadable-object (this s :type t :identity t)
    (format s "name=~A" (name this))))

(defun create-message (role content)
  (if (and role content (not (string-equal "" content)))
      (make-instance 'llm-response:llm-response
                     :output-type "message"
                     :role role
                     :text content)))

(defun create-function-output (llm-response-function-call success result)
  (make-instance 'llm-response:llm-response
                 :output-type "function_call_output"
                 :call-id (llm-response:call-id llm-response-function-call)
                 :name (llm-response:name llm-response-function-call)
                 :arguments (llm-response:arguments llm-response-function-call)
                 :status success
                 :text result))

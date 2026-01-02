(defpackage :agent-code/src/llm-response
	(:use :cl)
    (:nicknames :llm-response)
    (:import-from :defclass-std)
	(:export
     #:llm-response
     #:output-type
     #:role
     #:text
     #:call-id
     #:name
     #:arguments
     #:status))

(in-package :agent-code/src/llm-response)

(defclass-std:defclass/std llm-response ()
  ((output-type)
   (role)
   (text)
   (call-id)
   (name)
   (arguments)
   (status)))

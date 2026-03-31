(defpackage :agent-code/src/persona
	(:use :cl)
    (:nicknames :persona)
    (:import-from :agent-code/src/tool)
	(:export
     #:persona
     #:name
     #:description
     #:system
     #:developer
     #:user
     #:get-user-prompt
     #:assistant
     #:tools
     #:use-weaker-model-p
     #:parallel-p
     #:before-in-chain))

(in-package :agent-code/src/persona)

(defclass-std:defclass/std persona ()
  ((name description)
   (system :std
           "You are sofware developer.")
   (developer
    user
    assistant)
   (tools)
   (use-weaker-model-p :std nil)
   (before-in-chain :std nil)
   (parallel-p :std nil)))

(defmethod get-user-prompt ((this persona) tools)
  (format nil (user this)
          (serapeum:string-join
           (mapcar (lambda (tool)
                     (format nil "~A~%Description: ~A~%Parameters: ~A~%"
                             (tool:name tool)
                             (tool:description tool)
                             (cl-json:encode-json-alist-to-string (tool:properties tool))))
                   tools)
           #\NewLine)))


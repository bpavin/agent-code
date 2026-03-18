(defpackage :agent-code/src/conditions
  (:use :cl)
  (:nicknames :conditions)
  (:export
   #:llm-condition
   #:text
   #:print-log

   #:llm-request
   #:llm-response
   #:json

   #:tool-call
   #:tool-response
   #:name
   #:args
   #:result))

(in-package :agent-code/src/conditions)

(define-condition llm-condition ()
  ((text :initarg :text :reader text :initform nil)))

(defgeneric print-log (this))

(defmethod print-log ((this llm-condition))
  (log:info "~A" (text this)))

(define-condition llm-request (llm-condition)
  ((json :initarg :json :reader json :initform nil)))

(defmethod print-log ((this llm-request))
  (log:info "~A" (if (text this) (text this))))

(define-condition llm-response (llm-condition)
  ((json :initarg :json :reader json :initform nil)))

(defmethod print-log ((this llm-response))
  (log:info "~A" (if (text this) (text this))))

(define-condition tool-call (llm-condition)
  ((name :initarg :name :reader name :initform nil)
   (args :initarg :args :reader args :initform nil)))

(defmethod print-log ((this tool-call))
  (log:info "~A name=~A args=~A"
            (if (text this) (text this)) (name this) (args this)))

(define-condition tool-response (llm-condition)
  ((name :initarg :name :reader name :initform nil)
   (args :initarg :args :reader args :initform nil)
   (result :initarg :result :reader result :initform nil)))

(defmethod print-log ((this tool-response))
  (if (log:debug)
      (log:debug "~A name=~A args=~A" (if (text this) (text this))
                 (name this) (args this))
      (log:info "~A name=~A" (if (text this) (text this)) (name this))))

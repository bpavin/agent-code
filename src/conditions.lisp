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
   ))

(in-package :agent-code/src/conditions)

(define-condition llm-condition ()
  ((text :initarg :text :reader text :initform nil)))

(defgeneric print-log (this))

(defmethod print-log ((this llm-condition))
  (format nil "~A" (text this)))

(define-condition llm-request (llm-condition)
  ((json :initarg :json :reader json :initform nil)))

(defmethod print-log ((this llm-request))
  (format nil "~A" (if (text this) (text this))))

(define-condition llm-response (llm-condition)
  ((json :initarg :json :reader json :initform nil)))

(defmethod print-log ((this llm-response))
  (format nil "~A" (if (text this) (text this))))

(define-condition tool-call (llm-condition)
  ((name :initarg :name :reader name :initform nil)
   (args :initarg :args :reader args :initform nil)))

(defmethod print-log ((this tool-call))
  (format nil "~A ~A args: ~A"
          (if (text this) (text this))
          (name this)
          (if (args this)
              (args this)
              "no args")))

(define-condition tool-response (llm-condition)
  ((name :initarg :name :reader name :initform nil)
   (args :initarg :args :reader args :initform nil)))

(defmethod print-log ((this tool-response))
  (format nil "~A ~A"
          (if (text this) (text this))
          (name this)))

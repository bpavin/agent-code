(defpackage :agent-code/src/tool
	(:use :cl)
    (:nicknames :tool)
	(:export
     #:tool

     #:name
     #:description
     #:parameters

     #:to-alist
     #:tool-execute

     #:read-tool
     #:write-tool
     #:delete-tool))

(in-package :agent-code/src/tool)

(defclass tool ()
    ((name :type string :accessor name)
     (description :type string :accessor description)
     (parameters :type list :accessor parameters)
     (required :type list :accessor required)))

(defgeneric tool-execute (this args)
  (:documentation "Abstract method for tool implementation."))

(defgeneric to-alist (this)
  (:documentation "Abstract method that returns alist description of the tool."))

(defmethod to-alist (this)
    `((:name . ,(name this))
      (:description . ,(description this))
      (:parameters .
            ((:type . :object)
             (:properties . ,(parameters this))))
      (:required . ,(required this))))

(defun aget (alist item)
  (alexandria:assoc-value alist item))

(defclass read-tool (tool)
  ((name :initform "read_file")
   (description :initform "Reads a file from disk.")
   (parameters :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool read-tool) args)
  (if (null args)
      (error "No file specified for reading.")
      (alexandria:read-file-into-string (aget args :path))))

(defclass write-tool (tool)
  ((name :initform "write_file")
   (description :initform "Writes content to a file on disk.")
   (parameters :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:content . ((:type . :string)
                                        (:description . "Content of the file.")))))
   (required :initform '(:path :content))))

(defmethod tool-execute ((tool write-tool) args)
  (if (< (length args) 2)
      (error "Not enough arguments specified for writing.")
      (alexandria:write-string-into-file (aget args :content) (aget args :path))))

(defclass delete-tool (tool)
  ((name :initform "delete_file")
   (description :initform "Deletes a file from disk.")
   (parameters :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool delete-tool) args)
  (if (null args)
      (error "No file specified for deletion.")
      (delete-file (aget args :path))))

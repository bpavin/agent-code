(defpackage :agent-code/src/tool
	(:use :cl)
    (:nicknames :tool)
	(:export
     #:tool

     #:name
     #:description
     #:properties

     #:to-alist
     #:tool-execute

     #:read-tool
     #:write-tool
     #:delete-tool
     #:bash-tool))

(in-package :agent-code/src/tool)

(defclass tool ()
    ((name :type string :accessor name)
     (description :type string :accessor description)
     (properties :type list :accessor properties)
     (required :type list :accessor required)))

(defgeneric tool-execute (this args)
  (:documentation "Abstract method for tool implementation."))

(defgeneric to-alist (this)
  (:documentation "Abstract method that returns alist description of the tool."))

(defmethod to-alist (this)
  `((:type . :function)
    (:name . ,(name this))
    (:description . ,(description this))
    (:parameters . ((:type . :object)
                    (:properties . ,(properties this))
                    (:required . ,(required this))))))

(defun aget (alist item)
  (alexandria:assoc-value alist item))

(defclass read-tool (tool)
  ((name :initform "read_file")
   (description :initform "Reads a file from disk.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool read-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No file specified for reading.")
        (alexandria:read-file-into-string (aget args :path)))))

(defclass write-tool (tool)
  ((name :initform "write_file")
   (description :initform "Writes content to a file on disk.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:content . ((:type . :string)
                                        (:description . "Content of the file.")))))
   (required :initform '(:path :content))))

(defmethod tool-execute ((tool write-tool) args)
  (if (< (length args) 2)
      (error "Not enough arguments specified for writing.")
      (with-error-as-result
        (alexandria:write-string-into-file (aget args :content) (aget args :path)))))

(defclass delete-tool (tool)
  ((name :initform "delete_file")
   (description :initform "Deletes a file from disk.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool delete-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No file specified for deletion.")
        (delete-file (aget args :path)))))

(defclass bash-tool (tool)
  ((name :initform "bash_command")
   (description :initform "Invoke any bash command.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Command and arguments of the bash command.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool bash-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No command specified."))
    (let ((cmd (aget args :command)))
      (if (serapeum:string-contains-p " rm " cmd)
          (error "Command is not allowed ~A" "rm"))
      (uiop:run-program cmd :output '(:string :stripped t)))))

(defmacro with-error-as-result (&body body)
  `(handler-case
       (progn ,@body)
     (error (e)
       (princ-to-string e))))

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

     #:read-many-files-tool
     #:write-tool
     #:delete-tool
     #:bash-tool
     #:git-tool
     #:dir-tool
     #:grep-tool
     #:edit-file-tool))

(in-package :agent-code/src/tool)

(defmacro with-error-as-result (&body body)
  `(handler-case
       (progn ,@body)
     (error (e)
       (princ-to-string e))))

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

(defclass read-many-files-tool (tool)
  ((name :initform "read_many_files")
   (description :initform "Reads multiple files from the disk. Input is array of paths to read.")
   (properties :initform '((:paths . ((:type . :array)
                                      (:description . "Array of the absolute paths of the files.")))))
   (required :initform '(:paths))))

(defmethod tool-execute ((tool read-many-files-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No file specified for reading.")
        (with-output-to-string (out)
          (dolist (path (aget args :paths) out)
            (format out "----- ~A -----~%~%~A~%~%" path (read-file path)))))))

(defun read-file (path)
  (with-error-as-result
    (alexandria:read-file-into-string path)))

(defclass write-tool (tool)
  ((name :initform "write_file")
   (description :initform "Writes content to a file on disk. File must already exist, and it won't overwrite existing files.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:content . ((:type . :string)
                                        (:description . "Content of the file.")))))
   (required :initform '(:path :content))))

(defmethod tool-execute ((tool write-tool) args)
  (with-error-as-result
    (if (< (length args) 2)
        (error "Not enough arguments specified for writing.")
        (alexandria:write-string-into-file (aget args :content) (aget args :path)))))

(defclass edit-file-tool (tool)
  ((name :initform "edit_file")
   (description :initform "Edit content of an existing file on the disk. .")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:old-content . ((:type . :string)
                                            (:description . "Content that will be replaced.")))
                           (:new-content . ((:type . :string)
                                            (:description . "New content that will overwrite the old content.")))))
   (required :initform '(:path :old-content :new-content))))

(defmethod tool-execute ((tool edit-file-tool) args)
  (with-error-as-result
    (if (< (length args) 3)
        (error "Not enough arguments specified for writing.")
        (let* ((path (aget args :path))
               (file-content (alexandria:read-file-into-string path))
               (file-content (serapeum:string-replace-all
                              (aget args :old-content) file-content (aget args :new-content))))
          (alexandria:write-string-into-file file-content path)
          "File is edited successfully."))))

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

(defclass git-tool (tool)
  ((name :initform "git_command")
   (description :initform "Invoke git commands.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Arguments of the git command.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool git-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No command specified."))
    (let* ((cmd-raw (aget args :command))
           (cmd (if (serapeum:string-prefix-p "git" cmd-raw)
                    cmd-raw
                    (format nil "git ~A" cmd-raw))))
      (if (serapeum:string-contains-p " rm " cmd)
          (error "Command is not allowed ~A" "rm"))
      (uiop:run-program cmd :output '(:string :stripped t)))))

(defclass grep-tool (tool)
  ((name :initform "grep_command")
   (description :initform "Invoke grep commands.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Arguments of the grep command. Paths must be absolute paths.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool grep-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No command specified."))
    (let* ((cmd-raw (aget args :command))
           (cmd (if (or (serapeum:string-prefix-p "grep " cmd-raw)
                        (serapeum:string-prefix-p "find " cmd-raw))
                    cmd-raw
                    (format nil "grep ~A" cmd-raw))))
      (if (serapeum:string-contains-p " rm " cmd)
          (error "Command is not allowed ~A" "rm"))

      (let ((result (uiop:run-program "cat"
                                      :input
                                      (uiop:process-info-output
                                       (uiop:launch-program cmd
                                                            :output :stream))
                                      :output '(:string :stripped t))))
        (if (string-equal "" result)
            "No results."
            result)))))

(defclass dir-tool (tool)
  ((name :initform "dir_command")
   (description :initform "List all files in a directory.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of a directory. Wildcards are not accepted.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool dir-tool) args)
  (with-error-as-result
    (if (null args)
        (error "No arguments specified."))
    (let* ((path (aget args :path)))
      (if (not (serapeum:string-suffix-p "/" path))
          (setf path (format nil "~A/" path)))
      (format nil "~A" (uiop:directory-files path)))))

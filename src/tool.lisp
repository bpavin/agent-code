(defpackage :agent-code/src/tool
	(:use :cl)
    (:nicknames :tool)
	(:export
     #:tool

     #:name
     #:description
     #:properties
     #:required

     #:to-alist
     #:tool-execute

     #:read-many-files-tool
     #:write-tool
     #:delete-tool
     #:bash-tool
     #:git-tool
     #:dir-tool
     #:grep-tool
     #:edit-file-tool
     #:patch-tool))

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

(defclass read-many-files-tool (tool)
  ((name :initform "read_many_files")
   (description :initform "Reads multiple files from the disk. Input is array of paths to read.")
   (properties :initform '((:paths . ((:type . :array)
                                      (:description . "Array of the absolute paths of the files. Directories are not allowed. Wildcards are not allowed.")))))
   (required :initform '(:paths))))

(defmethod tool-execute ((tool read-many-files-tool) args)
  (if (null args)
      (error "No file specified for reading.")
      (with-output-to-string (out)
        (dolist (path (aget args :paths) out)
          (format out "----- ~A -----~%~%~A~%~%" path (read-file path))))))

(defun read-file (path)
  (alexandria:read-file-into-string path))

(defclass write-tool (tool)
  ((name :initform "write_file")
   (description :initform "Writes content to a file on disk. File must already exist, and it won't overwrite existing files.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:content . ((:type . :string)
                                        (:description . "Content of the file.")))))
   (required :initform '(:path :content))))

(defmethod tool-execute ((tool write-tool) args)
  (if (< (length args) 2)
      (error "Not enough arguments specified for writing."))
  (alexandria:write-string-into-file (aget args :content) (aget args :path))
  "Success, file is written.")

(defclass edit-file-tool (tool)
  ((name :initform "edit_file")
   (description :initform "Edit content of an existing file on the disk. .")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:old-content . ((:type . :string)
                                            (:description . "Content that exists in the file and that will be replaced.")))
                           (:new-content . ((:type . :string)
                                            (:description . "New content that will overwrite the old content.")))))
   (required :initform '(:path :old-content :new-content))))

(defmethod tool-execute ((tool edit-file-tool) args)
  (if (< (length args) 3)
      (error "Not enough arguments specified for writing.")
      (let* ((path (aget args :path))
             (old-content (aget args :old-content))
             (original-file-content (alexandria:read-file-into-string path))
             (edited-file-content (serapeum:string-replace-all
                                   old-content original-file-content (aget args :new-content))))
        (if (= (length original-file-content)
               (length edited-file-content))
            (if (search old-content original-file-content)
                (error "Old content was not found in the file.")
                (error "Edit failed. No changes were applied to the file.")))
        (alexandria:write-string-into-file edited-file-content path :if-exists :supersede)
        "File is edited successfully.")))

(defclass delete-tool (tool)
  ((name :initform "delete_file")
   (description :initform "Deletes a file from disk.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool delete-tool) args)
  (if (null args)
      (error "No file specified for deletion.")
      (delete-file (aget args :path))))

(defclass bash-tool (tool)
  ((name :initform "bash_command")
   (description :initform "Invoke any bash command.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Command and arguments of the bash command.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool bash-tool) args)
  (if (null args)
      (error "No command specified."))
  (let ((cmd (aget args :command)))
    (if (serapeum:string-contains-p " rm " cmd)
        (error "Command is not allowed ~A" "rm"))

    (call-system-shell cmd)))

(defclass git-tool (tool)
  ((name :initform "git_command")
   (description :initform "Invoke git commands.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Arguments of the git command.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool git-tool) args)
  (if (null args)
      (error "No command specified."))
  (let* ((cmd-raw (aget args :command))
         (cmd (if (serapeum:string-prefix-p "git" cmd-raw)
                  cmd-raw
                  (format nil "git ~A" cmd-raw))))
    (if (serapeum:string-contains-p " rm " cmd)
        (error "Command is not allowed ~A" "rm"))
    (uiop:run-program cmd :output '(:string :stripped t))))

(defclass patch-tool (tool)
  ((name :initform "patch_file")
   (project-directory :initarg :project-directory :accessor project-directory)
   (description :initform "Apply diff with a patch command to the file.")
   (properties :initform '((:project-dir . ((:type . :string)
                                            (:description . "Absolute path of the project's directory.")))
                           (:diff . ((:type . :string)
                                     (:description . "Diff formatted as a `diff` command. Use `diff` unified format:
### **Example Unified diff snippet**

--- a/hello.txt
+++ b/hello.txt
@@ -1,3 +1,3 @@
-Hello world
-This is a test
+Hello world!
+This is a simple test
 Goodbye

What the parts mean

--- a/hello.txt → original file
+++ b/hello.txt → new file
@@ -<old start>,<old count + 1> +<new start>,<new count + 1> @@ → hunk header (lines affected, line count must be incremented by 1)
Lines starting with - were removed
Lines starting with + were added
Lines without a prefix are unchanged
")))))
   (required :initform '(:project-dir :diff))))

(defmethod tool-execute ((tool patch-tool) args)
  (if (null args)
      (error "No command specified."))
  (let* ((diff (aget args :diff)))
    (if (null diff)
        (error "Argument diff is not specified."))
    (let ((cmd
            (format nil "patch --strip=1 -d ~A <<'EOF'~%~A~%EOF"
                    (or (aget args :project-dir)
                        (project-directory tool))
                    diff)))
      (log:trace cmd)
      (call-system-shell cmd))))

(defclass grep-tool (tool)
  ((name :initform "grep_command")
   (description :initform "Invoke grep commands.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Arguments of the grep command. Paths must be absolute paths.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool grep-tool) args)
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
          result))))

(defclass dir-tool (tool)
  ((name :initform "dir_command")
   (description :initform "List all files in a directory.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of a directory. Wildcards are not accepted.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool dir-tool) args)
  (if (null args)
      (error "No arguments specified."))
  (let* ((path (aget args :path)))
    (if (not (serapeum:string-suffix-p "/" path))
        (setf path (format nil "~A/" path)))
    (let ((files (uiop:directory-files path)))
      (format nil "~A" files))))

(defun call-system-shell (cmd)
  (multiple-value-bind (out err code)
      (uiop:run-program cmd
                        :ignore-error-status t
                        :error-output :string
                        :output :string)
    (if (and (> code 0) err (not (string-equal "" err)))
        (error err)
        out)))

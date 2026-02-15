(defpackage :agent-code/src/tool
	(:use :cl)
    (:nicknames :tool)
    (:import-from :cl-json)
	(:export
     #:tool

     #:name
     #:description
     #:properties
     #:required

     #:aget
     #:to-alist
     #:tool-execute

     #:read-many-files-tool
     #:write-tool
     #:delete-tool
     #:bash-tool
     #:dir-tool
     #:edit-file-tool
     #:patch-tool))

(in-package :agent-code/src/tool)

(defclass tool ()
    ((name :type string :accessor name)
     (description :type string :accessor description)
     (properties :type list :accessor properties)
     (required :type list :accessor required)))

(defgeneric tool-execute (this llm args)
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

(defmethod tool-execute ((tool read-many-files-tool) llm args)
  (if (null args)
      (error "No file specified for reading."))
  (let ((paths (aget args :paths)))
    (if (stringp paths)
        (setf paths (cl-json:decode-json-from-string paths)))

    (with-output-to-string (out)
      (dolist (path paths out)
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

(defmethod tool-execute ((tool write-tool) llm args)
  (if (< (length args) 2)
      (error "Not enough arguments specified for writing."))
  (alexandria:write-string-into-file (aget args :content) (aget args :path))
  "Success, file is written.")

(defclass edit-file-tool (tool)
  ((name :initform "edit_file")
   (description :initform "Edit content of an existing file on the disk.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))
                           (:old-content . ((:type . :string)
                                            (:description . "Content that exists in the file and that will be replaced.")))
                           (:new-content . ((:type . :string)
                                            (:description . "New content that will overwrite the old content.")))))
   (required :initform '(:path :old-content :new-content))))

(defmethod tool-execute ((tool edit-file-tool) llm args)
  (if (< (length args) 3)
      (error "Not enough arguments specified for writing."))

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
    "File is edited successfully."))

(defclass delete-tool (tool)
  ((name :initform "delete_file")
   (description :initform "Deletes a file from disk.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of the file.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool delete-tool) llm args)
  (if (null args)
      (error "No file specified for deletion.")
      (delete-file (aget args :path))))

(defclass bash-tool (tool)
  ((name :initform "bash_command")
   (description :initform "Invoke any bash command.")
   (properties :initform '((:command . ((:type . :string)
                                        (:description . "Command and arguments of the bash command. Include cd of the directory you want to work with.")))))
   (required :initform '(:command))))

(defmethod tool-execute ((tool bash-tool) llm args)
  (if (null args)
      (error "No command specified."))
  (let ((cmd (aget args :command)))
    (if (serapeum:string-contains-p " rm " cmd)
        (error "Command is not allowed ~A" "rm"))

    (call-system-shell cmd)))

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

• Unified diff represents changes between two versions of a file
• Diff starts with file headers: --- old_file and +++ new_file
• Multiple hunks are NOT ALLOWED
• Hunk begins with a header: @@ -old_start,old_len +new_start,new_len @@
• A hunk contains:
 - Context (unchanged) lines starting with a space
 - Removed lines starting with -
 - Added lines starting with +
")))))
   (required :initform '(:project-dir :diff))))

(defmethod tool-execute ((tool patch-tool) llm args)
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

(defclass line-edit-tool (tool)
  ((name :initform "patch_file")
   (project-directory :initarg :project-directory :accessor project-directory)
   (description :initform "Apply diff with a patch command to the file.")
   (properties :initform '((:file-path . ((:type . :string)
                                          (:description . "Absolute path of the file.")))
                           (:operations .
                            ((:type . :array)
                             (:description . "Array of operations")
                             (:operation . ((:type . :string)
                                            (:description . "Operation that will be executed on the file.
Can be one of: add, remove or replace.")))
                             (:start-line . ((:type . :integer)
                                             (:description . "Start line of the change.")))
                             (:end-line . ((:type . :integer)
                                           (:description . "End line of the change.")))
                             (:content . ((:type . :string)
                                          (:description . "Content that will be applied to the file. Required for add and replace operations.")))))))
   (required :initform '(:file-path :operations :start-line :end-line)))
  (:documentation "Tool for performing line-based edits with context validation.
Input: JSON array of operations with structure:
  {\"operation\": \"add|remove|replace\",
   \"line_number\": integer,
   \"content\": \"string\",
   \"context_lines\": 2 (optional)}
Validates operations against file content, generates unified diff, and applies via patch-tool logic.
Safety checks: max 20 operations, no overlapping line ranges."))

(defmethod tool-execute ((tool line-edit-tool) llm args)
  (let ((path (aget args :file-path))
        (operations (aget args :operations))
        (max-operations 20)
        (op-list nil))
    (if (null path)
        (error "File path must be defined."))
    (if (> (length operations) max-operations)
        (error "Too many operations: ~a (max ~a)" (length operations) max-operations))

    (dolist (op-alist operations)
      (let* ((operation (aget op-alist :operation))
             (start (aget op-alist :start-line))
             (end (aget op-alist :end-line))
             (content (aget op-alist :content)))

        (if (or (null start) (null end) (null content))
            (error "~S operation must have start-line, end-line and content." operation))

        (alexandria:switch (operation :test #'string-equal)
          ("add"
           (setf operation :add))

          ("remove"
           (setf operation :remove))

          ("replace"
           (setf operation :replace)))

        (push (list operation start end content)
              op-list)))

    (setf op-list
          (sort op-list (lambda (l r) (<= (second l) (second r)))))

    (apply-changes-to-file path op-list)))

(defun apply-changes-to-file (file-path operations)
  (with-output-to-string (str)
    (with-open-file (file-stream file-path :direction :input)
      (do ((line (read-line file-stream nil nil))
           (count 1)
           (op-id 0))
          ((null line)
           str)

        (if (null (nth op-id operations))
            (progn
              (format str "~A~%" line)
              (setf line (read-line file-stream nil nil)
                    count (+ 1 count)) )

            (destructuring-bind (op start end content) (nth op-id operations)
              (cond ((or (and start (< count start))
                         (and end (> count end)))
                     (format str "~A~%" line)
                     (setf line (read-line file-stream nil nil)
                           count (+ 1 count)))

                    ((eq op :add)
                     (when (and start (= start count))
                       (incf op-id)
                       (format str "~A~%~A~%" content line)
                       (setf line (read-line file-stream nil nil)
                             count (+ 1 count))))

                    ((eq op :replace)
                     (when (and start (= start count))
                       (format str "~A~%" content))
                     (when (and start end
                                (<= start count end))
                       (if (= end count)
                           (incf op-id))

                       (setf line (read-line file-stream nil nil)
                             count (+ 1 count))))

                    ((eq op :remove)
                     (if (and end (<= count end))
                         (setf line (read-line file-stream nil nil)
                               count (+ 1 count)))
                     (if (and end (= count end))
                         (incf op-id))))))))))

(defclass dir-tool (tool)
  ((name :initform "dir_command")
   (description :initform "List all files in a directory. This will list only first level files, it won't list subdirectories.")
   (properties :initform '((:path . ((:type . :string)
                                     (:description . "Absolute path of a directory. Wildcards are not accepted.")))))
   (required :initform '(:path))))

(defmethod tool-execute ((tool dir-tool) llm args)
  (if (null args)
      (error "No arguments specified."))

  (let* ((path (aget args :path)))
    (if (null path)
        (error "Path not specified."))

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
        (if (string-equal "" out)
            "Command was completed successfully."
            out))))

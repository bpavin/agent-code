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
     #:patch-tool
     #:line-edit-tool))

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
   (description :initform "Reads multiple files from the disk. Input is array of paths to read.
Files are prepended with line numbers separated from file content with character |.")
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
  (with-output-to-string (str)
    (with-open-file (stream path :direction :input)
      (do ((count 1 (+ count 1))
           (line (read-line stream nil nil)))
          ((null line)
           str)
        (format str "~A|~A~%" count line)
        (setf line (read-line stream nil nil))))))

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
  ((name :initform "line_edit")
   (project-directory :initarg :project-directory :accessor project-directory)
   (description :initform "Edit file by specifying lines that must be added, replaced or removed.
CRITICAL: In case change is including only one single line, end-line is equal to start-line.

Example:
Add new line at the start of the file:
{
\"operation\": \"add\",
\"startLine\": 1,
\"endLine\": 1,
\"content\": \"new line\"
}

Replace lines from lines 2 to 4:
{
\"operation\": \"replace\",
\"startLine\": 2,
\"endLine\": 4,
\"content\": \"line 1\\nline 2\\nline 3\\n\"
}

Remove line 6:
{
\"operation\": \"remove\",
\"startLine\": 6,
\"endLine\": 6
}

There can be only one operation.
Ranges must not overlap.
")
   (properties :initform '((:file-path . ((:type . :string)
                                          (:description . "Absolute path of the file.")))
                           (:operation . ((:type . :string)
                                          (:description . "Operation that will be executed on the file.
Can be one of: add, remove or replace.")))
                           (:start-line . ((:type . :integer)
                                           (:description . "Start line of the change. First line in the file starts with 1.")))
                           (:end-line . ((:type . :integer)
                                         (:description . "End line of the change. In case change is including single line, end-line is equal to start-line.")))
                           (:content . ((:type . :string)
                                        (:description . "Content that will be applied to the file. Required for add and replace operations.")))))
   (required :initform '(:file-path :operations :start-line :end-line)))
  (:documentation "Tool for performing line-based edits with context validation.
Input: JSON array of operations with structure:
  {\"operation\": \"add|remove|replace\",
   \"startLine\": integer,
   \"endLine\": integer,
   \"content\": \"string\"
   }
Validates operations against file content.
Safety checks: max 1 operation, no overlapping line ranges."))

(defmethod tool-execute ((tool line-edit-tool) llm args)
  (let ((path (aget args :file-path))
        (op-list nil))
    (if (null path)
        (error "File path must be defined."))

    (let* ((operation (aget args :operation))
           (start-str (aget args :start-line))
           (start (and start-str (parse-integer start-str)))
           (end-str (aget args :end-line))
           (end (and end-str (parse-integer end-str)))
           (content (or (aget args :content) "")))

      (alexandria:switch (operation :test #'string-equal)
        ("add"
         (if (or (null start) (null end) (null content))
             (error "~S operation must have start-line, end-line and content." operation))
         (setf operation :add))

        ("remove"
         (if (or (null start) (null end))
             (error "~S operation must have start-line and end-line." operation))
         (setf operation :remove))

        ("replace"
         (if (or (null start) (null end) (null content))
             (error "~S operation must have start-line, end-line and content." operation))

         (setf operation :replace))

        (t
         (error "~A operation is invalid." operation)))

      (if (< start 1)
          (error "Lines must start with 1. start-line ~A is invalid." start))

      (if (< end start)
          (error "end-line must be greater or equal to start-line. start-line: ~A, end-line: ~A" start end))

      (push (list operation start end content)
            op-list))

    (if (>= (length op-list) 2)
        (do ((i 1 (+ i 1)))
            ((>= i (length op-list)))
          (destructuring-bind (op-1 start-1 end-1 _) (nth (- i 1) op-list)
            (destructuring-bind (op-2 start-2 end-2 _) (nth i op-list)
              (if (>= end-1 start-2)
                  (error "Overlapping operations are not allowed. start-line: ~A, end-line: ~A and start-line: ~A, end-line: ~A"
                         start-1 end-1 start-2 end-2))))))

    (let ((edited-file (apply-changes-to-file path op-list)))
      (alexandria:write-string-into-file edited-file path :if-exists :supersede)
      edited-file)))

(defun apply-changes-to-file (file-path operations)
  (with-open-file (file-stream file-path :direction :input)
    (do ((result)
         (line (read-line file-stream nil nil))
         (count 1)
         (op-id 0)
         (op-endp nil))
        ((and (null line) op-endp)
         (format nil "~{~A~^~%~}" (nreverse result)))

      (if (null (nth op-id operations))
          (progn
            (if line
                (push line result))
            (setf line (read-line file-stream nil nil)
                  count (+ 1 count)
                  op-endp t))

          (destructuring-bind (op start end content) (nth op-id operations)
            (cond ((or (and start (< count start))
                       (and end (> count end)))
                   (push (if line line "") result)
                   (setf line (read-line file-stream nil nil)
                         count (+ 1 count)))

                  ((eq op :add)
                   (when (and start (= start count))
                     (incf op-id)
                     (push content result)
                     (if line
                         (push line result))
                     (setf line (read-line file-stream nil nil)
                           count (+ 1 count))))

                  ((eq op :replace)
                   (when (and start (= start count))
                     (push content result))
                   (when (and start end
                              (<= start count end))
                     (if (= end count)
                         (incf op-id))

                     (setf line (read-line file-stream nil nil)
                           count (+ 1 count))))

                  ((eq op :remove)
                   (if (and end (= count end))
                       (incf op-id))
                   (if (and end (<= count end))
                       (setf line (read-line file-stream nil nil)
                             count (+ 1 count))))))))))

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

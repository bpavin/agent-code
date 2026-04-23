(defpackage :agent-code/src/task-tool
  (:use :cl)
  (:nicknames :task-tool)
  (:import-from :agent-code/src/llm)
  (:import-from :agent-code/src/tool)
  (:export
   #:task-tool
   #:+task-tool-name+))

(in-package :agent-code/src/task-tool)

(defparameter +task-tool-name+ "task_list")

(defclass task-tool (tool:tool)
  ((tool:name :initform +task-tool-name+)
   (can-write-p :initarg :can-write-p :initform nil :accessor can-write-p)
   (tool:description
    :initform "Tool used for managing list of tasks. Use this often to change plan according to user's inputs.")
   (tool:properties :initform nil)
   (tool:required :initform '(:operation))))

(defmethod initialize-instance :after ((this task-tool) &key)
  (cond ((can-write-p this)
         (setf (tool:properties this)
               (create-properties
                "WRITE: Write all tasks to the file. Path is required."))
         (nconc (tool:properties this)
                '((:path . ((:type . :string)
                            (:description . "Absolute path for file."))))))

        (t (setf (tool:properties this)
                 (create-properties "")))))

(defun create-properties (write-operation-description)
  `((:operation . ((:type . :string)
                   (:description .
                                 ,(format nil "Operation for updating task list.
Must be one of:
INSERT, UPDATE, REMOVE: Insert new task item or update/delete existing.

LIST: List will return all the tasks currently in the list.

~A" write-operation-description))))
    (:index . ((:type . :integer)
               (:description . "Tasks are kept in a list. Each task item is prefixed with index.
First index is one.
Use this index to specify which task item you want to update. Index is mandatory for update and delete.")))
    (:content . ((:type . :string)
                 (:description . "Information you want to update task with. Content is mandatory for insert and update.")))
    (:status . ((:type . :string)
                (:description . "Status of the task. Can be 'pending', 'approved'. Status is mandatory for insert and update.")))))

(defmethod get-history ((this task-tool) llm)
  (llm:shared-memory llm))

(defmethod tool:tool-execute ((tool task-tool) args &rest options)
  (if (null args)
      (error "No arguments specified."))

  (destructuring-bind (&key (llm nil)) options
    (let* ((history (get-history tool llm))
           (operation (tool:aget args :operation)))
      (if (null operation)
          (error "Operation is not specified."))

      (alexandria:switch (operation :test #'string-equal)
        ("list"
         (if (= 0 (length history))
             "Task list is empty."
             (let ((c 0))
               (reduce (lambda (sum i)
                         (format nil "~A~%Task id: ~A~%~%Task status: ~A~%~%Task content:~%~A~%-----"
                                 sum
                                 (incf c)
                                 (tool:aget i :status)
                                 (tool:aget i :content)))
                       history
                       :initial-value ""))))

        ("insert"
         (let ((content (tool:aget args :content))
               (status (tool:aget args :status)))
           (if (null content)
               (error "Insert requires content."))
           (if (null status)
               (error "Insert requires status."))
           (setf (llm:shared-memory llm)
                 (append history
                         (list
                          (list (cons :content content)
                                (cons :status status))))))
         "Task successfully inserted.")

        ("update"
         (let* ((index (if (stringp (tool:aget args :index))
                           (parse-integer (tool:aget args :index))
                           (tool:aget args :index)))
                (i (max 0 (- index 1)))
                (content (tool:aget args :content))
                (status (tool:aget args :status)))
           (if (null content)
               (error "Insert requires content."))
           (if (null status)
               (error "Insert requires status."))
           (setf (nth i history)
                 (list (cons :content content)
                       (cons :status status)))
           "Task successfully updated."))

        ("remove"
         (let* ((index (if (stringp (tool:aget args :index))
                           (parse-integer (tool:aget args :index))
                           (tool:aget args :index)))
                (i (max 0 (- index 1))))
           (setf (llm:shared-memory llm)
                 (delete (nth i history)
                         history)))
         "Task successfully removed.")

        ("write"
         (unless (can-write-p tool)
           (error "Write is not permited."))

         (let ((path (tool:aget args :path)))
           (unless path
             (error "Path must be specified for write."))
           (write-tasks llm path)

           "Tasks are written to the file successfully."))

        (t
         (error "Invalid operation: ~A" operation))))))

(defun write-tasks (llm path)
  (let ((sum ""))
   (dolist (task (llm:shared-memory llm))
     (setf sum (format nil "~A~%~A" sum (cdr (assoc :content task)))))
    (alexandria:write-string-into-file sum path)))

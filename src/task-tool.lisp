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
   (tool:description
    :initform "Tool used for managing list of tasks. Use this often to change plan according to user's inputs.")
   (tool:properties :initform '((:operation . ((:type . :string)
                                               (:description . "Operation for updating task list. Must be one of: LIST, INSERT, UPDATE, REMOVE.
Insert new task item or update/delete existing. List will return all the tasks currently in the list.")))
                                (:index . ((:type . :integer)
                                           (:description . "Tasks are kept in a list. Each task item is prefixed with index.
First index is one.
Use this index to specify which task item you want to update. Index is mandatory for update and delete.")))
                                (:content . ((:type . :string)
                                             (:description . "Information you want to update task with. Content is mandatory for insert and update.")))
                                (:status . ((:type . :string)
                                            (:description . "Status of the task. Can be 'pending', 'approved'. Status is mandatory for insert and update.")))))
   (tool:required :initform '(:operation))))

(defmethod get-history ((this task-tool) llm)
  (llm:shared-memory llm))

(defmethod tool:tool-execute ((tool task-tool) args &rest options)
  (if (null args)
      (error "No arguments specified."))

  (destructuring-bind (&key (llm nil) (shared-memory nil)) options
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

        (t
         (error "Invalid operation: ~A" operation))))))

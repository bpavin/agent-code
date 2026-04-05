(defpackage :agent-code/src/task-tool
  (:use :cl)
  (:nicknames :task-tool)
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
First index is zero.
Use this index to specify which task item you want to update. Index is mandatory for update and delete.")))
                                (:content . ((:type . :string)
                                             (:description . "Information you want to update task with. Content is mandatory for insert and update.")))))
   (tool:required :initform '(:operation))
   (history :initform nil :accessor history)))

(defmethod tool:tool-execute ((tool task-tool) args &rest options)
  (if (null args)
      (error "No arguments specified."))

  (let* ((operation (tool:aget args :operation)))
    (if (null operation)
        (error "Operation is not specified."))

    (alexandria:switch (operation :test #'string-equal)
      ("list"
       (if (= 0 (length (history tool)))
           "Task list is empty."
           (history tool)))
      ("insert"
       (let ((content (tool:aget args :content)))
         (if (null content)
             (error "Insert requires content."))
         (setf (history tool)
               (append (history tool)
                       (list content))))
       "Task successfully inserted.")
      ("update"
       (let ((i (- (parse-integer (tool:aget args :index)) 1))
             (content (tool:aget args :content)))
         (setf (nth i (history tool)) content)
         "Task successfully updated."))
      ("remove"
       (setf (history tool)
             (delete (nth (tool:aget args :index) (history tool))
                     (history tool)))
       "Task successfully removed.")
      (t
       (error "Invalid operation: ~A" operation)))))

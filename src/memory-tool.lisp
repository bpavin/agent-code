(defpackage :agent-code/src/memory-tool
  (:use :cl)
  (:nicknames :memory-tool)
  (:import-from :agent-code/src/tool)
  (:export
   #:memory-tool
   #:+memory-tool-name+))

(in-package :agent-code/src/memory-tool)

(defparameter +memory-tool-name+ "update_memory")

(defclass memory-tool (tool:tool)
  ((tool:name :initform +memory-tool-name+)
   (tool:description
    :initform "Tool used for retaining key information. Use this often to store key information about previous conversation.
Critical: Calling this tool will remove previous conversations, so make sure to include all information that is important for context.")
   (tool:properties :initform '((:operation . ((:type . :string)
                                          (:description . "Operation for updating memory. Must be one of: INSERT, UPDATE, REMOVE. You can insert new memory item or update/delete existing.")))
                           (:index . ((:type . :integer)
                                      (:description . "Memory is kept in a list. Each memory item is prefixed with index.
Use this index to specify which memory item you want to update. Index is mandatory for update and delete.")))
                           (:content . ((:type . :string)
                                        (:description . "Information you want to update memory with. Content is mandatory for insert and update.")))))
   (tool:required :initform '(:operation))
   (history :initform nil)))

(defmethod tool:tool-execute ((tool memory-tool) llm args)
  (if (null args)
      (error "No arguments specified."))

  (let* ((operation (tool:aget args :operation)))
    (if (null operation)
        (error "Operation is not specified."))

    (alexandria:switch (operation :test #'string-equal)
      ("insert"
       (setf (history tool)
             (append (history tool)
                     (list (tool:aget args :content)))))
      ("update"
       (let ((i (- (parse-integer (tool:aget args :index)) 1)))
         (setf (nth i (history tool))
               (tool:aget args :content))))
      ("remove"
       (setf (history tool)
             (delete (nth (tool:aget args :index) (history tool))
                     (history tool))))
      (t
       (error "Invalid operation: ~A" operation)))
    "Memory successfully updated."))

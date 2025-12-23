(defpackage :llm-local/src/main
  (:use #:cl)
  (:import-from :log4cl)
  (:import-from :dexador)
  (:import-from :cl-json)
  (:import-from :rutils)
  (:import-from :serapeum)
  (:export
   #:main
   #:ask))
(in-package :llm-local/src/main)

(defclass ai-chat ()
  ((url :initform ""
        :accessor url)
   (chat-completion :initform "/v1/chat/completions"
                    :accessor chat-completion)
   (history :initform nil
            :accessor history)))

(defmethod call-chat-completion ((this ai-chat) query)
  (let* ((system-prompt "You are software developer. You don't halucinate,
you work only with provided files. If you are missing a context for the file, ask for the file by providing the filepath.")
         (content (format nil
                   "{
                        \"model\": \"DeepSeek-V3.2\",
                        \"messages\": [
                            {\"role\": \"system\", \"content\": ~A},
                            {\"role\": \"user\", \"content\": ~A}
                        ],
                        \"stream\": false,
                        \"max_tokens\": 1000
                    }"
                   (cl-json:encode-json-to-string system-prompt)
                   (cl-json:encode-json-to-string query))))
    (when (log:debug)
      (log:debug content))

    (dex:post (format nil "~A~A" (url this) (chat-completion this))
              :insecure t
              :headers '(("Content-type" . "application/json"))
              :content content)))

(defmethod send-chat ((this ai-chat) query)
  (push (sanitize query) (history this))

  (let* ((response (call-chat-completion this (serapeum:string-join (reverse (history this)) "")))
         (alist (cl-json:decode-json-from-string response)))

    (dolist (choice (alexandria:assoc-value alist :choices))
      (let ((result (rutils:-> (alexandria:assoc-value choice :message)
                        (alexandria:assoc-value rutils:% :content))))
        (push (sanitize result) (history this))
        (return result)))))

(defun sanitize (str)
  (cl-ppcre:regex-replace-all "\\r\\n|\\n|\\t|\\r" str ""))

(defparameter *ctx* nil)

(defun main ()
  (init))

(defun init (&key (force-init-p t))
  (if (or force-init-p (null *ctx*))
      (setf *ctx* (make-instance 'ai-chat))))

(defun ask (query)
  (init :force-init-p nil)

  (let ((response (send-chat *ctx* query)))
    (log:info "~%~A" response)))

(defpackage :agent-code/src/summary-persona
	(:use :cl)
    (:nicknames :summary-persona)
    (:import-from :agent-code/src/persona)
	(:export
     #:summary-persona))

(in-package :agent-code/src/summary-persona)

(defparameter summary-persona
  (make-instance 'persona:persona
                 :name "summary"
                 :description "Summarization assistant for purposes of compacting the previous conversations."
                 :system
                 "You are summarization assistant."
                 :parallel-p nil
                 :use-weaker-model-p t
                 :user "Summarize all of the conversation. Output numbered list of main points."))

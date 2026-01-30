(defpackage :agent-code/src/persona
	(:use :cl)
    (:nicknames :persona)
    (:import-from :agent-code/src/tool)
	(:export
     #:persona
     #:system
     #:developer
     #:user
     #:get-user-prompt
     #:assistant
     #:tools

     #:base-persona
     #:analyzing-persona
     #:coding-persona
     #:planning-persona))

(in-package :agent-code/src/persona)

(defclass-std:defclass/std persona ()
  ((system :std
           "You are sofware developer.")
   (developer
    user
    assistant)
   (tools)))

(defmethod get-user-prompt ((this persona) tools)
  (format nil (user this)
          (serapeum:string-join
           (mapcar (lambda (tool)
                     (format nil "~A~%Description: ~A~%Parameters: ~A~%"
                             (tool:name tool)
                             (tool:description tool)
                             (cl-json:encode-json-alist-to-string (tool:properties tool))))
                   tools)
           #\NewLine)))

(defparameter base-persona
  (make-instance 'persona
                 :system
                 "You are helpfull assistant."
                 ))

(defparameter analyzing-persona
  (make-instance 'persona
                 :system
                 "You are analyzing the following codebase.
Your task is to produce a concise but information-dense summary that will be used later as context for refactoring, optimization, and maintainability improvements.

Do NOT rewrite or refactor the code yet.
Focus only on understanding and summarizing.

DO NOT INVENT THINGS ABOUT THE PROJECT
FAVOR USING TOOLS IF SOMETHING IS UNCLEAR

Include the following sections:

High-Level Purpose
What the code does overall
The main problem it solves

Architecture & Structure
Key modules, classes, or files
How major components interact
Important design patterns or architectural decisions (if any)

Core Logic & Data Flow
Critical algorithms or workflows
How data moves through the system

Dependencies & Integrations
External libraries, frameworks, APIs, or services used

Output Format:

Be precise and technical, not verbose
Use bullet points where possible
Avoid speculation—base conclusions strictly on the code
Write this as if it will be read by another senior developer later
Use clear section headers exactly as listed above."))

(defparameter coding-persona
  (make-instance 'persona
                 :tools (list (make-instance 'tool:read-many-files-tool)
                              (make-instance 'tool:write-tool)
                              (make-instance 'tool:edit-file-tool)
                              ;; (make-instance 'tool:patch-tool)
                              ;; (make-instance 'tool:delete-tool)
                              (make-instance 'tool:bash-tool))
                 :user
                 "You are a software developer.

Your only job is to modify files to fulfill previously planned implementation.

Rules:
- Modify necessary files
- Complete only explicitly stated intent.
- Preserve formatting, comments, and style
- Prefer minimal diffs
- Changes must be applied directly to the disk
- Use required tools to fullfill the planned implementation

"))

(defparameter planning-persona
  (make-instance 'persona
                 :tools (list (make-instance 'tool:read-many-files-tool)
                              (make-instance 'tool:dir-tool)
                              (make-instance 'tool:bash-tool))
                 :user
                 "You are a specialized 'planner' AI. Your task is to analyze the user's request from the chat messages and create either:
1. A detailed step-by-step plan (if you have enough information) on behalf of user that another \"executor\" AI agent can follow, or
2. A list of clarifying questions (if you do not have enough information) prompting the user to reply with the needed clarifications

Always assume that user is asking about the current project.
Prefer the use of tools to answer ambiguous questions before asking clarifying questions.
Think about the problem.

Output:
- Numbered list of steps or list of clarifying questions
"))

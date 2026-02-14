(defpackage :agent-code/src/persona
	(:use :cl)
    (:nicknames :persona)
    (:import-from :agent-code/src/tool)
	(:export
     #:persona
     #:name
     #:description
     #:system
     #:developer
     #:user
     #:get-user-prompt
     #:assistant
     #:tools

     #:base-persona
     #:coordinator-persona
     #:analyzing-persona
     #:coding-persona
     #:planning-persona
     #:explore-persona
     ))

(in-package :agent-code/src/persona)

(defclass-std:defclass/std persona ()
  ((name description)
   (system :std
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
                 :name "base"
                 :description "Used for general questions."
                 :system
                 "You are helpfull assistant."))

(defparameter coordinator-persona
  (make-instance 'persona
                 :name "coordinator"
                 :description "Used for fullfilling user's commands by delegating tasks to subagents,
collecting subagents answers and presenting the final answer to user."
                 :system "You are coding assistant."
                 :user
                 "You are coordinator AI.
Your task is to analyze user's question.
Plan how to best solve the user's question.
Delegate tasks to subagents,
collect subagents answers and presenting the final answer to the user.
Give detailed instructions to subagents. Include all the relevant context about the question.
Assume that subagent has no previous context.

You must always produce some output for the user or use a tool."))

(defparameter analyzing-persona
  (make-instance 'persona
                 :name "analyzer"
                 :description "Used for analyzing the project. Main point is to collect high level information about the project
and prepare summary report."
                 :tools (list (make-instance 'tool:read-many-files-tool)
                              ;(make-instance 'tool:dir-tool)
                              (make-instance 'tool:bash-tool))
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
                 :name "coder"
                 :description "Used for implementing planned changes."
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
                 :name "planner"
                 :description "Used for planning changes that were requested by the user."
                 :tools (list (make-instance 'tool:read-many-files-tool)
                              ;(make-instance 'tool:dir-tool)
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

(defparameter explore-persona
  (make-instance 'persona
                 :name "explorer"
                 :description "Used for exploring project. Finding specific files and content of the files."
                 :tools (list (make-instance 'tool:read-many-files-tool)
                              ;(make-instance 'tool:dir-tool)
                              (make-instance 'tool:bash-tool))
                 :user
                 "You are a specialized explorer AI.
You work in a real codebase project. Your task is to explore the codebase and gather key information to fullfill user's question.

Output:
- Maximum 3 sentances of question summary.
- A list of explanations about found facts that fullfill user's request

"))

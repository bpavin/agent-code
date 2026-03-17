# Validator Persona and Validation Tool Implementation Plan

## Overview
This document outlines the implementation plan for adding a validator persona and validation tool to the agent-code project. The validator persona will systematically verify code changes, check syntax, run tests, and ensure code quality after implementation tasks.

## 1. Implementation Steps

### **Step 1: Create Validation Tool Class in `tool.lisp`**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/src/tool.lisp`

Add after existing tool classes (around line 382):

```lisp
(defclass validation-tool (tool)
  ((name :initform "run_validation")
   (description :initform "Executes predefined validation functions on specified files or code sections. Returns validation results with success/error details.")
   (properties :initform '((:validation-type . ((:type . :string)
                                               (:description . "Type of validation to perform. Must be one of: syntax-check, test-run, lint-check, compile-check, custom-function.")))
                          (:target-path . ((:type . :string)
                                          (:description . "Absolute path to file or directory to validate.")))
                          (:validation-function . ((:type . :string)
                                                  (:description . "Custom validation function name (only for validation-type: custom-function).")))
                          (:parameters . ((:type . :object)
                                         (:description . "Additional parameters for the validation function.")))))
   (required :initform '(:validation-type :target-path)))
  (:documentation "Tool for running validation checks on code or files."))

(defmethod tool-execute ((tool validation-tool) llm args)
  (if (null args)
      (error "No arguments specified for validation."))
  
  (let* ((validation-type (aget args :validation-type))
         (target-path (aget args :target-path))
         (validation-function (aget args :validation-function))
         (parameters (aget args :parameters))
         (result))
    
    (alexandria:switch (validation-type :test #'string-equal)
      ("syntax-check"
       (setf result (validate-syntax target-path parameters)))
      ("test-run"
       (setf result (run-tests target-path parameters)))
      ("lint-check"
       (setf result (run-lint target-path parameters)))
      ("compile-check"
       (setf result (compile-check target-path parameters)))
      ("custom-function"
       (if (null validation-function)
           (error "validation-function is required for custom-function type"))
       (setf result (run-custom-validation validation-function target-path parameters)))
      (t
       (error "Invalid validation-type: ~A. Must be one of: syntax-check, test-run, lint-check, compile-check, custom-function." validation-type)))
    
    (format nil "Validation Result:~%Type: ~A~%Target: ~A~%Status: ~A~%Details: ~A"
            validation-type target-path (getf result :status) (getf result :details))))

(defun validate-syntax (path parameters)
  "Validate syntax of Common Lisp file."
  (let* ((cmd (format nil "cd ~A && sbcl --noinform --non-interactive --eval \"(compile-file \\\"~A\\\")\"" 
                     (directory-namestring path) path))
         (output (uiop:run-program cmd :ignore-error-status t :output :string :error-output :string)))
    (if (search "Compilation failed" output)
        `(:status "error" :details ,output)
        `(:status "success" :details "Syntax check passed"))))

(defun run-tests (path parameters)
  "Run tests for specified file or directory."
  (let* ((test-cmd (or (getf parameters :test-command)
                       (format nil "cd ~A && sbcl --noinform --non-interactive --eval \"(asdf:test-system :~A)\"" 
                               (directory-namestring path) (pathname-name path))))
         (output (uiop:run-program test-cmd :ignore-error-status t :output :string :error-output :string)))
    (if (or (search "FAIL" output) (> (uiop:run-program test-cmd :ignore-error-status t) 0))
        `(:status "error" :details ,output)
        `(:status "success" :details "All tests passed"))))

(defun run-lint (path parameters)
  "Run linter on specified file."
  (let* ((linter (or (getf parameters :linter) "cl-lint"))
         (cmd (format nil "~A ~A" linter path))
         (output (uiop:run-program cmd :ignore-error-status t :output :string :error-output :string)))
    (if (search "error" output)
        `(:status "error" :details ,output)
        `(:status "success" :details "Lint check passed"))))

(defun compile-check (path parameters)
  "Check if file compiles successfully."
  (let* ((cmd (format nil "cd ~A && sbcl --noinform --non-interactive --eval \"(load \\\"~A\\\")\"" 
                     (directory-namestring path) path))
         (output (uiop:run-program cmd :ignore-error-status t :output :string :error-output :string)))
    (if (search "error" output)
        `(:status "error" :details ,output)
        `(:status "success" :details "Compilation successful"))))

(defun run-custom-validation (function-name target-path parameters)
  "Run custom validation function."
  (let ((result (funcall (find-symbol (string-upcase function-name) :keyword) target-path parameters)))
    `(:status ,(if result "success" "error") :details ,(format nil "Custom validation: ~A" result))))
```

### **Step 2: Update Tool Package Export**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/src/tool.lisp`

Add `#:validation-tool` to the export list (around line 24):

```lisp
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
 #:line-edit-tool
 #:validation-tool))  ; Add this line
```

### **Step 3: Create Validator Persona in `persona.lisp`**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/src/persona.lisp`

Add after existing personas (around line 316):

```lisp
(defparameter validator-persona
  (make-instance 'persona
                 :name "validator"
                 :description "Validation specialist for verifying code changes, checking syntax, running tests, and ensuring code quality. Use after coding persona completes implementation to validate changes before finalizing."
                 :system "You are a specialized validation AI responsible for verifying code quality, correctness, and adherence to requirements. Your role is to systematically validate implementations using predefined validation tools and report issues with exact details."
                 :before-in-chain "coder"
                 :tools (list (make-instance 'tool:read-many-files-tool)
                              (make-instance 'tool:validation-tool)
                              (make-instance 'tool:bash-tool))
                 :user "You are the **validator subagent**.
Your main purpose is to **validate implementations** after the coder persona completes changes.

**CRITICAL VALIDATION RULES:**

1. **SYSTEMATIC VALIDATION:**
   - Always start with syntax validation using `run_validation` tool with `validation-type: syntax-check`
   - If tests exist, run test validation with `validation-type: test-run`
   - Perform lint checks with `validation-type: lint-check`
   - Verify compilation with `validation-type: compile-check`

2. **VALIDATION REPORT FORMAT:**
   - For each validation type, output: `[TYPE] Validation: [STATUS]`
   - Include exact error messages if validation fails
   - Provide actionable fixes for validation failures
   - Summarize overall validation status

3. **FAILURE HANDLING:**
   - If any validation fails, provide specific error details
   - Suggest exact fixes or ask coder persona to correct issues
   - Do not proceed until all validations pass

4. **SUCCESS CRITERIA:**
   - All syntax checks must pass
   - All tests must pass (if applicable)
   - No lint errors (warnings allowed)
   - Code must compile successfully

**VALIDATION WORKFLOW:**
1. Identify changed files from coder persona output
2. Run syntax validation on each changed file
3. Run test validation if test files exist
4. Run lint validation for code style
5. Run compilation validation
6. Report validation results with exact status

**OUTPUT FORMAT:**
```
Validation Report for [FILE/PROJECT]

1. Syntax Validation:
   - Status: [SUCCESS/ERROR]
   - Details: [output]

2. Test Validation:
   - Status: [SUCCESS/ERROR]
   - Details: [output]

3. Lint Validation:
   - Status: [SUCCESS/ERROR/WARNING]
   - Details: [output]

4. Compilation Validation:
   - Status: [SUCCESS/ERROR]
   - Details: [output]

Overall Status: [PASS/FAIL]
Issues Requiring Fix: [list of specific issues]
```
"))
```

### **Step 4: Update Persona Package Export**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/src/persona.lisp`

Add `#:validator-persona` to the export list (around line 24):

```lisp
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
 #:summary-persona
 #:coordinator-persona
 #:analyzing-persona
 #:coding-persona
 #:planning-persona
 #:explore-persona
 #:parallel-p
 #:before-in-chain
 #:validator-persona))  ; Add this line
```

```


### **Step 7: Create Validation Configuration File**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/validation-config.lisp`

Create a new file for validation configuration:

```lisp
(defpackage :agent-code/validation-config
  (:use :cl)
  (:export #:*validation-rules*
           #:*test-commands*
           #:*linter-configs*
           #:get-validation-rule))

(in-package :agent-code/validation-config)

(defparameter *validation-rules*
  '((:common-lisp . ((:syntax-check . t)
                     (:compile-check . t)
                     (:lint-check . :warning-only)
                     (:test-run . :if-tests-exist)))
    (:python . ((:syntax-check . t)
                (:lint-check . t)
                (:test-run . :if-tests-exist)))
    (:javascript . ((:syntax-check . t)
                    (:lint-check . t)))
    (:default . ((:syntax-check . t)))))

(defparameter *test-commands*
  '((:common-lisp . "sbcl --noinform --non-interactive --eval \"(asdf:test-system :~A)\"")
    (:python . "python -m pytest ~A")
    (:javascript . "npm test")))

(defparameter *linter-configs*
  '((:common-lisp . "cl-lint")
    (:python . "pylint")
    (:javascript . "eslint")))

(defun get-validation-rule (file-type rule)
  (let ((rules (cdr (assoc file-type *validation-rules*))))
    (if rules
        (cdr (assoc rule rules))
        (cdr (assoc rule (cdr (assoc :default *validation-rules*)))))))

(defun file-type-from-extension (path)
  (let ((ext (pathname-type path)))
    (cond ((or (string-equal ext "lisp") (string-equal ext "lsp") (string-equal ext "cl"))
           :common-lisp)
          ((or (string-equal ext "py"))
           :python)
          ((or (string-equal ext "js") (string-equal ext "ts"))
           :javascript)
          (t :default))))
```

### **Step 8: Update Validation Tool to Use Configuration**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/src/tool.lisp`

Update the validation tool methods to use the configuration:

```lisp
(defmethod tool-execute ((tool validation-tool) llm args)
  (if (null args)
      (error "No arguments specified for validation."))
  
  (let* ((validation-type (aget args :validation-type))
         (target-path (aget args :target-path))
         (validation-function (aget args :validation-function))
         (parameters (aget args :parameters))
         (file-type (agent-code/validation-config:file-type-from-extension target-path))
         (result))
    
    (alexandria:switch (validation-type :test #'string-equal)
      ("syntax-check"
       (if (agent-code/validation-config:get-validation-rule file-type :syntax-check)
           (setf result (validate-syntax target-path parameters))
           (setf result '(:status "skipped" :details "Syntax check disabled for this file type"))))
      ("test-run"
       (let ((test-rule (agent-code/validation-config:get-validation-rule file-type :test-run)))
         (cond ((eq test-rule :if-tests-exist)
                (if (test-files-exist-p target-path)
                    (setf result (run-tests target-path parameters))
                    (setf result '(:status "skipped" :details "No test files found"))))
               (test-rule
                (setf result (run-tests target-path parameters)))
               (t
                (setf result '(:status "skipped" :details "Test run disabled for this file type"))))))
      ("lint-check"
       (let ((lint-rule (agent-code/validation-config:get-validation-rule file-type :lint-check)))
         (cond ((eq lint-rule :warning-only)
                (setf result (run-lint target-path (list* :warnings-only t parameters))))
               (lint-rule
                (setf result (run-lint target-path parameters)))
               (t
                (setf result '(:status "skipped" :details "Lint check disabled for this file type"))))))
      ("compile-check"
       (if (agent-code/validation-config:get-validation-rule file-type :compile-check)
           (setf result (compile-check target-path parameters))
           (setf result '(:status "skipped" :details "Compilation check disabled for this file type"))))
      ("custom-function"
       (if (null validation-function)
           (error "validation-function is required for custom-function type"))
       (setf result (run-custom-validation validation-function target-path parameters)))
      (t
       (error "Invalid validation-type: ~A. Must be one of: syntax-check, test-run, lint-check, compile-check, custom-function." validation-type)))
    
    (format nil "Validation Result:~%Type: ~A~%Target: ~A~%File Type: ~A~%Status: ~A~%Details: ~A"
            validation-type target-path file-type (getf result :status) (getf result :details))))

(defun test-files-exist-p (path)
  "Check if test files exist for the given path."
  (let* ((dir (directory-namestring path))
         (name (pathname-name path))
         (test-patterns (list (format nil "~A/test-~A.*" dir name)
                              (format nil "~A/~A-test.*" dir name)
                              (format nil "~A/tests/*" dir))))
    (some #'probe-file test-patterns)))
```

### **Step 9: Update Main System to Support Validation Workflow**
**File**: `/mnt/ssd-disk/p2/projects/agent-code/src/main.lisp`

Add helper function for validation workflow:

```lisp
(defun validate-implementation (file-path &key (validation-types '("syntax-check" "test-run" "lint-check" "compile-check")))
  "Run validation on implemented file."
  (let* ((ctx *ctx*)
         (validator-tool (make-instance 'tool:validation-tool))
         (results))
    
    (dolist (vtype validation-types)
      (push (tool:tool-execute validator-tool ctx 
                               `((:validation-type . ,vtype)
                                 (:target-path . ,file-path)))
            results))
    
    (format nil "Validation Results for ~A:~%~{~A~^~%~}" file-path (reverse results))))
```


## 2. Integration Points Summary

1. **Tool System Integration**: Validation tool integrates with existing `tool.lisp` class hierarchy
2. **Persona System Integration**: Validator persona follows same pattern as other personas
3. **Subagent Tool Integration**: Added to `subagent-tool` personas list for delegation
4. **Coordinator Integration**: Updated to include validator in workflow descriptions
5. **Configuration Integration**: Separate config file for validation rules
6. **Error Handling**: Uses existing error handling from `conditions.lisp`

## 3. Usage Flow

1. **User Request**: "Implement feature X"
2. **Coordinator**: Delegates to planner → coder → validator
3. **Planner**: Creates implementation plan
4. **Coder**: Implements changes
5. **Validator**: Automatically invoked after coder completes
6. **Validation Steps**:
   - Syntax check on changed files
   - Test run (if tests exist)
   - Lint check
   - Compilation check
7. **Results**: Reported back to coordinator for next steps

## 5. Testing Plan

### Unit Tests
- Validation tool executes each validation type
- Configuration file loads correctly
- Persona exports work

### Integration Tests
- Coordinator → Coder → Validator workflow
- Validation results formatting
- Error handling in validation failures

### Manual Tests
- Run validation on existing Common Lisp files
- Test with different file types (Python, JavaScript)
- Verify coordinator delegation logic

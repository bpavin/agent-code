(defpackage :agent-code/tests/tool-test
  (:use :cl
   :agent-code/src/tool
        :rove)
  (:import-from :alexandria))

(in-package :agent-code/tests/tool-test)

(defparameter patch-tool (make-instance 'patch-tool))

(deftest test-patch-tool-valid
  (testing "Valid patch application"
    (let* ((test-file "tests/resources/testfile.txt")
           (diff
             "--- a/testfile.txt
+++ b/testfile.txt
@@ -1,1 +1,2 @@
 Original content
+New line")
           (project-dir (asdf:system-source-directory :agent-code))
           (abs-test-file (merge-pathnames test-file project-dir)))
      ;; Create the test file
      (with-open-file (stream abs-test-file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "Original content" stream))
      ;; Apply the patch
      (let ((result (tool:tool-execute patch-tool
                                       nil
                                       `((:project-dir . ,project-dir)
                                         (:diff . ,diff)))))
        (ok (string= (alexandria:read-file-into-string abs-test-file) "Original content\nNew line"))
        (ok (string= result "Patch applied successfully"))))))

(deftest test-patch-tool-missing-parameters
  (testing "Missing required parameters should raise an error"
    (handler-case
        (tool:tool-execute patch-tool nil nil)
      (error (e)
        (ok (string= (format nil "~A" e) "No command specified."))))))

;; Line edit tool tests
(deftest test-line-edit-tool-valid-add
  (testing "Adds line at position 1"
    (let* ((project-dir (asdf:system-source-directory :agent-code))
           (file (merge-pathnames (format nil "temp-add-~d.txt" (get-universal-time)) project-dir))
           (temp-file (format nil "~A" file))
           (expected (format nil "new-line~%original")))
      (unwind-protect
           (progn
             (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-line "original" s))
             (tool:tool-execute (make-instance 'tool:line-edit-tool)
                                nil
                                `((:file-path . ,temp-file)
                                  (:operation . "add") (:start-line . "1") (:end-line . "1") (:content . "new-line")))
             (ok (string= expected (alexandria:read-file-into-string temp-file))))
        (ignore-errors (delete-file temp-file))))))

(deftest test-line-edit-tool-valid-remove
  (testing "Removes line at position 1"
    (let* ((project-dir (asdf:system-source-directory :agent-code))
           (file (merge-pathnames (format nil "temp-remove-~d.txt" (get-universal-time)) project-dir))
           (temp-file (format nil "~A" file))
           (expected "line2"))
      (unwind-protect
           (progn
             (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-line "line1" s)
               (write-line "line2" s))
             (tool:tool-execute (make-instance 'tool:line-edit-tool)
                                nil
                                `((:file-path . ,temp-file)
                                  (:operation . "remove") (:start-line . "1") (:end-line . "1")))
             (ok (string= expected (alexandria:read-file-into-string temp-file))))
        (ignore-errors (delete-file temp-file))))))

(deftest test-line-edit-tool-valid-replace
  (testing "Replaces line at position 1"
    (let* ((project-dir (asdf:system-source-directory :agent-code))
           (file (merge-pathnames (format nil "temp-replace-~d.txt" (get-universal-time)) project-dir))
           (temp-file (format nil "~A" file))
           (expected (format nil "replaced~%line2")))
      (unwind-protect
           (progn
             (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-line "line1" s)
               (write-line "line2" s))
             (tool:tool-execute (make-instance 'tool:line-edit-tool)
                                nil
                                `((:file-path . ,temp-file)
                                  (:operation . "replace") (:start-line . "1") (:end-line . "1") (:content . "replaced")))
             (ok (string= expected (alexandria:read-file-into-string temp-file))))
        (ignore-errors (delete-file temp-file))))))

(deftest test-line-edit-tool-missing-parameters
  (testing "Missing required parameters"
    (ok (signals
            (tool:tool-execute (make-instance 'tool:line-edit-tool)
                               nil
                               '((:file-path . "/dev/null")))
            'error))))

(deftest test-line-edit-tool-line-0
  (testing "Handles line 0 operations"
    (let* ((project-dir (asdf:system-source-directory :agent-code))
           (file (merge-pathnames (format nil "temp-line0-~d.txt" (get-universal-time)) project-dir))
           (temp-file (format nil "~A" file)))
      (unwind-protect
           (progn
             (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-line "original" s))
             (ok (signals
                     (tool:tool-execute (make-instance 'tool:line-edit-tool)
                                        nil
                                        `((:file-path . ,temp-file)
                                          (:operation . "add") (:start-line . "0") (:end-line . "0") (:content . "new-line")))
                     'error)))
        (ignore-errors (delete-file temp-file))))))

(deftest test-line-edit-tool-invalid-operation
  (testing "Rejects invalid operation types"
    (let* ((project-dir (asdf:system-source-directory :agent-code))
           (file (merge-pathnames (format nil "temp-invalid-~d.txt" (get-universal-time)) project-dir))
           (temp-file (format nil "~A" file)))
      (unwind-protect
           (progn
             (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-line "original" s))
             (ok (signals
                     (tool:tool-execute (make-instance 'tool:line-edit-tool)
                                        nil
                                        '((:file-path . temp-file)
                                          (:operation . "invalid") (:start-line . 1)))
                     'error)))
        (ignore-errors (delete-file temp-file))))))


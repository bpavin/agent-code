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
      (let ((result (tool-execute patch-tool
                                  `((:project-dir . ,project-dir)
                                    (:diff . ,diff)))))
        (ok (string= (alexandria:read-file-into-string abs-test-file) "Original content\nNew line"))
        (ok (string= result "Patch applied successfully"))))))

(deftest test-patch-tool-missing-parameters
    (testing "Missing required parameters should raise an error"
             (handler-case
                 (tool-execute patch-tool nil)
               (error (e)
                 (ok (string= (format nil "~A" e) "No command specified."))))))

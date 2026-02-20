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

;; (deftest test-line-edit-tool-20-operations
;;   (testing "Applies maximum 20 operations"
;;     (let* ((project-dir (asdf:system-source-directory :agent-code))
;;            (file (merge-pathnames (format nil "temp-20-ops-~d.txt" (get-universal-time)) project-dir))
;;            (temp-file (format nil "~A" file))
;;            (expected (with-output-to-string (s)
;;                        (write-line "original" s)
;;                        (do ((res) (i 1 (+ i 1)))
;;                            ((= 21 i)
;;                             (format s "~{~A~^~%~}" (nreverse res)))
;;                          (push (format nil "line ~a" i) res)))))
;;       (unwind-protect
;;            (progn
;;              (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
;;                (write-line "original" s))
;;              (let ((ops (loop for i from 2 to 21
;;                               collect `((:operation . "add") (:start-line . ,i) (:end-line . ,i)
;;                                         (:content . ,(format nil "line ~a" (- i 1)))))))
;;                (tool:tool-execute (make-instance 'tool:line-edit-tool)
;;                                   nil
;;                                   `((:file-path . ,temp-file)
;;                                     (:operations ,@ops)))
;;                (ok (string= expected (alexandria:read-file-into-string temp-file)))))
;;         (ignore-errors (delete-file temp-file))))))

;; (deftest test-line-edit-tool-21-operations
;;   (testing "Rejects more than 20 operations"
;;     (let* ((project-dir (asdf:system-source-directory :agent-code))
;;            (file (merge-pathnames (format nil "temp-21-ops-~d.txt" (get-universal-time)) project-dir))
;;            (temp-file (format nil "~A" file)))
;;       (unwind-protect
;;            (progn
;;              (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
;;                (write-line "original" s))
;;              (let ((ops (loop for i from 1 to 21
;;                               collect `((:operation . "add") (:start-line . 1) (:end-line . 1) (:content . "extra")))))
;;                (ok (signals
;;                        (tool:tool-execute (make-instance 'tool:line-edit-tool)
;;                                           nil
;;                                           `((:file-path . ,temp-file)
;;                                             (:operations ,@ops)))
;;                        'error))))
;;         (ignore-errors (delete-file temp-file))))))

(deftest test-line-edit-tool-missing-parameters
  (testing "Missing required parameters"
    (ok (signals
            (tool:tool-execute (make-instance 'tool:line-edit-tool)
                               nil
                               '((:file-path . "/dev/null")))
            'error))))

;; (deftest test-line-edit-tool-overlapping-ranges
;;   (testing "Detects overlapping operation ranges"
;;     (ok (signals (tool:tool-execute (make-instance 'tool:line-edit-tool)
;;                                     nil
;;                                     `((:file-path . "")
;;                                       (:operations
;;                                        ((:operation . "remove") (:start-line . 1) (:end-line . 2))
;;                                        ((:operation . "add") (:start-line . 1) (:end-line . 1) (:content . "overlap")))))
;;             'error))

;;     (ok (signals (tool:tool-execute (make-instance 'tool:line-edit-tool)
;;                                     nil
;;                                     `((:file-path . "")
;;                                       (:operations
;;                                        ((:operation . "remove") (:start-line . 1) (:end-line . 1))
;;                                        ((:operation . "add") (:start-line . 1) (:end-line . 1) (:content . "overlap")))))
;;             'error))

;;     (ok (signals (tool:tool-execute (make-instance 'tool:line-edit-tool)
;;                                     nil
;;                                     `((:file-path . "")
;;                                       (:operations ((:operation . "add") (:start-line . 2) (:end-line . 6) (:content . "new-line"))
;;                                                    ((:operation . "remove") (:start-line . 4) (:end-line . 4))
;;                                                    ((:operation . "remove") (:start-line . 5) (:end-line . 5))
;;                                                    ((:operation . "remove") (:start-line . 1) (:end-line . 3)))))
;;             'error))))

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

;; (deftest test-line-edit-tool-dependency-validation
;;   (testing "Validates operation dependencies"
;;     (let* ((project-dir (asdf:system-source-directory :agent-code))
;;            (file (merge-pathnames (format nil "temp-dep-~d.txt" (get-universal-time)) project-dir))
;;            (temp-file (format nil "~A" file))
;;            (expected (format nil "modified")))
;;       (unwind-protect
;;            (progn
;;              (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
;;                (write-line "original" s)
;;                (write-line "line2" s))
;;              (tool:tool-execute (make-instance 'tool:line-edit-tool)
;;                                 nil
;;                                 `((:file-path . ,temp-file)
;;                                   ((:operation . "replace") (:start-line . 1) (:end-line . 1) (:content . "modified"))
;;                                   ((:operation . "remove") (:start-line . 2) (:end-line . 2))))
;;              (ok (string= expected (alexandria:read-file-into-string temp-file))))
;;         (ignore-errors (delete-file temp-file))))))

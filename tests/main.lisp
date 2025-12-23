(defpackage llm-local/tests/main
  (:use :cl
        :llm-local
        :rove))
(in-package :llm-local/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :llm-local)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

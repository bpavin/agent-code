(defsystem "agent-code"
  :version "0.0.1"
  :author ""
  :license ""
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("agent-code/src/main")
  :description ""
  :in-order-to ((test-op (test-op "agent-code/tests"))))

(defsystem "agent-code/tests"
  :author ""
  :license ""
  :depends-on ("agent-code"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "tool-test")
                 (:file "api-provider-test"))))
  :description "Test system for agent-code"
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "llm-code-cli"
  :version "0.0.1"
  :author ""
  :license ""
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("llm-code-cli/src/main")
  :description ""
  :in-order-to ((test-op (test-op "llm-code-cli/tests"))))

(defsystem "llm-code-cli/tests"
  :author ""
  :license ""
  :depends-on ("llm-code-cli"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for llm-code-cli"
  :perform (test-op (op c) (symbol-call :rove :run c)))

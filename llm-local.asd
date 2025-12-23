(defsystem "llm-local"
  :version "0.0.1"
  :author ""
  :license ""
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("llm-local/src/main")
  :description ""
  :in-order-to ((test-op (test-op "llm-local/tests"))))

(defsystem "llm-local/tests"
  :author ""
  :license ""
  :depends-on ("llm-local"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for llm-local"
  :perform (test-op (op c) (symbol-call :rove :run c)))

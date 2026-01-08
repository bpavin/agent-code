(defpackage :agent-code/tests/api-provider-test
  (:use :cl :rove)
  (:import-from :agent-code/src/api-provider)
  (:export))

(in-package :agent-code/tests/api-provider-test)

(deftest responses-handle-response
  (testing "test responses-api-provider handle-response method"
    (let* ((api-response "{
  \"id\": \"chatcmpl-858\",
  \"object\": \"chat.completion\",
  \"created\": 1767888381,
  \"model\": \"qwen3:8b\",
  \"system_fingerprint\": \"fp_ollama\",
  \"choices\": [
    {
      \"index\": 0,
      \"message\": {
        \"role\": \"assistant\",
        \"content\": \"\",
        \"reasoning\": \"Okay, the user wants me to read the cl-reader.asd file. Let me think about how to approach this.\",
        \"tool_calls\": [
          {
            \"id\": \"call_4o6h1ie2\",
            \"index\": 0,
            \"type\": \"function\",
            \"function\": {
              \"name\": \"read_many_files\",
              \"arguments\": \"{\\\"paths\\\":[\\\"/home/borna/common-lisp/cl-reader/cl-reader.asd\\\"]}\"
            }
          }
        ]
      },
      \"finish_reason\": \"tool_calls\"
    }
  ],
  \"usage\": {
    \"prompt_tokens\": 1013,
    \"completion_tokens\": 302,
    \"total_tokens\": 1315
  }
}")
           (llm-responses
             (api-provider:handle-response (make-instance 'api-provider:chat-completion-api-provider) api-response)))
      (ok (< 0 (length llm-responses))))))

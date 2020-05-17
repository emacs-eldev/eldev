(require 'test/common)


(ert-deftest eldev-loading-modes-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--byte-compiled" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should (string= stdout (eldev--test-lines "t")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--source" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should (string= stdout (eldev--test-lines "nil")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--packaged" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should (string= stdout (eldev--test-lines "t")))
      (should (= exit-code 0)))))


(provide 'test/loading-modes)

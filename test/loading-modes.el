(require 'test/common)


(ert-deftest emake-test-loading-modes-1 ()
  (let ((emake--test-project "project-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("clean")
      (should (= exit-code 0)))
    (emake--test-run nil ("--byte-compiled" "eval" "(byte-code-function-p (symbol-function 'project-a-hello))")
      (should (string= stdout (emake--test-lines "t")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--source" "eval" "(byte-code-function-p (symbol-function 'project-a-hello))")
      (should (string= stdout (emake--test-lines "nil")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--packaged" "eval" "(byte-code-function-p (symbol-function 'project-a-hello))")
      (should (string= stdout (emake--test-lines "t")))
      (should (= exit-code 0)))))


(provide 'test/loading-modes)

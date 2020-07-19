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

(ert-deftest eldev-loading-modes-2 ()
  (let ((eldev--test-project "project-g"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--byte-compiled" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should (string= stdout (eldev--test-lines "t")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--source" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should (string= stdout (eldev--test-lines "nil")))
      (should (= exit-code 0)))
    ;; Would fail because of a bug in Emacs itself.  On Emacs 24.x would fail simply
    ;; because reloading files with byte-compiled versions had not been implemented in
    ;; pre-25 versions.
    (eldev--test-run nil ("--packaged" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should (string= stdout (eldev--test-lines "t")))
      (should (= exit-code 0)))))

(ert-deftest eldev-loading-modes-3 ()
  (let ((eldev--test-project "project-j"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--byte-compiled" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should (string= stdout (eldev--test-lines "t")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--source" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should (string= stdout (eldev--test-lines "nil")))
      (should (= exit-code 0)))
    ;; Would fail because of a bug triggered by combination of `autoloads' plugin and
    ;; build system.
    (eldev--test-run nil ("--packaged" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should (string= stdout (eldev--test-lines "t")))
      (should (= exit-code 0)))))


(provide 'test/loading-modes)

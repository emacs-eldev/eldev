(require 'test/common)


(ert-deftest eldev-test-buttercup-project-e-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-e" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-buttercup-project-e-2 ()
  ;; Second tests fails with this setup.
  (eldev--test-run "project-e" ("--setup" "(defvar eldev--buttercup-do-fail t)" "test")
    (should (= exit-code 1))))

(ert-deftest eldev-test-buttercup-project-e-3 ()
  ;; Second tests still fails, but it should not be executed.
  (eldev--test-run "project-e" ("--setup" "(defvar eldev--buttercup-do-fail t)" "test" "dummy passing")
    (should (= exit-code 0))))


(provide 'test/integration/buttercup)

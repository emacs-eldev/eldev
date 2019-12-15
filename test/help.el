(require 'test/common)


(ert-deftest emake-test-help-1 ()
  (emake--test-run "empty-project" ("help")
    ;; `emake-help' also specifies default options, which are
    ;; difficult to syncronize between the two processes.
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-first-line (emake--test-capture-output (emake-help))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest emake-test-help-2 ()
  (emake--test-run "empty-project" ("--help")
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-first-line (emake--test-capture-output (emake-help))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest emake-test-help-command-1 ()
  (emake--test-run "empty-project" ("help" "init")
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-first-line (emake--test-capture-output (emake-help "init"))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest emake-test-help-command-2 ()
  (emake--test-run "empty-project" ("init" "--help")
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-first-line (emake--test-capture-output (emake-help "init"))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest emake-test-help-missing-dependency-1 ()
  (emake--test-run "missing-dependency-a" ("help")
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-first-line (emake--test-capture-output (emake-help))))
                             stdout))
    (should (= exit-code 0))))


(provide 'test/help)

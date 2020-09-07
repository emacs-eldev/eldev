(require 'test/common)


(ert-deftest eldev-help-1 ()
  (eldev--test-run "empty-project" ("help")
    ;; `eldev-help' also specifies default options, which are
    ;; difficult to syncronize between the two processes.
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))
    (should (= exit-code 0))))

;; While `--help' is not advertised, we silently support it.
(ert-deftest eldev-help-2 ()
  (eldev--test-run "empty-project" ("--help")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-help-command-1 ()
  (eldev--test-run "empty-project" ("help" "init")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help "init"))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-help-command-2 ()
  (eldev--test-run "empty-project" ("init" "--help")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help "init"))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-help-missing-dependency-1 ()
  (eldev--test-run "missing-dependency-a" ("help")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))
    (should (= exit-code 0))))


(provide 'test/help)

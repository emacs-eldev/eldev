;;  -*- lexical-binding: t -*-

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

(ert-deftest eldev-help-unknown-command ()
  (eldev--test-run "empty-project" ("help" "there-is-no-such-command")
    (should (string-match "there-is-no-such-command" stderr))
    (should (string-match "list of known commands"   stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-help-missing-dependency-1 ()
  (eldev--test-run "missing-dependency-a" ("help")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))
    (should (= exit-code 0))))


(eldev-ert-defargtest eldev-help-sorts-option (short-first)
                      (nil t)
  (eldev--test-run "empty-project" ("--setup" `(eldev-defoption eldev--test-custom-option ()
                                                 "Only for testing"
                                                 :options     ,(if short-first '(-X --custom-option) '(--custom-option -X))
                                                 :for-command help)
                                    "help" "help")
    ;; Should sort short options first, even if (accidentally) specified otherwise when defining.
    (should (string-match-p "-X, --custom-option" stdout))
    (should (= exit-code 0))))


(provide 'test/help)

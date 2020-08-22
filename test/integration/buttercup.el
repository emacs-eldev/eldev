(require 'test/common)


(ert-deftest eldev-test-buttercup-project-e-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-e" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-buttercup-project-e-2 ()
  ;; Second tests fails with this setup.
  (eldev--test-run "project-e" ("--setup" `(defvar eldev--buttercup-do-fail t) "test")
    (should (= exit-code 1))))

(ert-deftest eldev-test-buttercup-project-e-3 ()
  ;; Second tests still fails, but it should not be executed.
  (eldev--test-run "project-e" ("--setup" `(defvar eldev--buttercup-do-fail t) "test" "dummy passing")
    (should (= exit-code 0))))


(ert-deftest eldev-test-buttercup-erroneous-backtrace-1 ()
  ;; Until Eldev 0.7 (and Buttercup 1.23) a pointless and unneded backtrace would have
  ;; been printed if a test failed and Eldev was in debug mode.
  (eldev--test-run "project-e" ("--debug" "--setup" `(defvar eldev--buttercup-do-fail t) "test")
    ;; Intentionally no closing paren so that the test doesn't break if the function's
    ;; signature changes in the future.
    (should (not (string-match-p (rx "eldev-test\\(") stdout)))
    (should (= exit-code 1))))


(provide 'test/integration/buttercup)

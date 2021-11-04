(require 'test/common)


(ert-deftest eldev-test-ecukes-project-h-1 ()
  ;; Three tests, all pass.
  (eldev--test-run "project-h" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-ecukes-project-h-2 ()
  ;; Second tests fails with this setup.
  (eldev--test-run "project-h" ("--setup" `(defvar eldev--ecukes-pass-if "bar") "test")
    (should (= exit-code 1))))

(ert-deftest eldev-test-ecukes-project-h-3 ()
  ;; Second tests still fails, but it should not be executed.
  (eldev--test-run "project-h" ("--setup" `(defvar eldev--ecukes-pass-if "bar") "test" "~@shaky")
    (should (= exit-code 0))))

(ert-deftest eldev-test-ecukes-project-h-4 ()
  ;; Second tests still fails, but it should not be executed.
  (eldev--test-run "project-h" ("--setup" `(defvar eldev--ecukes-pass-if "bar") "test" "robust.feature")
    (should (= exit-code 0))))


(provide 'test/integration/ecukes)

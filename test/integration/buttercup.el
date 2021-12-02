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


;; `project-i' has two ERT and two Buttercup tests.  Make sure that this combination works
;; both in autodetermined and explicit ways.

(eldev-ert-defargtest eldev-test-buttercup-project-i-1 (command frameworks-configured with-ert-fileset)
                      ;; Test most combinations.
                      (('test           nil nil)
                       ('test           t   nil)
                       ('test           t   t)
                       ('test-ert       nil nil)
                       ('test-ert       nil t)
                       ('test-ert       t   nil)
                       ('test-ert       t   t)
                       ('test-buttercup nil nil)
                       ('test-buttercup t   nil)
                       ('test-buttercup t   t))
  (let ((eldev--test-project "project-i")
        testing
        ran-ert
        ran-buttercup
        ran-both)
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(setf eldev-test-framework   ,(when frameworks-configured ''(ert buttercup)))
                          "--setup" `(setf eldev-test-ert-fileset ,(when with-ert-fileset      "ert.el"))
                          command)
      (setf testing call-info)
      ;; Make sure that the exact tests executed match the issued command.
      (setf ran-ert       (string-match-p "Running 2 tests"            stdout)
            ran-buttercup (string-match-p "Running 2 specs"            stdout)
            ran-both      (string-match-p "Testing summary: 4 test(s)" stdout))
      (pcase command
        (`test           (should     (and ran-ert ran-buttercup ran-both)))
        (`test-ert       (should     ran-ert)
                         (should-not (or ran-buttercup ran-both)))
        (`test-buttercup (should     ran-buttercup)
                         (should-not (or ran-ert ran-both))))
      (should (= exit-code 0)))
    ;; Currently disabled, see comments in `eldev--test-autoinstalling-framework'.  I
    ;; don't want to change this now, not for a patch release.  So, in this test Buttercup
    ;; always gets installed if `frameworks-configured' is non-nil and we don't count that
    ;; as a failure yet.
    (when nil
      (unless ran-buttercup
        ;; If we run only ERT tests, Buttercup should not be even installed (depending on
        ;; test parameters), thanks to setting variable `eldev-test-ert-fileset' above,
        ;; which prevents the test file with form `(require 'buttercup)' from loading.
        (eldev--test-run nil ("version" "buttercup")
          :previous-call "Testing command" testing
          (should (= exit-code (if with-ert-fileset 1 0))))))))


(ert-deftest eldev-test-buttercup-erroneous-backtrace-1 ()
  ;; Until Eldev 0.7 (and Buttercup 1.23) a pointless and unneded backtrace would have
  ;; been printed if a test failed and Eldev was in debug mode.
  (eldev--test-run "project-e" ("--debug" "--setup" `(defvar eldev--buttercup-do-fail t) "test")
    ;; Intentionally no closing paren so that the test doesn't break if the function's
    ;; signature changes in the future.
    (should (not (string-match-p (rx "eldev-test\\(") stdout)))
    (should (= exit-code 1))))


(provide 'test/integration/buttercup)

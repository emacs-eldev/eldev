;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-test-doctest-project-m-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-m" ("test")
    (eldev--test-skip-if-missing-tool exit-code stderr)
    (should (string-match-p "2 passed" stdout))
    (should (string-match-p "0 failed" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-test-doctest-project-m-2 ()
  ;; Two tests, and we make of them fail.
  (eldev--test-run "project-m" ("--setup" `(setf project-m-hello-to-verb "Goodbye") "test")
    (eldev--test-skip-if-missing-tool exit-code stderr)
    (should (string-match-p "1 passed" stdout))
    (should (string-match-p "1 failed" stdout))
    (should (= exit-code 1))))


(provide 'test/integration/doctest)

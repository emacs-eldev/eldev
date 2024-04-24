;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-test-doctest-project-m-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-m" ("test")
    (eldev--test-skip-if-missing-tool exit-code stderr)
    (should (string-match-p "2 passed" stdout))
    (should (string-match-p "0 failed" stdout))
    (should (= exit-code 0))))


(provide 'test/integration/doctest)

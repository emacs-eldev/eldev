(require 'test/common)


(ert-deftest eldev-defcommand-1 ()
  ;; `project-g/Eldev' defines a new command that is like `test', but for a specific file.
  (eldev--test-run "project-g" ("test-integration")
    (should (string-match-p "passed.+project-g-integration-1" stdout))
    (should (string-match-p "Ran 1 test" stdout))
    (should (= exit-code 0)))
  ;; The new command inherits most options from standard `test'.
  (eldev--test-run "project-g" ("test-integration" "--stop" "-B" "--expect=1")
    (should (string-match-p "passed.+project-g-integration-1" stdout))
    (should (string-match-p "Ran 1 test" stdout))
    (should (= exit-code 0)))
  ;; But it doesn't inherit option `--file', as it doesn't make sense for it.
  (eldev--test-run "project-g" ("test-integration" "--file" "*.el")
    (should (string-match-p "Unknown option .--file." stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-defcommand-inheriting-options-1 ()
  ;; `eldev-inherit-options' should produce help similar to the source command.
  (eldev--test-run "project-g" ("help" "test-integration")
    (should (member "  -r, --runner=NAME     Choose test runner [default: simple]" (eldev--test-line-list stdout)))
    (should (= exit-code 0))))


(provide 'test/extending)

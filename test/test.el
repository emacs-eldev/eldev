(require 'test/common)


(ert-deftest eldev-test-test-project-a-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-a" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-test-project-b-1 ()
  ;; Two tests, one of them fails.
  (eldev--test-run "project-b" ("test")
    (should (= exit-code 1))))

(ert-deftest eldev-test-test-project-b-2 ()
  ;; Only one passing test.
  (eldev--test-run "project-b" ("test" "hello")
    (should (= exit-code 0))))

(ert-deftest eldev-test-test-project-b-3 ()
  ;; Only one failing test.
  (eldev--test-run "project-b" ("test" "failing")
    (should (= exit-code 1))))

(ert-deftest eldev-test-test-project-b-4 ()
  ;; No files match, so zero tests.
  (eldev--test-run "project-b" ("test" "--file" "there-is-no-such-file.el")
    (should (string= stdout "No test files to load\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-test-test-project-b-5 ()
  ;; No tests match, but the file should be loaded.
  (eldev--test-run "project-b" ("test" "there-are-no-such-tests")
    (should (not (string= stdout "No test files to load\n")))
    (should (= exit-code 0))))

(ert-deftest eldev-test-test-project-c-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-c" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-test-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("test")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(provide 'test/test)

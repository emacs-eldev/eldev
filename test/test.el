(require 'test/common)


(ert-deftest eldev-test-project-a-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-a" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-b-1 ()
  ;; Two tests, one of them fails.
  (eldev--test-run "project-b" ("test")
    (should (= exit-code 1))))

(ert-deftest eldev-test-project-b-2 ()
  ;; Only one passing test.
  (eldev--test-run "project-b" ("test" "hello")
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-b-3 ()
  ;; Only one failing test.
  (eldev--test-run "project-b" ("test" "failing")
    (should (= exit-code 1))))

(ert-deftest eldev-test-project-b-4 ()
  ;; No files match, so zero tests.
  (eldev--test-run "project-b" ("test" "--file" "there-is-no-such-file.el")
    (should (string= stdout "No test files to load\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-b-5 ()
  ;; No tests match, but the file should be loaded.
  (eldev--test-run "project-b" ("test" "there-are-no-such-tests")
    (should (not (string= stdout "No test files to load\n")))
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-c-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-c" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-g-1 ()
  ;; One passing test.  This project uses a loading root for its tests.
  (eldev--test-run "project-g" ("test")
    (should (= exit-code 0))))

(ert-deftest eldev-test-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("test")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


;; Pre-0.1.2 ERT integration code used to fail on Emacs 26.x because
;; of a faulty advice for `ert--print-backtrace'.
(ert-deftest eldev-test-ert-print-backtrace-advise-1 ()
  (eldev--test-run "project-d" ("test")
    ;; If Eldev itself (rather than tests) fails, there will be a
    ;; different message.
    (should (string-match-p "\\`\n*[0-9]+ tests? produced an unexpected result\n*\\'" stderr))))


(ert-deftest eldev-test-utility-files-in-package-mode-1 ()
  ;; Pre-0.8 Eldev would fail to test projects loaded in packaged mode if they had any
  ;; test utility files (subject to file ordering, so we specify target file explicitly).
  (eldev--test-run "project-c" ("--packaged" "test" "project-c.el")
    (should (= exit-code 0))))


(provide 'test/test)

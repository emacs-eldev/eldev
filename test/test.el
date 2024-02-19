;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-test-project-b-1 ()
  ;; Two tests, one of them fails.
  (eldev--test-run "project-b" ("test")
    (should (string-match-p "passed.+project-b-test-hello" stdout))
    (should (string-match-p "FAILED.+project-b-test-triviality-failing" stdout))
    (should (string-match-p "Ran 2 tests" stdout))
    (should (= exit-code 1))))

(ert-deftest eldev-test-project-b-2 ()
  ;; Only one passing test.
  (eldev--test-run "project-b" ("test" "hello")
    (should (string-match-p "passed.+project-b-test-hello" stdout))
    (should (string-match-p "Ran 1 test" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-b-3 ()
  ;; Only one failing test.
  (eldev--test-run "project-b" ("test" "failing")
    (should (string-match-p "Ran 1 test" stdout))
    (should (= exit-code 1))))

(ert-deftest eldev-test-project-b-4 ()
  ;; No files match, so zero tests.
  (eldev--test-run "project-b" ("test" "--file" "there-is-no-such-file.el")
    (should (string= stdout "No test files to use\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-b-5 ()
  ;; No tests match, but the file should be loaded.
  (eldev--test-run "project-b" ("test" "there-are-no-such-tests")
    (should (not (string= stdout "No test files to use\n")))
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-c-1 ()
  ;; Two tests, all pass.
  (eldev--test-run "project-c" ("test")
    (should (string-match-p "passed.+project-c-test-hello" stdout))
    (should (string-match-p "passed.+project-c-test-triviality" stdout))
    (should (string-match-p "Ran 2 tests" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-test-project-g-1 ()
  ;; Two passing tests, one is "integration test", but here we run it too.  This project
  ;; uses a loading root for its tests.
  (eldev--test-run "project-g" ("test")
    (should (string-match-p "passed.+project-g-1" stdout))
    (should (string-match-p "passed.+project-g-integration-1" stdout))
    (should (string-match-p "Ran 2 tests" stdout))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-test-project-l-1 (loading-mode)
                      ('source 'byte-compiled 'packaged)
  ;; Project with special source directories.  One of the tests needs a resource located
  ;; in the second source directory; make sure it can be found in various loading modes.
  (eldev--test-run "project-l" ("--loading" loading-mode "test")
    (should (string-match-p "passed.+project-l-test-hello" stdout))
    (should (string-match-p "passed.+project-l-resource-contents" stdout))
    (should (string-match-p "Ran 2 tests" stdout))
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
  ;; To make sure dependencies are there, else stderr will be "polluted".
  (eldev--test-run "project-d" ("prepare")
    (should (= exit-code 0)))
  (eldev--test-run "project-d" ("test")
    ;; If Eldev itself (rather than tests) fails, there will be a
    ;; different message.
    (should (string-match-p "\\`\n*[0-9]+ ERT tests? produced an unexpected result\n*\\'" stderr))))


(ert-deftest eldev-test-utility-files-in-package-mode-1 ()
  ;; Pre-0.8 Eldev would fail to test projects loaded in packaged mode if they had any
  ;; test utility files (subject to file ordering, so we specify target file explicitly).
  (eldev--test-run "project-c" ("--packaged" "test" "project-c.el")
    (should (string-match-p "passed.+project-c-test-hello" stdout))
    (should (string-match-p "passed.+project-c-test-triviality" stdout))
    (should (string-match-p "Ran 2 tests" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-test-utility-files-in-package-mode-2 ()
  ;; Same as above, for a project with special source directories.
  (eldev--test-run "project-l" ("--packaged" "test" "project-l.el")
    (should (string-match-p "passed.+project-l-test-hello" stdout))
    (should (string-match-p "passed.+project-l-resource-contents" stdout))
    (should (string-match-p "Ran 2 tests" stdout))
    (should (= exit-code 0))))


(ert-deftest eldev-test-expect-1 ()
  ;; Using `test-ert' here to also indirectly make sure that the command understands the
  ;; option, just like `test'.
  (eldev--test-run "project-a" ("test-ert" "--expect" 2)
    (should (string-match-p "passed.+project-a-test-hello" stdout))
    (should (string-match-p "passed.+project-a-test-triviality" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-test-expect-2 ()
  (eldev--test-run "project-a" ("test" "--expect" 10)
    (should (string-match-p "Too few tests" stderr))
    (should (= exit-code 1))))


(eldev-ert-defargtest eldev-test-read-ert-results (inject-cyclic-list)
                      (nil t)
  (let ((eldev--test-project "project-b")
        ;; Previously Eldev would fail to read back ERT results because
        ;; `eldev-do-save-cache-file' explicitly disabled proper circular structure saving
        ;; by setting `print-circle' to nil for whatever reason.  This would cause
        ;; `test-ert :failed' to be no-op below, so the process would succeed, making this
        ;; test (`eldev-test-read-ert-results') fail.
        (test-injection      (when inject-cyclic-list
                               '("--setup" (with-eval-after-load 'ert
                                             (ert-deftest project-b-injected ()
                                               (let ((cyclic (list nil)))
                                                 (setf (cdr cyclic) cyclic)
                                                 (should (equal cyclic nil)))))))))
    (eldev--test-delete-cache)
    (eldev--test-run nil (:eval `(,@test-injection "test-ert"))
      (should (= exit-code 1)))
    (eldev--test-run nil (:eval `(,@test-injection "test-ert" :failed))
      (should (= exit-code 1)))))


(ert-deftest eldev-test-project-a-concise ()
  (eldev--test-run "project-a" ("test" "--runner" "concise")
    (should-not (string-match-p "project-a-test-hello" stdout))
    (should     (string-match-p (rx bol ".. 2/2" eol) stdout))
    (should     (= exit-code 0))))


(ert-deftest eldev-test-runner-concise-tick ()
  ;; Also simulate a "need to print failure information" after "test" #155.
  (eldev--test-run "trivial-project" ("exec" `(dolist (n (number-sequence 1 220)) (eldev-test-runner-concise-tick (= n 155) n 220)))
    (should (string= stdout "\
..................................................  50/220
.................................................. 100/220
.................................................. 150/220
..... 155/220
.................................................. 205/220
............... 220/220
"))
    (should (= exit-code 0))))


(ert-deftest eldev-test-project-c-disabled-dependencies-1 ()
  ;; Dependencies are disabled.  Testing a project with dependencies without setting
  ;; `load-path' must fail.
  (let ((eldev--test-project "project-c"))
    (eldev--test-delete-cache)
    ;; Installing dependencies in the project's cache must not help.
    (eldev--test-run nil ("prepare")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--disable-dependencies" "test")
      (should (string-match-p "Cannot open.+project-c" stderr))
      (should (= exit-code 1)))))

(ert-deftest eldev-test-project-c-disabled-dependencies-2 ()
  ;; Dependencies are disabled, but we supply `load-path', so testing should work.
  (let* ((eldev--test-project "project-c")
         ;; In real use, `dependency-a' would be found from the project sources by whoever
         ;; sets `EMACSLOADPATH'.  Here we hardcode it to simplify things.
         (process-environment `(,(eldev--test-emacsloadpath (eldev--test-project-dir) (eldev--test-project-dir "dependency-a"))
                                ,@process-environment)))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--disable-dependencies" "test")
      (should (string-match-p "Ran 2 tests" stdout))
      (should (= exit-code 0)))))


(provide 'test/test)

(require 'test/common)


;; Intentionally always running child Emacs in batch mode.

(ert-deftest emake-test-emacs-1 ()
  (emake--test-run "trivial-project" ("emacs" "--batch")
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-2 ()
  ;; Unlike our `eval' Emacs doesn't print `--eval' results.
  (emake--test-run "trivial-project" ("emacs" "--batch" "--eval" "(prin1 (+ 1 2))")
    (should (string= stdout "3\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-3 ()
  (emake--test-run "project-a" ("--quiet" "emacs" "--batch" "--eval" "(princ (project-a-hello))")
    (should (string= stdout "Hello\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-4 ()
  (emake--test-run "project-b" ("--quiet" "emacs" "--batch" "--eval" "(princ (project-b-hello))")
    (should (string= stdout "Hello\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-5 ()
  (emake--test-run "project-c" ("--quiet" "emacs" "--batch" "--eval" "(princ (project-c-hello))")
    (should (string= stdout "Hello\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-6 ()
  ;; Important to test as the "project" involves some macro magic.
  (emake--test-run "project-e" ("--quiet" "emacs" "--batch" "--eval" "(princ (project-e-hello))")
    (should (string= stdout "Hello\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("emacs")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(provide 'test/emacs)

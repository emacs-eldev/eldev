(require 'test/common)


;; Intentionally always running child Emacs in batch mode.

(ert-deftest eldev-emacs-1 ()
  (eldev--test-run "trivial-project" ("emacs" "--batch")
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-2 ()
  ;; Unlike our `eval' Emacs doesn't print `--eval' results.
  (eldev--test-run "trivial-project" ("emacs" "--batch" "--eval" `(prin1 (+ 1 2)))
    (should (string= stdout "3"))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-3 ()
  (eldev--test-run "project-a" ("--quiet" "emacs" "--batch" "--eval" `(princ (project-a-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-4 ()
  (eldev--test-run "project-b" ("--quiet" "emacs" "--batch" "--eval" `(princ (project-b-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-5 ()
  (eldev--test-run "project-c" ("--quiet" "emacs" "--batch" "--eval" `(princ (project-c-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-6 ()
  ;; Important to test as the "project" involves some macro magic.
  (eldev--test-run "project-e" ("--quiet" "emacs" "--batch" "--eval" `(princ (project-e-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("emacs")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

;; Most of the test for debugging output are in `exec.el'.  We don't really need to spawn
;; yet another Emacs to test it.
(ert-deftest eldev-emacs-debugging-output-1 ()
  ;; Default value of `eldev-emacs-autorequire-eldev' means that function `eldev-debug' is
  ;; available automatically.
  (eldev--test-run "project-a" ("--quiet" "emacs" "--batch" "--eval" `(eldev-debug "Hello"))
    ;; Because of how `:forward-output' of `eldev-call-process' works (this is a
    ;; limitation of Elisp though, not Eldev), inner Eldev process forwards Emacs' stderr
    ;; to its own stdout.  So there.
    (should (string= stdout "Hello\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-project-isolation-1 ()
  (eldev--test-run "trivial-project" ("emacs" "--batch" "--eval" ` (princ user-emacs-directory))
    (should (file-in-directory-p (string-trim stdout) (eldev--test-project-dir)))
    (should (= exit-code 0))))

(ert-deftest eldev-emacs-project-isolation-2 ()
  (eldev--test-run "trivial-project" ("emacs" "--batch" "--eval" `(progn (require 'package) (princ package-user-dir)))
    (should (file-in-directory-p (string-trim stdout) (eldev--test-project-dir)))
    (should (= exit-code 0))))


(provide 'test/emacs)

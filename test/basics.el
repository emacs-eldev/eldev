(require 'test/common)


(ert-deftest eldev-just-run-1 ()
  (eldev--test-run "empty-project" ()
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
    (should (= exit-code 0))))

;; This should work even in broken projects.
(ert-deftest eldev-just-run-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ()
      (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
      (should (= exit-code 0)))))

(ert-deftest eldev-bootstrapping-1 ()
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (let ((eldev--test-project     "trivial-project")
        (eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
        (eldev--test-eldev-dir   (eldev--test-tmp-subdir "bootstrap-root")))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" "(byte-code-function-p (symbol-function 'eldev-cli))")
      (should (string= stdout "t\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-command-hook-1 ()
  ;; Just make sure that the hook is executed.
  (eldev--test-run "empty-project" ("--setup" (prin1-to-string '(add-hook 'eldev-help-hook (lambda () (error "fail!")))) "help")
    (should (= exit-code 1))))


(provide 'test/basics)

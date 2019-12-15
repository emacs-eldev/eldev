(require 'test/common)


(ert-deftest emake-test-just-run-1 ()
  (emake--test-run "empty-project" ()
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-capture-output (emake-usage))) stdout))
    (should (= exit-code 0))))

;; This should work even in broken projects.
(ert-deftest emake-test-just-run-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ()
      (should (string-prefix-p (emake--test-in-project-environment (emake--test-capture-output (emake-usage))) stdout))
      (should (= exit-code 0)))))

(ert-deftest emake-test-bootstrapping-1 ()
  (emake--test-create-emake-archive "emake-archive-1")
  (let ((emake--test-project     "trivial-project")
        (emake--test-emake-local (concat ":pa:" (emake--test-tmp-subdir "emake-archive-1")))
        (emake--test-emake-dir   (emake--test-tmp-subdir "bootstrap-root")))
    (ignore-errors (delete-directory emake--test-emake-dir t))
    (emake--test-run nil ("version")
      (should (string= stdout (format "emake %s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "(byte-code-function-p (symbol-function 'emake-cli))")
      (should (string= stdout "t\n"))
      (should (= exit-code 0)))))


(provide 'test/basics)

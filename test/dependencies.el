(require 'test/common)


(ert-deftest emake-test-dependencies-1 ()
  (emake--test-run "trivial-project" ("dependencies")
    (should (string-match "no dependencies" stdout))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependencies-2 ()
  (emake--test-run "trivial-project" ("--quiet" "dependencies")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependencies-3 ()
  (emake--test-run "project-a" ("dependencies")
    (should (string= stdout "dependency-a (any)\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependencies-4 ()
  (emake--test-run "project-b" ("dependencies")
    (should (string= stdout "dependency-b (any)\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependencies-5 ()
  (emake--test-run "project-c" ("dependencies")
    (should (string= stdout "dependency-a (any)\n"))
    (should (= exit-code 0))))

;; It doesn't matter that the dependency is unresolvable.
(ert-deftest emake-test-dependencies-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("dependencies")
      (should (string= stdout "dependency-a (any)\n"))
      (should (= exit-code 0)))))


(provide 'test/dependencies)

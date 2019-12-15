(require 'test/common)


(ert-deftest emake-test-dependency-tree-1 ()
  (emake--test-run "trivial-project" ("dependency-tree")
    (should (string-match "trivial-project 1.0\n" stdout))
    (should (= exit-code 0))))

;; Dependencies might already be installed due to some previous tests,
;; or they might be not.

(ert-deftest emake-test-dependency-tree-2 ()
  (emake--test-run "project-a" ("--quiet" "dependency-tree")
    (should (or (string= stdout "project-a 1.0\n    dependency-a (any)\n")
                (string= stdout "project-a 1.0\n    dependency-a (any)    [1.0 installed]\n")))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependency-tree-3 ()
  (emake--test-run "project-b" ("--quiet" "dependency-tree")
    (should (or (string= stdout "project-b 1.0\n    dependency-b (any)\n        dependency-a (any)\n")
                (string= stdout "project-b 1.0\n    dependency-b (any)    [1.0 installed]\n        dependency-a (any)    [1.0 installed]\n")))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependency-tree-4 ()
  (emake--test-run "project-c" ("--quiet" "dependency-tree")
    (should (or (string= stdout "project-c 1.0\n    dependency-a (any)\n")
                (string= stdout "project-c 1.0\n    dependency-a (any)    [1.0 installed]\n")))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependency-tree-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("--quiet" "dependency-tree")
      (should (string= stdout "missing-dependency-a 1.0\n    dependency-a (any)    [UNAVAILABLE]\n"))
      (should (= exit-code 0)))))


(provide 'test/dependency-tree)

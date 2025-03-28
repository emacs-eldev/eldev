;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-dependencies-1 ()
  (eldev--test-run "trivial-project" ("dependencies")
    (should (string-match "no dependencies" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-dependencies-2 ()
  (eldev--test-run "trivial-project" ("--quiet" "dependencies")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-dependencies-3 ()
  (eldev--test-run "project-a" ("dependencies")
    (should (string= stdout "dependency-a 0.9\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-dependencies-4 ()
  (eldev--test-run "project-b" ("dependencies")
    (should (string= stdout "dependency-b (any)\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-dependencies-5 ()
  (eldev--test-run "project-c" ("dependencies")
    (should (string= stdout "dependency-a (any)\n"))
    (should (= exit-code 0))))

;; It doesn't matter that the dependency is unresolvable.
(ert-deftest eldev-dependencies-missing-dependency-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    ;; It might be installed by a different test that provides a
    ;; suitable archive in setup form.
    (eldev--test-delete-cache)
    (eldev--test-run nil ("dependencies")
      (should (string= stdout "dependency-a 0.1\n"))
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-dependencies-extra-1 (dependency)
                      ('dependency-b '(:package dependency-b))
  (eldev--test-run "project-a" ("--setup" `(eldev-add-extra-dependencies 'build ',dependency) "dependencies" "build")
    (should (string= stdout (eldev--test-lines "dependency-a 0.9" "dependency-b (any)    [for `build']")))
    (should (= exit-code 0))))


(provide 'test/dependencies)

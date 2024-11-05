;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-dependency-tree-1 ()
  (eldev--test-run "trivial-project" ("dependency-tree")
    (should (string-match "trivial-project 1.0\n" stdout))
    (should (= exit-code 0))))

;; Dependencies might already be installed due to some previous tests,
;; or they might be not.

(ert-deftest eldev-dependency-tree-2 ()
  (eldev--test-run "project-a" ("--quiet" "dependency-tree")
    (should (or (string= stdout (eldev--test-lines "project-a 1.0"
                                                   "    dependency-a 0.9"))
                (string= stdout (eldev--test-lines "project-a 1.0"
                                                   "    dependency-a 0.9    [1.0 installed]"))))
    (should (= exit-code 0))))

(ert-deftest eldev-dependency-tree-3 ()
  (eldev--test-run "project-b" ("--quiet" "dependency-tree")
    (should (or (string= stdout (eldev--test-lines "project-b 1.0"
                                                   "    dependency-b (any)"
                                                   "        dependency-a (any)"))
                (string= stdout (eldev--test-lines "project-b 1.0"
                                                   "    dependency-b (any)    [1.0 installed]"
                                                   "        dependency-a (any)    [1.0 installed]"))))
    (should (= exit-code 0))))

(ert-deftest eldev-dependency-tree-4 ()
  (eldev--test-run "project-c" ("--quiet" "dependency-tree")
    (should (or (string= stdout (eldev--test-lines "project-c 1.0"
                                                   "    dependency-a (any)"))
                (string= stdout (eldev--test-lines "project-c 1.0"
                                                   "    dependency-a (any)    [1.0 installed]"))))
    (should (= exit-code 0))))

(ert-deftest eldev-dependency-tree-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "dependency-tree")
      (should (string= stdout (eldev--test-lines "missing-dependency-a 1.0"
                                                 "    dependency-a 0.1    [UNAVAILABLE]")))
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-dependency-tree-extra-1 (dependency)
                      ('dependency-b '(:package dependency-b))
  (eldev--test-run "project-a" ("--setup" `(eldev-add-extra-dependencies 'build ',dependency) "dependency-tree" "build")
    (should (or (string= stdout (eldev--test-lines "project-a 1.0"
                                                   "    dependency-a 0.9"
                                                   "dependency-b (any)    [for `build']"
                                                   "    dependency-a (any)"))
                (string= stdout (eldev--test-lines "project-a 1.0"
                                                   "    dependency-a 0.9    [1.0 installed]"
                                                   "dependency-b (any)    [for `build'; 1.0 installed]"
                                                   "    dependency-a (any)    [1.0 installed]"))
                ;; Some test could upgrade installed dependencies using `archive-b'.
                (string= stdout (eldev--test-lines "project-a 1.0"
                                                   "    dependency-a 0.9    [1.1 installed]"
                                                   "dependency-b (any)    [for `build'; 1.1 installed]"
                                                   "    dependency-a 1.1"))))
    (should (= exit-code 0))))


(provide 'test/dependency-tree)

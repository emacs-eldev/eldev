(require 'test/common)


(ert-deftest eldev-package-lint-project-a-1 ()
  (eldev--test-run "project-a" ("lint" "package" "--required")
    (eldev--test-skip-if-missing-linter exit-code stderr)
    (should (= exit-code 0))))

(ert-deftest eldev-package-lint-project-b-1 ()
  ;; E.g. some headers don't follow linter's suggestions.
  (eldev--test-run "project-b" ("lint" "package" "--required")
    (eldev--test-skip-if-missing-linter exit-code stderr)
    (should (= exit-code 1))))

(ert-deftest eldev-package-lint-project-c-1 ()
  ;; Starting with Eldev 0.10 `package-lint' should be configured to know project's main
  ;; file.  Previously it would complain about missing headers in `project-c.el'.
  (eldev--test-run "project-c" ("lint" "package" "--required")
    (eldev--test-skip-if-missing-linter exit-code stderr)
    (should (= exit-code 0))))

(ert-deftest eldev-package-lint-project-d-1 ()
  ;; See comments in `eldev-package-lint-project-c-1'.
  (eldev--test-run "project-d" ("lint" "package" "--required")
    (eldev--test-skip-if-missing-linter exit-code stderr)
    (should (= exit-code 0))))


;; https://github.com/doublep/eldev/issues/19
;;
;; `package-lint' would deem local dependencies uninstallable if they were not available
;; from "normal" archives.
(ert-deftest eldev-package-lint-local-dependencies-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a")
                          "lint" "package" "--required")
      (eldev--test-skip-if-missing-linter exit-code stderr)
      (should (= exit-code 0)))))


(provide 'test/integration/package-lint)

(require 'test/common)


(ert-deftest eldev-package-lint-project-a-1 ()
  (eldev--test-run "project-a" ("lint" "package")
    (should (= exit-code 0))))

(ert-deftest eldev-package-lint-project-b-1 ()
  ;; E.g. some headers don't follow linter's suggestions.
  (eldev--test-run "project-b" ("lint" "doc")
    (should (= exit-code 1))))


(provide 'test/integration/package-lint)

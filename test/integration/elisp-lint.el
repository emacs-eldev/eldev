(require 'test/common)


(ert-deftest eldev-elisp-lint-project-a-1 ()
  ;; I hope eventually the linter itself solves this crap with creating files.
  (eldev--test-without-files "project-a" ("project-a-autoloads.el" "project-a-autoloads.el~")
    (eldev--test-run nil ("lint" "el")
      (should (= exit-code 0)))))

(ert-deftest eldev-elisp-lint-project-b-1 ()
  (eldev--test-without-files "project-b" ("project-b-autoloads.el" "project-b-autoloads.el~")
    ;; E.g. some headers don't follow linter's suggestions.
    (eldev--test-run nil ("lint" "el")
      (should (= exit-code 1)))))


(provide 'test/integration/package-lint)

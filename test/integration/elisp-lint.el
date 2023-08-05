(require 'test/common)


(ert-deftest eldev-elisp-lint-project-a-1 ()
  ;; I hope eventually the linter itself solves this crap with creating files.
  (eldev--test-without-files "project-a" ("project-a-autoloads.el" "project-a-autoloads.el~")
    (eldev--test-run nil ("lint" "el" "--required")
      (eldev--test-skip-if-missing-linter exit-code stderr)
      (should (= exit-code 0)))))

(ert-deftest eldev-elisp-lint-project-b-1 ()
  (eldev--test-without-files "project-b" ("project-b-autoloads.el" "project-b-autoloads.el~")
    ;; E.g. some headers don't follow linter's suggestions.
    (eldev--test-run nil ("lint" "el" "--required")
      (eldev--test-skip-if-missing-linter exit-code stderr)
      (should (= exit-code 1)))))


;; https://github.com/emacs-eldev/eldev/issues/19
;;
;; `package-lint' (called by by `elisp-lint') would deem local dependencies uninstallable
;; if they were not available from "normal" archives.
(ert-deftest eldev-elisp-lint-local-dependencies-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-without-files nil ("missing-dependency-a-autoloads.el" "missing-dependency-a-autoloads.el~")
      (eldev--test-delete-cache)
      (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a")
                            "lint" "el" "--required")
        (eldev--test-skip-if-missing-linter exit-code stderr)
        (should (= exit-code 0))))))


(provide 'test/integration/package-lint)

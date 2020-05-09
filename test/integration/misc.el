(require 'test/common)


;; https://github.com/doublep/eldev/issues/10
;; Command `upgrade-self' would fail.
(ert-deftest eldev-issue-10 ()
  (eldev--test-run "empty-project" ("upgrade-self" "--dry-run")
    (should (= exit-code 0))))

;; https://github.com/doublep/eldev/issues/12
;; Autoloading Buttercup and using `melpa-stable' package archive in
;; `Eldev' at the same time would fail.
(ert-deftest eldev-issue-12 ()
  (eldev--test-without-files "issue-12-project" "mypkg.elc"
    (eldev--test-delete-cache)
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "mypkg.elc")
      (should (= exit-code 0)))))


(provide 'test/integration/misc)

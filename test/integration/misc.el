(require 'test/common)


;; https://github.com/doublep/eldev/issues/10
;;
;; Command `upgrade-self' would fail.
(ert-deftest eldev-issue-10 ()
  (eldev--test-run "empty-project" ("upgrade-self" "--dry-run")
    (should (= exit-code 0))))

;; https://github.com/doublep/eldev/issues/12
;;
;; Autoloading Buttercup and using `melpa-stable' package archive in
;; `Eldev' at the same time would fail.
(ert-deftest eldev-issue-12 ()
  (eldev--test-without-files "issue-12-project" "mypkg.elc"
    (eldev--test-delete-cache)
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "mypkg.elc")
      (should (= exit-code 0)))))

;; https://github.com/doublep/eldev/issues/18
;;
;; Eldev wouldn't install new `Org' if a project wanted a version newer than built-in if
;; another dependency on Org is declared.  To test this, we declare that Org 999.999 is
;; needed and add a dummy dependency.
(ert-deftest eldev-issue-18 ()
  (let ((eldev--test-project "issue-18-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      (should (string-match-p (rx "version 999.999 is required") stderr))
      (should (= exit-code 1)))))

;; https://github.com/doublep/eldev/issues/32
;;
;; Eldev would fail to provide Org snapshot to a project that depends on Org version newer
;; than what is built into Emacs, even if appropriate package archive was configured.  Was
;; triggered by the bug in `eldev--global-cache-url-retrieve-synchronously'.
(ert-deftest eldev-issue-32 ()
  (let ((eldev--test-project "issue-32-project"))
    (eldev--test-delete-cache)
    ;; Test that it fails when no archive is configured.
    (eldev--test-run nil ("prepare")
      (should (= exit-code 1)))
    (eldev--test-delete-cache)
    ;; But with an appropriate archive it should work.
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive `("org" . "https://orgmode.org/elpa/")) "prepare")
      (should (= exit-code 0)))))


(provide 'test/integration/misc)

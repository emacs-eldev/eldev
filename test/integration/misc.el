;;  -*- lexical-binding: t -*-

(require 'test/common)


;; https://github.com/emacs-eldev/eldev/issues/10
;;
;; Command `upgrade-self' would fail.
(ert-deftest eldev-issue-10 ()
  (eldev--test-run "empty-project" ("upgrade-self" "--dry-run")
    (should (= exit-code 0))))

;; https://github.com/emacs-eldev/eldev/issues/12
;;
;; Autoloading Buttercup and using `melpa-stable' package archive in
;; `Eldev' at the same time would fail.
(ert-deftest eldev-issue-12 ()
  (eldev--test-without-files "issue-12-project" "mypkg.elc"
    (eldev--test-delete-cache)
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "mypkg.elc")
      (should (= exit-code 0)))))

;; https://github.com/emacs-eldev/eldev/issues/18
;;
;; Eldev wouldn't install new `Org' if a project wanted a version newer than built-in if
;; another dependency on Org is declared.  To test this, we declare that Org 999.999 is
;; needed and add a dummy dependency.
(ert-deftest eldev-issue-18 ()
  (let ((eldev--test-project "issue-18-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      ;; Can be a couple of different messages, but they must all mention the insane
      ;; requirement.
      (should (string-match-p (rx "version 999.999") stderr))
      (should (= exit-code 1)))))

;; https://github.com/emacs-eldev/eldev/issues/32
;;
;; Eldev would fail to provide Org snapshot to a project that depends on Org version newer
;; than what is built into Emacs, even if appropriate package archive was configured.  Was
;; triggered by the bug in `eldev--global-cache-url-retrieve-synchronously'.
(ert-deftest eldev-issue-32 ()
  (when (< emacs-major-version 25)
    ;; Cannot connect to `orgmode.org' using ancient Emacs 24 anymore, TLS error
    ;; "handshake failed".  Don't care enough to search for another workaround, so just
    ;; disabling the test instead.  To check if it still fails:
    ;;
    ;;     $ eldev docker 24 exec "(eldev-use-package-archive '(\"org\" . \"https://orgmode.org/elpa/\")) (eldev-add-extra-dependencies 'runtime 'org) (eldev-load-extra-dependencies 'runtime)"
    (ert-skip "This test fails on Emacs 24 because of TLS issues"))
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

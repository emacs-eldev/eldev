(require 'test/common)


;; https://github.com/doublep/eldev/issues/29
;;
;; Not about any particular command, but about initializing dependencies.  Eldev would
;; often report dependencies as required by wrong packages, if they were required by
;; several, but in different versions.
(ert-deftest eldev-issue-29 ()
  (eldev--test-run "issue-29-project" ("--setup" `(eldev-use-local-dependency "../issue-29-insane-dependency") "prepare")
    (should (string-match-p "issue-29-insane-dependency" stderr))
    (should (= exit-code 1))))


;; https://github.com/doublep/eldev/issues/32
;;
;; Eldev would fail to provide Org snapshot to a project that depends on Org version newer
;; than what is built into Emacs, even if appropriate package archive was configured.  It
;; actually worked locally (bug in our `eldev--global-cache-url-retrieve-synchronously',
;; was triggered only for remote URLs), see corresponding integration tests.  This one is
;; added for completeness, to catch potential errors in the future.
(ert-deftest eldev-issue-32-local ()
  (let ((eldev--test-project "issue-32-project"))
    (eldev--test-delete-cache)
    ;; Test that it fails when no archive is configured.
    (eldev--test-run nil ("prepare")
      (should (= exit-code 1)))
    (eldev--test-delete-cache)
    ;; But with an appropriate archive it should work.
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive `("org-pseudoarchive" . ,(expand-file-name "../org-pseudoarchive"))) "prepare")
      (should (= exit-code 0)))
    (eldev--test-run nil ("dependency-tree" "--list-built-ins")
      (should (eldev-any-p (string-match-p "\\<org\\>.+overriden.+99999999.9999" it) (eldev--test-line-list stdout)))
      (should (= exit-code 0)))))


;; Not a bug, just doesn't seem to fit anywhere else.  Test that
;; `eldev-known-tool-packages' can be customized.
(ert-deftest eldev-known-tool-packages-1 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(push '(relint :archive relint-pseudoarchive) eldev-known-tool-packages)
                          "--setup" `(setf eldev--known-package-archives '((relint-pseudoarchive ("relint-pseudoarchive" . ,(expand-file-name "../relint-pseudoarchive")) 0)))
                          "eval" `(progn (eldev-add-extra-dependencies 'runtime '(:tool relint))
                                         (eldev-load-extra-dependencies 'runtime)
                                         (require 'relint)
                                         (relint-hello)))
      (should (string= stdout "\"Hello, I'm a fake\"\n"))
      (should (= exit-code 0)))))


;; https://github.com/doublep/eldev/issues/57
;;
;; `(message nil)' is a valid call in Emacs, so it must not fail under Eldev either.
(ert-deftest eldev-message-nil ()
  (eldev--test-run "trivial-project" ("exec" `(message nil))
    (should (string= stdout ""))
    (should (string= stderr "\n"))
    (should (= exit-code 0))))


(provide 'test/integration/misc)

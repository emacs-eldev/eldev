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


(provide 'test/integration/misc)

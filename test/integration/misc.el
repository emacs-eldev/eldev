(require 'test/common)


(ert-deftest eldev-issue-12 ()
  (eldev--test-without-files "issue-12-project" "mypkg.elc"
    (eldev--test-delete-cache)
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "mypkg.elc")
      (should (= exit-code 0)))))


(provide 'test/integration/misc)

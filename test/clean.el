(require 'test/common)


;; Don't use our builder here, we are not testing it in this testsuite.
(defun eldev--test-pretend-byte-compile (project-dir el-file)
  (copy-file (expand-file-name el-file project-dir) (expand-file-name (concat el-file "c") project-dir)))


(ert-deftest eldev-clean-default-1 ()
  (eldev--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (eldev--test-pretend-byte-compile project-dir "project-a.el")
    (eldev--test-assert-files project-dir preexisting-files "project-a.elc")
    (eldev--test-run nil ("-dt" "clean")
      (eldev--test-assert-files project-dir preexisting-files)
      (should (= exit-code 0)))))

(ert-deftest eldev-clean-elc-1 ()
  (eldev--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (eldev--test-pretend-byte-compile project-dir "project-a.el")
    (eldev--test-assert-files project-dir preexisting-files "project-a.elc")
    (eldev--test-run nil ("clean" ".elc")
      (eldev--test-assert-files project-dir preexisting-files)
      (should (= exit-code 0)))))

(ert-deftest eldev-clean-elc-2 ()
  (eldev--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (eldev--test-pretend-byte-compile project-dir "project-a.el")
    (eldev--test-assert-files project-dir preexisting-files "project-a.elc")
    (eldev--test-run nil ("clean" "elc")
      (eldev--test-assert-files project-dir preexisting-files)
      (should (= exit-code 0)))))


(provide 'test/clean)

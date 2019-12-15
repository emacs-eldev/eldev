(require 'test/common)


;; Don't use our builder here, we are not testing it in this testsuite.
(defun emake--test-pretend-byte-compile (project-dir el-file)
  (copy-file (expand-file-name el-file project-dir) (expand-file-name (concat el-file "c") project-dir)))


(ert-deftest emake-test-clean-default-1 ()
  (emake--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (emake--test-pretend-byte-compile project-dir "project-a.el")
    (emake--test-assert-files project-dir preexisting-files "project-a.elc")
    (emake--test-run nil ("-dt" "clean")
      (emake--test-assert-files project-dir preexisting-files)
      (should (= exit-code 0)))))

(ert-deftest emake-test-clean-elc-1 ()
  (emake--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (emake--test-pretend-byte-compile project-dir "project-a.el")
    (emake--test-assert-files project-dir preexisting-files "project-a.elc")
    (emake--test-run nil ("clean" ".elc")
      (emake--test-assert-files project-dir preexisting-files)
      (should (= exit-code 0)))))

(ert-deftest emake-test-clean-elc-2 ()
  (emake--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (emake--test-pretend-byte-compile project-dir "project-a.el")
    (emake--test-assert-files project-dir preexisting-files "project-a.elc")
    (emake--test-run nil ("clean" "elc")
      (emake--test-assert-files project-dir preexisting-files)
      (should (= exit-code 0)))))


(provide 'test/clean)

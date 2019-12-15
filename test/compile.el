(require 'test/common)


(ert-deftest emake-test-compile-everything-1 ()
  (emake--test-without-files "trivial-project" "trivial-project.elc"
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "trivial-project.elc")
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-everything-2 ()
  (emake--test-without-files "project-a" "project-a.elc"
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-a.elc")
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-everything-3 ()
  (emake--test-without-files "project-b" "project-b.elc"
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-b.elc")
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-everything-4 ()
  (emake--test-without-files "project-c" "project-c.elc"
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-c.elc")
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-everything-5 ()
  (emake--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-everything-6 ()
  ;; This test is particularly important, as it highlights that
  ;; certain files must be loaded before compilation.
  (emake--test-without-files "project-e" ("project-e.elc" "project-e-misc.elc" "project-e-util.elc")
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-e.elc" "project-e-misc.elc" "project-e-util.elc")
      (should (= exit-code 0)))))


(ert-deftest emake-test-compile-erroneous-project-1 ()
  (emake--test-without-files "project-f" ("project-f-no-errors-1.elc"
                                          "project-f-no-errors-2.elc"
                                          "project-f-no-errors-3.elc"
                                          "project-f-no-errors-4.elc"
                                          "project-f-util.elc")
    (emake--test-run nil ("compile")
      (should (<= (length (emake--test-find-files project-dir)) (+ (length preexisting-files) 5)))
      (should (= exit-code 1)))))

(ert-deftest emake-test-compile-erroneous-project-2 ()
  (emake--test-without-files "project-f" ("project-f-no-errors-1.elc"
                                          "project-f-no-errors-2.elc"
                                          "project-f-no-errors-3.elc"
                                          "project-f-no-errors-4.elc"
                                          "project-f-util.elc")
    (emake--test-run nil ("compile" "--keep-going")
      (emake--test-assert-files project-dir preexisting-files
                                "project-f-no-errors-1.elc"
                                "project-f-no-errors-2.elc"
                                "project-f-no-errors-3.elc"
                                "project-f-no-errors-4.elc"
                                "project-f-util.elc")
      (should (= exit-code 1)))))


(ert-deftest emake-test-compile-dependencies-1 ()
  (emake--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (emake--test-run nil ("compile")
      ;; `project-d.elc' must be recompiled because of dependency.
      (emake--test-assert-building stdout '("project-d.el" "project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-dependencies-2 ()
  (emake--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (emake--test-run nil ("compile" "project-d.el")
      ;; `project-d.elc' must be recompiled because of dependency.
      ;; However, it is not needed to recompile `project-d-misc.el'.
      (emake--test-assert-building stdout '("project-d.el"))
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-dependencies-3 ()
  (emake--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (emake--test-run nil ("compile" "project-d-misc.el")
      (emake--test-assert-building stdout '("project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest emake-test-compile-dependencies-4 ()
  (emake--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (emake--test-run nil ("compile")
      (emake--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (emake--test-run nil ("compile" "project-d-util.el")
      (should (string= stdout "Nothing to do\n"))
      (should (= exit-code 0)))))


(provide 'test/compile)

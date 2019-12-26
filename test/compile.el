(require 'test/common)


(ert-deftest eldev-test-compile-everything-1 ()
  (eldev--test-without-files "trivial-project" "trivial-project.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "trivial-project.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-everything-2 ()
  (eldev--test-without-files "project-a" "project-a.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-a.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-everything-3 ()
  (eldev--test-without-files "project-b" "project-b.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-b.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-everything-4 ()
  (eldev--test-without-files "project-c" "project-c.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-c.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-everything-5 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-everything-6 ()
  ;; This test is particularly important, as it highlights that
  ;; certain files must be loaded before compilation.
  (eldev--test-without-files "project-e" ("project-e.elc" "project-e-misc.elc" "project-e-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-e.elc" "project-e-misc.elc" "project-e-util.elc")
      (should (= exit-code 0)))))


(ert-deftest eldev-test-compile-erroneous-project-1 ()
  (eldev--test-without-files "project-f" ("project-f-no-errors-1.elc"
                                          "project-f-no-errors-2.elc"
                                          "project-f-no-errors-3.elc"
                                          "project-f-no-errors-4.elc"
                                          "project-f-util.elc")
    (eldev--test-run nil ("compile")
      (should (<= (length (eldev--test-find-files project-dir)) (+ (length preexisting-files) 5)))
      (should (= exit-code 1)))))

(ert-deftest eldev-test-compile-erroneous-project-2 ()
  (eldev--test-without-files "project-f" ("project-f-no-errors-1.elc"
                                          "project-f-no-errors-2.elc"
                                          "project-f-no-errors-3.elc"
                                          "project-f-no-errors-4.elc"
                                          "project-f-util.elc")
    (eldev--test-run nil ("compile" "--keep-going")
      (eldev--test-assert-files project-dir preexisting-files
                                "project-f-no-errors-1.elc"
                                "project-f-no-errors-2.elc"
                                "project-f-no-errors-3.elc"
                                "project-f-no-errors-4.elc"
                                "project-f-util.elc")
      (should (= exit-code 1)))))


(ert-deftest eldev-test-compile-dependencies-1 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (eldev--test-run nil ("compile")
      ;; `project-d.elc' must be recompiled because of dependency.
      (eldev--test-assert-building stdout '("project-d.el" "project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-dependencies-2 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (eldev--test-run nil ("compile" "project-d.el")
      ;; `project-d.elc' must be recompiled because of dependency.
      ;; However, it is not needed to recompile `project-d-misc.el'.
      (eldev--test-assert-building stdout '("project-d.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-dependencies-3 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (eldev--test-run nil ("compile" "project-d-misc.el")
      (eldev--test-assert-building stdout '("project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-dependencies-4 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (set-file-times (expand-file-name "project-d-misc.el" project-dir))
    (eldev--test-run nil ("compile" "project-d-util.el")
      (should (string= stdout "Nothing to do\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-test-compile-circular-requires-1 ()
  (eldev--test-without-files "project-g" ("project-g.elc" "project-g-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-g.elc" "project-g-util.elc")
      (should (= exit-code 0)))
    ;; `project-g.el' and `project-g-util.el' require each other
    ;; (though the first one does that only after calling `provide').
    ;; Make sure Eldev is not confused by that.
    (eldev--test-run nil ("compile")
      (should (string= stdout "Nothing to do\n"))
      (eldev--test-assert-files project-dir preexisting-files "project-g.elc" "project-g-util.elc")
      (should (= exit-code 0)))))


(provide 'test/compile)

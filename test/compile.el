(require 'test/common)


(defun eldev--test-compile-pretend-source-is-changed (el-file &optional test-project)
  (let* ((el-file  (expand-file-name el-file (eldev--test-project-dir test-project)))
         (elc-file (concat el-file "c")))
    (while (progn (set-file-times el-file)
                  (not (file-newer-than-file-p el-file elc-file)))
      ;; Apparently if OS time granularity is large enough, we can set
      ;; `.el' modification time equal to that of `.elc', not newer.
      ;; Working with time in Elisp is a fucking nightmare, let's just
      ;; sleep instead.
      (sleep-for 0.1))))


(ert-deftest eldev-compile-everything-1 ()
  (eldev--test-without-files "trivial-project" "trivial-project.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "trivial-project.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-everything-2 ()
  (eldev--test-without-files "project-a" "project-a.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-a.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-everything-3 ()
  (eldev--test-without-files "project-b" "project-b.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-b.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-everything-4 ()
  (eldev--test-without-files "project-c" "project-c.elc"
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-c.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-everything-5 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-everything-6 ()
  ;; `project-e' contains files that must be loaded before
  ;; compilation.
  (eldev--test-without-files "project-e" ("project-e.elc" "project-e-misc.elc" "project-e-util.elc")
    (eldev--test-run nil ("compile" "--load-before-compiling")
      (eldev--test-assert-files project-dir preexisting-files "project-e.elc" "project-e-misc.elc" "project-e-util.elc")
      (should (= exit-code 0)))))


(ert-deftest eldev-compile-doesnt-load-when-not-asked-1 ()
  ;; `project-e-misc.el' is somewhat broken in that it cannot be
  ;; compiled before being loaded.  Make sure that Eldev doesn't load
  ;; `.el' files by default: only when asked.
  (eldev--test-without-files "project-e" ("project-e.elc" "project-e-misc.elc" "project-e-util.elc")
    (eldev--test-run nil ("compile" "project-e-misc.el")
      (should (= exit-code 1)))))


(ert-deftest eldev-compile-erroneous-project-1 ()
  (eldev--test-without-files "project-f" ("project-f-no-errors-1.elc"
                                          "project-f-no-errors-2.elc"
                                          "project-f-no-errors-3.elc"
                                          "project-f-no-errors-4.elc"
                                          "project-f-util.elc")
    (eldev--test-run nil ("compile")
      (should (<= (length (eldev--test-find-files project-dir)) (+ (length preexisting-files) 5)))
      (should (= exit-code 1)))))

(ert-deftest eldev-compile-erroneous-project-2 ()
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


(ert-deftest eldev-compile-dependencies-1 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-compile-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile")
      ;; `project-d.elc' must be recompiled because of dependency.
      (eldev--test-assert-building stdout '("project-d.el" "project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-dependencies-2 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-compile-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile" "project-d.el")
      ;; `project-d.elc' must be recompiled because of dependency.
      ;; However, it is not needed to recompile `project-d-misc.el'.
      (eldev--test-assert-building stdout '("project-d.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-dependencies-3 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-compile-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile" "project-d-misc.el")
      (eldev--test-assert-building stdout '("project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-dependencies-4 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-compile-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile" "project-d-util.el")
      (should (string= stdout "Nothing to do\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-circular-requires-1 ()
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


(ert-deftest eldev-compile-warnings-as-errors-1 ()
  (eldev--test-without-files "project-a" "project-a.elc"
    (eldev--test-run nil ("compile" "--warnings-as-errors")
      (eldev--test-assert-files project-dir preexisting-files "project-a.elc")
      ;; There must be no warnings.
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-warnings-as-errors-2 ()
  (eldev--test-without-files "project-b" "project-b.elc"
    (eldev--test-run nil ("compile" "--warnings-as-errors")
      ;; Compilation must produce a warning, which is elevated and
      ;; causes build to fail.
      (should (= exit-code 1)))))


(provide 'test/compile)

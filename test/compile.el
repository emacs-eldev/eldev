;;  -*- lexical-binding: t -*-

(require 'test/common)


(defun eldev--test-on-demand-to-loading-mode-option (on-demand)
  (eldev-pcase-exhaustive on-demand
    (`nil    "--as-is")
    (`normal "--compiled-on-demand")
    (`noisy  "--noisy-compiled-on-demand")))


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

;; In this and the following tests make sure that loading mode `compiled-on-demand'
;; doesn't screw up explicit compilation.
(eldev-ert-defargtest eldev-compile-everything-5 (on-demand)
                      (nil 'normal 'noisy)
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ((eldev--test-on-demand-to-loading-mode-option on-demand) "compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-compile-everything-6 (on-demand)
                      (nil 'normal 'noisy)
  ;; `project-e' contains files that must be loaded before
  ;; compilation.
  (eldev--test-without-files "project-e" ("project-e.elc" "project-e-misc.elc" "project-e-util.elc")
    (eldev--test-run nil ((eldev--test-on-demand-to-loading-mode-option on-demand) "compile" "--load-before-compiling")
      (eldev--test-assert-files project-dir preexisting-files "project-e.elc" "project-e-misc.elc" "project-e-util.elc")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-compile-everything-7 (on-demand)
                      (nil 'normal 'noisy)
  ;; `project-l' has special source directories.
  (eldev--test-without-files "project-l" ("src/project-l.elc" "src/project-l-misc.elc" "src/project-l-util.elc")
    (eldev--test-run nil ((eldev--test-on-demand-to-loading-mode-option on-demand) "compile")
      (eldev--test-assert-files project-dir preexisting-files "src/project-l.elc" "src/project-l-misc.elc" "src/project-l-util.elc")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-compile-everything-8 (on-demand)
                      (nil 'normal 'noisy)
  ;; `project-j' has a file with `no-byte-compile' and autoloads.
  (eldev--test-without-files "project-j" ("project-j-autoloads.el" "project-j.elc" "project-j-advanced.elc")
    (should (member "project-j-uncompilable.el" preexisting-files))
    (eldev--test-run nil ((eldev--test-on-demand-to-loading-mode-option on-demand) "compile")
      (eldev--test-assert-files project-dir preexisting-files "project-j-autoloads.el" "project-j.elc" "project-j-advanced.elc")
      (should (= exit-code 0)))))


(eldev-ert-defargtest eldev-compile-test-files-1 (everything)
                      (nil t)
  (eldev--test-without-files "project-a" ("project-a.elc" "test/project-a.elc")
    (eldev--test-run nil ("compile" "--set" (if everything "all" "test"))
      (eldev--test-assert-files project-dir preexisting-files (when everything "project-a.elc") "test/project-a.elc")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-compile-test-files-2 (everything)
                      (nil t)
  ;; This project has an additional loading root for tests.
  (eldev--test-without-files "project-g" ("project-g.elc" "project-g-util.elc"
                                          "test/test-g-1.elc" "test/test-g-integration.elc" "test/test-g-util.elc")
    (eldev--test-run nil ("compile" "--set" (if everything "all" "test"))
      (eldev--test-assert-files project-dir preexisting-files
                                (when everything '("project-g.elc" "project-g-util.elc"))
                                "test/test-g-1.elc" "test/test-g-integration.elc" "test/test-g-util.elc")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-compile-test-files-3 (everything)
                      (nil t)
  ;; This project has an additional dependency for tests.  It is needed not only to run,
  ;; but also to compile them.
  (eldev--test-without-files "project-j" ("project-j-autoloads.el" "project-j.elc" "project-j-advanced.elc"
                                          "test/project-j.elc")
    (eldev--test-run nil ("compile" "--set" (if everything "all" "test"))
      (eldev--test-assert-files project-dir preexisting-files
                                (when everything '("project-j-autoloads.el" "project-j.elc" "project-j-advanced.elc"))
                                "test/project-j.elc")
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
    (eldev--test-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile")
      :important-files ("project-d-misc.el" "project-d-misc.elc" "project-d.elc")
      ;; `project-d.elc' must be recompiled because of dependency.
      (eldev--test-assert-building stdout '("project-d.el" "project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-dependencies-2 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile" "project-d.el")
      :important-files ("project-d-misc.el" "project-d.elc")
      ;; `project-d.elc' must be recompiled because of dependency.
      ;; However, it is not needed to recompile `project-d-misc.el'.
      (eldev--test-assert-building stdout '("project-d.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-dependencies-3 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-pretend-source-is-changed "project-d-misc.el")
    (eldev--test-run nil ("compile" "project-d-misc.el")
      :important-files ("project-d-misc.el" "project-d-misc.elc")
      (eldev--test-assert-building stdout '("project-d-misc.el"))
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-dependencies-4 ()
  (eldev--test-without-files "project-d" ("project-d.elc" "project-d-misc.elc" "project-d-util.elc")
    (eldev--test-run nil ("compile")
      (eldev--test-assert-files project-dir preexisting-files "project-d.elc" "project-d-misc.elc" "project-d-util.elc")
      (should (= exit-code 0)))
    (eldev--test-pretend-source-is-changed "project-d-misc.el")
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
      ;; Compilation must produce two warnings, which are elevated and cause the build to
      ;; fail.  Previously Eldev would use built-in `byte-compile-error-on-warn' and thus
      ;; stop after the first warning.  Now we require that both are printed.
      (should (string-match-p "project-b-never-declared-this-variable" stderr))
      (should (string-match-p "noprefixforthisvar"                     stderr))
      (should (= exit-code 1)))))

(eldev-ert-defargtest eldev-compile-warnings-as-errors-recursive-1 (with-main-file)
                      (nil t)
  (eldev--test-without-files "project-k" ("project-k.elc" "project-k-util.elc")
    ;; Make sure to delete everything first, so that the building doesn't know that
    ;; `project-k.el' depends on `project-k-util.el'.
    (eldev--test-run nil ("clean" "all")
      (should (= exit-code 0)))
    (eldev--test-run nil (:eval `("compile" "--warnings-as-errors" ,@(when with-main-file '("project-k.el")) "project-k-util.el"))
      ;; Compilation must produce a warnings in `project-k-util.el', which is elevated and
      ;; causes the build to fail.  It shouldn't therefore even get to warnings in
      ;; `project-k.el' (but must start with it, if `with-main-file' is set).
      (should (eldev-xor (not with-main-file) (string-match-p "ELC +project-k\\.el" stdout)))
      (should (string-match-p "ELC +project-k-util\\.el" stdout))
      (should (string-match-p "project-k-util-never-declared-this-variable" stderr))
      ;; No `.elc' files must be produced.
      (eldev--test-assert-files project-dir preexisting-files)
      (should (= exit-code 1)))))

(ert-deftest eldev-compile-suppress-warnings-1 ()
  (eldev--test-without-files "project-k" ("project-k.elc" "project-k-util.elc")
    (eldev--test-run nil ("compile" "--suppress-warnings" "project-k.el" "project-k-util.el")
      (should-not (string-match-p "warning" stderr))
      (eldev--test-assert-files project-dir preexisting-files "project-k.elc" "project-k-util.elc")
      (should (= exit-code 0)))))

(ert-deftest eldev-compile-suppress-warnings-2 ()
  (eldev--test-without-files "project-k" ("project-k-erroneous.elc")
    (eldev--test-run nil ("compile" "--suppress-warnings" "project-k-erroneous.el")
      ;; Make sure it doesn't suppress the _error_.
      (should (string-match-p "end of file" stderr))
      (eldev--test-assert-files project-dir preexisting-files)
      (should (= exit-code 1)))))


(ert-deftest eldev-compile-force-1 ()
  (eldev--test-without-files "project-g" ("project-g.elc" "project-g-util.elc")
    (eldev--test-run nil ("compile")
      (should     (string-match-p "ELC +project-g\\.el"      stdout))
      (should     (string-match-p "ELC +project-g-util\\.el" stdout))
      (eldev--test-assert-files project-dir preexisting-files "project-g.elc" "project-g-util.elc")
      (should     (= exit-code 0)))
    ;; Compiling again should be a no-op.
    (eldev--test-run nil ("compile")
      (should     (string= stdout "Nothing to do\n"))
      (should     (= exit-code 0)))
    ;; Compiling with `--force' but without arguments should be a no-op too (either use
    ;; `--force-all' or specify the forced target).
    (eldev--test-run nil ("compile" "--force")
      (should     (string= stdout "Nothing to do\n"))
      (should     (= exit-code 0)))
    ;; Compiling one source file again, with forcing all explicitly listed sources
    ;; (i.e. the same file).
    (eldev--test-run nil ("compile" "--force" "project-g.el")
      (should     (string-match-p "ELC +project-g\\.el"      stdout))
      (should-not (string-match-p "ELC +project-g-util\\.el" stdout))
      (should     (= exit-code 0)))
    ;; Compiling one source file again, with forcing it by name: this shouldn't do
    ;; anything since it's not a target, but a source file.
    (eldev--test-run nil ("compile" "--force=project-g.el" "project-g.el")
      (should     (string= stdout "Nothing to do\n"))
      (should     (= exit-code 0)))
    ;; Compiling one source file again, with forcing its resulting _target_ by name.
    (eldev--test-run nil ("compile" "--force=project-g.elc" "project-g.el")
      (should     (string-match-p "ELC +project-g\\.el"      stdout))
      (should-not (string-match-p "ELC +project-g-util\\.el" stdout))
      (should     (= exit-code 0)))
    ;; Same as above without listing sources explicitly; result must be the same, as the
    ;; forced target is a dependency of `:compile'.
    (eldev--test-run nil ("compile" "--force=project-g.elc")
      (should     (string-match-p "ELC +project-g\\.el"      stdout))
      (should-not (string-match-p "ELC +project-g-util\\.el" stdout))
      (should     (= exit-code 0)))
    ;; Compiling one file and forcing all targets; unrelated targets still must not be
    ;; rebuilt.
    (eldev--test-run nil ("compile" "--force-all" "project-g.el")
      (should     (string-match-p "ELC +project-g\\.el"      stdout))
      (should-not (string-match-p "ELC +project-g-util\\.el" stdout))
      (should     (= exit-code 0)))
    ;; Compiling everything and forcing all targets.
    (eldev--test-run nil ("compile" "--force-all")
      (should     (string-match-p "ELC +project-g\\.el"      stdout))
      (should     (string-match-p "ELC +project-g-util\\.el" stdout))
      (should     (= exit-code 0)))))


(provide 'test/compile)

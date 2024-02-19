;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-autoloads-1 ()
  ;; This project includes its own autoloads file.  Certainly not typical; this emulates
  ;; the file being generated through some external means.
  (let ((eldev--test-project "project-i"))
    (eldev--test-run nil ("eval" `(project-i-hello))
      (should (string= stdout "\"Hello\"\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(project-i-hello-to "world"))
      (should (string= stdout "\"Hello, world!\"\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-autoloads-2 ()
  ;; This project activates `autoloads' plugin.
  (let ((eldev--test-project "project-j"))
    (eldev--test-run nil ("eval" `(project-j-hello))
      (should (string= stdout "\"Hello\"\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(project-j-hello-to "world"))
      (should (string= stdout "\"Hello, world!\"\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-autoloads-3 ()
  ;; Test that when using local dependencies for `project-i', its autoloads are still
  ;; available.
  (let ((eldev--test-project "project-j"))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../project-i") "eval" `(project-j-hello))
      (should (string= stdout "\"Hello\"\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../project-i") "eval" `(project-j-hello-to "world"))
      (should (string= stdout "\"Hello, world!\"\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-autoloads-4 ()
  ;; Like the previous test, only with a different loading mode.
  (let ((eldev--test-project "project-j"))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../project-i" 'packaged) "eval" `(project-j-hello))
      (should (string= stdout "\"Hello\"\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../project-i" 'packaged) "eval" `(project-j-hello-to "world"))
      (should (string= stdout "\"Hello, world!\"\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-autoloads-5 ()
  ;; Test that when `project-j' is used as a local dependency, its autoloads file is still
  ;; generated automatically.
  (dolist (loading-mode '(as-is source byte-compiled))
    (let ((eldev--test-project "project-j"))
      (eldev--test-run nil ("clean")
        (should (= exit-code 0)))
      (eldev--test-delete-cache))
    (let ((eldev--test-project "project-a"))
      (eldev--test-delete-cache)
      (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval 'project-j)
                            "--setup" `(eldev-use-local-dependency "../project-j" ',loading-mode)
                            "eval" `(project-j-hello))
        (should (string= stdout "\"Hello\"\n"))
        (should (= exit-code 0))))))

(ert-deftest eldev-autoloads-6 ()
  ;; This project has special source directories and activates `autoloads' plugin.
  (let ((eldev--test-project "project-l"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("eval" "--dont-require" `(project-l-hello))
      (should (string= stdout "\"Hello\"\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" "--dont-require" `(project-l-misc-hello))
      (should (string= stdout "\"Hello\"\n"))
      (should (= exit-code 0)))
    ;; It's a dependency of the project, and also has autoloaded functions.
    (eldev--test-run nil ("eval" "--dont-require" `(dependency-d-autoloaded) `(dependency-d-stable))
      (should (string= stdout (eldev--test-lines "\"Loaded automatically\"" "t")))
      (should (= exit-code 0)))
    ;; Make sure this works also with local dependencies.
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-d")
                          "eval" "--dont-require" `(dependency-d-autoloaded) `(dependency-d-stable))
      (should (string= stdout (eldev--test-lines "\"Loaded automatically\"" "nil")))
      (should (= exit-code 0)))))

(ert-deftest eldev-autoloads-7 ()
  ;; This dependency library has a special source directory and activates `autoloads' plugin.
  (let ((eldev--test-project "dependency-d"))
    (eldev--test-run nil ("eval" "--dont-require" `(dependency-d-autoloaded))
      (should (string= stdout "\"Loaded automatically\"\n"))
      (should (= exit-code 0)))))


(ert-deftest eldev-autoloads-no-backup-1 ()
  (eldev--test-without-files "project-j" ("project-j-autoloads.el" "project-j-autoloads.el~")
    (eldev--test-run nil ("build")
      (eldev--test-assert-files project-dir preexisting-files "project-j-autoloads.el")
      (should (= exit-code 0)))))


(ert-deftest eldev-autoloads-compile-1 ()
  (eldev--test-without-files "project-j" ("project-j-autoloads.el" "project-j.elc")
    (eldev--test-run nil ("compile" "project-j.el" "--warnings-as-errors")
      (should (= exit-code 0)))))


(ert-deftest eldev-autoloads-disabled-dependencies ()
  (let ((eldev--test-project "project-j"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      (should (= exit-code 0)))
    ;; In real use, dependency `project-i' would be found from the project sources by
    ;; whoever sets `EMACSLOADPATH'.  Here we hardcode it to simplify things.
    (let ((process-environment `(,(eldev--test-emacsloadpath (eldev--test-project-dir) (eldev--test-project-dir "project-i"))
                                 ,@process-environment)))
      ;; This would work without `--disable-dependencies', as the function should be
      ;; autoloaded.  However, at least as of now, `--disable-dependencies' also disables
      ;; autoload handling.  Codify this in tests to be reminded if this somehow changes.
      ;; `project-j' itself may or may not be loaded, depending on target `:autoloads'
      ;; being rebuilt (see also `eldev--unload-package' which is _not_ called when using
      ;; `--disable-dependencies'); so it fails either already on `project-j-hello', or
      ;; else on `project-i-hello'.
      (eldev--test-run nil ("--disable-dependencies" "eval" "--dont-require" `(project-j-hello))
        (should (string-match-p "function definition is void.+project-[ij]-hello" stderr))
        (should (= exit-code 1)))
      ;; Works only when requiring everything (`project-j' is autorequired by the command).
      (eldev--test-run nil ("--disable-dependencies" "eval" `(progn (require 'project-i) (project-j-hello)))
        (should (string= stdout "\"Hello\"\n"))
        (should (= exit-code 0))))))


(provide 'test/autoloads)

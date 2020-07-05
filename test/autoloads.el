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


(ert-deftest eldev-autoloads-no-backup-1 ()
  (eldev--test-without-files "project-j" ("project-j-autoloads.el" "project-j-autoloads.el~")
    (eldev--test-run nil ("build")
      (eldev--test-assert-files project-dir preexisting-files "project-j-autoloads.el")
      (should (= exit-code 0)))))


(provide 'test/autoloads)

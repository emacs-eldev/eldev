;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-info-1 ()
  (eldev--test-run "trivial-project" ("info")
    (should (string= stdout "trivial-project 1.0\n\nTrivial test project\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-info-missing-dependency-1 ()
  (eldev--test-run "missing-dependency-a" ("info")
    ;; Different Emacs versions result in different output, but I think it's not up to
    ;; Eldev to change it, so we accept both.
    (should (or (string= stdout "missing-dependency-a 1.0\n\nTest project with an unavailable dependency\n")
                (string= stdout "missing-dependency-a 1.0\n\nComments to make linters happy.\n")))
    (should (= exit-code 0))))

(ert-deftest eldev-info-with-project-source-dirs-1 ()
  (eldev--test-run "dependency-d" ("info")
    (should (string= stdout "dependency-d 1.0.99\n\nDependency test package D with a special source directory\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-info-with-project-source-dirs-2 ()
  (let ((eldev--test-project "project-l"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("info")
      ;; Accounting for different Emacs versions as with missing-dependency-1 test
      (should (or (string= stdout "project-l 1.0\n\nTest project where source code is contained in a subdirectory of project root; and with autoloads\n")
                  (string= stdout "project-l 1.0\n\nComments to make linters happy.\n")))
      (should (= exit-code 0)))))

(provide 'test/info)

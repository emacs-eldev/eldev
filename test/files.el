;;  -*- lexical-binding: t -*-

(require 'test/common)


(defmacro eldev--test-files (options expected-files)
  `(eldev--test-run nil ("files" ,@options)
     (should (string= stdout (apply #'eldev--test-lines ,expected-files)))
     (should (= exit-code 0))))

(defmacro eldev--test-files-only-existing/creatable (options existing-files &optional creatable-files)
  `(progn (eldev--test-files ,(append options '("--only-existing")) ,existing-files)
          (eldev--test-files ,(append options '("--creatable"))     ,(or creatable-files existing-files))))


(ert-deftest eldev-project-a-files-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-run nil ("clean")
      (should (= exit-code 0)))
    ;; Everywhere.
    (eldev--test-files                         ("--normal")                 '("Eldev" "project-a.el" "test/project-a.el"))
    (eldev--test-files-only-existing/creatable ("--generated")              '() '("project-a.elc" "test/project-a.elc"))
    (eldev--test-files-only-existing/creatable ("--all")
                                               '("Eldev" "project-a.el" "test/project-a.el")
                                               '("Eldev" "project-a.el" "project-a.elc" "test/project-a.el" "test/project-a.elc"))
    ;; Fileset `main'.
    (eldev--test-files                         ("--set=main" "--normal")    '("Eldev" "project-a.el"))
    (eldev--test-files-only-existing/creatable ("--set=main" "--generated") '() '("project-a.elc"))
    (eldev--test-files-only-existing/creatable ("--set=main" "--all")
                                               '("Eldev" "project-a.el")
                                               '("Eldev" "project-a.el" "project-a.elc"))
    ;; Fileset `test'.
    (eldev--test-files                         ("--set=test" "--normal")    '("test/project-a.el"))
    (eldev--test-files-only-existing/creatable ("--set=test" "--generated") '() '("test/project-a.elc"))
    (eldev--test-files-only-existing/creatable ("--set=test" "--all")
                                               '("test/project-a.el")
                                               '("test/project-a.el" "test/project-a.elc"))))

(ert-deftest eldev-project-a-files-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-run nil ("clean")
      (should (= exit-code 0)))
    (eldev--test-run nil ("compile" "--set=all")
      (should (= exit-code 0)))
    ;; Everywhere.
    (eldev--test-files-only-existing/creatable ("--generated")              '("project-a.elc" "test/project-a.elc"))
    (eldev--test-files-only-existing/creatable ("--all")                    '("Eldev" "project-a.el" "project-a.elc" "test/project-a.el" "test/project-a.elc"))
    ;; Fileset `main'.
    (eldev--test-files-only-existing/creatable ("--set=main" "--generated") '("project-a.elc"))
    (eldev--test-files-only-existing/creatable ("--set=main" "--all")       '("Eldev" "project-a.el" "project-a.elc"))
    ;; Fileset `test'.
    (eldev--test-files-only-existing/creatable ("--set=test" "--generated") '("test/project-a.elc"))
    (eldev--test-files-only-existing/creatable ("--set=test" "--all")       '("test/project-a.el" "test/project-a.elc"))))


(provide 'test/files)

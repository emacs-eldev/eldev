(require 'test/common)


(ert-deftest eldev-optional-dependencies-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval '(:package dependency-c :optional t))
                          "eval" `(require 'dependency-c nil t))
      (should (string= stdout "nil\n"))
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-optional-dependencies-2 (main-project optional-dependency)
                      ;; The difference is that `project-a' specifies a (sane) required
                      ;; Emacs version, while `project-b' doesn't specify any.
                      (("project-a" 'uninstallable-a)
                       ("project-a" 'uninstallable-b)
                       ("project-b" 'uninstallable-a)
                       ("project-b" 'uninstallable-b))
  (let ((eldev--test-project main-project))
    (eldev--test-delete-cache)
    ;; Dependency package itself is available, but cannot be installed because its
    ;; dependencies are not.  Still it must not be an error, as it is optional.
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval '(:package ,optional-dependency :optional t))
                          "--setup" `(eldev-use-package-archive `("archive-e" . ,(expand-file-name "../package-archive-e")))
                          "eval"
                          `(not (null (assq ',optional-dependency package-archive-contents)))
                          `(not (null (assq ',optional-dependency package-alist)))
                          `(require ',optional-dependency nil t))
      ;; The package should be available in archives, but not installed and not
      ;; `require'able.
      (should (string= stdout (eldev--test-lines "t" "nil" "nil")))
      (should (= exit-code 0)))))


(ert-deftest eldev-optional-dependencies-no-retries-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval '(:package dependency-c :optional t))
                          "eval" `(require 'dependency-c nil t))
      (should (string= stdout "nil\n"))
      (should (= exit-code 0)))
    ;; Make sure Eldev remembers that it has tried to resolve it, so even with a proper
    ;; archive it still shouldn't be available.
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval '(:package dependency-c :optional t))
                          "--setup" `(eldev-use-package-archive `("archive-c" . ,(expand-file-name "../package-archive-c")))
                          "eval" `(require 'dependency-c nil t))
      (should (string= stdout "nil\n"))
      (should (= exit-code 0)))))


(eldev-ert-defargtest eldev-optional-dependencies-upgrade-1 (command)
                      ('("upgrade") '("upgrade" "dependency-c"))
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval '(:package dependency-c :optional t))
                          "eval" `(require 'dependency-c nil t))
      (should (string= stdout "nil\n"))
      (should (= exit-code 0)))
    ;; Explicit `upgrade' must install the now available dependency.
    (eldev--test-run nil (:eval `("--setup" ,`(eldev-add-extra-dependencies 'eval '(:package dependency-c :optional t))
                                  "--setup" ,`(eldev-use-package-archive `("archive-c" . ,(expand-file-name "../package-archive-c")))
                                  ,@command))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval '(:package dependency-c :optional t))
                          "eval" `(require 'dependency-c nil t))
      (should (string= stdout "dependency-c\n"))
      (should (= exit-code 0)))))


(provide 'test/optional-dependencies)

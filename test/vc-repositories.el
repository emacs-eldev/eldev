;;  -*- lexical-binding: t -*-

(require 'test/common)


(eldev-ert-defargtest eldev-vc-repositories-1 (stable from-pa-first)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (when stable
        (eldev-vc-create-tag "1.1" dependency-a-dir))
      (eldev--test-delete-cache)
      (dolist (from-pa (if from-pa-first '(t nil) '(nil t)))
        (eldev--test-run nil ("--setup" (if from-pa
                                            `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                                          `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir))
                              "eval" `(dependency-a-hello) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
          :description (if from-pa "Using package archive to resolve `dependency-a'" "Using Git repository to resolve `dependency-a'")
          ;; Unlike with local package sources, exchanging archives and VC repositories
          ;; must not affect installed packages: they will remain untouched until you
          ;; issue `upgrade' or `clean ...'.  So, the expected output is determined by the
          ;; first run.
          (should (string= (nth 0 (eldev--test-line-list stdout)) "\"Hello\""))
          (cond (from-pa-first (should (string= (nth 1 (eldev--test-line-list stdout)) "(1 0)")))
                (stable        (should (string= (nth 1 (eldev--test-line-list stdout)) "(1 1)")))
                (t             (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (nth 1 (eldev--test-line-list stdout))))))
          (should (= exit-code 0)))))))

(eldev-ert-defargtest eldev-vc-repositories-2 (remove-installed-package)
                      (nil t)
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (eldev--test-delete-cache)
      ;; Simply do this twice to make sure nothing gets broken.
      (dotimes (pass 2)
        (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                              "eval" `(dependency-a-stable))
          :description (format "Pass #%d" (1+ pass))
          (should (string= stdout (eldev--test-lines "nil")))
          (should (= exit-code 0)))
        (when (and remove-installed-package (= pass 0))
          (eldev--test-run nil ("clean" "dependencies")
            (should (= exit-code 0))))))))

;; A project with two dependencies.  Test all possible combinations of getting them from
;; an archive or VC repository.
(eldev-ert-defargtest eldev-vc-repositories-3 (dependency-a-from-pa dependency-e-from-pa)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  (eldev--test-with-temp-copy "dependency-a" 'Git
    :enabled (not dependency-a-from-pa)
    (let ((dependency-a-dir eldev--test-project))
      (eldev--test-with-temp-copy "dependency-e" 'Git
        :enabled (not dependency-e-from-pa)
        (let ((dependency-e-dir eldev--test-project)
              (eldev--test-project "vc-dep-project-b"))
          (eldev--test-delete-cache)
          (eldev--test-run nil ("--setup" (if dependency-a-from-pa
                                            `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                                          `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir))
                                ;; The dependency is multifile, which might be an additional test for handling of VC repositories.
                                "--setup" (if dependency-e-from-pa
                                            `(eldev-use-package-archive `("archive-e" . ,(expand-file-name "../package-archive-e")))
                                          `(eldev-use-vc-repository 'dependency-e :git ,dependency-e-dir))
                                "eval"
                                `(vc-dep-project-b-hello-to "world")
                                `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a))
                                `(dependency-e-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-e)))
            (should (string= (nth 0 (eldev--test-line-list stdout)) "\"Hello, world!\""))
            (should (string= (nth 1 (eldev--test-line-list stdout)) (if dependency-a-from-pa "t" "nil")))
            ;; Check dependency A.
            (if dependency-a-from-pa
                (should (string= (nth 2 (eldev--test-line-list stdout)) "(1 0)"))
              (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (nth 2 (eldev--test-line-list stdout)))))
            (should (string= (nth 3 (eldev--test-line-list stdout)) (if dependency-e-from-pa "t" "nil")))
            ;; And dependency E.
            (if dependency-e-from-pa
                (should (string= (nth 4 (eldev--test-line-list stdout)) "(1 0)"))
              (should (string-match-p (eldev--test-unstable-version-rx (version-to-list "1.1alpha") t) (nth 4 (eldev--test-line-list stdout)))))
            (should (= exit-code 0))))))))

;; Like the previous test, but add a second VC repository as a separate step, after the
;; first is already fetched.
(ert-deftest eldev-vc-repositories-4 ()
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir eldev--test-project)
          (eldev--test-project "vc-dep-project-b"))
      (eldev--test-delete-cache)
      ;; This is expected to fail: we don't explain where to find `dependency-e'.
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval"
                            `(vc-dep-project-b-hello-to "world"))
        (should (string-match-p "dependency-e.+not available" stderr))
        (should (/= exit-code 0)))
      (eldev--test-with-temp-copy "dependency-e" 'Git
        (let ((dependency-e-dir eldev--test-project)
              (eldev--test-project "vc-dep-project-b"))
          ;; Make sure that adding more repositories works without clearing the cache.
          (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                                "--setup" `(eldev-use-vc-repository 'dependency-e :git ,dependency-e-dir)
                                "eval"
                                `(vc-dep-project-b-hello-to "world"))
            (should (string= stdout (eldev--test-lines "\"Hello, world!\"")))
            (should (= exit-code 0))))))))

(eldev-ert-defargtest eldev-vc-repositories-fixed-commit (tag-it remove-installed-package)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let* ((dependency-a-dir    eldev--test-project)
           (eldev--test-project "vc-dep-project-a")
           (commit              (if tag-it
                                    (progn (eldev-vc-create-tag "1.1" dependency-a-dir) "1.1")
                                  ;; Short identifier won't do it in this case!
                                  (eldev-vc-commit-id nil dependency-a-dir))))
      (let ((default-directory dependency-a-dir))
        (eldev-with-file-buffer "dependency-a.el"
          (re-search-forward (rx "1.0.99"))
          (replace-match "1.1.99"))
        (eldev-call-process (eldev-git-executable) `("commit" "--all" "--message=1.0.99->1.1.99")))
      (eldev--test-delete-cache)
      ;; Simply do this twice to make sure nothing gets broken.
      (dotimes (pass 2)
        (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir :commit ,commit)
                              "eval" `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
          :description (format "Pass #%d" (1+ pass))
          (if tag-it
              (should (string= stdout (eldev--test-lines "(1 1)")))
            (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (eldev--test-first-line stdout))))
          (should (= exit-code 0)))
        (when (and remove-installed-package (= pass 0))
          (eldev--test-run nil ("clean" "dependencies")
            (should (= exit-code 0))))))))

(eldev-ert-defargtest eldev-vc-repositories-stable/unstable (stable remove-installed-package)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (eldev-vc-create-tag "1.1" dependency-a-dir)
      (let ((default-directory dependency-a-dir))
        (eldev-with-file-buffer "dependency-a.el"
          (re-search-forward (rx "1.0.99"))
          (replace-match "1.1.99"))
        (eldev-call-process (eldev-git-executable) `("commit" "--all" "--message=1.0.99->1.1.99")))
      (eldev--test-delete-cache)
      ;; Simply do this twice to make sure nothing gets broken.  For example, that the
      ;; stable version is still properly recognized on the second pass, when the
      ;; repository clone doesn't have to be modified.
      (dotimes (pass 2)
        (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                              (if stable "--stable" "--unstable")
                              "eval" `(dependency-a-hello) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
          :description (format "Pass #%d" (1+ pass))
          (should (string= (nth 0 (eldev--test-line-list stdout)) "\"Hello\""))
          (if stable
              ;; Must use the stable version when `--stable' (default), even if this is
              ;; not the latest version overall.
              (should (string= (nth 1 (eldev--test-line-list stdout)) "(1 1)"))
            (should (string-match-p (eldev--test-unstable-version-rx '(1 1 99) t) (nth 1 (eldev--test-line-list stdout)))))
          (should (= exit-code 0)))
        (when (and remove-installed-package (= pass 0))
          (eldev--test-run nil ("clean" "dependencies")
            (should (= exit-code 0))))))))


(eldev-ert-defargtest eldev-vc-repositories-upgrade-1 (command)
                      ('("upgrade") '("upgrade" "dependency-a"))
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (eldev--test-delete-cache)
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(dependency-a-hello) `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "Using original version of `dependency-a'"
        (should (equal (butlast (eldev--test-line-list stdout)) '("\"Hello\"" "nil")))
        (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (nth 2 (eldev--test-line-list stdout))))
        (should (string-match-p (format "1/1.+Installing.+dependency-a.+from.+%s" (regexp-quote dependency-a-dir)) stderr))
        (should (= exit-code 0)))
      (let ((default-directory dependency-a-dir))
        (eldev-with-file-buffer "dependency-a.el"
          (re-search-forward (rx "1.0.99"))
          (replace-match "1.1.99"))
        (eldev-call-process (eldev-git-executable) `("commit" "--all" "--message=1.0.99->1.1.99")))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(dependency-a-hello) `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "After creating `dependency-a' 1.1.99, but before upgrading"
        ;; Upgrading VC dependencies must be explicit, just like for regular dependencies.
        (should (equal (butlast (eldev--test-line-list stdout)) '("\"Hello\"" "nil")))
        (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (nth 2 (eldev--test-line-list stdout))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                                    ,@command))
        :description "Upgrading"
        (should (string-match-p (format "1/1.+Upgrading.+dependency-a.+from.+%s" (regexp-quote dependency-a-dir)) stderr))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(dependency-a-hello) `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "Using `dependency-a' 1.1.99"
        (should (equal (butlast (eldev--test-line-list stdout)) '("\"Hello\"" "nil")))
        (should (string-match-p (eldev--test-unstable-version-rx '(1 1 99) t) (nth 2 (eldev--test-line-list stdout))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                                    ,@command))
        :description "Upgrading for the second time, must be a no-op"
        (should (string= stdout (eldev--test-lines "All dependencies are up-to-date")))
        (should (= exit-code 0))))))

(ert-deftest eldev-vc-repositories-stable/unstable-upgrade ()
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (eldev-vc-create-tag "1.1" dependency-a-dir)
      (let ((default-directory dependency-a-dir))
        (eldev-with-file-buffer "dependency-a.el"
          (re-search-forward (rx "1.0.99"))
          (replace-match "1.1.99"))
        (eldev-call-process (eldev-git-executable) `("commit" "--all" "--message=1.0.99->1.1.99")))
      (eldev--test-delete-cache)
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "Initial installation: must be stable"
        (should (string= (eldev--test-first-line stdout) "(1 1)"))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "upgrade")
        :description "Upgrading: nothing to do"
        (should (string= stdout (eldev--test-lines "All dependencies are up-to-date")))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "--unstable" "upgrade")
        :description "Upgrading to unstable version"
        (should (string-match-p (format "1.1.+Upgrading.+dependency-a.+from.+%s" (regexp-quote dependency-a-dir)) stderr))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "Must be unstable now"
        (should (string-match-p (eldev--test-unstable-version-rx '(1 1 99) t) (eldev--test-first-line stdout)))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "upgrade")
        :description "Must not downgrade if not specifically asked to"
        (should (string= stdout (eldev--test-lines "All dependencies are up-to-date")))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "upgrade" "--downgrade")
        :description "Downgrading back to stable version"
        (should (string-match-p (format "1.1.+Downgrading.+dependency-a.+from.+%s" (regexp-quote dependency-a-dir)) stderr))
        (should (= exit-code 0))))))


(ert-deftest eldev-vc-repositories-setup-form ()
  (eldev--test-with-temp-copy "dependency-b" 'Git
    (let* ((dependency-b-dir    eldev--test-project)
           (eldev--test-project "vc-dep-project-c"))
      (eldev--test-delete-cache)
      ;; It's not enough to provide `dependency-b', because it doesn't explain where to find its own
      ;; dependencies (it needs `dependency-a'), so Eldev cannot build a package out of it.
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-b :git ,dependency-b-dir)
                            ;; This `eldev-use-package-archive' call applies to outer Eldev (for
                            ;; `vc-dep-project-c'), so it doesn't help the inner Eldev.
                            "--setup" `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                            "prepare")
        (should (string-match-p "dependency-a.+not available" stderr))
        (should (= exit-code 1)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-b :git ,dependency-b-dir
                                                                ;; But this can be resolved by providing `:setup' to `eldev-use-vc-repository' call.
                                                                :setup `(eldev-use-package-archive '("archive-a" . ,(expand-file-name "../package-archive-a"))))
                            "--setup" `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                            "prepare")
        (should (= exit-code 0))))))

(eldev-ert-defargtest eldev-vc-repositories-with-source-dirs (set-using-setup)
                      (nil t)
  (eldev--test-with-temp-copy "dependency-d" 'Git
    (let* ((dependency-d-dir    eldev--test-project)
           (eldev--test-project "vc-dep-project-d"))
      ;; `dependency-d' actually has file `Eldev', where it declares that the sources can be found in
      ;; subdirectory `lisp'.  Optionally, we also test that this information can be supplied via `:setup'
      ;; form passed through `eldev-use-vc-repository', see issue #114.
      (when set-using-setup
        (let ((default-directory dependency-d-dir))
          (delete-file "Eldev")
          (eldev-call-process (eldev-git-executable) `("commit" "Eldev" "--message=remove file 'Eldev'"))))
      (eldev--test-delete-cache)
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-d :git ,dependency-d-dir
                                                                ,@(when set-using-setup `(:setup `(setf eldev-project-source-dirs "lisp"))))
                            "prepare")
        (should (= exit-code 0))))))


(ert-deftest eldev-vc-repositories-wrong-package ()
  (eldev--test-with-temp-copy "dependency-e" 'Git
    (let ((dependency-e-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (eldev--test-delete-cache)
      ;; Make sure that such misconfiguration, i.e. when the repository doesn't provide the "promised"
      ;; package, are caught early and result in an understandable failure.
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-e-dir)
                            "prepare")
        (should (string-match-p (rx-to-string `(seq ,dependency-e-dir (0+ any) "failed to provide" (0+ any) "dependency-a")) stderr))
        (should (string-match-p (rx "repository contains package" (0+ any) "dependency-e") stderr))
        (should (= exit-code 1))))))


(provide 'test/vc-repositories)

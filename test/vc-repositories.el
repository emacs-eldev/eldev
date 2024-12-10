;;  -*- lexical-binding: t -*-

(require 'test/common)


(eldev-ert-defargtest eldev-vc-repositories-1 (from-pa-first)
                      (nil t)
  (eldev--test-with-temp-copy "dependency-a" 'Git
    (let ((dependency-a-dir    eldev--test-project)
          (eldev--test-project "vc-dep-project-a"))
      (eldev--test-delete-cache)
      (dolist (from-pa (if from-pa-first '(t nil) '(nil t)))
        (eldev--test-run nil ("--setup" (if from-pa
                                            `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                                          `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir))
                              "eval" `(dependency-a-hello) `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
          :description (if from-pa "Using package archive to resolve `dependency-a'" "Using Git repository to resolve `dependency-a'")
          ;; Unlike with local package sources, exchanging archives must not affect
          ;; installed packages: they will remain untouched until you issue `upgrade' or
          ;; `clean ...'.  So, the expected output is determined by the first run.
          (should (string= (nth 0 (eldev--test-line-list stdout)) "\"Hello\""))
          (should (string= (nth 1 (eldev--test-line-list stdout)) (if from-pa-first "t" "nil")))
          (if from-pa-first
              (should (string= (nth 2 (eldev--test-line-list stdout)) "(1 0)"))
            (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (nth 2 (eldev--test-line-list stdout)))))
          (should (= exit-code 0)))))))


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
        (should (string-match-p (format "Installing.+dependency-a.+from.+%s" (regexp-quote dependency-a-dir)) stderr))
        (should (= exit-code 0)))
      (let ((default-directory dependency-a-dir))
        (eldev-with-file-buffer "dependency-a.el"
          (re-search-forward (rx "1.0.99"))
          (replace-match "1.0.100"))
        (eldev-call-process (eldev-git-executable) `("commit" "--all" "--message=whatever")))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(dependency-a-hello) `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "After creating `dependency-a' 1.0.100, but before upgrading"
        ;; Upgrading VC dependencies must be explicit, just like for regular dependencies.
        (should (equal (butlast (eldev--test-line-list stdout)) '("\"Hello\"" "nil")))
        (should (string-match-p (eldev--test-unstable-version-rx '(1 0 99) t) (nth 2 (eldev--test-line-list stdout))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                                    ,@command))
        :description "Upgrading"
        (should (string-match-p (format "Upgrading.+dependency-a.+from.+%s" (regexp-quote dependency-a-dir)) stderr))
        (should (= exit-code 0)))
      (eldev--test-run nil ("--setup" `(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                            "eval" `(dependency-a-hello) `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
        :description "Using `dependency-a' 1.0.100"
        (should (equal (butlast (eldev--test-line-list stdout)) '("\"Hello\"" "nil")))
        (should (string-match-p (eldev--test-unstable-version-rx '(1 0 100) t) (nth 2 (eldev--test-line-list stdout))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(eldev-use-vc-repository 'dependency-a :git ,dependency-a-dir)
                                    ,@command))
        :description "Upgrading for the second time, must be a no-op"
        (should (string= stdout (eldev--test-lines "All dependencies are up-to-date")))
        (should (= exit-code 0))))))


(provide 'test/vc-repositories)

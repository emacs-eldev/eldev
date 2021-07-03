(require 'tar-mode)
(require 'test/common)


;; `upgrade-self' must run exactly the same in normal and `external dependencies' mode,
;; i.e. basically ignore the latter.

;; Upgrading _self_ must succeed even from a non-project directory (`empty-project').
(eldev-ert-defargtest eldev-upgrade-self-1 (test-project mode)
                      (("trivial-project" 'normal)
                       ("trivial-project" 'external)
                       ("empty-project"   'normal)
                       ("empty-project"   'external))
  (eldev--test-with-external-dir test-project ()
    :enabled (eq mode 'external)
    (eldev--test-create-eldev-archive "eldev-archive-1")
    (eldev--test-create-eldev-archive "eldev-archive-2" "999.9")
    (let ((eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
          (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
      (ignore-errors (delete-directory eldev--test-eldev-dir t))
      (eldev--test-run nil ("version")
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-2"))
                                    ,@(when (eq mode 'external) `(,(format "--external=%s" external-dir)))
                                    "upgrade-self"))
        (should (string= stdout "Upgraded or installed 1 package\n"))
        (should (= exit-code 0)))
      (eldev--test-run nil ("version")
        (should (string= stdout "eldev 999.9\n"))
        (should (= exit-code 0))))))

;; Trying to upgrade from the archive we have bootstrapped.  Nothing to do.
(eldev-ert-defargtest eldev-upgrade-self-2 (mode)
                      ('normal 'external)
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (eldev--test-with-external-dir "trivial-project" ()
    :enabled (eq mode 'external)
    (let ((eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
          (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
      (ignore-errors (delete-directory eldev--test-eldev-dir t))
      (eldev--test-run nil ("version")
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-1"))
                                    ,@(when (eq mode 'external) `(,(format "--external=%s" external-dir)))
                                    "upgrade-self"))
        (should (string= stdout "Eldev is up-to-date\n"))
        (should (= exit-code 0)))
      (eldev--test-run nil ("version")
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
        (should (= exit-code 0))))))


(eldev-ert-defargtest eldev-upgrade-self-dry-run-1 (mode)
                      ('normal 'external)
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (eldev--test-create-eldev-archive "eldev-archive-2" "999.9")
  (eldev--test-with-external-dir "trivial-project" ()
    :enabled (eq mode 'external)
    (let ((eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
          (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
      (ignore-errors (delete-directory eldev--test-eldev-dir t))
      (eldev--test-run nil ("version")
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-2"))
                                    ,@(when (eq mode 'external) `(,(format "--external=%s" external-dir)))
                                    "upgrade-self" "--dry-run"))
        ;; `--dry-run' intentionally produces exactly the same output.
        (should (string= stdout "Upgraded or installed 1 package\n"))
        (should (= exit-code 0)))
      (eldev--test-run nil ("version")
        ;; But it doesn't actually upgrade anything.
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
        (should (= exit-code 0))))))


;; Reported as issue #43.  The problem was that when the new version had new (or
;; substantially changed) macro definitions, other files would be byte-compiled
;; incorrectly, against old (or missing) definitions.
(eldev-ert-defargtest eldev-upgrade-self-new-macros-1 (mode)
                      ('normal 'external)
  (eldev--test-create-eldev-archive "eldev-archive-1")
  ;; Inject a macro for testing purposes.  If the macro and its usage were in the same
  ;; file, bug would not be triggered.
  (eldev--test-create-eldev-archive "eldev-archive-2" "999.9"
                                    `("eldev.el"      ,(rx "\n(provide 'eldev)\n")      "(defun eldev--test-function () (eldev--test-new-macro))\n")
                                    `("eldev-util.el" ,(rx "\n(provide 'eldev-util)\n") "(defmacro eldev--test-new-macro () 1)\n"))
  (eldev--test-with-external-dir "trivial-project" ()
    :enabled (eq mode 'external)
    (let ((eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
          (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
      (ignore-errors (delete-directory eldev--test-eldev-dir t))
      (eldev--test-run nil ("version")
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
        (should (= exit-code 0)))
      (eldev--test-run nil (:eval `("--setup" ,`(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-2"))
                                    ,@(when (eq mode 'external) `(,(format "--external=%s" external-dir)))
                                    "upgrade-self"))
        (should (string= stdout "Upgraded or installed 1 package\n"))
        (should (= exit-code 0)))
      (eldev--test-run nil ("version")
        (should (string= stdout "eldev 999.9\n"))
        (should (= exit-code 0)))
      (eldev--test-run nil ("eval" `(eldev--test-function))
        (should (string= stdout "1\n"))
        (should (= exit-code 0))))))


(eldev-ert-defargtest eldev-upgrade-self-scripts-1 (mode)
                      ('normal 'external)
  (eldev--test-with-temp-script-copy
    (eldev--test-create-eldev-archive "eldev-archive-1")
    ;; Modify our scripts for testing purposes only.
    (eldev--test-create-eldev-archive "eldev-archive-2" "999.9"
                                      `("bin/eldev"     ,(rx "#! /bin/sh\n") "# TEST-COMMENT\n")
                                      `("bin/eldev.ps1" nil                  "# TEST-COMMENT\n")
                                      `("bin/eldev.bat" ,(rx "@echo off")    "REM TEST-COMMENT\n"))
    (eldev--test-with-external-dir "trivial-project" ()
      :enabled (eq mode 'external)
      (let ((eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
            (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
        (ignore-errors (delete-directory eldev--test-eldev-dir t))
        (eldev--test-run nil ("version")
          (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
          (should (= exit-code 0)))
        (eldev--test-run nil (:eval `("--setup" ,`(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-2"))
                                      ,@(when (eq mode 'external) `(,(format "--external=%s" external-dir)))
                                      "upgrade-self"))
          (let ((lines   (eldev--test-line-list stdout))
                (scripts (list (eldev--shell-script-name))))
            ;; Special case for Windows, where we actually have _two_ scripts.
            (when (string= (car scripts) "eldev.bat")
              (push "eldev.ps1" scripts))
            (should (string= (car lines) "Upgraded or installed 1 package"))
            (should (= (length lines) (1+ (length scripts))))
            (should (eldev-all-p (string-match-p "Upgraded script" it) (cdr lines)))
            (dolist (script scripts)
              (with-temp-buffer
                (insert-file-contents (expand-file-name (format "bin/%s" script) temp-script-dir))
                (should (search-forward "TEST-COMMENT" nil t)))))
          (should (= exit-code 0)))
        (eldev--test-run nil ("version")
          (should (string= stdout "eldev 999.9\n"))
          (should (= exit-code 0)))))))


(provide 'test/upgrade-self)

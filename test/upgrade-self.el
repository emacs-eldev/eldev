(require 'test/common)


(defmacro eldev--test-do-upgrade-self->999.9 (&optional test-project)
  `(let* ((eldev--test-project (or ,test-project eldev--test-project)))
     (eldev--test-create-eldev-archive "eldev-archive-1")
     (eldev--test-create-eldev-archive "eldev-archive-2" "999.9")
     (let ((eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
           (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
       (ignore-errors (delete-directory eldev--test-eldev-dir t))
       (eldev--test-run nil ("version")
         (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
         (should (= exit-code 0)))
       (eldev--test-run nil ("--setup" (prin1-to-string `(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-2"))) "upgrade-self")
         (should (string= stdout "Upgraded or installed 1 package\n"))
         (should (= exit-code 0)))
       (eldev--test-run nil ("version")
         (should (string= stdout "eldev 999.9\n"))
         (should (= exit-code 0))))))

(ert-deftest eldev-upgrade-self-1 ()
  (eldev--test-do-upgrade-self->999.9 "trivial-project"))

;; Trying to upgrade from the archive we have bootstrapped.  Nothing to do.
(ert-deftest eldev-upgrade-self-2 ()
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (let ((eldev--test-project     "trivial-project")
        (eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
        (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" (prin1-to-string `(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-1"))) "upgrade-self")
      (should (string= stdout "Eldev is up-to-date\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

;; Upgrading _self_ must succeed even from a non-project directory.
(ert-deftest eldev-upgrade-self-3 ()
  (eldev--test-do-upgrade-self->999.9 "empty-project"))


(ert-deftest eldev-upgrade-self-dry-run-1 ()
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (eldev--test-create-eldev-archive "eldev-archive-2" "999.9")
  (let ((eldev--test-project     "trivial-project")
        (eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
        (eldev--test-eldev-dir   (eldev--test-tmp-subdir "upgrade-self-root")))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" (prin1-to-string `(setf eldev--upgrade-self-from-forced-pa ,(eldev--test-tmp-subdir "eldev-archive-2"))) "upgrade-self" "--dry-run")
      ;; `--dry-run' intentionally produces exactly the same output.
      (should (string= stdout "Upgraded or installed 1 package\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("version")
      ;; But it doesn't actually upgrade anything.
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))


(provide 'test/upgrade-self)

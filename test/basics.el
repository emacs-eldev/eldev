(require 'test/common)


(ert-deftest eldev-just-run-1 ()
  (eldev--test-run "empty-project" ()
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
    (should (= exit-code 0))))

;; This should work even in broken projects.
(ert-deftest eldev-just-run-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ()
      (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
      (should (= exit-code 0)))))

(ert-deftest eldev-bootstrapping-1 ()
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (let ((eldev--test-project     "trivial-project")
        (eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
        (eldev--test-eldev-dir   (eldev--test-tmp-subdir "bootstrap-root")))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(byte-code-function-p (symbol-function 'eldev-cli)))
      (should (string= stdout "t\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-command-hook-1 ()
  ;; Just make sure that the hook is executed.
  (eldev--test-run "empty-project" ("--setup" `(add-hook 'eldev-help-hook (lambda () (error "fail!"))) "help")
    (should (string-match-p (rx "fail!") stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-setup-first-1 ()
  (eldev--test-run "project-a" ("--setup-first" `(progn (when package-archives (error "not first!")) (setf i-was-executed t))
                                "--setup" `(unless package-archives (error "expected some archives!"))
                                "--setup" `(unless i-was-executed (error "--setup-first had no effect!"))
                                "exec" `(ignore))
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-skip-project-config-1 ()
  (eldev--test-run "project-a" ("--setup-first" `(setf eldev-skip-project-config t)
                                "--setup" `(when package-archives (error "fail!"))
                                "exec" "--dont-load" `(ignore))
    (should (string= stdout ""))
    (should (= exit-code 0))))


(ert-deftest eldev-unreadable-locks-1 ()
  ;; Test that Eldev doesn't faint just because something in Emacs is
  ;; not saved.  Note that the error is not always reproducible: it
  ;; depends on unspecified order of result of `directory-files'
  ;; function.
  (let* ((eldev--test-project "project-a")
         (el-file             (expand-file-name "project-a.el" (eldev--test-project-dir))))
    (with-temp-buffer
      (insert-file-contents el-file t)
      (set-visited-file-name el-file t)
      (should (file-locked-p el-file))
      (unwind-protect
          (eldev--test-run nil ("eval" "1")
            (should (string= stdout "1\n"))
            (should (= exit-code 0)))
        (restore-buffer-modified-p nil)
        (should (not (file-locked-p el-file)))))))


(defmacro eldev--test-no-littering (command-line &rest body)
  (declare (indent 1))
  `(let ((eldev--test-project "empty-project"))
     (eldev--test-delete-cache)
     (eldev--test-run nil ,command-line
       ,@body
       (should (not (file-exists-p (eldev--test-project-cache-dir)))))))


;; Make sure that `.eldev' is not created when not really needed.

(ert-deftest eldev-no-littering-1 ()
  (eldev--test-no-littering ()
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-no-littering-2 ()
  (eldev--test-no-littering ("help")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-no-littering-3 ()
  (eldev--test-no-littering ("--quiet" "version" "eldev" "emacs")
    (should (string= stdout (eldev--test-lines (eldev-message-version (eldev-find-package-descriptor 'eldev)) emacs-version)))
    (should (= exit-code 0))))

(ert-deftest eldev-no-littering-4 ()
  (eldev--test-no-littering ("info")
    (should (= exit-code 1))))

(ert-deftest eldev-no-littering-5 ()
  ;; This should obviously fail in a non-project directory.
  (eldev--test-no-littering ("compile")
    (should (= exit-code 1))))


(provide 'test/basics)

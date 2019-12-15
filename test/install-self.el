(require 'test/common)


(ert-deftest emake-test-install-self-1 ()
  (emake--test-create-emake-archive "emake-archive-1")
  (let* ((emake--test-emake-dir (emake--test-tmp-subdir "install-dir"))
         (non-existing-dir      (emake--test-tmp-subdir "non-existing-dir"))
         (bin-dir               (emake--test-tmp-subdir "bin-dir"))
         (installed-executable  (expand-file-name "emake" bin-dir)))
    (ignore-errors (delete-directory non-existing-dir t))
    (ignore-errors (delete-directory install-dir t))
    (ignore-errors (delete-directory bin-dir t))
    (make-directory bin-dir t)
    (emake--test-run ".." ("install-self" bin-dir)
      (should (file-executable-p installed-executable))
      (should (string= (with-temp-buffer (insert-file-contents-literally installed-executable) (buffer-string))
                       (with-temp-buffer (insert-file-contents-literally (expand-file-name "bin/emake" emake-project-dir)) (buffer-string))))
      (should (= exit-code 0)))
    (let ((emake--test-project     "trivial-project")
          ;; Test the installed Emake.  In case it is not actually
          ;; installed and needs to bootstrap itself, point it to a
          ;; non-existing directory as package archive, so that it
          ;; fails instantly.
          (emake--test-emake-local (concat ":pa:" non-existing-dir)))
      (emake--test-run nil ("version")
        (should (string= stdout (format "emake %s\n" (emake-message-version (emake-package-descriptor)))))
        (should (= exit-code 0)))
      (emake--test-run nil ("eval" "(byte-code-function-p (symbol-function 'emake-cli))")
        (should (string= stdout "t\n"))
        (should (= exit-code 0))))))


(provide 'test/install-self)

(require 'test/common)


(ert-deftest eldev-install-self-1 ()
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (let* ((eldev--test-eldev-dir (eldev--test-tmp-subdir "install-dir"))
         (non-existing-dir      (eldev--test-tmp-subdir "non-existing-dir"))
         (bin-dir               (eldev--test-tmp-subdir "bin-dir"))
         (shell-script-name (eldev--shell-script-name))
         (bin/eldev (format "bin/%s" shell-script-name))
         (installed-executable  (expand-file-name shell-script-name bin-dir)))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    (ignore-errors (delete-directory non-existing-dir t))
    (ignore-errors (delete-directory bin-dir t))
    (make-directory bin-dir t)
    (eldev--test-run ".." ("install-self" bin-dir)
      (should (file-executable-p installed-executable))
      (should (string= (with-temp-buffer (insert-file-contents-literally installed-executable) (buffer-string))
                       (with-temp-buffer (insert-file-contents-literally (expand-file-name bin/eldev eldev-project-dir)) (buffer-string))))
      (should (= exit-code 0)))
    (let ((eldev--test-project     "trivial-project")
          ;; Test the installed Eldev.  In case it is not actually
          ;; installed and needs to bootstrap itself, point it to a
          ;; non-existing directory as package archive, so that it
          ;; fails instantly.
          (eldev--test-eldev-local (concat ":pa:" non-existing-dir)))
      (eldev--test-run nil ("version")
        (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-package-descriptor)))))
        (should (= exit-code 0)))
      (eldev--test-run nil ("eval" `(byte-code-function-p (symbol-function 'eldev-cli)))
        (should (string= stdout "t\n"))
        (should (= exit-code 0))))))


(provide 'test/install-self)

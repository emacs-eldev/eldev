;;; dummy-lint-a.el --- Dummy linter

;; Version: 1.0

(defun dummy-lint-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (while (search-forward "DUMMY-LINT-WARN-HERE" nil t)
      (dummy-lint-warn file))))

(defun dummy-lint-warn (file)
  (message "%s:%d: warning: %s" (file-name-nondirectory file) (line-number-at-pos)
           (buffer-substring (line-beginning-position) (line-end-position))))

(provide 'dummy-lint)

;;; dummy-lint.el ends here

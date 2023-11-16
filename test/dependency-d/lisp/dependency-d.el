;;; dependency-d.el --- Dependency test package D with a special source directory

;; Version: 1.0.99

(defun dependency-d-hello ()
  "Hello")

(defun dependency-d-stable ()
  nil)

;;;###autoload
(defun dependency-d-autoloaded ()
  "Loaded automatically")

(provide 'dependency-d)

;;; dependency-d.el ends here

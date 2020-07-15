;;; project-j.el --- Test project with autoload cookies in multiple files

;; Version: 1.0
;; Package-Requires: (project-i)
;; Homepage: https://example.com/

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

;;;###autoload
(defun project-j-hello ()
  ;; Not requiring anything for this function: it must be autoloaded.
  (project-i-hello))

;;;###autoload
(defun project-j-hello-to-me ()
  ;; Not requiring anything for this function: it must be autoloaded.
  ;; We also test that byte-compilation sees the autoloads this way.
  (project-j-hello-to "me"))

(provide 'project-j)

;;; project-j.el ends here

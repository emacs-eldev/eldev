;;; project-i.el --- Test project with autoload cookies in multiple files

;; Version: 1.0
;; Homepage: https://example.com/

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

;;;###autoload
(defun project-i-hello ()
  "Hello")

(provide 'project-i)

;;; project-i.el ends here

;;; project-l.el --- Test project where source code is contained in a subdirectory of project root; and with autoloads

;; Version: 1.0
;; Package-Requires: ((dependency-a "0.1") (dependency-d "1.0"))
;; Homepage: https://example.com/

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'project-l-misc)

;;;###autoload
(defun project-l-hello ()
  (project-l-misc-hello))

(defun project-l-simple-resource ()
  (with-temp-buffer
    (insert-file-contents (or (locate-file "project-l/simple-resource.txt" load-path) (error "resource not found")))
    (buffer-string)))

(provide 'project-l)

;;; project-l.el ends here

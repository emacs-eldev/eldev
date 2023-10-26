;;; project-d.el --- Test project with several cross-depending `.el' files

;; Version: 1.0
;; Package-Requires: ((dependency-a "0.1"))
;; Homepage: https://example.com/

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'project-d-misc)

(defun project-d-hello ()
  (project-d-misc-hello))

(project-d-defun project-d-custom ()
  nil)

(provide 'project-d)

;;; project-d.el ends here

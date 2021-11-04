;;; project-h.el --- Test project with one dependency, a stable/unstable archive and Ecukes tests

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((dependency-a "1.0"))

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'dependency-a)

(defun project-h-hello ()
  "Invoke `dependency-a-hello'.
This docstring exists to make linters happy."
  (dependency-a-hello))

(provide 'project-h)

;;; project-h.el ends here

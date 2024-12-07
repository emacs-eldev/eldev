;;; vc-dep-project-a.el --- Test project with one dependency fetched from Git  -*- lexical-binding: t -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((emacs "24") (dependency-a "0.9"))

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'dependency-a)

(defun vc-dep-project-a-hello ()
  "Invoke `dependency-a-hello'.
This docstring exists to make linters happy."
  (dependency-a-hello))

(provide 'vc-dep-project-a)

;;; vc-dep-project-a.el ends here

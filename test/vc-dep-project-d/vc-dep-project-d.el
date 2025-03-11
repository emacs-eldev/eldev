;;; vc-dep-project-c.el --- Test project with one dependency fetched from Git  -*- lexical-binding: t -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((emacs "24") (dependency-d "1.0"))

;;; Commentary:

(require 'dependency-d)

(defun vc-dep-project-d-hello ()
  (dependency-d-hello))

(provide 'vc-dep-project-d)

;;; vc-dep-project-d.el ends here

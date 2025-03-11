;;; vc-dep-project-c.el --- Test project with one dependency fetched from Git  -*- lexical-binding: t -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((emacs "24") (dependency-b "1.0"))

;;; Commentary:

(require 'dependency-b)

(defun vc-dep-project-c-hello ()
  (dependency-b-hello))

(provide 'vc-dep-project-c)

;;; vc-dep-project-c.el ends here

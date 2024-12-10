;;; vc-dep-project-b.el --- Test project with two dependencies  -*- lexical-binding: t -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((emacs "24") (dependency-a "0.9") (dependency-e "1"))

;;; Commentary:

(require 'dependency-a)
(require 'dependency-e)

(defun vc-dep-project-b-hello ()
  (dependency-a-hello))

(defun vc-dep-project-b-hello-to (whom)
  (dependency-e-hello-to whom))

(provide 'vc-dep-project-b)

;;; vc-dep-project-b.el ends here

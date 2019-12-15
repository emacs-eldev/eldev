;;; project-a.el --- Test project with one dependency

;; Version: 1.0
;; Package-Requires: (dependency-a)

(require 'dependency-a)

(defun project-a-hello ()
  (dependency-a-hello))

(provide 'project-a)

;;; project-a.el ends here

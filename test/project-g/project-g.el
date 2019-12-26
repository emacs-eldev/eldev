;;; project-g.el --- Test project with circular but valid `require's

;; Version: 1.0

(defun project-g-hello ()
  (project-g-util-hello))

(provide 'project-g)

(require 'project-g-util)

;;; project-g.el ends here

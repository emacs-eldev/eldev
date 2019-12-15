;;; project-d.el --- Test project with several cross-depending `.el' files

;; Version: 1.0
;; Package-Requires: (dependency-a)

(require 'project-d-misc)

(defun project-d-hello ()
  (project-d-misc-hello))

(provide 'project-d)

;;; project-d.el ends here

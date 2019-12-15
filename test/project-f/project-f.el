;;; project-f.el --- Test project with several erroneous `.el' files

;; Version: 1.0
;; Package-Requires: (dependency-a)

(require 'project-f-misc)

(defun project-f-hello ()
  (project-f-misc-hello))

(provide 'project-f)

;;; project-f.el ends here

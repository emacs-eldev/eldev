;;; missing-dependency-a.el --- Test project with an unavailable dependency

;; Version: 1.0
;; Package-Requires: (dependency-a)

(require 'dependency-a)

(defun missing-dependency-a-hello ()
  (dependency-a-hello))

(provide 'missing-dependency-a)

;;; missing-dependency-a.el ends here

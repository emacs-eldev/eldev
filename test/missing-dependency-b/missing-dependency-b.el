;;; missing-dependency-b.el --- Test project with an unavailable dependency

;; Version: 1.0
;; Package-Requires: (dependency-b)

(require 'dependency-b)

(defun missing-dependency-b-hello ()
  (dependency-b-hello))

(provide 'missing-dependency-b)

;;; missing-dependency-b.el ends here

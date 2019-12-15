;;; dependency-b.el --- Dependency test package B

;; Version: 1.0.99
;; Package-Requires: (dependency-a)


(require 'dependency-a)

(defun dependency-b-hello ()
  (dependency-a-hello))

(defun dependency-b-stable ()
  nil)

(provide 'dependency-b)

;;; dependency-b.el ends here

;;; dependency-b.el --- Dependency test package B

;; Version: 1.1
;; Package-Requires: (dependency-a 1.1)


(require 'dependency-a)

(defun dependency-b-hello ()
  (dependency-a-hello))

(defun dependency-b-stable ()
  t)

(provide 'dependency-b)

;;; dependency-b.el ends here

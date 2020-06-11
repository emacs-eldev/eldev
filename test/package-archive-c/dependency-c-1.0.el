;;; dependency-c.el --- Dependency test package C

;; Version: 1.0
;; Package-Requires: (dependency-a)


(require 'dependency-a)

(defun dependency-c-hello ()
  (dependency-a-hello))

(defun dependency-c-stable ()
  t)

(provide 'dependency-c)

;;; dependency-c.el ends here

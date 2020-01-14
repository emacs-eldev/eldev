;;; project-b.el --- Test project with one dependency that has its own dependency in turn; also with "manual"!

;; Version: 1.0
;; Package-Requires: (dependency-b)

(require 'dependency-b)

(defun project-b-hello ()
  (dependency-b-hello))

;; This function is syntactically correct, but produces a
;; byte-compilation warning.
(defun project-b-unused-argument ()
  (setf project-b-never-declared-this-variable nil))

(provide 'project-b)

;;; project-b.el ends here

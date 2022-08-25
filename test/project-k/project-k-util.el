(defun project-k-util-hello ()
  "Hello")

;; This function is syntactically correct, but produces a byte-compilation warning.
(defun project-k-util-unused-argument ()
  (setf project-k-util-never-declared-this-variable nil))

(provide 'project-k-util)

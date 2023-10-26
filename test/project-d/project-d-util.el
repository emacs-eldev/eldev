(require 'dependency-a)

(defun project-d-util-hello ()
  (dependency-a-hello))

(defmacro project-d-defun (name arglist &rest body)
  (declare (indent 1))
  `(defun ,name ,arglist ,@body 1))

(provide 'project-d-util)

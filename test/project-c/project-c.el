;;; Copyright (C) 2000 John Doe
(require 'dependency-a)

(defun project-c-hello ()
  (dependency-a-hello))

(provide 'project-c)

(require 'dependency-a)

;; This macro and the next function are only needed so that the file
;; cannot be byte-compiled without being loaded first.
(defmacro project-e-util-funcall (fn &rest args)
  (project-e-util-funcall-impl fn args))

(defun project-e-util-funcall-impl (fn args)
  `(funcall ,fn ,@args))

(defun project-e-util-hello ()
  (project-e-util-funcall #'dependency-a-hello))

(provide 'project-e-util)

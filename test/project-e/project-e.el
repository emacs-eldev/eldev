;;; project-e.el --- Test project with several cross-depending `.el' files and macros

;; Version: 1.0
;; Package-Requires: (dependency-a)

(require 'project-e-misc)

;; This macro and the next function are only needed so that the file
;; cannot be byte-compiled without being loaded first.
(defmacro project-e-funcall (fn &rest args)
  (project-e-funcall-impl fn args))

(defun project-e-funcall-impl (fn args)
  `(funcall ,fn ,@args))

(defun project-e-hello ()
  (project-e-funcall 'project-e-misc-hello))

(provide 'project-e)

;;; project-e.el ends here

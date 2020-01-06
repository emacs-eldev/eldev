(require 'project-e-util)

;; This macro and the next function are only needed so that the file
;; cannot be byte-compiled without being loaded first.  Note that it
;; MUST NOT be fixed with `eval-and-compile': this is important for
;; testing.
(defmacro project-e-misc-funcall (fn &rest args)
  (project-e-misc-funcall-impl fn args))

(defun project-e-misc-funcall-impl (fn args)
  `(funcall ,fn ,@args))

(defun project-e-misc-hello ()
  (project-e-misc-funcall #'project-e-util-hello))

(provide 'project-e-misc)

;;  -*- lexical-binding: t; no-byte-compile: t -*-

;; Usage of `no-byte-compile' is not an accident: testing that such files are handled
;; correctly too.

;;;###autoload
(defun project-j-never-compiled-hello ()
  ;; Not requiring anything for this function: it must be autoloaded.
  (project-i-hello))

(provide 'project-j-uncompilable)

;;  -*- lexical-binding: t -*-

;;;###autoload
(defun project-j-hello-to (whom)
  ;; Not requiring anything for this function: it must be autoloaded.
  (project-i-hello-to whom))

(provide 'project-j-advanced)

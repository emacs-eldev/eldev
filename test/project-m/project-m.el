;;; project-m.el --- Simple test project with no dependencies that uses `doctest'  -*- lexical-binding: t -*-

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((emacs "24"))

(defun project-m-hello ()
  "Doctest me:
>> (project-m-hello)
=> \"Hello\""
  "Hello")

(provide 'project-m)

;;; project-m.el ends here

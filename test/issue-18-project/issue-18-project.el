;;; issue-18-project.el --- A project that wants insanely new `Org'

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((dependency-org "1.0") (org "999.999"))

;;; Commentary:

;; Note that ordering of dependencies in the header is important for certain test!

;;; Code:

(require 'org)

(defun issue-18-project-hello ()
  "Hello")

(provide 'issue-18-project)

;;; issue-18-project.el ends here

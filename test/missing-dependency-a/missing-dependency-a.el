;;; missing-dependency-a.el --- Test project with an unavailable dependency

;; Version: 1.0
;; Homepage: https://example.com/
;; Package-Requires: ((dependency-a "0.1"))

;;; Commentary:

;; Comments to make linters happy.

;;; Code:

(require 'dependency-a)

(defun missing-dependency-a-hello ()
  "Dummy documentation for linters."
  (dependency-a-hello))

(provide 'missing-dependency-a)

;;; missing-dependency-a.el ends here

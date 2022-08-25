;;; project-k.el --- Test project with two files that both make byte-compiler issue warnings.

;; Version: 1.0

;; Don't polish this source file: Eldev tests expect that the byte-compiler issue warnings
;; on it.

(require 'project-k-util)

(defun project-k-hello ()
  (project-k-util-hello))

;; This function is syntactically correct, but produces a byte-compilation warning.
(defun project-k-unused-argument ()
  (setf project-k-never-declared-this-variable nil))

;; A different type of warning so that it is certainly not merged with the first one (same
;; type could potentially be merged one day).
(defvar noprefixforthisvar nil)

(provide 'project-k)

;;; project-k.el ends here

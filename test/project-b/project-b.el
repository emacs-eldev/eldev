;;; project-b.el --- Test project with one dependency that has its own dependency in turn; also with "manual"!

;;; Copyright (C) 2999 John Doe

;; Version: 1.0
;; Package-Requires: (dependency-b)

(require 'dependency-b)

;; Don't polish this source file: Eldev tests expect that various linters and the
;; byte-compiler issue warnings on it.

(defun project-b-hello ()
  ;; DUMMY-LINT-WARN-HERE
  (dependency-b-hello))

;; This function is syntactically correct, but produces a byte-compilation warning.
(defun project-b-unused-argument ()
  (setf project-b-never-declared-this-variable nil))

;; A different type of warning so that it is certainly not merged with the first one (same
;; type could potentially be merged one day).
(defvar noprefixforthisvar nil)

(defun project-b-never-used-function (s)
  ;; Regexp below is intentionally faulty: we test that `relint'
  ;; complains about it.
  (string-match-p "^\\(.)$" s))

(provide 'project-b)

;;; project-b.el ends here

;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2020 Paul Pogonyshev

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses.

;;; Code:

(require 'eldev)


(defvar buttercup-suites)
(defvar buttercup-reporter-batch-quiet-statuses)
(defvar buttercup-color)
(defvar buttercup-stack-frame-style)

(declare-function buttercup-mark-skipped "buttercup")
(declare-function buttercup-spec-full-name "buttercup")
(declare-function buttercup-run "buttercup")
(declare-function buttercup-suites-total-specs-defined "buttercup")
(declare-function buttercup-suites-total-specs-failed "buttercup")
(declare-function buttercup-suites-total-specs-pending "buttercup")


(defun eldev-test-buttercup-preprocess-selectors (selectors)
  "Convert SELECTORS to Buttercup patterns."
  (dolist (pattern selectors)
    (unless (eldev-valid-regexp-p pattern)
      (signal 'eldev-error `("Selector/pattern `%s' is not a valid regular expression" ,pattern))))
  selectors)

(defun eldev-run-buttercup-tests (selectors &optional environment)
  "Run Buttercup tests according to given SELECTORS (patterns)."
  (when eldev-test-stop-on-unexpected
    (eldev-warn "Option `--stop-on-unexpected' (`-s') is not supported with Buttercup framework"))
  (unless eldev-test-print-backtraces
    (eldev-warn "Option `--omit-backtraces' (`-B') is not supported with Buttercup framework"))
  (eldev-bind-from-environment environment (buttercup-reporter-batch-quiet-statuses buttercup-stack-frame-style buttercup-color)
    (when (integerp eldev-test-print-backtraces)
      ;; Just always use `crop' for now: Buttercup doesn't support varying width yet.
      (setf buttercup-stack-frame-style (if (> eldev-test-print-backtraces 1) 'crop 'full)))
    (when selectors
      ;; With not-yet-released 1.24 could be just: (buttercup-mark-skipped selectors t)
      (buttercup-mark-skipped (mapconcat (lambda (selector) (concat "\\(?:" selector "\\)")) selectors "\\|") t))
    (eldev-test-validate-amount (- (buttercup-suites-total-specs-defined buttercup-suites) (buttercup-suites-total-specs-pending buttercup-suites)))
    (unless (buttercup-run t)
      (signal 'eldev-error `("%s failed" ,(eldev-message-plural (buttercup-suites-total-specs-failed buttercup-suites) "test"))))))


(provide 'eldev-buttercup)

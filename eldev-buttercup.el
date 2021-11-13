;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2020-2021 Paul Pogonyshev

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
(declare-function buttercup-suites-total-specs-status "buttercup")


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
  (eldev-bind-from-environment environment (buttercup-reporter-batch-quiet-statuses buttercup-stack-frame-style buttercup-color)
    (pcase (eldev-shrink-screen-width-as-needed eldev-test-print-backtraces)
      (`nil                        (setf buttercup-stack-frame-style 'omit))
      ;; Just always use `crop' for now: Buttercup doesn't support varying width yet.
      ((and (pred integerp) width) (setf buttercup-stack-frame-style (if (> width 0) 'crop 'full)))
      ;; Otherwise use value of `eldev-backtrace-style', but only if test runner doesn't
      ;; specify anything.
      (_                           (unless (assq 'buttercup-stack-frame-style environment)
                                     (setf buttercup-stack-frame-style (if (and (integerp eldev-backtrace-style) (> eldev-backtrace-style 1)) 'crop 'full)))))
    (when selectors
      (buttercup-mark-skipped selectors t))
    (let* ((result     (buttercup-run t))
           (num-failed (buttercup-suites-total-specs-failed buttercup-suites)))
      (setf eldev-test-num-passed  (+ eldev-test-num-passed  (buttercup-suites-total-specs-status buttercup-suites 'passed))
            eldev-test-num-failed  (+ eldev-test-num-failed  num-failed)
            eldev-test-num-skipped (+ eldev-test-num-skipped (buttercup-suites-total-specs-status buttercup-suites 'skipped)))
      ;; Even if unsupported by the framework, at least update the variable so that we can
      ;; stop before running further test types.
      (when eldev-test-stop-on-unexpected
        (setf eldev-test-stop-on-unexpected (- eldev-test-stop-on-unexpected num-failed)))
      (unless result
        (signal 'eldev-error `("%s failed" ,(if (> num-failed 0) (eldev-message-plural num-failed "Buttercup test") "Buttercup")))))))

(defun eldev-count-buttercup-tests (selectors)
  "Count Buttercup tests matching given SELECTORS."
  (when selectors
    (buttercup-mark-skipped selectors t))
  (- (buttercup-suites-total-specs-defined buttercup-suites) (buttercup-suites-total-specs-pending buttercup-suites)))


(provide 'eldev-buttercup)

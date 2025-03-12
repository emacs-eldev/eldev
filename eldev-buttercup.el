;;; eldev-buttercup.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2020-2025 Paul Pogonyshev

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
(defvar buttercup-reporter)
(defvar buttercup-reporter-batch-quiet-statuses)
(defvar buttercup-color)
(defvar buttercup-stack-frame-style)

(declare-function buttercup-mark-skipped "buttercup")
(declare-function buttercup-spec-full-name "buttercup")
(declare-function buttercup-spec-status "buttercup")
(declare-function buttercup-run "buttercup")
(declare-function buttercup-suites-total-specs-defined "buttercup")
(declare-function buttercup-suites-total-specs-failed "buttercup")
(declare-function buttercup-suites-total-specs-pending "buttercup")
(declare-function buttercup-suites-total-specs-status "buttercup")


(defvar eldev--test-buttercup-concise-expected nil)


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
  (eldev-bind-from-environment environment (buttercup-reporter-batch-quiet-statuses buttercup-stack-frame-style buttercup-color eldev--test-buttercup-concise-expected)
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
    ;; Inject profiling support if wanted.
    (let* (num-total
           (original-reporter buttercup-reporter)
           (result     (eldev-advised ('buttercup--run-spec :around (when eldev--effective-profile-mode
                                                                      (lambda (original &rest args)
                                                                        (eldev-backtrace-notch 'eldev
                                                                          (eldev-profile-body
                                                                            (apply original args))))))
                         (eldev-advised ('buttercup-reporter-batch :around
                                                                   (when eldev--test-buttercup-concise-expected
                                                                     (lambda (original event arg &rest etc)
                                                                       ;; At least when testing in Buttercup project itself, there
                                                                       ;; can be nested invocations.  Ignore them.  There appears
                                                                       ;; to be no better way to find them.
                                                                       (when (eq buttercup-reporter original-reporter)
                                                                         (pcase event
                                                                           (`buttercup-started
                                                                            (setf num-total (buttercup-suites-total-specs-defined arg)))
                                                                           (`spec-done
                                                                            (eldev-test-runner-concise-tick (not (memq (buttercup-spec-status arg)
                                                                                                                       buttercup-reporter-batch-quiet-statuses))
                                                                                                            nil num-total))))
                                                                       (apply original event arg etc))))
                           ;; Not trying to highlight the summary, Buttercup already does that itself.
                           (buttercup-run t))))
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

;;; eldev-buttercup.el ends here

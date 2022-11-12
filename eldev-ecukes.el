;;; eldev.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2021-2022 Paul Pogonyshev

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


(defvar ecukes-verbose)
(defvar ecukes-only-failing)
(defvar ecukes-failing-scenarios-file)
(defvar ecukes-patterns)
(defvar ecukes-anti-patterns)
(defvar ecukes-include-tags)
(defvar ecukes-exclude-tags)
(defvar ecukes-stats-scenarios)
(defvar ecukes-stats-scenarios-passed)
(defvar ecukes-stats-scenarios-failed)

(declare-function ecukes-load "ecukes")
(declare-function ecukes-reporter-use "ecukes")
(declare-function ecukes-run "ecukes")


(defvar eldev--ecukes-backtraces)
(defvar eldev--ecukes-current-step)

(defvar eldev--test-ecukes-initialized nil)


(defun eldev--test-init-ecukes ()
  ;; Apparently these variables are touched only by `ecukes-cli', which is not included,
  ;; but we add a safeguard in case Ecukes is changed later.
  (unless eldev--test-ecukes-initialized
    (let ((debugger       debugger)
          (debug-on-error debug-on-error))
      (require 'ecukes))
    (ecukes-load)
    (setf eldev--test-ecukes-initialized t)))

(defun eldev-test-ecukes-preprocess-selectors (selectors)
  "Convert SELECTORS to Ecukes patterns and tags."
  (dolist (selector selectors)
    (let ((pattern (if (string-prefix-p "~" selector) (substring selector 1) selector)))
      (unless (or (string-prefix-p "@" pattern) (eldev-valid-regexp-p pattern))
        (signal 'eldev-error `("Non-tag selector/pattern `%s' is not a valid regular expression" ,selector)))))
  selectors)


(defmacro eldev--test-ecukes-split-selectors (selectors &rest body)
  (declare (indent 1))
  `(let (ecukes-only-failing
         (ecukes-failing-scenarios-file (expand-file-name "failing-scenarios.ecukes" (eldev-cache-dir t t)))
         ecukes-patterns
         ecukes-anti-patterns
         ecukes-include-tags
         ecukes-exclude-tags)
     (dolist (selector (reverse ,selectors))
       (if (member selector '(":failed" ":failing"))
           (setf ecukes-only-failing t)
         (let ((negated (string-prefix-p "~" selector)))
           (when negated
             (setf selector (substring selector 1)))
           (let ((tag (string-prefix-p "@" selector)))
             (when tag
               (setf selector (substring selector 1)))
             (push selector (if tag
                                (if negated ecukes-exclude-tags ecukes-include-tags)
                              (if negated ecukes-anti-patterns ecukes-patterns)))))))
     ,@body))

(defun eldev-run-ecukes-tests (feature-files selectors &optional environment)
  "Run Ecukes tests according to given SELECTORS (patterns)."
  (eldev--test-init-ecukes)
  (eldev-bind-from-environment environment (ecukes-verbose)
    (ecukes-reporter-use "dot")
    (eldev--test-ecukes-split-selectors selectors
      (let* (skip-the-rest
             have-skipped-something
             (maybe-skip (lambda ()
                           (when skip-the-rest
                             (unless have-skipped-something
                               (eldev-warn "Stopping early because of the failed scenario%s" (if (> ecukes-stats-scenarios-failed 1) "s" "")))
                             (setf have-skipped-something t)))))
        (eldev-advised ('ecukes-run-feature :around (lambda (original &rest args)
                                                      (unless (funcall maybe-skip)
                                                        (apply original args))))
          (eldev-advised ('ecukes-run-scenario :around (lambda (original &rest args)
                                                         (unless (funcall maybe-skip)
                                                           (let ((failures-before ecukes-stats-scenarios-failed))
                                                             (apply original args)
                                                             (when (and eldev-test-stop-on-unexpected
                                                                        (> ecukes-stats-scenarios-failed failures-before)
                                                                        (<= (setf eldev-test-stop-on-unexpected (1- eldev-test-stop-on-unexpected)) 0))
                                                               (setf skip-the-rest t))))))
            ;; Inject profiling support if wanted.
            (eldev-advised ('ecukes-run-steps :around (when eldev--effective-profile-mode
                                                        (lambda (original &rest args)
                                                          (eldev-profile-body
                                                            (apply original args)))))
              (if eldev-test-print-backtraces
                  (let ((eldev--ecukes-backtraces (make-hash-table :test #'eq)))
                    (eldev-advised ('ecukes-run-step :around #'eldev--ecukes-run-step)
                      (eldev-advised ('ecukes-reporter-print-step :after (lambda (step)
                                                                           (let ((backtrace (gethash step eldev--ecukes-backtraces)))
                                                                             (when backtrace
                                                                               (eldev-output "%s" backtrace)))))
                        (ecukes-run feature-files))))
                ;; Not trying to highlight the summary, Ecukes already does that itself.
                (ecukes-run feature-files)))))
        (setf eldev-test-num-passed (+ eldev-test-num-passed ecukes-stats-scenarios-passed)
              eldev-test-num-failed (+ eldev-test-num-failed ecukes-stats-scenarios-failed))
        (when (> ecukes-stats-scenarios-failed 0)
          (signal 'eldev-error `("%s failed" ,(eldev-message-plural ecukes-stats-scenarios-failed "Ecukes scenario"))))))))

(defun eldev--ecukes-run-step (original step &rest args)
  (let ((eldev--ecukes-current-step step)
        (debug-on-error t)
        (debugger       #'eldev--ecukes-backtrace-collector))
    (apply original step args)))

(defun eldev--ecukes-backtrace-collector (&rest args)
  (when (eq (car args) 'error)
    (let* ((frames (cdr (eldev-backtrace-frames #'eldev--ecukes-backtrace-collector)))
           (scan   (reverse frames))
           ;; We are going to indent it.
           (eldev-backtrace-style (if (integerp eldev-backtrace-style) (- eldev-backtrace-style 4) eldev-backtrace-style)))
      (while scan
        (let ((frame (pop scan)))
          (when (eq (eldev-backtrace-frame-function frame) #'eldev--ecukes-run-step)
            ;; Heuristic: if not byte-compiled, drop `let' frame (see above).
            (when (and (not (byte-code-function-p #'eldev--ecukes-run-step)) (eq (eldev-backtrace-frame-function (car scan)) 'let))
              (setf scan (cdr scan)))
            ;; Heuristic: drop a few more uninteresting frames.
            (when (and (eq (eldev-backtrace-frame-function (nth 0 scan)) 'apply)
                       (byte-code-function-p (eldev-backtrace-frame-function (nth 1 scan)))
                       (eq (eldev-backtrace-frame-function (nth 2 scan)) 'apply)
                       (eq (car-safe (eldev-backtrace-frame-function (nth 3 scan))) 'lambda))
              (setf scan (nthcdr 4 scan)))
            (setf frames (nreverse scan)
                  scan   nil))))
      (puthash eldev--ecukes-current-step (replace-regexp-in-string "^" "    " (eldev-backtrace-to-string frames) t t)
               eldev--ecukes-backtraces))))


(provide 'eldev-ecukes)

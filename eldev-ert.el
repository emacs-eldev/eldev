;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2019 Paul Pogonyshev

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
(require 'ert)


;; Forward-declare some variables.
(defvar ert-quiet)
(defvar ert-batch-backtrace-right-margin)


;; Functions for binding Eldev with ERT testing framework.  Broken out
;; into a separate file so that `eldev.el' doesn't have to require
;; `ert' feature.

(defvar eldev--test-ert-short-backtraces nil)
(defvar eldev--test-ert-results nil)

(defun eldev-test-ert-preprocess-selectors (selectors)
  "Convert SELECTORS to ERT format."
  (eldev-test-selectors-to-elisp-values selectors t))

(defun eldev-test-ert-load-results ()
  "Load previous ERT test results if they are present."
  (eldev-test-do-load-results "ert" "previous ERT test results" 1
    (let ((results (cdr (assq 'results contents))))
      (dolist (result results)
        (when (ert-test-boundp (car result))
          (setf (ert-test-most-recent-result (ert-get-test (car result))) (cdr result))))
      (setf eldev--test-ert-results results))))

(defun eldev-test-ert-save-results ()
  "Save ERT test results for future use."
  (eldev-test-do-save-results "ert" "ERT test results" 1
    (let (results)
      (mapatoms (lambda (symbol)
                  (when (ert-test-boundp symbol)
                    ;; Of course we cannot print-read the backtraces (and probably
                    ;; something else) directly.  Lisp...
                    (let ((result (ert-test-most-recent-result (ert-get-test symbol))))
                      (when result
                        (setf result (copy-sequence result))
                        (unless (or (ert-test-passed-p result) (ert-test-aborted-with-non-local-exit-p result))
                          (setf (ert-test-result-with-condition-condition result) '(...)
                                (ert-test-result-with-condition-backtrace result) '(...)
                                (ert-test-result-with-condition-infos     result) '(...))))
                      (push `(,symbol . ,result) results)))))
      ;; Use `eldev--test-ert-results' to not forget results of tests that were not loaded
      ;; this time.
      (dolist (result eldev--test-ert-results)
        (unless (assq (car result) results)
          (push result results)))
      `((results . ,results)))))

(defun eldev-run-ert-tests (selectors &optional environment)
  "Run ERT tests according to given SELECTORS.
This is a wrapper around `ert-run-tests-batch' that handles
`eldev-test-stop-on-unexpected'.  Test runners should generally
use this for ERT framework, unless they can do better."
  ;; Since ERT doesn't support features we want out-of-the-box, we have to hack.
  (eldev-bind-from-environment environment (ert-quiet ert-batch-backtrace-right-margin eldev--test-ert-short-backtraces)
    (let (completed-tests)
      (eldev-advised (#'ert-run-tests
                      ;; There is a difference in number arguments in Emacs 24, so just hide
                      ;; the extra arguments with `&rest'.
                      :around (lambda (original selector listener &rest rest)
                                (apply original selector
                                       (lambda (event-type &rest arguments)
                                         ;; Older ERT versions have `ert--print-backtrace',
                                         ;; newer use `backtrace-to-string'.  Not using
                                         ;; function-quoting to avoid warnings.
                                         (prog1 (eldev-advised ('backtrace-to-string
                                                                :around (lambda (original &optional frames)
                                                                          (if eldev-test-print-backtraces
                                                                              (funcall original (eldev--ert-maybe-shorten-backtrace frames))
                                                                            "    [omitted]")))
                                                  (eldev-advised ('ert--print-backtrace
                                                                  :around (lambda (original &optional frames &rest arguments)
                                                                            (if eldev-test-print-backtraces
                                                                                (apply original (eldev--ert-maybe-shorten-backtrace frames) arguments)
                                                                              (insert "    [omitted]\n"))))
                                                    (apply listener event-type arguments)))
                                           (pcase event-type
                                             (`run-started
                                              (eldev-test-validate-amount (ert-stats-total (nth 0 arguments))))
                                             (`test-ended
                                              (when eldev-test-stop-on-unexpected
                                                (let ((stats             (nth 0 arguments))
                                                      (test              (nth 1 arguments))
                                                      (result            (nth 2 arguments))
                                                      (num-tests-ignored 0))
                                                  (push test completed-tests)
                                                  (unless (ert-test-result-expected-p test result)
                                                    ;; Since this really goes into internals, assert some things beforehand.
                                                    (when (and (fboundp #'ert--stats-tests) (fboundp #'ert-test-most-recent-result)
                                                               (vectorp (ert--stats-tests stats)))
                                                      (setf num-tests-ignored        (- (length (ert--stats-tests stats)) (length completed-tests))
                                                            (ert--stats-tests stats) (vconcat (nreverse completed-tests))))
                                                    (when (> num-tests-ignored 0)
                                                      (eldev-warn "\nStopping before %s" (eldev-message-plural num-tests-ignored "more test")))
                                                    (signal 'eldev-quit 1))))))))
                                       rest)))
        (let* ((statistics     (ert-run-tests-batch (eldev-build-ert-selector selectors)))
               (num-unexpected (ert-stats-completed-unexpected statistics)))
          (unless (= num-unexpected 0)
            (signal 'eldev-error `("%s produced an unexpected result" ,(eldev-message-plural num-unexpected "test")))))))))

(defun eldev--ert-maybe-shorten-backtrace (frames)
  (when eldev--test-ert-short-backtraces
    ;; Drop the frames that are inside ERT and Eldev.
    (let ((scan (reverse frames)))
      (while scan
        (let ((frame (pop scan)))
          (when (eq (eldev--ert-frame-function frame) 'ert--run-test-internal)
            ;; Heuristic: older Emacs versions have two more "uninteresting" frames where
            ;; first is a `funcall' and second is some byte-compiled function.
            (when (and (eq (eldev--ert-frame-function (car scan)) 'funcall)
                       (byte-code-function-p (eldev--ert-frame-function (cadr scan))))
              (setf scan (cddr scan)))
            (setf frames (nreverse scan)
                  scan   nil))))))
  frames)

(declare-function backtrace-frame-fun "backtrace" (frame))

(defun eldev--ert-frame-function (frame)
  (cond ((listp frame)                   (nth 1 frame))  ;; Older Emacs versions.
        ((fboundp #'backtrace-frame-fun) (backtrace-frame-fun frame))
        (t                               (aref 1 frame))))

(defun eldev-build-ert-selector (selectors)
  "Convert a list of SELECTORS to a single ERT selector.
If the list contains several selectors, they are combined with
`or' operator, as promised by the `test' command documentation.

When `eldev-test-dwim' is set, any symbol that is not an exact
test name is instead treated as a regular expression that is
supposed to match test names."
  (let ((ert-selectors (mapcar (lambda (selector)
                                 (let ((as-elisp  (car selector))
                                       (as-string (cdr selector)))
                                   (if (and eldev-test-dwim
                                            (or (and (symbolp as-elisp) (not (memq as-elisp '(nil t))) (not (keywordp as-elisp)) (not (ert-test-boundp as-elisp)))
                                                (numberp as-elisp)))
                                       as-string
                                     as-elisp)))
                               selectors)))
    (if (cdr ert-selectors)
        `(or ,@ert-selectors)
      (car ert-selectors))))


(provide 'eldev-ert)

;;; eldev-ert.el ends here

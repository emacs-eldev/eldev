;;; eldev-doctest.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2024 Paul Pogonyshev

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


(defvar doctest-message-level)
(defvar doctest-after-every-test-functions)
(defvar doctest-after-all-tests-hook)

(declare-function doctest-files "doctest")
(declare-function doctest-state "doctest")


(defvar eldev--test-doctest-concise-expected nil)


(defun eldev-test-doctest-preprocess-selectors (selectors)
  (when selectors
    (eldev-warn "Doctest currently doesn't support selectors; they will be ignored")))

(defun eldev-run-doctest-tests (files &optional environment)
  "Run Doctest tests in specified FILES."
  (when eldev-test-stop-on-unexpected
    (eldev-warn "Option `--stop-on-unexpected' (`-s') is not supported with Doctest framework"))
  ;; Unlike with other testing frameworks, the feature here is not required by the test
  ;; files: those files are the same as program's source.
  (eldev--require-external-feature 'doctest)
  (eldev--test-load-files files)
  (eldev-bind-from-environment environment (doctest-message-level eldev--test-doctest-concise-expected)
    (let ((doctest-after-every-test-functions doctest-after-every-test-functions)
          (doctest-after-all-tests-hook       doctest-after-all-tests-hook))
      (when eldev--test-doctest-concise-expected
        (push (lambda (params)
                (let ((state (doctest-state)))
                  (eldev-test-runner-concise-tick (and (eq (cdr (assq 'result params)) 'failure)
                                                       ;; Only force a number if Doctest is going to print something.
                                                       (eq doctest-message-level 'info))
                                                  (cdr (assq 'total state)))))
              doctest-after-every-test-functions)
        (push (lambda ()
                (let* ((state     (doctest-state))
                       (num-total (cdr (assq 'total state))))
                  (unless (= eldev--test-runner-concise-num-reported num-total)
                    (eldev-test-runner-concise-tick t num-total))))
              doctest-after-all-tests-hook))
      (doctest-files files)))
  (let* ((state      (doctest-state))
         (num-passed (cdr (assq 'passed state)))
         (num-failed (cdr (assq 'failed state))))
    (setf eldev-test-num-passed  (+ eldev-test-num-passed num-passed)
          eldev-test-num-failed  (+ eldev-test-num-failed num-failed))
    ;; Even if unsupported by the framework, at least update the variable so that we can
    ;; stop before running further test types.
    (when eldev-test-stop-on-unexpected
      (setf eldev-test-stop-on-unexpected (- eldev-test-stop-on-unexpected num-failed)))
    (unless (= num-failed 0)
      (signal 'eldev-error `("%s failed" ,(if (> num-failed 0) (eldev-message-plural num-failed "doctest") "doctest"))))))


(provide 'eldev-doctest)

;;; eldev-doctest.el ends here

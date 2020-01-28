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


;; FIXME: Let's hope for improvement in Buttercup itself.  Large parts
;;        of this integration code can be pushed into Buttercup.


(defvar buttercup-stack-frame-style)
(defvar buttercup-suites)
(defvar buttercup-color)
(defvar buttercup-reporter)

(declare-function buttercup--specs "buttercup")
(declare-function buttercup-spec-full-name "buttercup")
(declare-function buttercup-run "buttercup")
(declare-function buttercup-spec-status "buttercup")
(declare-function buttercup-spec-failure-description "buttercup")
(declare-function buttercup-suites-total-specs-defined "buttercup")


(defvar eldev--buttercup-silent-skipping nil)
(defvar eldev--buttercup-quiet nil)


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
  (eldev-bind-from-environment environment (buttercup-color eldev--buttercup-silent-skipping eldev--buttercup-quiet buttercup-stack-frame-style)
    ;; Ugly, but as of 1.19 there is no better way in Buttercup.
    (let ((num-skipped 0))
      (when selectors
        (dolist (spec (buttercup--specs buttercup-suites))
          (let ((spec-full-name (buttercup-spec-full-name spec)))
            (unless (eldev-any-p (string-match it spec-full-name) selectors)
              (eval `(setf (buttercup-spec-function ,spec)
                           (lambda () (signal 'buttercup-pending "SKIPPED")))
                    t)
              (setf num-skipped (1+ num-skipped))))))
      (eldev-test-validate-amount (- (buttercup-suites-total-specs-defined buttercup-suites) num-skipped)))
    (let ((buttercup-reporter (if (or eldev--buttercup-silent-skipping eldev--buttercup-quiet)
                                  (eldev--buttercup-reporter-delaying-adapter buttercup-reporter)
                                buttercup-reporter)))
      (buttercup-run))))


;; I submitted a more complicated version of this with additional
;; features as a PR to Buttercup.
(defun eldev--buttercup-reporter-delaying-adapter (base-reporter)
  (let (pending)
    (lambda (event arg)
      (pcase event
        ((or `suite-started `spec-started)
         (push (list event arg) pending))
        ((or `suite-done `spec-done)
         (if (if (eq event 'suite-done)
                 pending
               (eldev--buttercup-spec-omitted-p arg))
             (pop pending)
           (dolist (event-arg (nreverse pending))
             (apply base-reporter event-arg))
           (funcall base-reporter event arg)
           (setf pending nil)))
        (_
         ;; Other events are never delayed.
         (funcall base-reporter event arg))))))

(defun eldev--buttercup-spec-omitted-p (spec)
  (or (and eldev--buttercup-quiet
           (not (eq (buttercup-spec-status spec) 'failed)))
      (and eldev--buttercup-silent-skipping
           (eq (buttercup-spec-status spec) 'pending)
           (equal (buttercup-spec-failure-description spec) "SKIPPED"))))


(provide 'eldev-buttercup)

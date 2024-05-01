;;  -*- lexical-binding: t -*-

(require 'test/common)


;; Test integration with Docker and Podman.  Since those are nearly compatible, test both
;; with the same code.


(defun eldev--test-container-emacs-version (type)
  (eldev-pcase-exhaustive type
    (`docker "27.2")
    (`podman "28.2")))

(defvar eldev--test-container-img-installed nil)


(defun eldev--test-container-maybe-skip (type)
  (unless (eldev--container-on-supported-os)
    (ert-skip (eldev-format-message "Skipping `%s' tests: not on a supported OS" type)))
  (unless (eldev-pcase-exhaustive type
            (`docker (eldev-docker-executable t))
            (`podman (eldev-podman-executable t)))
    (ert-skip (eldev-format-message "Skipping `%s' tests: the program is not installed" type))))

(defmacro eldev--test-run-in-container (test-project type command-line &rest body)
  (declare (indent 2) (debug (stringp sexp body)))
  (let ((image (cadr (member type command-line))))
    (unless image
      (error "Cannot infer Docker/Podman image from the command line `%S'" command-line))
    `(progn
       (eldev--test-container-maybe-skip ,type)
       (let ((eldev--test-eldev-local (expand-file-name default-directory)))
         (eldev--test-ensure-container-img ,type ,image)
         (eldev--test-run ,test-project ,command-line
           ,@body)))))

(defun eldev--test-ensure-container-img (type image)
  (unless (member (cons type image) eldev--test-container-img-installed)
    ;; Don't want to figure minimal command line, just run Eldev.  If we don't make sure
    ;; that the necessary Docker image is available now, this can later screw up output of
    ;; actual tests.
    (eldev-call-process eldev--test-shell-command `(,(symbol-name type) ,image) :die-on-error t)
    (push (cons type image) eldev--test-container-img-installed)))


(eldev-ert-defargtest eldev-container-emacs-1 (type)
                      ('docker 'podman)
  (eldev--test-run-in-container "trivial-project" type ("--quiet" type (eldev--test-container-emacs-version type)
                                                        "emacs" "--batch" "--eval" `(prin1 (+ 1 2)))
    (should (string= stdout "3"))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-container-emacs-2 (type)
                      ('docker 'podman)
  (eldev--test-run-in-container "trivial-project" type ("--quiet" type "25"
                                                        "emacs" "--batch" "--eval" `(prin1 (+ 1 2)))
    (should (string= stdout "3"))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-container-test-1 (type)
                      ('docker 'podman)
  (eldev--test-container-maybe-skip type)
  (let ((eldev--test-project "project-c"))
    (eldev--test-run nil ("clean" "all")
      (should (= exit-code 0)))
    (eldev--test-run-in-container nil type (type (eldev--test-container-emacs-version type) "test")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-container-color-propagation-1 (type)
                      ('docker 'podman)
  (eldev--test-run-in-container "trivial-project" type ("--color=never" type (eldev--test-container-emacs-version type)
                                                        "exec" `(eldev-warn "test"))
    :discard-ansi nil
    (should (string= stdout "test\n"))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-container-color-propagation-2 (type)
                      ('docker 'podman)
  (eldev--test-run-in-container "trivial-project" type ("--color=always" type (eldev--test-container-emacs-version type)
                                                        "exec" `(eldev-warn "test"))
    :discard-ansi nil
    ;; This is the word "test" colored red, hardcoded.  I doubt we need a function for this.
    (should (string= stdout "\33[31mtest\33[0m\n"))
    (should (= exit-code 0))))


(ert-deftest eldev-robust-mode-effectively-nil ()
  ;; Put here rather than in `robust-mode.el', since the test needs Docker.  Robust mode
  ;; defaults to "auto" and must effectively result in nil on non-CI machines, which
  ;; Docker image emulates.
  (eldev--test-run-in-container "trivial-project" 'docker ('docker (eldev--test-container-emacs-version 'docker)
                                                                  "eval" `(eldev-retry-on-errors-p))
    (should (string= stdout "nil\n"))
    (should (= exit-code 0))))


(provide 'test/emacs-container)

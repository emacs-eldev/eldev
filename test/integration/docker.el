(require 'test/common)


(defvar eldev--test-docker-emacs-version "27.2")

(defvar eldev--test-docker-img-installed nil)


;; Emacs 25-27 gets confused otherwise, probably because we "call" the function from
;; another macro, `eldev--test-run-in-docker'.
(declare-function skip-unless nil)


(defmacro eldev--test-run-in-docker (test-project command-line &rest body)
  (declare (indent 2) (debug (stringp sexp body)))
  (let ((image (cadr (member "docker" command-line))))
    (unless image
      (error "Cannot infer docker image from the command line %S" command-line))
    `(progn
       (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
       (let ((eldev--test-eldev-local (expand-file-name default-directory)))
         (eldev--test-ensure-docker-img ,image)
         (eldev--test-run ,test-project ,command-line
           ,@body)))))

(defun eldev--test-ensure-docker-img (image)
  (unless (member image eldev--test-docker-img-installed)
    ;; Don't want to figure minimal command line, just run Eldev.  If we don't make sure
    ;; that the necessary Docker image is available now, this can later screw up output of
    ;; actual tests.
    (eldev-call-process eldev--test-shell-command `("docker" ,image) :die-on-error t)
    (push image eldev--test-docker-img-installed)))


(ert-deftest eldev-docker-emacs-1 ()
  (eldev--test-run-in-docker "trivial-project" ("--quiet" "docker" eldev--test-docker-emacs-version
                                                "emacs" "--batch" "--eval" `(prin1 (+ 1 2)))
    (should (string= stdout "3"))
    (should (= exit-code 0))))

(ert-deftest eldev-docker-emacs-2 ()
  (eldev--test-run-in-docker "trivial-project" ("--quiet" "docker" "25"
                                                "emacs" "--batch" "--eval" `(prin1 (+ 1 2)))
    (should (string= stdout "3"))
    (should (= exit-code 0))))

(ert-deftest eldev-docker-test-1 ()
  (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
  (let ((eldev--test-project "project-c"))
    (eldev--test-run nil ("clean" "all")
      (should (= exit-code 0)))
    (eldev--test-run-in-docker nil ("docker" eldev--test-docker-emacs-version "test")
      (should (= exit-code 0)))))

(ert-deftest eldev-docker-color-propagation-1 ()
  (eldev--test-run-in-docker "trivial-project" ("--color=never" "docker" eldev--test-docker-emacs-version
                                                "exec" `(eldev-warn "test"))
    :discard-ansi nil
    (should (string= stdout "test\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-docker-color-propagation-2 ()
  (eldev--test-run-in-docker "trivial-project" ("--color=always" "docker" eldev--test-docker-emacs-version
                                                "exec" `(eldev-warn "test"))
    :discard-ansi nil
    ;; This is the word "test" colored red, hardcoded.  I doubt we need a function for this.
    (should (string= stdout "\33[31mtest\33[0m\n"))
    (should (= exit-code 0))))


(provide 'test/emacs-docker)

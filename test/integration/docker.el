(require 'test/common)


(defvar eldev--test-docker-emacs-version "27.2")


(ert-deftest eldev-docker-emacs-1 ()
  (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
  (let ((eldev--test-eldev-local (expand-file-name default-directory)))
    (eldev--test-run "trivial-project"
        ("--quiet"
         "docker"
         eldev--test-docker-emacs-version
         "emacs"
         "--batch"
         "--eval"
         `(prin1 (+ 1 2)))
      (should (string-suffix-p "3" stdout))
      (should (= exit-code 0)))))

(ert-deftest eldev-docker-emacs-2 ()
  (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
  (let ((eldev--test-eldev-local (expand-file-name default-directory)))
    (eldev--test-run "trivial-project"
        ("--quiet"
         "docker"
         "25"
         "emacs"
         "--batch"
         "--eval"
         `(prin1 (+ 1 2)))
      (should (string-suffix-p "3" stdout))
      (should (= exit-code 0)))))

(ert-deftest eldev-docker-test-1 ()
  (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
  (eldev--test-run "project-c" ("clean" "all")
    (should (= exit-code 0)))
  (let ((eldev--test-eldev-local (expand-file-name default-directory)))
    (eldev--test-run "project-c"
        ("docker"
        eldev--test-docker-emacs-version
        "test")
      (should (= exit-code 0)))))

(ert-deftest eldev-docker-color-propagation-1 ()
  (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
  (let ((eldev--test-eldev-local (expand-file-name default-directory)))
    (eldev--test-run "trivial-project" ("--color=never" "docker" eldev--test-docker-emacs-version "exec" `(eldev-warn "test"))
      :discard-ansi nil
      (should (string= stdout "test\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-docker-color-propagation-2 ()
  (skip-unless (and (eldev--docker-on-supported-os) (eldev-docker-executable t)))
  (let ((eldev--test-eldev-local (expand-file-name default-directory)))
    (eldev--test-run "trivial-project" ("--color=always" "docker" eldev--test-docker-emacs-version "exec" `(eldev-warn "test"))
      :discard-ansi nil
      ;; This is the word "test" colored red, hardcoded.  I doubt we need a function for this.
      (should (string= stdout "\33[31mtest\33[0m\n"))
      (should (= exit-code 0)))))


(provide 'test/emacs-docker)

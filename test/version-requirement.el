(require 'test/common)


(defmacro eldev--test-require-version (test-project command-line succeeds &rest body)
  (declare (indent 3) (debug (stringp sexp booleanp body)))
  `(eldev--test-run ,test-project ("--setup" `(eldev-require-version "999.9") ,@command-line)
     ,@(if succeeds
          `((should (= exit-code 0)))
        `((should (= exit-code 1))
          (should (string-match-p "requires Eldev version 999\\.9 or newer" stderr))))
     ,@body))


(ert-deftest eldev-require-version-archives-1 ()
  (eldev--test-require-version "project-a" ("archives") nil))

(ert-deftest eldev-require-version-build-1 ()
  (eldev--test-require-version "project-a" ("build") nil))

(ert-deftest eldev-require-version-clean-1 ()
  (eldev--test-require-version "project-a" ("clean") nil))

(ert-deftest eldev-require-version-compile-1 ()
  (eldev--test-require-version "project-a" ("compile") nil))

(ert-deftest eldev-require-version-dependencies-1 ()
  (eldev--test-require-version "project-a" ("dependencies") t))

(ert-deftest eldev-require-version-dependency-tree-1 ()
  (eldev--test-require-version "project-a" ("dependency-tree") nil))

(ert-deftest eldev-require-version-emacs-1 ()
  (eldev--test-require-version "project-a" ("emacs" "--batch") nil))

(ert-deftest eldev-require-version-eval ()
  (eldev--test-require-version "project-a" ("eval") nil))

(ert-deftest eldev-require-version-emacs-1 ()
  (eldev--test-require-version "project-a" ("exec") nil))

(ert-deftest eldev-require-version-help-1 ()
  (eldev--test-require-version "project-a" ("help") t
    ;; `eldev-help' also specifies default options, which are
    ;; difficult to syncronize between the two processes.
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))))

(ert-deftest eldev-require-version-info-1 ()
  (eldev--test-require-version "project-a" ("info") t
    ;; Depending on Emacs (and `package.el') version, summary can be different.
    (should (or (string= stdout "project-a 1.0\n\nTest project with one dependency\n")
                (string= stdout "project-a 1.0\n\nComments to make linters happy.\n")))))

(ert-deftest eldev-require-version-init-1 ()
  (eldev--test-require-version "project-a" ("init") nil))

(ert-deftest eldev-require-version-package-1 ()
  (eldev--test-require-version "project-a" ("package") nil))

(ert-deftest eldev-require-version-prepare-1 ()
  (eldev--test-require-version "project-a" ("prepare") nil))

(ert-deftest eldev-require-version-targets-1 ()
  (eldev--test-require-version "project-a" ("targets") nil))

(ert-deftest eldev-require-version-test-1 ()
  (eldev--test-require-version "project-a" ("test") nil))

(ert-deftest eldev-require-version-upgrade-1 ()
  (eldev--test-require-version "project-a" ("upgrade") nil))

(ert-deftest eldev-require-version-upgrade-self-1 ()
  (eldev--test-require-version "project-a" ("upgrade-self" "--dry-run") t))

(ert-deftest eldev-require-version-version-1 ()
  (eldev--test-require-version "project-a" ("version") t
    (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))))

(ert-deftest eldev-require-version-version-2 ()
  (eldev--test-require-version "project-a" ("version" "project-a") t
    (should (string= stdout "project-a 1.0\n"))))

(ert-deftest eldev-require-version-version-3 ()
  (eldev--test-require-version "project-a" ("version" "emacs") t
    (should (string= stdout (format "emacs %s\n" emacs-version)))))

(ert-deftest eldev-require-version-version-4 ()
  (eldev--test-require-version "project-a" ("version" "dependency-a") nil))


(provide 'test/version-requirement)

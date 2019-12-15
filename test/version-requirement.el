(require 'test/common)


(defmacro emake--test-require-version (test-project command-line succeeds &rest body)
  (declare (indent 3) (debug (stringp sexp booleanp body)))
  `(emake--test-run ,test-project ("--setup" "(emake-require-version \"999.9\")" ,@command-line)
     ,@(if succeeds
          `((should (= exit-code 0)))
        `((should (= exit-code 1))
          (should (string-match-p "requires Emake version 999\\.9 or newer" stderr))))
     ,@body))


(ert-deftest emake-test-require-version-archives-1 ()
  (emake--test-require-version "project-a" ("archives") nil))

(ert-deftest emake-test-require-version-build-1 ()
  (emake--test-require-version "project-a" ("build") nil))

(ert-deftest emake-test-require-version-clean-1 ()
  (emake--test-require-version "project-a" ("clean") nil))

(ert-deftest emake-test-require-version-compile-1 ()
  (emake--test-require-version "project-a" ("compile") nil))

(ert-deftest emake-test-require-version-dependencies-1 ()
  (emake--test-require-version "project-a" ("dependencies") t))

(ert-deftest emake-test-require-version-dependency-tree-1 ()
  (emake--test-require-version "project-a" ("dependency-tree") nil))

(ert-deftest emake-test-require-version-emacs-1 ()
  (emake--test-require-version "project-a" ("emacs" "--batch") nil))

(ert-deftest emake-test-require-version-eval ()
  (emake--test-require-version "project-a" ("eval") nil))

(ert-deftest emake-test-require-version-emacs-1 ()
  (emake--test-require-version "project-a" ("exec") nil))

(ert-deftest emake-test-require-version-help-1 ()
  (emake--test-require-version "project-a" ("help") t
    ;; `emake-help' also specifies default options, which are
    ;; difficult to syncronize between the two processes.
    (should (string-prefix-p (emake--test-in-project-environment (emake--test-first-line (emake--test-capture-output (emake-help))))
                             stdout))))

(ert-deftest emake-test-require-version-info-1 ()
  (emake--test-require-version "project-a" ("info") t
    (should (string= stdout "project-a 1.0\n\nTest project with one dependency\n"))))

(ert-deftest emake-test-require-version-init-1 ()
  (emake--test-require-version "project-a" ("init") nil))

(ert-deftest emake-test-require-version-package-1 ()
  (emake--test-require-version "project-a" ("package") nil))

(ert-deftest emake-test-require-version-prepare-1 ()
  (emake--test-require-version "project-a" ("prepare") nil))

(ert-deftest emake-test-require-version-targets-1 ()
  (emake--test-require-version "project-a" ("targets") nil))

(ert-deftest emake-test-require-version-test-1 ()
  (emake--test-require-version "project-a" ("test") nil))

(ert-deftest emake-test-require-version-upgrade-1 ()
  (emake--test-require-version "project-a" ("upgrade") nil))

;; FIXME
;; (ert-deftest emake-test-require-version-upgrade-self-1 ()
;;   (emake--test-require-version "project-a" ("upgrade") t))

(ert-deftest emake-test-require-version-version-1 ()
  (emake--test-require-version "project-a" ("version") t
    (should (string= stdout (format "emake %s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))))

(ert-deftest emake-test-require-version-version-2 ()
  (emake--test-require-version "project-a" ("version" "project-a") t
    (should (string= stdout "project-a 1.0\n"))))

(ert-deftest emake-test-require-version-version-3 ()
  (emake--test-require-version "project-a" ("version" "emacs") t
    (should (string= stdout (format "emacs %s\n" emacs-version)))))

(ert-deftest emake-test-require-version-version-4 ()
  (emake--test-require-version "project-a" ("version" "dependency-a") nil))


(provide 'test/version-requirement)

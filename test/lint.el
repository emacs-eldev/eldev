(require 'test/common)


;; We don't test built-in linters since that would make Eldev tests
;; unstable (i.e. could break just because a linter is changed
;; upstream) and slow (would need to download packages from MELPA or
;; GNU ELPA).

(eval-and-compile
  (defun eldev--test-dummy-linter (&optional archive)
    (unless archive
      (setf archive "archive-a"))
    (let ((archive-path (expand-file-name (format "test/package-%s" archive) eldev-project-dir)))
      `(eldev-deflinter eldev-linter-dummy ()
         (eldev-add-extra-dependencies 'runtime '(:package dummy-lint :archive (,archive . ,archive-path)))
         (eldev-load-extra-dependencies 'runtime)
         (require 'dummy-lint)
         (eldev-advised ('dummy-lint-warn :around (lambda (original &rest arguments)
                                                    (eldev-lint-printing-warning :warning
                                                      (apply original arguments))))
           (dolist (file (eldev-lint-find-files "*.el"))
             (eldev-lint-linting-file file
               (dummy-lint-file file))))))))

(defmacro eldev--test-linting (test-project linter-archive command-line &rest body)
  (declare (indent 3) (debug (stringp sexp sexp body)))
  `(eldev--test-run ,test-project ("--setup" ',(eldev--test-dummy-linter linter-archive) ,@command-line)
     ,@body))


(ert-deftest eldev-lint-project-a-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-linting nil "archive-a" ("lint" "dummy")
      (should (string-match-p "Installing.+dummy-lint" stderr))
      (should (= exit-code 0)))))

(ert-deftest eldev-lint-project-b-1 ()
  (let ((eldev--test-project "project-b"))
    (eldev--test-delete-cache)
    (eldev--test-linting nil "archive-a" ("lint" "dummy")
      (should (string-match-p "^project-b\\.el:.+: warning:" stderr))
      (should (string-match-p "Installing.+dummy-lint" stderr))
      (should (= exit-code 1)))))


(ert-deftest eldev-lint-missing-linter-1 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-linting nil "archive-b" ("lint" "dummy")
      (should (string-match-p "not available.+skipping" stderr))
      ;; When a linter is not available, Eldev counts it as "not
      ;; giving any warnings".  This is mostly for running `eldev
      ;; lint' without parameters on older Emacs versions.
      (should (= exit-code 0)))))

(ert-deftest eldev-lint-disabled-linter-message ()
  (eldev--test-run "empty-project" ("--setup" '(add-to-list 'eldev-lint-disabled 'elisp) "lint" "elisp")
    (should (string= stderr (eldev-format-message "Linter `elisp' is disabled (see variable `eldev-lint-disabled')\n")))
    (should (= exit-code 1))))

;; This file should be ignored, even when mentioned explicitly on the command line (we
;; treat command line arguments as a fileset, and don't distinguish between explicit names
;; and e.g. wildcards).  See issue #34.
(ert-deftest eldev-lint-ignores-pkg-file ()
  (eldev--test-run "project-c" ("lint" "checkdoc" "project-c-pkg.el")
    (should (string= stdout "Linter has no complaints\n"))
    (should (= exit-code 0))))


(ert-deftest eldev-lint-first-unavailable ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    ;; Install a fake of `relint'.  We don't care about the output, only that it is
    ;; available, so don't access the network needlessly.
    (eldev--test-run nil ("--setup" `(push '(relint :archive relint-pseudoarchive) eldev-known-tool-packages)
                          "--setup" `(setf eldev--known-package-archives '((relint-pseudoarchive ("relint-pseudoarchive" . ,(expand-file-name "../relint-pseudoarchive")) 0)))
                          "lint" "re")
      (should (= exit-code 0)))
    ;; We use `--external' only to forbid it from installing anything now.  Otherwise it
    ;; is the same directory as normally used.
    (eldev--test-run nil ("--setup" `(push '(relint :version "0.0.1") eldev-known-tool-packages)
                          (format "--external=%s" (let ((eldev-project-dir (eldev--test-project-dir)))
                                                    (expand-file-name "packages" (eldev-cache-dir t))))
                          "lint" "package" "re")
      ;; First linter is not available and should thus be skipped, but not `relint', which
      ;; has been "installed" previously.
      (should      (string-match-p "skipping linter .package." stderr))
      (should (not (string-match-p "skipping linter .re."      stderr)))
      (should (= exit-code 0)))))


(provide 'test/lint)

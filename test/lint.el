(require 'test/common)


;; We don't test built-in linters since that would make Eldev tests
;; unstable (i.e. could break just because a linter is changed
;; upstream) and slow (would need to download packages from Melpa or
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


(provide 'test/lint)

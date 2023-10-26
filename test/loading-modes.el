;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-loading-modes-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-a.elc"))))
    (eldev--test-run nil ("--byte-compiled" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should     (file-exists-p (eldev--test-file "project-a.elc"))))
    (eldev--test-run nil ("--source" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should     (string= stdout (eldev--test-lines "nil")))
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-a.elc"))))
    (eldev--test-run nil ("--packaged" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      ;; It should be compiled only in the installed package, somewhere inside `.eldev'.
      (should-not (file-exists-p (eldev--test-file "project-a.elc"))))
    (eldev--test-run nil ("--compiled-on-demand" "eval" `(byte-code-function-p (symbol-function 'project-a-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should     (file-exists-p (eldev--test-file "project-a.elc"))))))

(ert-deftest eldev-loading-modes-2 ()
  (let ((eldev--test-project "project-g"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-g.elc")))
      (should-not (file-exists-p (eldev--test-file "project-g-util.elc"))))
    (eldev--test-run nil ("--byte-compiled" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should     (file-exists-p (eldev--test-file "project-g.elc")))
      (should     (file-exists-p (eldev--test-file "project-g-util.elc"))))
    (eldev--test-run nil ("--source" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should     (string= stdout (eldev--test-lines "nil")))
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-g.elc")))
      (should-not (file-exists-p (eldev--test-file "project-g-util.elc"))))
    ;; Previously would fail because of a bug in Emacs itself.  On Emacs 24.x would fail
    ;; simply because reloading files with byte-compiled versions had not been implemented
    ;; in pre-25 versions.
    (eldev--test-run nil ("--packaged" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-g.elc")))
      (should-not (file-exists-p (eldev--test-file "project-g-util.elc"))))
    (eldev--test-run nil ("--compiled-on-demand" "eval" `(byte-code-function-p (symbol-function 'project-g-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should     (file-exists-p (eldev--test-file "project-g.elc")))
      (should     (file-exists-p (eldev--test-file "project-g-util.elc"))))))

(ert-deftest eldev-loading-modes-3 ()
  (let ((eldev--test-project "project-j"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-j.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-advanced.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-autoloads.elc"))))
    (eldev--test-run nil ("--byte-compiled" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should     (file-exists-p (eldev--test-file "project-j.elc")))
      (should     (file-exists-p (eldev--test-file "project-j-advanced.elc")))
      ;; Autoload file must still not be compiled.
      (should-not (file-exists-p (eldev--test-file "project-j-autoloads.elc"))))
    (eldev--test-run nil ("--source" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should     (string= stdout (eldev--test-lines "nil")))
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-j.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-advanced.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-autoloads.elc"))))
    ;; Previously would fail because of a bug triggered by combination of `autoloads'
    ;; plugin and build system.
    (eldev--test-run nil ("--packaged" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-j.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-advanced.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-autoloads.elc"))))
    (eldev--test-run nil ("--compiled-on-demand" "eval" `(byte-code-function-p (symbol-function 'project-j-hello)))
      (should     (string= stdout (eldev--test-lines "t")))
      (should     (= exit-code 0))
      (should     (file-exists-p (eldev--test-file "project-j.elc")))
      ;; Since `project-j' doesn't require `project-j-advanced', this file must not get compiled.
      (should-not (file-exists-p (eldev--test-file "project-j-advanced.elc")))
      (should-not (file-exists-p (eldev--test-file "project-j-autoloads.elc"))))))

(eldev-ert-defargtest eldev-loading-modes-warnings (mode)
                      ('byte-compiled 'built-and-compiled 'compiled-on-demand 'packaged)
  ;; This project uses `makeinfo'.  Maybe use another?
  (skip-unless (or (not (eq mode 'packaged)) (eldev-makeinfo-executable t)))
  (let ((eldev--test-project "project-b"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean")
      (should     (= exit-code 0))
      (should-not (file-exists-p (eldev--test-file "project-b.elc"))))
    ;; Run just the passing test.
    (eldev--test-run nil ((format "--loading=%s" mode) "test" "hello")
      ;; Compilation warnings must appear on Eldev's stderr, but otherwise the command and
      ;; project testing must succeed.
      (unless (<= emacs-major-version 25)
        ;; Emacs 24 and 25 apparently don't issue warnings on activating packages.  Don't
        ;; care, just avoid test failure.
        (should   (string-match-p "project-b-never-declared-this-variable" stderr))
        (should   (string-match-p "noprefixforthisvar"                     stderr)))
      ;; Also, we don't want actual output here, only stderr.
      (should-not (string-match-p "ELC"                                    stdout))
      (should     (= exit-code 0))
      (should     (eq (file-exists-p "project-b.elc") (not (eq mode 'packaged)))))))

(eldev-ert-defargtest eldev-loading-modes-recompiling (mode)
                      ('byte-compiled 'compiled-on-demand 'packaged)
  (eldev--test-with-temp-copy "project-d" nil
    (eldev--test-run nil ((format "--loading=%s" mode) "eval" `(project-d-custom))
      (should (string= stdout "1\n"))
      (should (= exit-code 0)))
    ;; Change return value of `project-d-defun'.
    (eldev--test-with-file-buffer nil "project-d-util.el"
      (re-search-forward (rx ",@body " (group "1")))
      (replace-match "2" t t nil 1))
    ;; FIXME: This currently depends on Eldev knowing target dependencies, which are found
    ;;        as side-effect of the first `eval'.  Is there a way to make it work even
    ;;        with target dependencies unknown?
    (eldev--test-run nil ((format "--loading=%s" mode) "eval" `(project-d-custom))
      (should (string= stdout "2\n"))
      (should (= exit-code 0)))))


(provide 'test/loading-modes)

;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-just-run-1 ()
  (eldev--test-run "empty-project" ()
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
    (should (= exit-code 0))))

;; This should work even in broken projects.
(ert-deftest eldev-just-run-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ()
      (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
      (should (= exit-code 0)))))

(ert-deftest eldev-bootstrapping-1 ()
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (let ((eldev--test-project     "trivial-project")
        (eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
        (eldev--test-eldev-dir   (eldev--test-tmp-subdir "bootstrap-root")))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(byte-code-function-p (symbol-function 'eldev-cli)))
      (should (string= stdout "t\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-bootstrapping-2 ()
  (when (< emacs-major-version 25)
    ;; But apparently there are no consequences, so let's ignore that.
    (ert-skip "This test fails on ancient Emacs versions"))
  (eldev--test-create-eldev-archive "eldev-archive-1")
  (let ((eldev--test-project     "trivial-project")
        (eldev--test-eldev-local (concat ":pa:" (eldev--test-tmp-subdir "eldev-archive-1")))
        (eldev--test-eldev-dir   (eldev--test-tmp-subdir "bootstrap-root")))
    (ignore-errors (delete-directory eldev--test-eldev-dir t))
    ;; Ensure that bootstrapped Eldev is immediately operational and is byte-compiled.
    ;; The first test `...-1' above wouldn't catch it since it issues `version' first.
    ;; However, let's keep the first test anyway.  This test is targeted at issue #76.
    (eldev--test-run nil ("eval" `(byte-code-function-p (symbol-function 'eldev-cli)) `(eldev-xor nil t))
      (should (string= stdout (eldev--test-lines "t" "t")))
      (should (= exit-code 0)))))

(ert-deftest eldev-command-hook-1 ()
  ;; Just make sure that the hook is executed.
  (eldev--test-run "empty-project" ("--setup" `(add-hook 'eldev-help-hook (lambda () (error "fail!"))) "help")
    (should (string-match-p (rx "fail!") stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-setup-first-1 ()
  (eldev--test-run "project-a" ("--setup-first" `(progn (when package-archives (error "not first!")) (setf i-was-executed t))
                                "--setup" `(unless package-archives (error "expected some archives!"))
                                "--setup" `(unless i-was-executed (error "--setup-first had no effect!"))
                                "exec" `(ignore))
    (should (string= stdout ""))
    (should (= exit-code 0))))

;; Check that `--setup-first' accepts multiple forms and that ordering is preserved.
(ert-deftest eldev-setup-first-2 ()
  (eldev--test-run "project-a" ("--setup-first" "(when (boundp 'middle-form-executed) (error \"not first!\")) (setf middle-form-executed t) (unless middle-form-executed (error \"not last!\"))"
                                "exec" `(ignore))
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-skip-project-config-1 ()
  (eldev--test-run "project-a" ("--setup-first" `(setf eldev-skip-project-config t)
                                "--setup" `(when package-archives (error "fail!"))
                                "exec" "--dont-load" `(ignore))
    (should (string= stdout ""))
    (should (= exit-code 0))))


(ert-deftest eldev-unreadable-locks-1 ()
  ;; Test that Eldev doesn't faint just because something in Emacs is
  ;; not saved.  Note that the error is not always reproducible: it
  ;; depends on unspecified order of result of `directory-files'
  ;; function.
  (let* ((eldev--test-project "project-a")
         (el-file             (expand-file-name "project-a.el" (eldev--test-project-dir))))
    (with-temp-buffer
      (insert-file-contents el-file t)
      (set-visited-file-name el-file t)
      (should (file-locked-p el-file))
      (unwind-protect
          (eldev--test-run nil ("eval" "1")
            (should (string= stdout "1\n"))
            (should (= exit-code 0)))
        (restore-buffer-modified-p nil)
        (should (not (file-locked-p el-file)))))))


(defmacro eldev--test-no-littering (command-line &rest body)
  (declare (indent 1))
  `(let ((eldev--test-project "empty-project"))
     (eldev--test-delete-cache)
     (eldev--test-run nil ,command-line
       ,@body
       (should (not (file-exists-p (eldev--test-project-cache-dir)))))))


;; Make sure that `.eldev' is not created when not really needed.

(ert-deftest eldev-no-littering-1 ()
  (eldev--test-no-littering ()
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-capture-output (eldev-usage))) stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-no-littering-2 ()
  (eldev--test-no-littering ("help")
    (should (string-prefix-p (eldev--test-in-project-environment (eldev--test-first-line (eldev--test-capture-output (eldev-help))))
                             stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-no-littering-3 ()
  (eldev--test-no-littering ("--quiet" "version" "eldev" "emacs")
    (should (string= stdout (eldev--test-lines (eldev-message-version (eldev-find-package-descriptor 'eldev)) emacs-version)))
    (should (= exit-code 0))))

(ert-deftest eldev-no-littering-4 ()
  (eldev--test-no-littering ("info")
    (should (= exit-code 1))))

(ert-deftest eldev-no-littering-5 ()
  ;; This should obviously fail in a non-project directory.
  (eldev--test-no-littering ("compile")
    (should (= exit-code 1))))


(ert-deftest eldev-timestamp-output-1 ()
  ;; Apparently `\n' in command line is not passed through correctly on Windows (seems
  ;; unrelated to `\r\n' as EOL sequence).  I don't know if it breaks in `make-process' or
  ;; in OS itself, but according to some investigations not in Eldev itself.  Oh well,
  ;; let's rewrite the test.
  (eldev--test-run "trivial-project" ("--time" "exec" `(eldev-print :nolf "abc") `(let ((lf (make-string 1 10))) (eldev-print :nolf (concat "de" lf "fg" lf "hij"))) `(eldev-print "!"))
    ;; "[" (= 9 anything) "]" matches timestamp.  There should be no timestamp on the last
    ;; line, since it is started together with line 2.
    (should (string-match-p (rx bos "[" (= 9 anything) "]  abcde\n" "[" (= 9 anything) "]  fg\n" "             hij!\n" eos) stdout))
    (should (= exit-code 0))))


(ert-deftest eldev-unknown-command ()
  (eldev--test-run "empty-project" ("there-is-no-such-command")
    (should (string-match "there-is-no-such-command"   stderr))
    (should (string-match "list of supported commands" stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-unknown-global-option ()
  (eldev--test-run "empty-project" ("--there-is-no-such-global-option" "info")
    (should (string-match "there-is-no-such-global-option" stderr))
    (should (string-match "eldev help"                     stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-unknown-command-option ()
  (eldev--test-run "empty-project" ("info" "--there-is-no-such-command-option")
    (should (string-match "there-is-no-such-command-option" stderr))
    (should (string-match "eldev help info"                 stderr))
    (should (= exit-code 1))))


(provide 'test/basics)

;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-exec-1 ()
  (eldev--test-run "trivial-project" ("exec" `1)
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-2 ()
  (eldev--test-run "trivial-project" ("exec" `(princ 1))
    (should (string= stdout "1"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-from-file-1 ()
  (let ((file (make-temp-file "eval-" nil ".el")))
    (with-temp-file file
      (insert "(defun add-numbers (&rest x) (apply '+ x)) ;; comments should be ignored\n(princ (add-numbers 1 2 3 4))\n"))
    (eldev--test-run "trivial-project" ("exec" "--file" file)
      (should (string= stdout "10"))
      (should (= exit-code 0)))))

(ert-deftest eldev-multiexec-1 ()
  (eldev--test-run "trivial-project" ("exec" `1 `(+ 2 3) `(cons 1 2))
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-multiexec-2 ()
  (eldev--test-run "trivial-project" ("exec" `(princ 1) `(+ 2 3) `(princ (cons 1 2)))
    (should (string= stdout "1(1 . 2)"))
    (should (= exit-code 0))))

(ert-deftest eldev-multiexec-3 ()
  ;; Since 0.11 we also support multiple forms in one argument, for convenience.
  (eldev--test-run "trivial-project" ("exec" "(princ 1) (+ 2 3) (princ (cons 1 2))")
    (should (string= stdout "1(1 . 2)"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-project-function-1 ()
  (eldev--test-run "trivial-project" ("exec" `(progn (require 'trivial-project) (princ (trivial-project-hello))))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-autorequire-feature-1 ()
  ;; (require 'trivial-project) from the previous test shouldn't actually be needed.
  (eldev--test-run "trivial-project" ("exec" `(princ (trivial-project-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-autorequire-feature-2 ()
  (eldev--test-run "project-a" ("--quiet" "exec" `(princ (project-a-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-autorequire-feature-3 ()
  (eldev--test-run "project-b" ("--quiet" "exec" `(princ (project-b-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-autorequire-feature-4 ()
  (eldev--test-run "project-c" ("--quiet" "exec" `(princ (project-c-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-autorequire-feature-5 ()
  ;; Important to test as the "project" involves some macro magic.
  (eldev--test-run "project-e" ("--quiet" "exec" `(princ (project-e-hello)))
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-autorequire-feature-disabled-1 ()
  ;; Unless autorequiring is disabled explicitly.
  (eldev--test-run "trivial-project" ("exec" "--dont-require" `(princ (trivial-project-hello)))
    (should (= exit-code 1))))

;; Make sure lexical evaluation is the default.
(ert-deftest eldev-exec-lexical-binding-1 ()
  (eldev--test-run "trivial-project" ("exec" `(let ((y (lambda () (princ (if (boundp 'x) 'dynamic 'lexical))))) ((lambda (x) (funcall y)) t)))
    (should (string= stdout "lexical"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-lexical-binding-2 ()
  (eldev--test-run "trivial-project" ("exec" "--lexical" `(let ((y (lambda () (princ (if (boundp 'x) 'dynamic 'lexical))))) ((lambda (x) (funcall y)) t)))
    (should (string= stdout "lexical"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-dynamic-binding-1 ()
  (eldev--test-run "trivial-project" ("exec" "--dynamic" `(let ((y (lambda () (princ (if (boundp 'x) 'dynamic 'lexical))))) ((lambda (x) (funcall y)) t)))
    (should (string= stdout "dynamic"))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("exec" `(princ 1))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(ert-deftest eldev-exec-magic-variables-1 ()
  (eldev--test-run "trivial-project" ("exec" `(require 'pcase) `(message "%s" @))
    (should (string= stderr "pcase\n"))
    (should (= exit-code 0))))


;; The following tests are for debugging output, not actually for command `exec' directly.
;; Usually the output is supposed to come from nested emacs invocation (see one test in
;; `emacs.el'), but it also works from evaluated expression.

(ert-deftest eldev-exec-nested-debugging-output-1 ()
  (eldev--test-run "project-a" ( "exec" `(progn (eldev-debug "Before")
                                                (eldev-nest-debugging-output
                                                  (eldev-debug "%s" "Nested"))
                                                (eldev-debug "After")))
    (should (string= stderr (eldev--test-lines "Before" "  Nested" "After")))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-optional-debugging-output-1 ()
  (eldev--test-run "project-a" ("exec" `(eldev-xdebug "must be disabled by default"))
    (should (string= stderr ""))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-exec-optional-debugging-output-2 (switching-form enabled &optional enabled-initially)
                      (('(eldev-enabling-xdebug)            t)
                       ('(eldev-disabling-xdebug)           nil)
                       ('(eldev-maybe-xdebug t)             t)
                       ('(eldev-maybe-xdebug nil)           nil)
                       ('(eldev-maybe-enabling-xdebug  nil) nil)
                       ('(eldev-maybe-enabling-xdebug  t)   t)
                       ('(eldev-maybe-enabling-xdebug  nil) t   t)
                       ('(eldev-maybe-enabling-xdebug  t)   t   t)
                       ('(eldev-maybe-disabling-xdebug nil) nil)
                       ('(eldev-maybe-disabling-xdebug t)   nil)
                       ('(eldev-maybe-disabling-xdebug nil) t   t)
                       ('(eldev-maybe-disabling-xdebug t)   nil t))
  (eldev--test-run "project-a" ((if enabled-initially "--xdebug" "--no-xdebug") "exec" `(,@switching-form (eldev-xdebug "optional")))
    (should (string= stderr (if enabled "optional\n" "")))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-exec-debugging-dump-1 (form output)
                      (('(+ 1 2)  "(+ 1 2) = 3")
                       ("string"  "string")
                       (''symbol  "'symbol")
                       (100       "100"))
  (eldev--test-run "project-a" ("exec" `(eldev-dump ,form))
    (should (string= stderr (format "%s\n" output)))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-exec-debugging-time-it-1 (format-string output)
                      ((""             ": 0.00 s")
                       ("spent"        "spent: 0.00 s")
                       ("spent: %.0fs" "spent: 0s"))
  (eldev--test-run "project-a" ("exec" `(eldev-time-it ,format-string (ignore)))
    (should (string= stderr (format "%s\n" output)))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-debugging-time-it-2 ()
  (eldev--test-run "project-a" ("exec" `(eldev-time-it "%.1f" (sleep-for 1)))
    ;; Just be extra-generous for CI machines.  Has been 1.2 at least once.
    (should (<= 0.9 (eldev-parse-number (string-trim stderr) :floating-point t) 2.0))
    (should (= exit-code 0))))

(ert-deftest eldev-exec-debugging-time-it-3 ()
  ;; Would fail with non-literal format string argument.
  (eldev--test-run "project-a" ("exec" `(eldev-time-it (concat "%.1f") (sleep-for 1)))
    (should (<= 0.9 (eldev-parse-number (string-trim stderr) :floating-point t) 2.0))
    (should (= exit-code 0))))


(provide 'test/exec)

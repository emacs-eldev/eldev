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


(provide 'test/exec)

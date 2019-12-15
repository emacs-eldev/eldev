(require 'test/common)


(ert-deftest emake-test-exec-1 ()
  (emake--test-run "trivial-project" ("exec" "1")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-2 ()
  (emake--test-run "trivial-project" ("exec" "(princ 1)")
    (should (string= stdout "1"))
    (should (= exit-code 0))))

(ert-deftest emake-test-multiexec-1 ()
  (emake--test-run "trivial-project" ("exec" "1" "(+ 2 3)" "(cons 1 2)")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest emake-test-multiexec-2 ()
  (emake--test-run "trivial-project" ("exec" "(princ 1)" "(+ 2 3)" "(princ (cons 1 2))")
    (should (string= stdout "1(1 . 2)"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-project-function-1 ()
  (emake--test-run "trivial-project" ("exec" "(progn (require 'trivial-project) (princ (trivial-project-hello)))")
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-autorequire-feature-1 ()
  ;; (require 'trivial-project) from the previous test shouldn't actually be needed.
  (emake--test-run "trivial-project" ("exec" "(princ (trivial-project-hello))")
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-autorequire-feature-2 ()
  (emake--test-run "project-a" ("--quiet" "exec" "(princ (project-a-hello))")
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-autorequire-feature-3 ()
  (emake--test-run "project-b" ("--quiet" "exec" "(princ (project-b-hello))")
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-autorequire-feature-4 ()
  (emake--test-run "project-c" ("--quiet" "exec" "(princ (project-c-hello))")
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-autorequire-feature-5 ()
  ;; Important to test as the "project" involves some macro magic.
  (emake--test-run "project-e" ("--quiet" "exec" "(princ (project-e-hello))")
    (should (string= stdout "Hello"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-autorequire-feature-disabled-1 ()
  ;; Unless autorequiring is disabled explicitly.
  (emake--test-run "trivial-project" ("exec" "--dont-require" "(princ (trivial-project-hello))")
    (should (= exit-code 1))))

;; Make sure lexical evaluation is the default.
(ert-deftest emake-test-exec-lexical-binding-1 ()
  (emake--test-run "trivial-project" ("exec" "(let ((y (lambda () (princ (if (boundp 'x) 'dynamic 'lexical))))) ((lambda (x) (funcall y)) t))")
    (should (string= stdout "lexical"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-lexical-binding-2 ()
  (emake--test-run "trivial-project" ("exec" "--lexical" "(let ((y (lambda () (princ (if (boundp 'x) 'dynamic 'lexical))))) ((lambda (x) (funcall y)) t))")
    (should (string= stdout "lexical"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-dynamic-binding-1 ()
  (emake--test-run "trivial-project" ("exec" "--dynamic" "(let ((y (lambda () (princ (if (boundp 'x) 'dynamic 'lexical))))) ((lambda (x) (funcall y)) t))")
    (should (string= stdout "dynamic"))
    (should (= exit-code 0))))

(ert-deftest emake-test-exec-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("exec" "(princ 1)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(provide 'test/exec)

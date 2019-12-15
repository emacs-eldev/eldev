(require 'test/common)


(ert-deftest emake-test-eval-1 ()
  (emake--test-run "trivial-project" ("eval" "1")
    (should (string= stdout "1\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-multieval-1 ()
  (emake--test-run "trivial-project" ("eval" "1" "(+ 2 3)" "(cons 1 2)")
    (should (string= stdout "1\n5\n(1 . 2)\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-project-function-1 ()
  (emake--test-run "trivial-project" ("eval" "(progn (require 'trivial-project) (trivial-project-hello))")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-autorequire-feature-1 ()
  ;; (require 'trivial-project) from the previous test shouldn't actually be needed.
  (emake--test-run "trivial-project" ("eval" "(trivial-project-hello)")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-autorequire-feature-2 ()
  (emake--test-run "project-a" ("--quiet" "eval" "(project-a-hello)")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-autorequire-feature-3 ()
  (emake--test-run "project-b" ("--quiet" "eval" "(project-b-hello)")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-autorequire-feature-4 ()
  (emake--test-run "project-c" ("--quiet" "eval" "(project-c-hello)")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-autorequire-feature-5 ()
  ;; Important to test as the "project" involves some macro magic.
  (emake--test-run "project-e" ("--quiet" "eval" "(project-e-hello)")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-autorequire-feature-disabled-1 ()
  ;; Unless autorequiring is disabled explicitly.
  (emake--test-run "trivial-project" ("eval" "--dont-require" "(trivial-project-hello)")
    (should (= exit-code 1))))

;; Make sure lexical evaluation is the default.
(ert-deftest emake-test-eval-lexical-binding-1 ()
  (emake--test-run "trivial-project" ("eval" "(let ((y (lambda () (if (boundp 'x) 'dynamic 'lexical)))) ((lambda (x) (funcall y)) t))")
    (should (string= stdout "lexical\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-lexical-binding-2 ()
  (emake--test-run "trivial-project" ("eval" "--lexical" "(let ((y (lambda () (if (boundp 'x) 'dynamic 'lexical)))) ((lambda (x) (funcall y)) t))")
    (should (string= stdout "lexical\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-dynamic-binding-1 ()
  (emake--test-run "trivial-project" ("eval" "--dynamic" "(let ((y (lambda () (if (boundp 'x) 'dynamic 'lexical)))) ((lambda (x) (funcall y)) t))")
    (should (string= stdout "dynamic\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-eval-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("eval" "1")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(ert-deftest emake-test-eval-extra-dependencies-1 ()
  ;; The project doesn't have such a function, so that this should fail.
  (emake--test-run "project-a" ("eval" "(dependency-b-hello)")
    (should (= exit-code 1)))
  ;; But we can define an extra dependency for `eval' command.
  (emake--test-run "project-a" ("--setup" "(emake-add-extra-dependencies 'eval 'dependency-b)" "--setup" "(setf emake-eval-required-features '(:project-name dependency-b))"
                                "eval" "(dependency-b-hello)")
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))


(provide 'test/eval)

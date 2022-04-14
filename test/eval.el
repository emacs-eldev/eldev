(require 'test/common)


(ert-deftest eldev-eval-1 ()
  (eldev--test-run "trivial-project" ("eval" `1)
    (should (string= stdout "1\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-multieval-1 ()
  (eldev--test-run "trivial-project" ("eval" `1 `(+ 2 3) `(cons 1 2))
    (should (string= stdout "1\n5\n(1 . 2)\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-multieval-2 ()
  ;; Since 0.11 we also support multiple expression in one argument, for convenience.
  (eldev--test-run "trivial-project" ("eval" "1 (+ 2 3) (cons 1 2)")
    (should (string= stdout "1\n5\n(1 . 2)\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-from-file-1 ()
  (let ((file (make-temp-file "eval-" nil ".el")))
    (with-temp-file file
      (insert "(+ 1 2) ;; comments should be ignored\n(+ 3 4)\n"))
    (eldev--test-run "trivial-project" ("eval" "--file" file)
      (should (string= stdout (eldev--test-lines "3" "7")))
      (should (= exit-code 0)))))

(ert-deftest eldev-eval-project-function-1 ()
  (eldev--test-run "trivial-project" ("eval" `(progn (require 'trivial-project) (trivial-project-hello)))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-autorequire-feature-1 ()
  ;; (require 'trivial-project) from the previous test shouldn't actually be needed.
  (eldev--test-run "trivial-project" ("eval" `(trivial-project-hello))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-autorequire-feature-2 ()
  (eldev--test-run "project-a" ("--quiet" "eval" `(project-a-hello))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-autorequire-feature-3 ()
  (eldev--test-run "project-b" ("--quiet" "eval" `(project-b-hello))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-autorequire-feature-4 ()
  (eldev--test-run "project-c" ("--quiet" "eval" `(project-c-hello))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-autorequire-feature-5 ()
  ;; Important to test as the "project" involves some macro magic.
  (eldev--test-run "project-e" ("--quiet" "eval" `(project-e-hello))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-autorequire-feature-disabled-1 ()
  ;; Unless autorequiring is disabled explicitly.
  (eldev--test-run "trivial-project" ("eval" "--dont-require" `(trivial-project-hello))
    (should (= exit-code 1))))


(eldev-ert-defargtest eldev-eval-compilation-1 (compile-first)
                      (nil t)
  (eldev--test-run "trivial-project" ("eval" (if compile-first "--compile" "--as-is")
                                      `(defun test-function (x) (+ x x)) `(byte-code-function-p (symbol-function 'test-function)))
    (should (equal (eldev--test-line-list stdout) `("test-function" ,(if compile-first "t" "nil"))))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-eval-macroexpansion-1 (macroexpand-first)
                      (nil t)
  (eldev--test-run "trivial-project" ("eval" (if macroexpand-first "--macroexpand" "--as-is")
                                      `(defun test-function (x) (pop x)) `(null (string-match-p (rx bow "pop" eow) (prin1-to-string (symbol-function 'test-function)))))
    (should (equal (eldev--test-line-list stdout) `("test-function" ,(if macroexpand-first "t" "nil"))))
    (should (= exit-code 0))))


;; Make sure lexical evaluation is the default.
(eldev-ert-defargtest eldev-eval-lexical-binding-1 (compile-first)
                      (nil t)
  (eldev--test-run "trivial-project" ("eval" (if compile-first "--compile" "--as-is")
                                      `(let ((y (lambda () (if (boundp 'x) 'dynamic 'lexical)))) ((lambda (x) (funcall y)) t)))
    (should (string= stdout "lexical\n"))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-eval-lexical-binding-2 (compile-first)
                      (nil t)
  (eldev--test-run "trivial-project" ("eval" "--lexical" (if compile-first "--compile" "--as-is")
                                      `(let ((y (lambda () (if (boundp 'x) 'dynamic 'lexical)))) ((lambda (x) (funcall y)) t)))
    (should (string= stdout "lexical\n"))
    (should (= exit-code 0))))

(eldev-ert-defargtest eldev-eval-dynamic-binding-1 (compile-first)
                      (nil t)
  (eldev--test-run "trivial-project" ("eval" "--dynamic" (if compile-first "--compile" "--as-is")
                                      `(let ((y (lambda () (if (boundp 'x) 'dynamic 'lexical)))) ((lambda (x) (funcall y)) t)))
    (should (string= stdout "dynamic\n"))
    (should (= exit-code 0))))


(eldev-ert-defargtest eldev-eval-repeat-1 (n)
                      (1 10 1000)
  (eldev--test-run "trivial-project" ("eval" "--repeat" n
                                      `(defvar test-counter 0) `(setf test-counter (1+ test-counter)) `test-counter)
    (should (string= stdout (eldev--test-lines "test-counter" (number-to-string n) (number-to-string n))))
    (should (= exit-code 0))))


(ert-deftest eldev-eval-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("eval" `1)
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(ert-deftest eldev-eval-extra-dependencies-1 ()
  ;; The project doesn't have such a function, so that this should fail.
  (eldev--test-run "project-a" ("eval" `(dependency-b-hello))
    (should (= exit-code 1)))
  ;; But we can define an extra dependency for `eval' command.
  (eldev--test-run "project-a" ("--setup" `(eldev-add-extra-dependencies 'eval 'dependency-b) "--setup" `(setf eldev-eval-required-features '(:project-name dependency-b))
                                "eval" `(dependency-b-hello))
    (should (string= stdout "\"Hello\"\n"))
    (should (= exit-code 0))))


(ert-deftest eldev-eval-magic-variables-1 ()
  (eldev--test-run "trivial-project" ("eval" `(+ 1 2) `(* @ @))
    (should (string= stdout (eldev--test-lines "3" "9")))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-magic-variables-2 ()
  ;; May not be referenced in the first form.
  (eldev--test-run "trivial-project" ("eval" `@)
    (should (string-match-p "value as variable is void: @" stderr))
    (should (= exit-code 1))))

(ert-deftest eldev-eval-magic-variables-3 ()
  (eldev--test-run "trivial-project" ("eval" `(+ 1 2) `(+ 3 4) `(* @1 @2))
    (should (string= stdout (eldev--test-lines "3" "7" "21")))
    (should (= exit-code 0))))

(ert-deftest eldev-eval-magic-variables-4 ()
  ;; Referencing to a yet-unknown result variable.
  (eldev--test-run "trivial-project" ("eval" `(+ 1 2) `@2)
    (should (string-match-p "value as variable is void: @2" stderr))
    (should (= exit-code 1))))


(provide 'test/eval)

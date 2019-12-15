(require 'test/common)


(ert-deftest emake-test-local-dependencies-1 ()
  (let ((emake--test-project "project-a"))
    (emake--test-delete-cache)
    ;; Run all commands in the same test to make sure that the various
    ;; setups don't influence each other in some way.
    (emake--test-run nil ("--quiet" "eval" "(dependency-a-stable)" "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "t" "(1 0)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\")"
                          "eval" "(dependency-a-stable)" "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "(dependency-a-stable)" "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "t" "(1 0)")))
      (should (= exit-code 0)))))

(ert-deftest emake-test-local-dependencies-2 ()
  (let ((emake--test-project "project-b"))
    (emake--test-delete-cache)
    ;; Similar to the previous test.  Now we have two dependencies, A
    ;; and B, so test all four cases and run the "both-nonlocal" at
    ;; the end again, to ensure lack of influence from other setup
    ;; variants.
    (emake--test-run nil ("--quiet" "eval" "(dependency-a-stable)" "(dependency-b-stable)"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))" "(package-desc-version (emake-find-package-descriptor 'dependency-b))")
      (should (string= stdout (emake--test-lines "t" "t" "(1 0)" "(1 0)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\")"
                          "eval" "(dependency-a-stable)" "(dependency-b-stable)"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))" "(package-desc-version (emake-find-package-descriptor 'dependency-b))")
      (should (string= stdout (emake--test-lines "nil" "t" "(1 0 99)" "(1 0)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-b\")"
                          "eval" "(dependency-a-stable)" "(dependency-b-stable)"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))" "(package-desc-version (emake-find-package-descriptor 'dependency-b))")
      (should (string= stdout (emake--test-lines "t" "nil" "(1 0)" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\")" "--setup" "(emake-use-local-dependency \"../dependency-b\")"
                          "eval" "(dependency-a-stable)" "(dependency-b-stable)"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))" "(package-desc-version (emake-find-package-descriptor 'dependency-b))")
      (should (string= stdout (emake--test-lines "nil" "nil" "(1 0 99)" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("--quiet" "eval" "(dependency-a-stable)" "(dependency-b-stable)"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))" "(package-desc-version (emake-find-package-descriptor 'dependency-b))")
      (should (string= stdout (emake--test-lines "t" "t" "(1 0)" "(1 0)")))
      (should (= exit-code 0)))))


(ert-deftest emake-test-local-dependency-fixes-missing-dependency-1 ()
  (emake--test-run "dependency-a" ("clean")
    (should (= exit-code 0)))
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `as-is' (default) loading mode `byte-code-function-p's
    ;; result should depend on the state of the dependency.
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\")"
                          "eval" "(dependency-a-stable)" "(byte-code-function-p (symbol-function 'dependency-a-stable))"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "nil" "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run "dependency-a" ("compile")
      (should (= exit-code 0)))
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\")"
                          "eval" "(dependency-a-stable)" "(byte-code-function-p (symbol-function 'dependency-a-stable))"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "nil" "t" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

(ert-deftest emake-test-local-dependency-fixes-missing-dependency-2 ()
  (emake--test-run "dependency-a" ("clean")
    (should (= exit-code 0)))
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `byte-compiled' loading mode `byte-code-function-p' must
    ;; always return t, i.e. dependency must be compiled implicitly.
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\" 'byte-compiled)"
                          "eval" "(dependency-a-stable)" "(byte-code-function-p (symbol-function 'dependency-a-stable))"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "nil" "t" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

(ert-deftest emake-test-local-dependency-fixes-missing-dependency-3 ()
  (emake--test-run "dependency-a" ("compile")
    (should (= exit-code 0)))
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `source' loading mode `byte-code-function-p' must always
    ;; return nil, i.e. dependency must be cleaned even if compiled
    ;; before.
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\" 'source)"
                          "eval" "(dependency-a-stable)" "(byte-code-function-p (symbol-function 'dependency-a-stable))"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "nil" "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

(ert-deftest emake-test-local-dependency-fixes-missing-dependency-4 ()
  (emake--test-run "dependency-a" ("clean")
    (should (= exit-code 0)))
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `packaged' loading mode `byte-code-function-p' must always
    ;; return t, i.e. dependency must be compiled when installed as a
    ;; package.  FIXME: Probably could use a better check.
    (emake--test-run nil ("--setup" "(emake-use-local-dependency \"../dependency-a\" 'packaged)"
                          "eval" "(dependency-a-stable)" "(byte-code-function-p (symbol-function 'dependency-a-stable))"
                          "(package-desc-version (emake-find-package-descriptor 'dependency-a))")
      (should (string= stdout (emake--test-lines "nil" "t" "(1 0 99)")))
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "(dependency-a-stable)")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(provide 'test/local-dependencies)

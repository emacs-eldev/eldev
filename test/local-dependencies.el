;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-local-dependencies-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    ;; Run all commands in the same test to make sure that the various
    ;; setups don't influence each other in some way.
    (eldev--test-run nil ("--quiet" "eval" `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "t" "(1 0)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a")
                          "eval" `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "t" "(1 0)")))
      (should (= exit-code 0)))))

(ert-deftest eldev-local-dependencies-2 ()
  (let ((eldev--test-project "project-b"))
    (eldev--test-delete-cache)
    ;; Similar to the previous test.  Now we have two dependencies, A
    ;; and B, so test all four cases and run the "both-nonlocal" at
    ;; the end again, to ensure lack of influence from other setup
    ;; variants.
    (eldev--test-run nil ("--quiet" "eval" `(dependency-a-stable) `(dependency-b-stable)
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)) `(package-desc-version (eldev-find-package-descriptor 'dependency-b)))
      (should (string= stdout (eldev--test-lines "t" "t" "(1 0)" "(1 0)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a")
                          "eval" `(dependency-a-stable) `(dependency-b-stable)
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)) `(package-desc-version (eldev-find-package-descriptor 'dependency-b)))
      (should (string= stdout (eldev--test-lines "nil" "t" "(1 0 99)" "(1 0)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-b")
                          "eval" `(dependency-a-stable) `(dependency-b-stable)
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)) `(package-desc-version (eldev-find-package-descriptor 'dependency-b)))
      (should (string= stdout (eldev--test-lines "t" "nil" "(1 0)" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a") "--setup" `(eldev-use-local-dependency "../dependency-b")
                          "eval" `(dependency-a-stable) `(dependency-b-stable)
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)) `(package-desc-version (eldev-find-package-descriptor 'dependency-b)))
      (should (string= stdout (eldev--test-lines "nil" "nil" "(1 0 99)" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--quiet" "eval" `(dependency-a-stable) `(dependency-b-stable)
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)) `(package-desc-version (eldev-find-package-descriptor 'dependency-b)))
      (should (string= stdout (eldev--test-lines "t" "t" "(1 0)" "(1 0)")))
      (should (= exit-code 0)))))


(ert-deftest eldev-local-dependency-fixes-missing-dependency-1 ()
  (eldev--test-run "dependency-a" ("clean")
    (should (= exit-code 0)))
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `as-is' (default) loading mode `byte-code-function-p's
    ;; result should depend on the state of the dependency.
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a")
                          "eval" `(dependency-a-stable) `(byte-code-function-p (symbol-function 'dependency-a-stable))
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run "dependency-a" ("compile")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a")
                          "eval" `(dependency-a-stable) `(byte-code-function-p (symbol-function 'dependency-a-stable))
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "t" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

(ert-deftest eldev-local-dependency-fixes-missing-dependency-2 ()
  (eldev--test-run "dependency-a" ("clean")
    (should (= exit-code 0)))
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `byte-compiled' loading mode `byte-code-function-p' must
    ;; always return t, i.e. dependency must be compiled implicitly.
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a" 'byte-compiled)
                          "eval" `(dependency-a-stable) `(byte-code-function-p (symbol-function 'dependency-a-stable))
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "t" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

(ert-deftest eldev-local-dependency-fixes-missing-dependency-3 ()
  (eldev--test-run "dependency-a" ("compile")
    (should (= exit-code 0)))
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `source' loading mode `byte-code-function-p' must always
    ;; return nil, i.e. dependency must be cleaned even if compiled
    ;; before.
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a" 'source)
                          "eval" `(dependency-a-stable) `(byte-code-function-p (symbol-function 'dependency-a-stable))
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))

(ert-deftest eldev-local-dependency-fixes-missing-dependency-4 ()
  (eldev--test-run "dependency-a" ("clean")
    (should (= exit-code 0)))
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))
    ;; In `packaged' loading mode `byte-code-function-p' must always
    ;; return t, i.e. dependency must be compiled when installed as a
    ;; package.  FIXME: Probably could use a better check.
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a" 'packaged)
                          "eval" `(dependency-a-stable) `(byte-code-function-p (symbol-function 'dependency-a-stable))
                          `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "t" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `(dependency-a-stable))
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


;; Make sure that local dependencies are still honored even when an external directory is
;; specified.
(ert-deftest eldev-local-dependencies-overwrite-external-dir-1 ()
  (eldev--test-with-external-dir "project-a" 'dependency-a
    (eldev--test-delete-cache)
    (eldev--test-run nil ((format "--external=%s" external-dir)
                          "eval" `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "t" "(1 0)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-local-dependency "../dependency-a") (format "--external=%s" external-dir)
                          "eval" `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "nil" "(1 0 99)")))
      (should (= exit-code 0)))
    (eldev--test-run nil ((format "--external=%s" external-dir)
                          "eval" `(dependency-a-stable) `(package-desc-version (eldev-find-package-descriptor 'dependency-a)))
      (should (string= stdout (eldev--test-lines "t" "(1 0)")))
      (should (= exit-code 0)))))


(provide 'test/local-dependencies)

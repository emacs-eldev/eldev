(require 'test/common)


(ert-deftest emake-test-info-1 ()
  (emake--test-run "trivial-project" ("info")
    (should (string= stdout "trivial-project 1.0\n\nTrivial test project\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-info-missing-dependency-1 ()
  (emake--test-run "missing-dependency-a" ("info")
    (should (string= stdout "missing-dependency-a 1.0\n\nTest project with an unavailable dependency\n"))
    (should (= exit-code 0))))


(provide 'test/info)

(require 'test/common)


(ert-deftest eldev-info-1 ()
  (eldev--test-run "trivial-project" ("info")
    (should (string= stdout "trivial-project 1.0\n\nTrivial test project\n"))
    (should (= exit-code 0))))

(ert-deftest eldev-info-missing-dependency-1 ()
  (eldev--test-run "missing-dependency-a" ("info")
    (should (string= stdout "missing-dependency-a 1.0\n\nComments to make linters happy.\n"))
    (should (= exit-code 0))))


(provide 'test/info)

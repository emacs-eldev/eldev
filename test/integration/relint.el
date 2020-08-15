(require 'test/common)


(ert-deftest eldev-relint-project-a-1 ()
  (eldev--test-run "project-a" ("lint" "re" "--required")
    (eldev--test-skip-if-missing-linter exit-code stderr)
    (should (string-match-p "no complaints" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-relint-project-b-1 ()
  ;; This project contains a never-called function with an invalid regexp.
  (eldev--test-run "project-b" ("lint" "re" "--required")
    (eldev--test-skip-if-missing-linter exit-code stderr)
    (should (string-match-p "string-match-p" stderr))
    (should (string-match-p "Found 1 warning" stderr))
    (should (= exit-code 1))))


(provide 'test/integration/relint)

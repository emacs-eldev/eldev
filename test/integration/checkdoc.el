(require 'test/common)


(ert-deftest eldev-checkdoc-project-a-1 ()
  (eldev--test-run "project-a" ("lint" "doc" "--required")
    (should (= exit-code 0))))

(ert-deftest eldev-checkdoc-project-b-1 ()
  ;; E.g. there are undocumented functions.
  (eldev--test-run "project-b" ("lint" "doc" "--required")
    (should (= exit-code 1))))


(provide 'test/integration/checkdoc)

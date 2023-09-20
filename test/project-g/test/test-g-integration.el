(require 'test-g-util)

(ert-deftest project-g-integration-1 ()
  ;; Basically just as `project-g-1'; important is that it also passes.
  (should (equal (project-g-hello) (project-g-test-util-hello))))

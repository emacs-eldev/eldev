(require 'test-g-util)

(ert-deftest project-g-1 ()
  (should (equal (project-g-hello) (project-g-test-util-hello))))

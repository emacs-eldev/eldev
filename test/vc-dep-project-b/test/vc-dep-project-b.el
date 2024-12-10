(require 'vc-dep-project-b)
(require 'ert)

;; All tests pass.

(ert-deftest vc-dep-project-b-test-hello ()
  (should (string= (vc-dep-project-b-hello) "Hello")))

(require 'vc-dep-project-a)
(require 'ert)

;; All tests pass.

(ert-deftest vc-dep-project-a-test-hello ()
  (should (string= (vc-dep-project-a-hello) "Hello")))

(ert-deftest vc-dep-project-a-test-triviality ()
  (should t))

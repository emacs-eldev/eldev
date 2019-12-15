(require 'project-a)
(require 'ert)

;; All tests pass.

(ert-deftest project-a-test-hello ()
  (should (string= (project-a-hello) "Hello")))

(ert-deftest project-a-test-triviality ()
  (should t))

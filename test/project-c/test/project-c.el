(require 'project-c)
(require 'ert)

;; All tests pass.

(ert-deftest project-c-test-hello ()
  (should (string= (project-c-hello) "Hello")))

(ert-deftest project-c-test-triviality ()
  (should t))

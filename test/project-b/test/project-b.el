(require 'project-b)
(require 'ert)

;; One test fails.

(ert-deftest project-b-test-hello ()
  (should (string= (project-b-hello) "Hello")))

(ert-deftest project-b-test-triviality-failing ()
  (should nil))

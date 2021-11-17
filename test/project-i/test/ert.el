(require 'project-i)
(require 'project-i-advanced)
(require 'ert)

;; All tests pass.

(ert-deftest project-i-test-hello ()
  (should (string= (project-i-hello) "Hello")))

(ert-deftest project-i-test-hello-advanced ()
  (should (string= (project-i-hello-to "world") "Hello, world!")))

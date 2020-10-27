(require 'project-c)
(require 'ert)

;; Doesn't contain anything, needed for `eldev-test-utility-files-in-package-mode-1'.
(require 'test/dummy)

;; All tests pass.

(ert-deftest project-c-test-hello ()
  (should (string= (project-c-hello) "Hello")))

(ert-deftest project-c-test-triviality ()
  (should t))

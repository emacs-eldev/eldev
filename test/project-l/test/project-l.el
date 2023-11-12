(require 'project-l)
(require 'ert)

;; Doesn't contain anything, needed for `eldev-test-utility-files-in-package-mode-2'.
(require 'test/dummy)

;; All tests pass.

(ert-deftest project-l-test-hello ()
  (should (string= (project-l-hello) "Hello")))

(ert-deftest project-l-resource-contents ()
  (should (string= (project-l-simple-resource) "I'm very simple.\n")))

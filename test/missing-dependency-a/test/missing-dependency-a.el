(require 'missing-dependency-a)
(require 'ert)

;; All tests pass.  However, this can be loaded only if `dependency-a'
;; is made available.

(ert-deftest missing-dependency-a-test-hello ()
  (should (string= (project-a-hello) "Hello")))

(ert-deftest missing-dependency-a-triviality ()
  (should t))

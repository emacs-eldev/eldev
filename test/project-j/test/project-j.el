;; `dependency-a' is declared as an additional test-only dependency in this project's file `Eldev'.
(require 'dependency-a)

(ert-deftest project-j-test-dependency ()
  (should (string= (dependency-a-hello) "Hello")))

(provide 'test/project-j)

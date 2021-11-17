(require 'project-i)
(require 'project-i-advanced)
(require 'buttercup)

;; All tests pass.

(describe "Project D tests"
  (it "has working `project-i-hello'"
    (expect (project-i-hello) :to-equal "Hello"))

  (it "has working `project-i-hello-to'"
    (expect (project-i-hello-to "world") :to-equal "Hello, world!")))

;;  -*- lexical-binding: t -*-

(require 'project-e)
(require 'buttercup)

;; All tests pass.

(defvar eldev--buttercup-do-fail nil)

(describe "Project E tests"
  (it "has a dummy passing test"
    (expect t :to-be t))

  (it "`eldev--buttercup-do-fail' to be nil"
    ;; Result of this test can be altered from command line.
    (expect eldev--buttercup-do-fail :to-be nil)))

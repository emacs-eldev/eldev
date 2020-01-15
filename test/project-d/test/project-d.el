(require 'project-d)
(require 'ert)

;; All tests fail (more can be added only if they also fail).

(ert-deftest project-d-fails-with-signal ()
  (project-d-this-function-is-not-defined))

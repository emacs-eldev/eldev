(require 'f)

(defvar project-h-support-path
  (f-dirname load-file-name))

(defvar project-h-features-path
  (f-parent project-h-support-path))

(defvar project-h-root-path
  (f-parent project-h-features-path))

(add-to-list 'load-path project-h-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'project-h)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )

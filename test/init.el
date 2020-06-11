(require 'test/common)


(defun eldev--test-replace-known-archives ()
  `(setf eldev--known-package-archives   '((archive-c       ("archive-c" . ,(expand-file-name "test/package-archive-c" eldev-project-dir)) 300)
                                           (archive-a       ("archive-a" . ,(expand-file-name "test/package-archive-a" eldev-project-dir)) 200)
                                           (archive-b       ("archive-b" . ,(expand-file-name "test/package-archive-b" eldev-project-dir)) 100)
                                           (stable-unstable (:stable archive-a :unstable archive-b)))
         eldev--stable/unstable-archives '((,(expand-file-name   "test/package-archive-a" eldev-project-dir)
                                            . ,(expand-file-name "test/package-archive-b" eldev-project-dir)))))


(ert-deftest eldev-init-1 ()
  (eldev--test-without-files "uninitialized-a" "Eldev"
    (eldev--test-run nil ("init" "--non-interactive")
      (should (string= stdout (eldev-format-message "Created file `%s' for this project\n" eldev-file)))
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Uncomment some calls below as needed for your project.
;(eldev-use-package-archive 'gnu)
;(eldev-use-package-archive 'melpa)
"))
      (should (= exit-code 0)))))

(ert-deftest eldev-init-2 ()
  (eldev--test-without-files "uninitialized-a" "Eldev"
    ;; It is fine to "guess archives" on this project, since it has no dependencies.
    (eldev--test-run nil ("init" "--interactive")
      :process-input "y\n"
      (should (string-match-p (eldev-format-message "Created file `%s' for this project" eldev-file) stdout))
      ;; Without dependencies Eldev should create the same file, with
      ;; commented out archive lines for possible later use.
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Uncomment some calls below as needed for your project.
;(eldev-use-package-archive 'gnu)
;(eldev-use-package-archive 'melpa)
"))
      (should (= exit-code 0)))))

(ert-deftest eldev-init-3 ()
  (eldev--test-without-files "uninitialized-b" "Eldev"
    (eldev--test-run nil ("--setup" (eldev--test-replace-known-archives) "init" "--interactive")
      :process-input "y\n"
      (should (string-match-p (eldev-format-message "Created file `%s' for this project" eldev-file) stdout))
      ;; Since `dependency-a' is also available from `archive-c', only this archive should
      ;; be autodetected as the most prioritized one.
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Autodetermined by `eldev init'.
(eldev-use-package-archive 'archive-c)
"))
      (should (= exit-code 0)))))

(ert-deftest eldev-init-4 ()
  (eldev--test-without-files "uninitialized-c" "Eldev"
    (eldev--test-run nil ("--setup" (eldev--test-replace-known-archives) "init" "--interactive")
      :process-input "y\n"
      (should (string-match-p (eldev-format-message "Created file `%s' for this project" eldev-file) stdout))
      ;; `stable-unstable' archive is enough, even if `dependency-a' is available also
      ;; from more prioritized `archive-c'.
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Autodetermined by `eldev init'.
(eldev-use-package-archive 'stable-unstable)
"))
      (should (= exit-code 0)))))

(ert-deftest eldev-init-5 ()
  (eldev--test-without-files "uninitialized-d" "Eldev"
    (eldev--test-run nil ("--setup" (eldev--test-replace-known-archives) "init" "--interactive")
      :process-input "y\n"
      (should (string-match-p (eldev-format-message "Created file `%s' for this project" eldev-file) stdout))
      ;; Both archives are needed.
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Autodetermined by `eldev init'.
(eldev-use-package-archive 'archive-c)
(eldev-use-package-archive 'stable-unstable)
"))
      (should (= exit-code 0)))))


(provide 'test/init)

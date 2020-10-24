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

(ert-deftest eldev-init-6 ()
  ;; This directory doesn't contain an Elisp package file.
  (eldev--test-without-files "uninitialized-e" "Eldev"
    (eldev--test-run nil ("init" "--interactive")
      :process-input "n\n"
      (should (string-match-p "Continue anyway\\?" stdout))
      (should (not (file-exists-p (expand-file-name "Eldev" (eldev--test-project-dir)))))
      (should (= exit-code 1)))))

(ert-deftest eldev-init-7 ()
  ;; However, it should still be possible to initialize it.
  (eldev--test-without-files "uninitialized-e" "Eldev"
    (eldev--test-run nil ("init" "--interactive")
      :process-input "y\n"
      (should (string-match-p "Continue anyway\\?" stdout))
      (should (string-match-p (eldev-format-message "Created file `%s' for this project" eldev-file) stdout))
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Uncomment some calls below as needed for your project.
;(eldev-use-package-archive 'gnu)
;(eldev-use-package-archive 'melpa)
"))
      (should (= exit-code 0)))))

(ert-deftest eldev-init-8 ()
  (eldev--test-without-files "uninitialized-f" "Eldev"
    (eldev--test-run nil ("init" "--non-interactive")
      (should (string= stdout (eldev-format-message "Created file `%s' for this project\n" eldev-file)))
      (should (string= (eldev--test-file-contents nil "Eldev") "\
; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

;; Uncomment some calls below as needed for your project.
;(eldev-use-package-archive 'gnu)
;(eldev-use-package-archive 'melpa)

(eldev-use-plugin 'autoloads)
"))
      (should (= exit-code 0)))))


(ert-deftest eldev-init-git-1 ()
  (eldev--test-with-temp-vc-copy "project-a" 'Git
    (eldev--test-delete-quietly nil "Eldev")
    (eldev--test-run nil ("init" "--non-interactive")
      (should (string= stdout (eldev-format-message "Created file `%s' for this project\nModified file `.gitignore'\n" eldev-file)))
      (should (string= (eldev--test-file-contents nil ".gitignore") (eldev-format-message "\
# Added automatically by `eldev init'.
/.eldev
/Eldev-local
"))))))

(ert-deftest eldev-init-hg-1 ()
  (eldev--test-with-temp-vc-copy "project-a" 'Hg
    (eldev--test-delete-quietly nil "Eldev")
    (eldev--test-run nil ("init" "--non-interactive")
      (should (string= stdout (eldev-format-message "Created file `%s' for this project\nModified file `.hgignore'\n" eldev-file)))
      (should (string= (eldev--test-file-contents nil ".hgignore") (eldev-format-message "\
# Added automatically by `eldev init'.
^\\.eldev$
^Eldev-local$
"))))))


(provide 'test/init)

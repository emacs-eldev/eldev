(require 'test/common)
(require 'eldev-plugins)


(defmacro eldev--test-maintainer-run (test-project command-line &rest body)
  (declare (indent 2) (debug (stringp sexp body)))
  (let (extra-setup
        other-keywords)
    (while (keywordp (car body))
      (eldev-pcase-exhaustive (pop body)
        (:extra-setup (push (pop body) extra-setup))
        (keyword      (push keyword    other-keywords)
                      (push (pop body) other-keywords))))
    (let ((actual-command-line `("--setup" `(eldev-use-plugin 'maintainer)
                                 "--setup" `(setf eldev-release-interactive nil)
                                 ,@(apply #'append (mapcar (lambda (form) `("--setup" ,form)) (nreverse extra-setup))))))
      (setf actual-command-line (pcase command-line
                                  (`(:eval ,expression) `(:eval (append (list ,@actual-command-line) ,expression)))
                                  (_                    (nconc actual-command-line command-line))))
      `(eldev--test-run ,test-project ,actual-command-line
         ,@(nreverse other-keywords)
         ,@body))))


;; The following tests always use run-together syntax, as Emacs 24 won't understand
;; something like "1.beta2".
(ert-deftest eldev-release-next-major-version-1 ()
  (should (equal (eldev-release-next-major-version nil)                             '(1)))
  (should (equal (eldev-release-next-major-version (version-to-list "1"))           '(2)))
  (should (equal (eldev-release-next-major-version (version-to-list "1.0"))         '(2)))
  (should (equal (eldev-release-next-major-version (version-to-list "1.5"))         '(2)))
  (should (equal (eldev-release-next-major-version (version-to-list "1.0.3"))       '(2)))
  (should (equal (eldev-release-next-major-version (version-to-list "1alpha"))      '(1)))
  (should (equal (eldev-release-next-major-version (version-to-list "1beta2"))      '(1)))
  (should (equal (eldev-release-next-major-version (version-to-list "1.2rc4"))      '(2)))
  (should (equal (eldev-release-next-major-version (version-to-list "1.2.3alpha6")) '(2)))
  (should (equal (eldev-release-next-major-version (version-to-list "1snapshot"))   '(1))))

(ert-deftest eldev-release-next-minor-version-1 ()
  (should (equal (eldev-release-next-minor-version nil)                             '(0 1)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1"))           '(1 1)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1.0"))         '(1 1)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1.5"))         '(1 6)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1.0.3"))       '(1 1)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1alpha"))      '(1)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1beta2"))      '(1)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1.2rc4"))      '(1 2)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1.2.3alpha6")) '(1 3)))
  (should (equal (eldev-release-next-minor-version (version-to-list "1snapshot"))   '(1))))

(ert-deftest eldev-release-next-patch-version-1 ()
  (should (equal (eldev-release-next-patch-version nil)                             '(0 0 1)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1"))           '(1 0 1)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1.0"))         '(1 0 1)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1.5"))         '(1 5 1)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1.0.3"))       '(1 0 4)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1alpha"))      '(1)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1beta2"))      '(1)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1.2rc4"))      '(1 2)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1.2.3alpha6")) '(1 2 3)))
  (should (equal (eldev-release-next-patch-version (version-to-list "1snapshot"))   '(1))))

(ert-deftest eldev-release-next-snapshot-version-1 ()
  (let ((eldev-release-min-version-size 1))
    (should (equal (eldev-release-next-snapshot-version nil)                                `(1 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1"))              `(2 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1.3"))            `(1 4 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1.3.5"))          `(1 3 6 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1snapshot"))      `(1 ,eldev--snapshot 2)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1snapshot2"))     `(1 ,eldev--snapshot 3)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1.3snapshot"))    `(1 3 ,eldev--snapshot 2)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1.3snapshot4.8")) `(1 3 ,eldev--snapshot 5))))
  (let ((eldev-release-min-version-size 2))
    (should (equal (eldev-release-next-snapshot-version nil)                                `(0 1 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1"))              `(1 1 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1.3"))            `(1 4 ,eldev--snapshot)))
    (should (equal (eldev-release-next-snapshot-version (version-to-list "1snapshot"))      `(1 ,eldev--snapshot 2)))))


(ert-deftest eldev-release-bad-version-1 ()
  (eldev--test-with-temp-copy "project-a" 'Git
    (eldev--test-maintainer-run nil ("release" "bla-bla-bla" "--dry-run")
      ;; Not checking the exact message, since it comes from Emacs.
      (should (string-match-p "bla-bla-bla" stderr))
      (should (= exit-code 1)))))

(ert-deftest eldev-release-bad-version-2 ()
  (eldev--test-with-temp-copy "project-a" 'Git
    ;; The "project" is already version 1.0.
    (eldev--test-maintainer-run nil ("-d" "release" "0.1" "--dry-run")
      (should (string-match-p "must be newer" stderr))
      (should (= exit-code 1)))
    (eldev--test-maintainer-run nil ("release" "1.0" "--dry-run")
      (should (string-match-p "must be newer" stderr))
      (should (= exit-code 1)))))


(ert-deftest eldev-release-no-vcs-1 ()
  (eldev--test-with-temp-copy "project-a" nil
    (eldev--test-maintainer-run "project-a" ("release" "1.5" "--dry-run")
      (should (string-match-p "Can only create releases in projects maintained by a supported VCS" stderr))
      (should (= exit-code 1)))))

(eldev-ert-defargtest eldev-release-unclean-1 (vc-backend)
                      ('Git 'Hg)
  (eldev--test-with-temp-copy "project-a" vc-backend
    (eldev--test-with-file-buffer nil "Eldev"
      (insert "\n"))
    (eldev--test-maintainer-run nil ("release" "1.5" "--dry-run")
      (should (string-match-p "working directory is not clean" stderr))
      (should (= exit-code 1)))))

(eldev-ert-defargtest eldev-release-unclean-2 (vc-backend)
                      ('Git 'Hg)
  (eldev--test-with-temp-copy "project-a" vc-backend
    (eldev--test-with-file-buffer nil "unknown-random-file")
    (eldev--test-maintainer-run nil ("--setup" `(setf eldev-release-ignore-untracked nil) "release" "1.5" "--dry-run")
      (should (string-match-p "working directory is not clean" stderr))
      (should (= exit-code 1)))))

(eldev-ert-defargtest eldev-release-unclean-3 (vc-backend)
                      ('Git 'Hg)
  (eldev--test-with-temp-copy "project-a" vc-backend
    (eldev--test-with-file-buffer nil "unknown-random-file")
    (eldev--test-maintainer-run nil ("--setup" `(setf eldev-release-ignore-untracked t) "release" "1.5" "--dry-run")
      ;; Should proceed, as unknown files are ignored.
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-release-from-branch-1 (vc-backend)
                      ('Git 'Hg)
  (eldev--test-with-temp-copy "project-a" vc-backend
    (eldev--test-switch-vc-branch vc-backend "foo")
    (eldev--test-maintainer-run nil ("release" "1.5" "--dry-run")
      (should (and (string-match-p "Refusing to release from non-main" stderr) (string-match-p "foo" stderr)))
      (should (= exit-code 1)))))

(eldev-ert-defargtest eldev-release-from-branch-2 (vc-backend)
                      ('Git 'Hg)
  (eldev--test-with-temp-copy "project-a" vc-backend
    (eldev--test-switch-vc-branch vc-backend "foo")
    (eldev--test-maintainer-run nil ("--setup" `(setf eldev-release-allowed-branch nil) "release" "1.5" "--dry-run")
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-release-from-branch-3 (vc-backend)
                      ('Git 'Hg)
  (eldev--test-with-temp-copy "project-a" vc-backend
    (eldev--test-switch-vc-branch vc-backend "foo")
    (eldev--test-maintainer-run nil ("--setup" `(setf eldev-release-allowed-branch '("foo" "branches/foo")) "release" "1.5" "--dry-run")
      (should (= exit-code 0)))))

(ert-deftest eldev-release-donotrelease-marker-1 ()
  (eldev--test-with-temp-copy "project-a" 'Git
    :after-copy (eldev-with-file-buffer "Eldev"
                  (insert ";; DONOT" "RELEASE\n"))
    (eldev--test-maintainer-run nil ("release" "1.5" "--dry-run")
      (should (string-match-p eldev-release-file-check-forbidden-regexp stderr))
      (should (= exit-code 1)))))

(ert-deftest eldev-release-failing-tests-1 ()
  (eldev--test-with-temp-copy "project-b" 'Git
    (eldev--test-maintainer-run nil ("release" "1.5" "--dry-run")
      ;; There are failing tests, but in the default configuration we don't run them.
      (should (= exit-code 0)))))

(ert-deftest eldev-release-failing-tests-2 ()
  (eldev--test-with-temp-copy "project-b" 'Git
    (eldev--test-maintainer-run nil ("release" "1.5" "--dry-run")
      :extra-setup `(setf eldev-release-test-local t)
      (should (string-match-p "tests do not pass" stderr))
      (should (= exit-code 1)))))


(eldev-ert-defargtest eldev-release-successful-1 (project vc-backend)
                      (("project-a" 'Git)
                       ("project-a" 'Hg)
                       ;; This project uses a `*-pkg.el' file rather than headers.
                       ("project-c" 'Git)
                       ("project-c" 'Hg))
  (eldev--test-with-temp-copy project vc-backend
    (eldev--test-maintainer-run nil ("--setup" `(setf eldev-release-allowed-branch nil) "release" "1.5")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--quiet" "version" project)
      (should (string= stdout "1.5\n")))
    ;; FIXME: Also validate the tag.
    ))

(eldev-ert-defargtest eldev-release-successful-2 (project vc-backend)
                      (("project-a" 'Git)
                       ("project-a" 'Hg)
                       ("project-c" 'Git)
                       ("project-c" 'Hg))
  (eldev--test-with-temp-copy project vc-backend
    (eldev--test-maintainer-run nil ("--setup" `(setf eldev-release-allowed-branch nil) "release" "1.5")
      :extra-setup `(setf eldev-release-post-release-commit (lambda (version)
                                                              (unless (eldev-version-snapshot-p version)
                                                                (let ((eldev-release-min-version-size 3))
                                                                  (eldev-release-next-snapshot-version version)))))
      :extra-setup `(push (lambda (version _post-release-version)
                            (unless (equal version '(1 5))
                              (error "Released unexpected version %s" version)))
                          eldev-release-post-release-preparators)
      (should (= exit-code 0)))
    (eldev--test-run nil ("--quiet" "version" project)
      (should (string= stdout "1.5.1snapshot\n")))
    ;; FIXME: Also validate the tag.
    ))


(eldev-ert-defargtest eldev-update-copyright-1 (year)
                      (2000 nil)
  (let* ((year-2 (or year (string-to-number (format-time-string "%Y"))))
         (year-1 (1- year-2))
         (year-0 (1- year-1)))
    (eldev--test-with-temp-copy "project-a" nil
      (eldev--test-with-file-buffer nil "project-a.el"
        (re-search-forward (rx ";;; Commentary:"))
        (beginning-of-line)
        (insert (format ";; Copyright (C) %d-%d John Doe\n" year-0 year-1)))
      (eldev--test-maintainer-run nil (:eval `("update-copyright" ,@(when year `(,year))))
        :important-value (eldev--test-file-contents nil "project-a.el")
        ;; Can be not equal to `year-2' in the extremely unlikely case the tests are run
        ;; on 31 December around 23:59:59.
        (let ((year-3  (or year (string-to-number (format-time-string "%Y"))))
              (updated (eldev--test-file-contents nil "project-a.el")))
          (should (or (string-match-p (rx-to-string (format "Copyright (C) %d-%d John Doe" year-0 year-2)) updated)
                      (when (> year-3 year-2)
                        (string-match-p (rx-to-string (format "Copyright (C) %d-%d, %d John Doe" year-0 year-1 year-3)) updated)))))
        (should (string-match-p (rx "Updated 1 copyright notice") stdout))
        (should (= exit-code 0))))))



(provide 'test/init)

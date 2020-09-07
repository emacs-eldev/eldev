(require 'test/common)


(ert-deftest eldev-own-version-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

(ert-deftest eldev-own-version-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "eldev")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

(ert-deftest eldev-own-version-3 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "version")
      (should (string= stdout (format "%s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

(ert-deftest eldev-own-version-4 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "version" "eldev")
      (should (string= stdout (format "%s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

;; Querying own version must work even from a non-project directory.
(ert-deftest eldev-own-version-5 ()
  (let ((eldev--test-project "empty-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

(ert-deftest eldev-own-version-6 ()
  (let ((eldev--test-project "empty-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "eldev")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

;; https://github.com/doublep/eldev/issues/21
;;
;; In short: would fail previously if run in debug mode.  Placing test here as it doesn't
;; require anything external.
(ert-deftest eldev-own-version-7 ()
  (let ((eldev--test-project "empty-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--debug" "version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))

(ert-deftest eldev-own-version-missing-dependency-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version")
      (should (string= stdout (format "eldev %s\n" (eldev-message-version (eldev-find-package-descriptor 'eldev)))))
      (should (= exit-code 0)))))


(ert-deftest eldev-emacs-version-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "emacs")
      (should (string= stdout (format "emacs %s\n" emacs-version)))
      (should (= exit-code 0)))))

(ert-deftest eldev-emacs-version-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "version" "emacs")
      (should (string= stdout (format "%s\n" emacs-version)))
      (should (= exit-code 0)))))

;; Querying Emacs version must work even from a non-project directory.
(ert-deftest eldev-emacs-version-3 ()
  (let ((eldev--test-project "empty-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "emacs")
      (should (string= stdout (format "emacs %s\n" emacs-version)))
      (should (= exit-code 0)))))

(ert-deftest eldev-emacs-version-missing-dependency-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "emacs")
      (should (string= stdout (format "emacs %s\n" emacs-version)))
      (should (= exit-code 0)))))


(ert-deftest eldev-project-version-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "project-a")
      (should (string= stdout "project-a 1.0\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-project-version-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "version" "project-a")
      (should (string= stdout "1.0\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-project-version-missing-dependency-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "missing-dependency-a")
      (should (string= stdout "missing-dependency-a 1.0\n"))
      (should (= exit-code 0)))))


(ert-deftest eldev-dependency-version-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-dependency-version-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "version" "dependency-a")
      (should (string= stdout "1.0\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-dependency-version-3 ()
  ;; It is OK to test it here and not in integration, since Org is a built-in package.
  ;; Make sure that `version' can be used for built-in dependencies too.
  (let ((eldev--test-project "dependency-org"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "org")
      (should (string-match-p "org" stdout))
      (should (= exit-code 0)))))

(ert-deftest eldev-dependency-version-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "dependency-a")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(ert-deftest eldev-multiple-versions-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "eldev" "emacs" "project-a" "dependency-a")
      (should (string= stdout (format "eldev %s\nemacs %s\nproject-a 1.0\ndependency-a 1.0\n"
                                      (eldev-message-version (eldev-find-package-descriptor 'eldev)) emacs-version)))
      (should (= exit-code 0)))))

(ert-deftest eldev-multiple-versions-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--quiet" "version" "eldev" "emacs" "project-a" "dependency-a")
      (should (string= stdout (format "%s\n%s\n1.0\n1.0\n"
                                      (eldev-message-version (eldev-find-package-descriptor 'eldev)) emacs-version)))
      (should (= exit-code 0)))))


(provide 'test/version)

(require 'test/common)


(ert-deftest emake-test-own-version-1 ()
  (emake--test-run "project-a" ("version")
    (should (string= stdout (format "emake %s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))
    (should (= exit-code 0))))

(ert-deftest emake-test-own-version-2 ()
  (emake--test-run "project-a" ("version" "emake")
    (should (string= stdout (format "emake %s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))
    (should (= exit-code 0))))

(ert-deftest emake-test-own-version-3 ()
  (emake--test-run "project-a" ("--quiet" "version")
    (should (string= stdout (format "%s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))
    (should (= exit-code 0))))

(ert-deftest emake-test-own-version-4 ()
  (emake--test-run "project-a" ("--quiet" "version" "emake")
    (should (string= stdout (format "%s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))
    (should (= exit-code 0))))

(ert-deftest emake-test-own-version-missing-dependency-1 ()
  (emake--test-run "missing-dependency-a" ("version")
    (should (string= stdout (format "emake %s\n" (emake-message-version (emake-find-package-descriptor 'emake)))))
    (should (= exit-code 0))))


(ert-deftest emake-test-emacs-version-1 ()
  (emake--test-run "project-a" ("version" "emacs")
    (should (string= stdout (format "emacs %s\n" emacs-version)))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-version-2 ()
  (emake--test-run "project-a" ("--quiet" "version" "emacs")
    (should (string= stdout (format "%s\n" emacs-version)))
    (should (= exit-code 0))))

(ert-deftest emake-test-emacs-version-missing-dependency-1 ()
  (emake--test-run "missing-dependency-a" ("version" "emacs")
    (should (string= stdout (format "emacs %s\n" emacs-version)))
    (should (= exit-code 0))))


(ert-deftest emake-test-project-version-1 ()
  (emake--test-run "project-a" ("version" "project-a")
    (should (string= stdout "project-a 1.0\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-project-version-2 ()
  (emake--test-run "project-a" ("--quiet" "version" "project-a")
    (should (string= stdout "1.0\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-project-version-missing-dependency-1 ()
  (emake--test-run "missing-dependency-a" ("version" "missing-dependency-a")
    (should (string= stdout "missing-dependency-a 1.0\n"))
    (should (= exit-code 0))))


(ert-deftest emake-test-dependency-version-1 ()
  (emake--test-run "project-a" ("version" "dependency-a")
    (should (string= stdout "dependency-a 1.0\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependency-version-2 ()
  (emake--test-run "project-a" ("--quiet" "version" "dependency-a")
    (should (string= stdout "1.0\n"))
    (should (= exit-code 0))))

(ert-deftest emake-test-dependency-version-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("version" "dependency-a")
      (should (string-match-p "dependency-a" stderr))
      (should (string= stdout ""))
      (should (= exit-code 1)))))


(ert-deftest emake-test-multiple-versions-1 ()
  (emake--test-run "project-a" ("version" "emake" "emacs" "project-a" "dependency-a")
    (should (string= stdout (format "emake %s\nemacs %s\nproject-a 1.0\ndependency-a 1.0\n"
                                    (emake-message-version (emake-find-package-descriptor 'emake)) emacs-version)))
    (should (= exit-code 0))))

(ert-deftest emake-test-multiple-versions-2 ()
  (emake--test-run "project-a" ("--quiet" "version" "emake" "emacs" "project-a" "dependency-a")
    (should (string= stdout (format "%s\n%s\n1.0\n1.0\n"
                                    (emake-message-version (emake-find-package-descriptor 'emake)) emacs-version)))
    (should (= exit-code 0))))


(provide 'test/version)

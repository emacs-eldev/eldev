(require 'test/common)


;; To silence byte-compilation warnings on Emacs 24.
(defvar package-archive-priorities)


(ert-deftest eldev-test-archives-1 ()
  (eldev--test-run "trivial-project" ("archives")
    (should (string-prefix-p "None specified" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-test-archives-2 ()
  (eldev--test-run "trivial-project" ("--quiet" "archives")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest eldev-test-archives-3 ()
  (eldev--test-run "project-a" ("archives")
    (should (string= stdout (format "archive-a: %s%s\n"
                                    (expand-file-name "test/package-archive-a" eldev-project-dir)
                                    (if (boundp 'package-archive-priorities) "  (priority: 0, defaulted)" ""))))
    (should (= exit-code 0))))

(ert-deftest eldev-test-archives-4 ()
  (eldev--test-run "project-b" ("archives")
    (should (string= stdout (format "archive-a: %s%s\n"
                                    (expand-file-name "test/package-archive-a" eldev-project-dir)
                                    (if (boundp 'package-archive-priorities) "  (priority: 0, defaulted)" ""))))
    (should (= exit-code 0))))

(ert-deftest eldev-test-archives-5 ()
  (eldev--test-run "project-c" ("archives")
    (should (string= stdout (format "archive-a: %s%s\n"
                                    (expand-file-name "test/package-archive-a" eldev-project-dir)
                                    (if (boundp 'package-archive-priorities) "  (priority: 0, defaulted)" ""))))
    (should (= exit-code 0))))

;; It doesn't matter that the project is broken.
(ert-deftest eldev-test-archives-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("archives")
      (should (string-prefix-p "None specified" stdout))
      (should (= exit-code 0)))))


(provide 'test/archives)

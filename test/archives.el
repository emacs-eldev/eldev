(require 'test/common)


(ert-deftest emake-test-archives-1 ()
  (emake--test-run "trivial-project" ("archives")
    (should (string-prefix-p "None specified" stdout))
    (should (= exit-code 0))))

(ert-deftest emake-test-archives-2 ()
  (emake--test-run "trivial-project" ("--quiet" "archives")
    (should (string= stdout ""))
    (should (= exit-code 0))))

(ert-deftest emake-test-archives-3 ()
  (emake--test-run "project-a" ("archives")
    (should (string= stdout (format "archive-a: %s%s\n"
                                    (expand-file-name "test/package-archive-a" emake-project-dir)
                                    (if (boundp 'package-archive-priorities) "  (priority: 0, defaulted)" ""))))
    (should (= exit-code 0))))

(ert-deftest emake-test-archives-4 ()
  (emake--test-run "project-b" ("archives")
    (should (string= stdout (format "archive-a: %s%s\n"
                                    (expand-file-name "test/package-archive-a" emake-project-dir)
                                    (if (boundp 'package-archive-priorities) "  (priority: 0, defaulted)" ""))))
    (should (= exit-code 0))))

(ert-deftest emake-test-archives-5 ()
  (emake--test-run "project-c" ("archives")
    (should (string= stdout (format "archive-a: %s%s\n"
                                    (expand-file-name "test/package-archive-a" emake-project-dir)
                                    (if (boundp 'package-archive-priorities) "  (priority: 0, defaulted)" ""))))
    (should (= exit-code 0))))

;; It doesn't matter that the project is broken.
(ert-deftest emake-test-archives-missing-dependency-1 ()
  ;; It might be installed by a different test that provides a
  ;; suitable archive in setup form.
  (let ((emake--test-project "missing-dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("archives")
      (should (string-prefix-p "None specified" stdout))
      (should (= exit-code 0)))))


(provide 'test/archives)

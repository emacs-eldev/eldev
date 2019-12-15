(require 'test/common)


(ert-deftest emake-test-prepare-1 ()
  (let ((emake--test-project "trivial-project"))
    (emake--test-delete-cache)
    (emake--test-run nil ("prepare")
      (should (= exit-code 0)))))

(ert-deftest emake-test-prepare-2 ()
  (let ((emake--test-project "project-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("prepare")
      (should (= exit-code 0)))))

(ert-deftest emake-test-prepare-3 ()
  (let ((emake--test-project "project-b"))
    (emake--test-delete-cache)
    (emake--test-run nil ("prepare")
      (should (= exit-code 0)))))

(ert-deftest emake-test-prepare-4 ()
  (let ((emake--test-project "project-c"))
    (emake--test-delete-cache)
    (emake--test-run nil ("prepare")
      (should (= exit-code 0)))))


;; The project's package is also available from a package archive used
;; during building.  Test that Emake doesn't get confused.
(ert-deftest emake-test-prepare-project-available-1 ()
  (let ((emake--test-project "dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("--setup" "(emake-use-package-archive `(\"archive-a\" . ,(expand-file-name \"../package-archive-a\")))"
                          "prepare")
      (should (= exit-code 0)))
    ;; This is to make sure that `prepare' hasn't screwed up, but
    ;; claimed success.
    (emake--test-run nil ("eval" "0")
      (should (= exit-code 0)))))

;; Same as above, but a _newer_ version is available from the archive.
(ert-deftest emake-test-prepare-project-available-2 ()
  (let ((emake--test-project "dependency-a"))
    (emake--test-delete-cache)
    (emake--test-run nil ("--setup" "(emake-use-package-archive `(\"archive-b\" . ,(expand-file-name \"../package-archive-b\")))"
                          "prepare")
      (should (= exit-code 0)))
    (emake--test-run nil ("eval" "0")
      (should (= exit-code 0)))))


(provide 'test/prepare)

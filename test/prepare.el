(require 'test/common)


(ert-deftest eldev-prepare-1 ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      (should (= exit-code 0)))))

(ert-deftest eldev-prepare-2 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      (should (= exit-code 0)))))

(ert-deftest eldev-prepare-3 ()
  (let ((eldev--test-project "project-b"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      (should (= exit-code 0)))))

(ert-deftest eldev-prepare-4 ()
  (let ((eldev--test-project "project-c"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("prepare")
      (should (= exit-code 0)))))


;; The project's package is also available from a package archive used
;; during building.  Test that Eldev doesn't get confused.
(ert-deftest eldev-prepare-project-available-1 ()
  (let ((eldev--test-project "dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                          "prepare")
      (should (= exit-code 0)))
    ;; This is to make sure that `prepare' hasn't screwed up, but
    ;; claimed success.
    (eldev--test-run nil ("eval" `0)
      (should (= exit-code 0)))))

;; Same as above, but a _newer_ version is available from the archive.
(ert-deftest eldev-prepare-project-available-2 ()
  (let ((eldev--test-project "dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive `("archive-b" . ,(expand-file-name "../package-archive-b")))
                          "prepare")
      (should (= exit-code 0)))
    (eldev--test-run nil ("eval" `0)
      (should (= exit-code 0)))))


;; This does not really use `prepare' command, but probably still best put here.
(ert-deftest eldev-prepare-outdated-archive-contents-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-a")))
                          "version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))
    ;; To simulate archive contents being outdated, we treacherously replace archive URL
    ;; here: `package-archive-b' has newer versions of various packages.
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive `("archive-a" . ,(expand-file-name "../package-archive-b")))
                          "--setup" `(eldev-add-extra-dependencies 'test 'dependency-b)
                          "version" "dependency-b")
      (should (string= stdout "dependency-b 1.1\n"))
      (should (= exit-code 0)))))


(provide 'test/prepare)

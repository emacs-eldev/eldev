;;  -*- lexical-binding: t -*-

(require 'test/common)


(eldev-ert-defargtest eldev-prepare-1 (test-project)
                      ("trivial-project" "project-a" "project-b"  "project-c")
  (let ((eldev--test-project test-project))
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


(eldev-ert-defargtest eldev-prepare-external-1 (test-project dependencies &optional clashing-archives)
                      (("trivial-project" ())
                       ("project-a"       '(dependency-a))
                       ("project-a"       '(dependency-a) '("--eldev--" "archive-a")))
  (eldev--test-with-external-dir test-project dependencies
    ;; Shouldn't matter, but just to make sure Eldev doesn't look at it.
    (eldev--test-delete-cache)
    (dolist (archive clashing-archives)
      ;; Make sure Eldev can rename its archives to avoid clashes with archives already
      ;; used in the external directory.
      (let ((fake-archive-file (eldev--package-archive-dir archive external-dir)))
        (with-temp-file fake-archive-file
          (insert "dummy"))
        (push (file-relative-name fake-archive-file external-dir) preexisting-files)))
    (eldev--test-run nil ((format "--external=%s" external-dir) "prepare")
      (should (= exit-code 0)))))


;; This test would make Eldev fall into an infinite loop previously.  Not nice, but now it
;; shouldn't do that anymore, and I don't know how to impose a timeout here.  If this test
;; starts failing again, it doesn't matter much how, something has to be fixed.
(ert-deftest eldev-prepare-duplicate-dependency ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'extra-set 'dependency-b 'dependency-b)
                          "prepare" "extra-set")
      ;; It also shouldn't try to install `dependency-b' twice.
      (should     (string-match-p "2/2.+Installing package.+dependency-b" stderr))
      (should-not (string-match-p "3/3.+Installing package.+dependency-b" stderr))
      (should     (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'extra-set 'dependency-b 'dependency-b)
                          "version" "dependency-b")
      (should (string= stdout "dependency-b 1.0\n")))))


(provide 'test/prepare)

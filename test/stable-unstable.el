;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-stable/unstable-1 ()
  (let ((eldev--test-project "project-h"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-stable/unstable-2 ()
  (let ((eldev--test-project "project-h"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--unstable" "version" "dependency-a")
      (should (string= stdout "dependency-a 1.1\n"))
      (should (= exit-code 0)))))

;; Even if stable and unstable archives have the same priority, Eldev
;; has to respect `eldev-prefer-stable-archives'.
(ert-deftest eldev-stable/unstable-3 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-a"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b")))
                                                                '(0 . 0))
                          "version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-stable/unstable-4 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-a"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b")))
                                                                '(0 . 0))
                          "--unstable" "version" "dependency-a")
      (should (string= stdout "dependency-a 1.1\n"))
      (should (= exit-code 0)))))

;; Real-world failure description: a project uses `melpa-unstable' archive and Buttercup
;; tests.  Before a fix got applied, Buttercup would also be installed from
;; `melpa-unstable', because that archive had been fetched already, so Eldev would try
;; with what it had, even if more prioritized `melpa-stable' had not been available.
(ert-deftest eldev-stable/unstable-5 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '("archive-b" . ,(expand-file-name "../package-archive-b")))
                          "prepare")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-a"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b"))))
                          "--setup" `(eldev-add-extra-dependencies 'eval 'dependency-b)
                          "eval" 1)
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval 'dependency-b)
                          "version" "dependency-b")
      (should (string= stdout "dependency-b 1.0\n"))
      (should (= exit-code 0)))))

;; Like the previous test, but with one more archive.  In real use this would be GNU ELPA.
(ert-deftest eldev-stable/unstable-6 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '("archive-d" . ,(expand-file-name "../package-archive-d")) 300)
                          "--setup" `(eldev-use-package-archive '("archive-b" . ,(expand-file-name "../package-archive-b")) 100)
                          "prepare")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '("archive-d" . ,(expand-file-name "../package-archive-d")) 300)
                          "--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-a"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b")))
                                                                100)
                          "--setup" `(eldev-add-extra-dependencies 'eval 'dependency-b)
                          "eval" 1)
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-add-extra-dependencies 'eval 'dependency-b)
                          "version" "dependency-b")
      (should (string= stdout "dependency-b 1.0\n"))
      (should (= exit-code 0)))))


(ert-deftest eldev-stable/unstable-upgrade-1 ()
  (let ((eldev--test-project "project-h"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("upgrade")
      (should (string= stdout "All dependencies are up-to-date\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--unstable" "upgrade")
      (should (string= stdout "Upgraded or installed 1 dependency package\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.1\n"))
      (should (= exit-code 0)))))

(ert-deftest eldev-stable/unstable-upgrade-2 ()
  (let ((eldev--test-project "project-h"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--unstable" "version" "dependency-a")
      (should (string= stdout "dependency-a 1.1\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("upgrade")
      (should (string= stdout "All dependencies are up-to-date\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.1\n"))
      (should (= exit-code 0)))
    ;; It should be possible to downgrade back to the stable version with an option.
    (eldev--test-run nil ("upgrade" "--downgrade")
      (should (string= stdout "Upgraded or installed 1 dependency package\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))))


;; Real situation: we bumped our requirement for Buttercup.  Certain project had contents
;; of MELPA Stable and Unstable fetched.  Now, `eldev test' in it would install unstable
;; Buttercup because the newly required 1.23 had not been available from Stable before,
;; and MELPA Unstable versions are dumb and always "satisfy" requirements anyway.  The fix
;; was to force refetch of Stable archive if Unstable (not the most prioritized) was
;; deemed needed for anything.
;;
;; This test tries to simulate this situation with local archives and extra dependencies.
(ert-deftest eldev-stable/unstable-required-refetch-1 ()
  (let ((eldev--test-project "missing-dependency-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-a"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b")))
                                                                '(0 . 0))
                          "version" "dependency-a")
      (should (string= stdout "dependency-a 1.0\n"))
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-a"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b")))
                                                                '(0 . 0))
                          "--unstable" "upgrade" "dependency-a")
      (should (= exit-code 0)))
    (eldev--test-run nil ("version" "dependency-a")
      (should (string= stdout "dependency-a 1.1\n"))
      (should (= exit-code 0)))
    ;; At this point both archives must be fetched.  We now intentionally substitude
    ;; `archive-a' to be `package-archive-c', so that `misc-a' is also available from it,
    ;; but only if Eldev refetches the contents.
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive '(:stable   ("archive-a" . ,(expand-file-name "../package-archive-c"))
                                                                  :unstable ("archive-b" . ,(expand-file-name "../package-archive-b")))
                                                                '(0 . 0))
                          ;; Require 1.1, so that it doesn't try `package-archive-a'.
                          "--setup" `(eldev-add-extra-dependencies 'test '(:package misc-a :version "1.1"))
                          "version" "misc-a")
      ;; In `package-archive-c' it has version 1.2, while in `package-archive-b' -- 1.1.
      (should (string= stdout "misc-a 1.2\n"))
      (should (= exit-code 0)))))


(provide 'test/archives)

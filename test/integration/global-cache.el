;;  -*- lexical-binding: t -*-

(require 'test/common)


(ert-deftest eldev-global-cache-1 ()
  (ignore-errors (delete-directory (eldev--test-tmp-subdir "stdroot/global-cache") t))
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("lint" "re" "--required")
      ;; Archive contents should be fetched even if then find out that the linter is not
      ;; installable.
      (should (file-exists-p (expand-file-name "archive-contents" (eldev--test-tmp-subdir "stdroot/global-cache/gnu"))))
      (eldev--test-skip-if-missing-linter exit-code stderr)
      (should (= exit-code 0)))
    ;; Linter must now be cached and no download should be necessary.  Skip `project-b':
    ;; it intentionally includes an erroneous regexp.
    (let ((eldev--test-project "project-c"))
      (eldev--test-delete-cache)
      ;; Don't assume `url-retrieve-synchronously' is always called with global cache
      ;; lookup enabled: e.g. look for `bad-signature' in the source code.
      (eldev--test-run nil ("--setup" `(advice-add 'url-retrieve-synchronously :around
                                                   (lambda (original &rest arguments)
                                                     (if (advice-member-p #'eldev--global-cache-url-retrieve-synchronously #'url-retrieve-synchronously)
                                                         (error "fail!")
                                                       (apply original arguments))))
                            "lint" "re" "--required")
        (should (= exit-code 0))))))

(ert-deftest eldev-global-cache-2 ()
  (ignore-errors (delete-directory (eldev--test-tmp-subdir "stdroot/global-cache") t))
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(advice-add 'url-retrieve-synchronously :around
                                                 (lambda (original &rest arguments)
                                                   (let ((buffer (apply original arguments)))
                                                     (with-current-buffer buffer
                                                       ;; Corrupt it!
                                                       (goto-char (point-max))
                                                       (backward-line)
                                                       (delete-region (point) (point-max))
                                                       (insert "(][) this is not valid lisp (][)"))
                                                     buffer)))
                          "--robust-mode=never"
                          "lint" "re")
      (eldev--test-skip-if-missing-linter exit-code stderr)
      ;; As it must have lead to an error, the file must not get stored in the GPA.
      (should-not (file-exists-p (expand-file-name "archive-contents" (eldev--test-tmp-subdir "stdroot/global-cache/gnu"))))
      (should     (/= exit-code 0)))))

(ert-deftest eldev-global-cache-clean-1 ()
  (make-directory (eldev--test-tmp-subdir "stdroot/global-cache") t)
  (eldev--test-run "project-a" ("clean" "global-cache")
    (should (= exit-code 0)))
  (should (not (file-exists-p (eldev--test-tmp-subdir "stdroot/global-cache")))))


(provide 'test/integration/global-cache)

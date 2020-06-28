(require 'test/common)


;; FIXME: Check if the package is available instead of hardcoding
;;        required Emacs version.
(defvar eldev--relint-available (not (version< emacs-version "26")))

(ert-deftest eldev-relint-project-a-1 ()
  (skip-unless eldev--relint-available)
  (eldev--test-run "project-a" ("lint" "re")
    (should (string-match-p "no complaints" stdout))
    (should (= exit-code 0))))

(ert-deftest eldev-relint-project-b-1 ()
  (skip-unless eldev--relint-available)
  ;; This project contains a never-called function with an invalid regexp.
  (eldev--test-run "project-b" ("lint" "re")
    (should (string-match-p "string-match-p" stderr))
    (should (string-match-p "Found 1 warning" stderr))
    (should (= exit-code 1))))


(provide 'test/integration/relint)

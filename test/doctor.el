(require 'test/common)


(ert-deftest eldev-doctor-no-eldev ()
  (eldev--test-run "trivial-project" ("doctor" "eldev-presence")
    (should (string-match-p "recommended to have file" stdout))
    (should (= exit-code 1)))
  (eldev--test-run "project-a" ("doctor" "eldev-presence")
    (should (= exit-code 0))))

(ert-deftest eldev-doctor-eldev-byte-compilable ()
  ;; File `Eldev' is not even present here.
  (eldev--test-run "trivial-project" ("doctor" "eldev-byte-compilable")
    (should (= exit-code 0)))
  (eldev--test-run "project-a" ("doctor" "eldev-byte-compilable")
    (should (= exit-code 0)))
  (eldev--test-run "project-b" ("doctor" "eldev-byte-compilable")
    (should (string-match-p "no-byte-compile: t" stdout))
    (should (= exit-code 1))))

(ert-deftest eldev-doctor-explicit-main-file ()
  ;; I'm too lazy to set up a project which doctor doesn't like, so testing only projects
  ;; where the doctest passes.
  (eldev--test-run "project-a" ("doctor" "explicit-main-file")
    (should (= exit-code 0)))
  (eldev--test-run "project-c" ("doctor" "explicit-main-file")
    (should (= exit-code 0)))
  (eldev--test-run "project-d" ("doctor" "explicit-main-file")
    (should (= exit-code 0))))

(ert-deftest eldev-doctor-explicit-emacs-version ()
  (eldev--test-run "project-a" ("doctor" "explicit-emacs-version")
    (should (= exit-code 0)))
  (eldev--test-run "project-b" ("doctor" "explicit-emacs-version")
    (should (string-match-p "required Emacs version" stdout))
    (should (= exit-code 1))))


(ert-deftest eldev-doctor-disabling-doctests ()
  ;; Pretend that the project has disabled it in its `Eldev'.
  (eldev--test-run "trivial-project" ("--setup" `(push 'eldev-presence eldev-doctor-disabled-tests) "doctor" "eldev-presence")
    (should (= exit-code 0)))
  (eldev--test-run "trivial-project" ("--setup" `(push 'eldev-presence eldev-doctor-disabled-tests) "doctor" "--list-tests")
    (should     (string-match-p "eldev-byte-compilable" stdout))
    (should-not (string-match-p "eldev-presence"        stdout))
    (should     (= exit-code 0))))


(provide 'test/doctor)

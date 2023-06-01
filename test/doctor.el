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

(ert-deftest eldev-doctor-recent-stable-releases ()
  ;; Too difficult to set up not-up-to-date pseudoprojects, so we test only a couple
  ;; setups that the doctor doesn't complain about.
  (eldev--test-with-temp-copy "project-a" 'Git
    (eldev--test-run nil ("doctor" "recent-stable-releases" "--successful")
      (should (string-match-p "no stable releases at all" stdout))
      (should (= exit-code 0)))
    (eldev-vc-create-tag "1.0" (eldev--test-project-dir))
    (eldev--test-run nil ("doctor" "recent-stable-releases" "--successful")
      (should (string-match-p "no commits after 1.0" stdout))
      (should (= exit-code 0)))))


(ert-deftest eldev-doctor-disabling-doctests ()
  ;; Pretend that the project has disabled it in its `Eldev'.
  (eldev--test-run "trivial-project" ("--setup" `(push 'eldev-presence eldev-doctor-disabled-tests) "doctor" "eldev-presence")
    (should (= exit-code 0)))
  (eldev--test-run "trivial-project" ("--setup" `(push 'eldev-presence eldev-doctor-disabled-tests) "doctor" "--list-tests")
    (should     (string-match-p "eldev-byte-compilable" stdout))
    (should-not (string-match-p "eldev-presence"        stdout))
    (should     (= exit-code 0))))

(ert-deftest eldev-doctor-tar-win ()
  (eldev--test-run "trivial-project" ("doctor" "tar-windows")
    (should (= exit-code 0)))
  (when (eq system-type 'windows-nt)
    (let* ((file (make-temp-file "doctor-" nil ".bat"))
	   (eldev-tar-executable file))
      (unwind-protect
	  ;; use a mock .bat file that will fail when run
	  (with-temp-file file
	    (insert "@echo off\nexit 1"))
	(eldev--test-run "trivial-project" ("--setup" `(setf eldev-tar-executable ,file) "doctor" "tar-windows")
			 (should (string-match-p "The tar executable" stdout))
			 (should (= exit-code 1)))
	(delete-file file)))))

(provide 'test/doctor)

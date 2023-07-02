(require 'test/common)


;; Not profiling itself, only setup.  Don't create a test per name pair to avoid unneeded
;; noise: this is not some important functionality.
(ert-deftest eldev-profile-derive-memory-file-1 ()
  (dolist (names '(("cpu.prof"                  "mem.prof")
                   ("~/cpu/bar/cpu.prof"        "~/cpu/bar/mem.prof")
                   ("cpu-cpu.prof"              "mem-mem.prof")
                   ("~/cpu/bar/cpu-cpu.prof"    "~/cpu/bar/mem-mem.prof")
                   ("profile-cpu.dat"           "profile-mem.dat")
                   ("~/cpu/bar/profile-cpu.dat" "~/cpu/bar/profile-mem.dat")
                   ("just-a-name"               "just-a-name-mem")
                   ("~/cpu/bar/just-a-name"     "~/cpu/bar/just-a-name-mem")))
    (should (string= (eldev--profile-derive-memory-file (car names)) (cadr names)))))

;; Profiler would choke when nothing was ever profiled.
(ert-deftest eldev-profile-no-op ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("profile" "--file" (make-temp-file "eldev-profile") "compile" "there-is-no-such-file.el")
      (should (string-match-p "Nothing to do" stdout))
      (should (= exit-code 0)))))

;; Profiler would choke on recursive compilation (`project-d.el' requires
;; `project-d-misc.el').
(ert-deftest eldev-profile-recursive-compilation ()
  (let ((eldev--test-project "project-d"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("clean" "all")
      (should (= exit-code 0)))
    (eldev--test-run nil ("profile" "--file" (make-temp-file "eldev-profile") "compile" "project-d.el" "project-d-misc.el")
      ;; Assert that the order is preserved, resulting in recursion to compile the second
      ;; file only once the compilation of the first is already started.  Ignore extra
      ;; crap written by ancient Emacs versions ('#' is just a "won't appear" character").
      (should (string-match-p "ELC +project-d\\.el[^#]+ELC +project-d-misc\\.el" stdout))
      (should (= exit-code 0)))))

;; Depending on Emacs version (27 and up, apparently), profiles created later would
;; replace earlier profiles, instead of being merged with them.  For the standard profiler
;; usage this would make sense, but certainly not when used from Eldev.
(eldev-ert-defargtest eldev-profile-merges-multiple (meaningful-is-last)
                      (nil t)
  (let ((profile-file (make-temp-file "eldev-profile")))
    (eldev--test-run "trivial-project" (:eval `("-d" "profile" "--file" ,profile-file
                                                "exec" ,`(defun must-appear-in-profile (x) (dotimes (_ 100000) (setf x (1+ x))) x)
                                                ,`(must-appear-in-profile 0) ,@(unless meaningful-is-last '(nil))))
      (should (string-match-p "must-appear-in-profile" (eldev--test-file-contents nil profile-file)))
      (should (= exit-code 0)))))


(provide 'test/profile)

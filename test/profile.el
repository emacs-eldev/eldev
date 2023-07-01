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


(provide 'test/profile)

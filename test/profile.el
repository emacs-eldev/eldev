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


(provide 'test/profile)

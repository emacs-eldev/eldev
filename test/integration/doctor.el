(require 'test/common)


(ert-deftest eldev-doctor-stable/unstable-archives ()
  (let ((eldev--test-project "trivial-project"))
    (eldev--test-run nil ("doctor" "stable/unstable-archives")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive 'gnu-elpa) "doctor" "stable/unstable-archives")
      (should (= exit-code 0)))
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive 'gnu) "doctor" "stable/unstable-archives")
      (should (string-match-p "gnu" stdout))
      (should (= exit-code 1)))
    (eldev--test-run nil ("--setup" `(eldev-use-package-archive 'gnu-devel) "doctor" "stable/unstable-archives")
      (should (string-match-p "gnu-devel" stdout))
      (should (= exit-code 1)))))


(provide 'test/integration/doctor)

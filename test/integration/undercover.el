(require 'test/common)


(ert-deftest eldev-undercover-1 ()
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-without-files nil ("simple-coverage-report.txt")
      (eldev--test-run nil ("--setup" `(eldev-use-plugin 'undercover) "test" "--undercover-report" "simple-coverage-report.txt")
        ;; The only function in the "project" is "tested", so the coverage must be 100%.
        (should (string-match-p (rx "100%") (eldev--test-file-contents nil "simple-coverage-report.txt")))
        (should (= exit-code 0))))))


(provide 'test/integration/undercover)

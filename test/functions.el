(require 'test/common)

;; Testing various functions, macros etc. without particular relation to any command.


;; Since `eldev-call-process' is no longer just a simple wrapper over `call-process', test
;; it at least somewhat.
(ert-deftest eldev-call-process-1 ()
  (eldev-call-process eldev-emacs-executable '("--batch" "--eval" "(print 1)")
    (should (equal (eldev--test-line-list (buffer-string)) '("1")))
    (should (= exit-code 0))))

(ert-deftest eldev-call-process-2 ()
  (eldev-call-process eldev-emacs-executable '("--batch" "--eval" "(print 1)" "--eval" "(message \"2\")")
    ;; At least on Emacs 24 (i.e. where it falls back to using `call-process') the output
    ;; comes in a mixed order.  We don't want to preserve this behavior, but also don't
    ;; count it as an error (as it depends on Emacs internals, which are as snot-bound as
    ;; it gets).  So, both orders are accepted.
    (should (member (eldev--test-line-list (buffer-string)) '(("1" "2") ("2" "1"))))
    (should (= exit-code 0))))

(ert-deftest eldev-call-process-3 ()
  (eldev-call-process eldev-emacs-executable '("--batch" "--eval" "(print 1)" "--eval" "(message \"2\")")
    ;; stderr is discarded.
    :destination '(t nil)
    (should (equal (eldev--test-line-list (buffer-string)) '("1")))
    (should (= exit-code 0))))


(provide 'test/functions)

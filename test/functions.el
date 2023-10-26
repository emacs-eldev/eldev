;;  -*- lexical-binding: t -*-

(require 'test/common)

;; Testing various functions, macros etc. without particular relation to any command.


(defun eldev--test-command-line-parsing (command-line without-options all-options &rest extra-parameters)
  ;; Always testing in dry-run mode, to avoid messing up with actual configuration.  Add
  ;; some "bullshit" key to make sure the function accepts "future keys" it doesn't know.
  (let ((result (apply #'eldev-parse-command-line command-line (append extra-parameters '(:dry-run t :some-sort-of-random-parameter xxx)))))
    (should (equal (plist-get result :full)            command-line))
    (should (equal (plist-get result :without-options) without-options))
    (should (equal (plist-get result :all-options)     all-options))))

(ert-deftest eldev-parse-command-line-1 ()
  (eldev--test-command-line-parsing '("--debug" "foo" "--trace")
                                    '("foo")
                                    '("--debug" "--trace"))
  (eldev--test-command-line-parsing '("--debug" "foo" "--trace")
                                    '("foo" "--trace")
                                    '("--debug")
                                    ;; This is how global command line is actually parsed.
                                    :stop-on-non-option t))

(ert-deftest eldev-parse-command-line-2 ()
  (eldev--test-command-line-parsing '("file1.el" "--expect=5" "file2.el")
                                    '("file1.el" "file2.el")
                                    '("--expect=5")
                                    :command 'test)
  (eldev--test-command-line-parsing '("file1.el" "--expect" "5" "file2.el")
                                    '("file1.el" "file2.el")
                                    '("--expect" "5")
                                    :command 'test)
  (eldev--test-command-line-parsing '("file1.el" "-X" "5" "file2.el")
                                    '("file1.el" "file2.el")
                                    '("-X" "5")
                                    :command 'test)
  (eldev--test-command-line-parsing '("file1.el" "-X5" "file2.el")
                                    '("file1.el" "file2.el")
                                    '("-X5")
                                    :command 'test)
  ;; Since `--stop' has _optional_ value, parsing works differently here.
  (eldev--test-command-line-parsing '("file1.el" "--stop=5" "file2.el")
                                    '("file1.el" "file2.el")
                                    '("--stop=5")
                                    :command 'test)
  (eldev--test-command-line-parsing '("file1.el" "--stop" "5" "file2.el")
                                    '("file1.el" "5" "file2.el")
                                    '("--stop")
                                    :command 'test)
  (eldev--test-command-line-parsing '("file1.el" "-s" "5" "file2.el")
                                    '("file1.el" "5" "file2.el")
                                    '("-s")
                                    :command 'test)
  (eldev--test-command-line-parsing '("file1.el" "-s5" "file2.el")
                                    '("file1.el" "file2.el")
                                    '("-s5")
                                    :command 'test))

(ert-deftest eldev-parse-command-line-3 ()
  (should-error (eldev-parse-command-line '("--there-is-no-such-option")))
  (eldev--test-command-line-parsing '("--there-is-no-such-option")
                                    '()
                                    '("--there-is-no-such-option")
                                    :allow-unknown t))


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

(ert-deftest eldev-call-process-stderr-forwarding ()
  ;; Starting a nested Eldev to catch the output it forwards from the nested (3rd level,
  ;; sort of) Emacs.  Testing that stderr forwarding doesn't inject extra newlines (it
  ;; could up until 1.7).
  (eldev--test-run "trivial-project" ("exec" `(eldev-call-process eldev-emacs-executable '("--batch" "--eval" "(dotimes (_ 10) (princ nil #'external-debugging-output) (sleep-for 0.01))")
                                                :forward-output 'stderr))
    (should-not (string-match-p "\n" stderr))
    (should     (= exit-code 0))))


(provide 'test/functions)

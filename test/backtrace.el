(require 'test/common)


(defun eldev--test-backtrace-lines (stderr)
  ;; Let's ignore empty lines too.
  (eldev-filter (and (> (length it) 0)
                     (not (string-match-p "Bootstrapping Eldev" it))
                     ;; For old Emacs versions where this is not silenced.
                     (not (string-match-p "Importing package-keyring.gpg" it)))
                (eldev--test-line-list stderr)))


;; In principle, we could call `eldev-backtrace' right in this process, but that would
;; introduce unwieldy dependency on the current environment.  E.g. running tests in
;; interactive Emacs would likely give different results.
(ert-deftest eldev-backtrace-1 ()
  (eldev--test-run "trivial-project" ("exec" `(eldev-backtrace nil 'this-function-is-never-called))
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should (null lines))
      (should (= exit-code 0)))))

(ert-deftest eldev-backtrace-2 ()
  (eldev--test-run "trivial-project" ("exec" `(eldev-backtrace nil 'eldev-cli))
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should     (eldev-any-p (string-match-p "eldev-cli"  it) lines))
      (should-not (eldev-any-p (string-match-p "eldev-exec" it) lines))
      (should     (= exit-code 0)))))

(ert-deftest eldev-backtrace-3 ()
  (eldev--test-run "trivial-project" ("exec" `(eldev-backtrace))
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should     (eldev-any-p (string-match-p "eldev-cli"  it) lines))
      (should     (eldev-any-p (string-match-p "eldev-exec" it) lines))
      (should     (= exit-code 0)))))


(eldev-ert-defargtest eldev-backtrace-length-limit-1 (with-time-diff)
                      (nil t)
  ;; Backtrace length limit should be adjusted accordingly when option `-T' is used.
  (eldev--test-run "trivial-project" ("--backtrace=30" (if with-time-diff "--time" "--no-time") "exec" `(eldev-backtrace))
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should (string= stdout ""))
      (should (eldev-all-p (<  (length it) 30) lines))
      (should (eldev-any-p (>= (length it) 29) lines))
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-backtrace-length-limit-2 (with-time-diff)
                      (nil t)
  (eldev--test-run "trivial-project" ("--backtrace=30" "--debug" (if with-time-diff "--time" "--no-time") "eval" "this-variable-is-not-bound")
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should (string= stdout ""))
      (should (eldev-all-p (<  (length it) 30) lines))
      (should (eldev-any-p (>= (length it) 29) lines))
      (should (/= exit-code 0)))))


(provide 'test/backtrace)

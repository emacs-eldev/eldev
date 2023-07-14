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


(defun eldev--test-infer-backtrace-indentation (lines with-time-diff error-backtrace)
  (when with-time-diff
    (setf lines (mapcar (lambda (line) (substring line 13)) lines)))
  (let ((indentation (progn (string-match "^ *" (car lines))
                            (- (length (match-string 0 (car lines))) (if error-backtrace 0 2)))))
    (if (eldev-all-p (or (= (length it) 0)
                         ;; Ignore lines used to mark backtrace cutting; `backtrace' module apparently does
                         ;; not indent them, at least in current Emacs versions.
                         (string-match-p (rx bol (0+ whitespace) "..." eol) it)
                         (progn (string-match "^ *" it) (= indentation (- (length (match-string 0 it)) 2)))
                         (eldev-dump it indentation)
                         nil)
                     (cdr lines))
        indentation
      'cannot-infer)))

(eldev-ert-defargtest eldev-backtrace-length-limit-1 (with-time-diff indent)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  ;; Backtrace length limit should be adjusted accordingly when option `-T' is used.
  (eldev--test-run "trivial-project" ("--backtrace=30" (if with-time-diff "--time" "--no-time") "exec"
                                      (if indent `(eldev-nest-debugging-output (eldev-backtrace)) `(eldev-backtrace)))
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should (string= stdout ""))
      (should (eldev-all-p (<  (length it) 30) lines))
      (should (eldev-any-p (>= (length it) 29) lines))
      (should (equal (eldev--test-infer-backtrace-indentation lines with-time-diff nil) (if indent 2 0)))
      (should (= exit-code 0)))))

(eldev-ert-defargtest eldev-backtrace-length-limit-2 (with-time-diff indent)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  (eldev--test-run "trivial-project" ("--backtrace=30" "--debug" (if with-time-diff "--time" "--no-time") "eval"
                                      (if indent `(eldev-nest-debugging-output this-variable-is-not-bound) `this-variable-is-not-bound))
    (let ((lines (eldev--test-backtrace-lines stderr)))
      (should (string= stdout ""))
      (should (eldev-all-p (<  (length it) 30) lines))
      (should (eldev-any-p (>= (length it) 29) lines))
      ;; Backtraces that are a result of an error must never be indented.
      (should (equal (eldev--test-infer-backtrace-indentation lines with-time-diff t) 0))
      (should (/= exit-code 0)))))


;; The error data should never be truncated, even if backtrace lines are.  A smaller limit
;; (e.g. 80) would truncate _the full line_, which is fine, see comments in `eldev-cli'.
(eldev-ert-defargtest eldev-untruncated-error-data (limit)
                      (0 1000)
  (let ((message "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."))
    (eldev--test-run "trivial-project" ("--debug" (format "--backtrace=%d" limit) "eval" `(error ,message))
      (should (string-match-p (rx-to-string `(seq bol "Debugger entered" (1+ any) ,message)) stderr))
      (should (/= exit-code 0)))))


(provide 'test/backtrace)

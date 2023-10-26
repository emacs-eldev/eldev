;;  -*- lexical-binding: t -*-

(require 'test/common)


(defun eldev--test-internals-inc (x)
  (1+ x))

;; Make `eldev--test-internals-inc' add 2, not 1, for BODY.  Nested calls still make it
;; add only 2 (not 3, 4, ...) because of how advices work; this is expected.
(defun eldev--test-internals-strengthened-inc (original x)
  (1+ (funcall original x)))
(defmacro eldev--test-internals-strengthen-inc (&rest body)
  (declare (indent 0))
  `(eldev-advised ('eldev--test-internals-inc :around 'eldev--test-internals-strengthened-inc)
     ,@body))


(ert-deftest eldev-advised-1 ()
  (should (equal (eldev--test-internals-inc 2) 3))
  ;; Test various advice types.
  (eldev-advised ('eldev--test-internals-inc :around (lambda (original x) (- (funcall original x))))
    (should (equal (eldev--test-internals-inc 2) -3)))
  (eldev-advised ('eldev--test-internals-inc :override (lambda (x) (- x)))
    (should (equal (eldev--test-internals-inc 2) -2)))
  (eldev-advised ('eldev--test-internals-inc :filter-args (lambda (x) (list (- (car x)))))
    (should (equal (eldev--test-internals-inc 2) -1)))
  (eldev-advised ('eldev--test-internals-inc :filter-return (lambda (x) (* x x)))
    (should (equal (eldev--test-internals-inc 2) 9)))
  ;; Must be back to normal now.
  (should (equal (eldev--test-internals-inc 2) 3)))

(ert-deftest eldev-advised-2 ()
  (should (equal (eldev--test-internals-inc 2) 3))
  ;; `nil' advices must be simply ignored.
  (eldev-advised ('eldev--test-internals-inc :around nil)
    (should (equal (eldev--test-internals-inc 2) 3)))
  (should (equal (eldev--test-internals-inc 2) 3)))

;; Real-world failure with nested `eldev-output-reroute-messages', only abstracted.
(ert-deftest eldev-advised-3 ()
  (should (equal (eldev--test-internals-inc 1) 2))
  (eldev--test-internals-strengthen-inc
    (should (equal (eldev--test-internals-inc 2) 4))
    (eldev--test-internals-strengthen-inc
      ;; Installing the same advice the second time shouldn't do anything.
      (should (equal (eldev--test-internals-inc 3) 5)))
    ;; Now the advice is still supposed to be present: installed twice (even if second
    ;; time is a no-op), but removed only once (must also be a no-op).
    (should (equal (eldev--test-internals-inc 4) 6)))
  ;; And only now gone completely.
  (should (equal (eldev--test-internals-inc 5) 6)))


(defmacro eldev--test-target-dependencies (&rest body)
  (declare (indent 0))
  `(let ((eldev-verbosity-level                  'quiet)
         (eldev--target-dependencies             nil)
         (eldev--target-dependencies-need-saving nil))
     ,@body))

(eldev-ert-defargtest eldev--maybe-with-target-dependencies-1 (do-set-up public)
                      ((nil nil)
                       (nil t)
                       (t   nil)
                       (t   t))
  (eldev--test-target-dependencies
    (eldev--maybe-with-target-dependencies do-set-up public
      (if do-set-up
          (progn (should eldev--target-dependencies)
                 (should (eq (car eldev--target-dependencies) (not (null public))))
                 (if public
                     (should (hash-table-p (cdr eldev--target-dependencies)))
                   ;; Must be left for lazy loading.
                   (should-not (cdr eldev--target-dependencies))))
        (should-not eldev--target-dependencies))
      (should-not eldev--target-dependencies-need-saving))))

(ert-deftest eldev--maybe-with-target-dependencies-2 ()
  (let ((eldev-verbosity-level 'quiet))
    (eldev--maybe-with-target-dependencies t nil
      (should (equal eldev--target-dependencies '(nil . nil)))
      ;; Make public and official.
      (eldev-with-target-dependencies
        (should (car eldev--target-dependencies))
        (should (hash-table-p (cdr eldev--target-dependencies))))
      (should-not eldev--target-dependencies-need-saving))))


(provide 'test/internals)

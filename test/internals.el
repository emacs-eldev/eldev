(require 'test/common)


(defun eldev--test-internals-inc (x)
  (1+ x))

;; Make `eldev--test-internals-inc' add 2, not 1, for BODY.  Nested calls still make it
;; add only 2 (not 3, 4, ...) because of how advices work; this is expected.
(defmacro eldev--test-internals-strengthen-inc (&rest body)
  (declare (indent 0))
  `(eldev-advised ('eldev--test-internals-inc :around (lambda (original x) (1+ (funcall original x))))
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


(provide 'test/internals)

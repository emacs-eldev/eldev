(defvar eldev--ecukes-pass-if "foo")


(But "^this always passes$"
  (lambda ()
    ))

(When "^`eldev--ecukes-pass-if' is \"\\(.+\\)\", then pass$"
  (lambda (value)
    (unless (equal value eldev--ecukes-pass-if)
      (error "Value `%s' is unexpected" value))))

(And "^ignore value \"\\([^\"]+\\)\"$"
  (lambda (value)
    (ignore value)))

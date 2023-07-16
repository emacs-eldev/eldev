(require 'test/common)


;; Trying to emulate an intermittent failure when updating package archive contents.
;; Ideally we'd cause a failure somewhere in `url-retrieve-synchronously', but I don't
;; want to go too deep.
(eldev-ert-defargtest eldev-robust-archive-fetching (num-retries)
                      (0 1 2)
  (let ((eldev--test-project "project-a"))
    (eldev--test-delete-cache)
    (eldev--test-run nil ("--setup" `(setf eldev-robust-mode-retry-delays ',(make-list num-retries 0))
                          "--setup" `(let ((num-calls 0))
                                       (advice-add 'package-refresh-contents :around
                                                   (lambda (original &rest arguments)
                                                     (if (<= (setf num-calls (1+ num-calls)) 2)
                                                         (error "random failure")
                                                       (apply original arguments)))))
                          "upgrade")
      :robust-mode t
      (when (>= num-retries 1)
        (should (string-match-p "Retry #1" stderr)))
      (when (>= num-retries 2)
        (should (string-match-p "Retry #2" stderr)))
      (if (>= num-retries 2)
          (should (= exit-code 0))
        (should (/= exit-code 0))))))


(provide 'test/robust-mode)

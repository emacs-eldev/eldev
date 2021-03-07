(require 'test/common)


(defmacro eldev--test-packaging (test-project &optional child-emacs-form &rest body)
  `(let* ((eldev--test-project (or ,test-project eldev--test-project))
          (test-emacs-dir      (expand-file-name "--package-test-emacs" (eldev--test-project-cache-dir)))
          (dist-dir            (expand-file-name "dist" (eldev--test-project-dir)))
          package-file)
     (eldev--test-run nil ("clean" "dist")
       (should (= exit-code 0)))
     (eldev--test-run nil ("package" "--print-filename")
       (let ((dist-files (eldev--test-find-files dist-dir)))
         (should (= (length dist-files) 1))
         (setf package-file (expand-file-name (car dist-files) dist-dir)))
       (should (string-suffix-p (eldev--test-lines package-file "generated") stdout))
       (should (= exit-code 0)))
     (eldev--test-run nil ("clean" "dist")
       (should (= exit-code 0)))
     (eldev--test-run nil ("package" "--entry-file" "--print-filename")
       (eldev--test-assert-files dist-dir
                                 (file-relative-name package-file dist-dir)
                                 (file-relative-name (eldev-replace-suffix (eldev-replace-suffix package-file ".el" ".entry") ".tar" ".entry") dist-dir))
       (should (string-suffix-p (eldev--test-lines package-file "generated") stdout))
       (should (= exit-code 0)))
     (let (descriptor)
       (eldev--test-run nil ("exec" `(prin1 (eldev-package-descriptor)))
         (should (= exit-code 0))
         (setf descriptor (read stdout)))
       (ignore-errors (delete-directory test-emacs-dir t))
       (eldev--test-call-process "Emacs" eldev-emacs-executable
                                 ("--batch" "--no-site-file" "--no-site-lisp" "--execute"
                                  `(progn
                                     (require 'package)
                                     (let ((package-user-dir ,test-emacs-dir)
                                           ;; Just use all we have, no need to
                                           ;; tailor for each test specifically.
                                           (package-archives '(("archive-a" . ,(expand-file-name "package-archive-a/" (eldev--test-dir)))
                                                               ("archive-b" . ,(expand-file-name "package-archive-b/" (eldev--test-dir))))))
                                       (package-initialize t)
                                       (package-refresh-contents)
                                       (package-install-file ,package-file)
                                       (package-activate ',(package-desc-name descriptor))
                                       (prin1 (cadr (assq ',(package-desc-name descriptor) package-alist)))
                                       ,',child-emacs-form)))
         (should (= exit-code 0))
         ;; Cannot compare just like that.
         (let ((installed-desciptor (read stdout)))
           (should (equal (package-desc-name    descriptor) (package-desc-name    installed-desciptor)))
           (should (equal (package-desc-version descriptor) (package-desc-version installed-desciptor)))
           (should (equal (package-desc-reqs    descriptor) (package-desc-reqs    installed-desciptor)))
           (should (equal (package-desc-extras  descriptor) (package-desc-extras  installed-desciptor))))
         ,@body))))


(ert-deftest eldev-package-1 ()
  (eldev--test-packaging "trivial-project"))

(ert-deftest eldev-package-2 ()
  (eldev--test-packaging "project-a"))

(ert-deftest eldev-package-3 ()
  ;; This project comes with an Info "manual".  Make sure it gets
  ;; installed.
  (skip-unless (eldev-makeinfo-executable t))

  (eldev--test-packaging "project-b" (info "project-b")))

(ert-deftest eldev-package-4 ()
  (eldev--test-packaging "project-c"))

(ert-deftest eldev-package-5 ()
  (eldev--test-packaging "project-d"))

(ert-deftest eldev-package-6 ()
  (eldev--test-packaging "project-e"))


(provide 'test/package)

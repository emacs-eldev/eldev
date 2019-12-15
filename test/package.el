(require 'test/common)


(defmacro emake--test-packaging (test-project &optional child-emacs-form &rest body)
  `(let* ((emake--test-project (or ,test-project emake--test-project))
          (test-emacs-dir      (expand-file-name "--package-test-emacs" (emake--test-project-cache-dir)))
          (dist-dir            (expand-file-name "dist" (emake--test-project-dir)))
          package-file)
     (emake--test-run nil ("clean" "dist")
       (should (= exit-code 0)))
     (emake--test-run nil ("package" "--print-filename")
       (let ((dist-files (emake--test-find-files dist-dir)))
         (should (= (length dist-files) 1))
         (setf package-file (expand-file-name (car dist-files) dist-dir)))
       (should (string-suffix-p (emake--test-lines package-file "generated") stdout))
       (should (= exit-code 0)))
     (emake--test-run nil ("clean" "dist")
       (should (= exit-code 0)))
     (emake--test-run nil ("package" "--entry-file" "--print-filename")
       (emake--test-assert-files dist-dir
                                 (file-relative-name package-file dist-dir)
                                 (file-relative-name (emake-replace-suffix (emake-replace-suffix package-file ".el" ".entry") ".tar" ".entry") dist-dir))
       (should (string-suffix-p (emake--test-lines package-file "generated") stdout))
       (should (= exit-code 0)))
     (let (descriptor)
       (emake--test-run nil ("exec" "(prin1 (emake-package-descriptor))")
         (should (= exit-code 0))
         (setf descriptor (read stdout)))
       (ignore-errors (delete-directory test-emacs-dir t))
       (emake--test-call-process "Emacs" emake-emacs-executable
                                 ("--batch" "--no-site-file" "--no-site-lisp" "--execute"
                                  (prin1-to-string `(progn
                                                      (require 'package)
                                                      (let ((package-user-dir ,test-emacs-dir)
                                                            ;; Just use all we have, no need to
                                                            ;; tailor for each test specifically.
                                                            (package-archives '(("archive-a" . ,(expand-file-name "package-archive-a/" (emake--test-dir)))
                                                                                ("archive-b" . ,(expand-file-name "package-archive-b/" (emake--test-dir))))))
                                                        (package-initialize t)
                                                        (package-refresh-contents)
                                                        (package-install-file ,package-file)
                                                        (package-activate ',(package-desc-name descriptor))
                                                        (prin1 (cadr (assq ',(package-desc-name descriptor) package-alist)))
                                                        ,',child-emacs-form))))
         (should (= exit-code 0))
         ;; Cannot compare just like that.
         (let ((installed-desciptor (read stdout)))
           (should (equal (package-desc-name    descriptor) (package-desc-name    installed-desciptor)))
           (should (equal (package-desc-version descriptor) (package-desc-version installed-desciptor)))
           (should (equal (package-desc-reqs    descriptor) (package-desc-reqs    installed-desciptor)))
           (should (equal (package-desc-extras  descriptor) (package-desc-extras  installed-desciptor))))
         ,@body))))


(ert-deftest emake-test-package-1 ()
  (emake--test-packaging "trivial-project"))

(ert-deftest emake-test-package-2 ()
  (emake--test-packaging "project-a"))

(ert-deftest emake-test-package-3 ()
  ;; This project comes with an Info "manual".  Make sure it gets
  ;; installed.
  (emake--test-packaging "project-b" (info "project-b")))

(ert-deftest emake-test-package-4 ()
  (emake--test-packaging "project-c"))

(ert-deftest emake-test-package-5 ()
  (emake--test-packaging "project-d"))

(ert-deftest emake-test-package-6 ()
  (emake--test-packaging "project-e"))


(provide 'test/package)

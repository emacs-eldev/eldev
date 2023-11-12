;;  -*- lexical-binding: t -*-

(require 'test/common)


(defmacro eldev--test-packaging (test-project expected-locatable-files &optional additional-child-emacs-form &rest body)
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
       ;; Test that the generated package can be installed in Emacs and produces the
       ;; expected package descriptor.
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
                                       (load (expand-file-name "./eldev-util.el"))
                                       (eldev--package-install-file ,package-file)
                                       (package-activate ',(package-desc-name descriptor))
                                       (prin1 (cadr (assq ',(package-desc-name descriptor) package-alist)))
                                       (dolist (file ,',expected-locatable-files)
                                         (unless (locate-file file load-path)
                                           (error "Cannot locate file `%s' after loading the generated package" file)))
                                       ,',additional-child-emacs-form)))
         (should (= exit-code 0))
         ;; Cannot compare just like that.
         (let ((installed-desciptor (read stdout)))
           (should (equal (package-desc-name    descriptor) (package-desc-name    installed-desciptor)))
           (should (equal (package-desc-version descriptor) (package-desc-version installed-desciptor)))
           (should (equal (package-desc-reqs    descriptor) (package-desc-reqs    installed-desciptor)))
           (should (equal (package-desc-extras  descriptor) (package-desc-extras  installed-desciptor))))
         ,@body))))


(ert-deftest eldev-package-1 ()
  (eldev--test-packaging "trivial-project" '("trivial-project.el")))

(ert-deftest eldev-package-2 ()
  (eldev--test-packaging "project-a" '("project-a.el")))

(ert-deftest eldev-package-3 ()
  ;; This project comes with an Info "manual".  Make sure it gets
  ;; installed.
  (skip-unless (eldev-makeinfo-executable t))

  (eldev--test-packaging "project-b" '("project-b.el" "project-b.info") (info "project-b")))

(ert-deftest eldev-package-4 ()
  (eldev--test-packaging "project-c" '("project-c.el")))

(ert-deftest eldev-package-5 ()
  (eldev--test-packaging "project-d" '("project-d.el" "project-d-misc.el" "project-d-util.el")))

(ert-deftest eldev-package-6 ()
  (eldev--test-packaging "project-e" '("project-e.el" "project-e-misc.el" "project-e-util.el")))

(ert-deftest eldev-package-7 ()
  ;; Project with special source directories.
  (eldev--test-packaging "project-l" '("project-l.el" "project-l-misc.el" "project-l-util.el" "project-l/simple-resource.txt")))


(provide 'test/package)

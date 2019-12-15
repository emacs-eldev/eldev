(require 'test/common)


(defvar emake--test-targets-trivial-project-main
  '(":default"
    ":package"
    "    dist/trivial-project-1.0.el  [PACK]"
    "        trivial-project.el"
    ":compile"
    "    trivial-project.elc  [ELC]"
    "        trivial-project.el"
    ":package-archive-entry"
    "    dist/trivial-project-1.0.entry  [repeated, see `dist/trivial-project-1.0.el' above]"))

(defvar emake--test-targets-trivial-project-test
  '(":default"))


(defvar emake--test-targets-project-a-main
  '(":default"
    ":package"
    "    dist/project-a-1.0.el  [PACK]"
    "        project-a.el"
    ":compile"
    "    project-a.elc  [ELC]"
    "        project-a.el"
    ":package-archive-entry"
    "    dist/project-a-1.0.entry  [repeated, see `dist/project-a-1.0.el' above]"))

(defvar emake--test-targets-project-a-test
  '(":default"
    ":compile"
    "    test/project-a.elc  [ELC]"
    "        test/project-a.el"
    "        [inh] project-a.elc"))


(defvar emake--test-targets-project-b-main
  '(":default"
    ":package"
    "    dist/project-b-1.0.tar  [PACK]"
    "        dir  [INFO-DIR]"
    "            project-b.info  [MKINFO]"
    "                project-b.texi"
    "        project-b.el"
    "        project-b.info  [repeated, see above]"
    ":compile"
    "    project-b.elc  [ELC]"
    "        project-b.el"
    ":package-archive-entry"
    "    dist/project-b-1.0.entry  [repeated, see `dist/project-b-1.0.tar' above]"))

(defvar emake--test-targets-project-b-test
  '(":default"
    ":compile"
    "    test/project-b.elc  [ELC]"
    "        test/project-b.el"
    "        [inh] project-b.elc"))


(defvar emake--test-targets-project-c-main
  '(":default"
    ":package"
    "    dist/project-c-1.0.tar  [PACK]"
    "        project-c-pkg.el"
    "        project-c.el"
    ":compile"
    ;; We currently don't exclude `no-byte-compile' files from
    ;; targets, but they will not really be built.
    "    project-c-pkg.elc  [ELC]"
    "        project-c-pkg.el"
    "    project-c.elc  [ELC]"
    "        project-c.el"
    ":package-archive-entry"
    "    dist/project-c-1.0.entry  [repeated, see `dist/project-c-1.0.tar' above]"))

(defvar emake--test-targets-project-c-test
  '(":default"
    ":compile"
    "    test/project-c.elc  [ELC]"
    "        test/project-c.el"
    "        [inh] project-c.elc"))


(defvar emake--test-targets-project-d-main
  '(":default"
    ":package"
    "    dist/project-d-1.0.tar  [PACK]"
    "        project-d-misc.el"
    "        project-d-util.el"
    "        project-d.el"
    ":compile"
    "    project-d-misc.elc  [ELC]"
    "        project-d-misc.el"
    "        [inh] project-d-util.elc"
    "    project-d-util.elc  [ELC]"
    "        project-d-util.el"
    "    project-d.elc  [ELC]"
    "        project-d.el"
    "        [inh] project-d-misc.elc"
    ":package-archive-entry"
    "    dist/project-d-1.0.entry  [repeated, see `dist/project-d-1.0.tar' above]"))

(defvar emake--test-targets-project-d-test
  '(":default"))


(defvar emake--test-targets-project-e-main
  '(":default"
    ":package"
    "    dist/project-e-1.0.tar  [PACK]"
    "        project-e-misc.el"
    "        project-e-util.el"
    "        project-e.el"
    ":compile"
    "    project-e-misc.elc  [ELC]"
    "        project-e-misc.el"
    "        [inh] project-e-util.elc"
    "    project-e-util.elc  [ELC]"
    "        project-e-util.el"
    "    project-e.elc  [ELC]"
    "        project-e.el"
    "        [inh] project-e-misc.elc"
    ":package-archive-entry"
    "    dist/project-e-1.0.entry  [repeated, see `dist/project-e-1.0.tar' above]"))

(defvar emake--test-targets-project-e-test
  '(":default"))


(defvar emake--test-targets-project-f-main
  '(":default"
    ":package"
    "    dist/project-f-1.0.tar  [PACK]"
    "        project-f-misc.el"
    "        project-f-no-errors-1.el"
    "        project-f-no-errors-2.el"
    "        project-f-no-errors-3.el"
    "        project-f-no-errors-4.el"
    "        project-f-util.el"
    "        project-f-with-errors-1.el"
    "        project-f-with-errors-2.el"
    "        project-f.el"
    ":compile"
    "    project-f-misc.elc  [ELC]"
    "        project-f-misc.el"
    "    project-f-no-errors-1.elc  [ELC]"
    "        project-f-no-errors-1.el"
    "    project-f-no-errors-2.elc  [ELC]"
    "        project-f-no-errors-2.el"
    "    project-f-no-errors-3.elc  [ELC]"
    "        project-f-no-errors-3.el"
    "    project-f-no-errors-4.elc  [ELC]"
    "        project-f-no-errors-4.el"
    "    project-f-util.elc  [ELC]"
    "        project-f-util.el"
    "    project-f-with-errors-1.elc  [ELC]"
    "        project-f-with-errors-1.el"
    "    project-f-with-errors-2.elc  [ELC]"
    "        project-f-with-errors-2.el"
    "    project-f.elc  [ELC]"
    "        project-f.el"
    ":package-archive-entry"
    "    dist/project-f-1.0.entry  [repeated, see `dist/project-f-1.0.tar' above]"))

(defvar emake--test-targets-project-f-test
  '(":default"))


(defmacro emake--test-project-dependencies (test-project sets &rest targets)
  `(let ((emake--test-project (or ,test-project emake--test-project))
         (expected            ,(if (= (length targets) 1) (car targets) `(emake--test-targets-combine ,@targets))))
     (emake--test-run nil ("targets" ,@sets "--no-dependencies")
       (should (string= stdout (emake--test-lines (emake-filter (not (string-match-p (rx "[" (or "dep" "inh") "]") it)) expected))))
       (should (= exit-code 0)))
     ;; Otherwise we could fail when trying to compile tests in
     ;; "projects" that have no tests.
     (when (emake-any-p (member ":compile" it) (list ,@targets))
       (emake--test-run nil ("clean" ".elc")
         (should (= exit-code 0)))
       ;; In certain test projects compilation fails, so don't test `exit-code'.
       (emake--test-run nil ("compile" ,@(mapcar (lambda (set) (format "--set=%s" set)) sets)))
       (emake--test-run nil ("targets" ,@sets "--dependencies")
         (should (string= stdout (emake--test-lines expected)))
         (should (= exit-code 0))))))

(defun emake--test-targets-combine (&rest lists)
  (let ((targets (copy-sequence (pop lists)))
        section)
    (dolist (list lists)
      (while list
        (let* ((section      (pop list))
               (insert-after (member section targets))
               contents)
          (while (and list (not (emake-virtual-target-p (car list))))
            (push (pop list) contents))
          (while (and (cdr insert-after) (not (emake-virtual-target-p (cadr insert-after))))
            (setf insert-after (cdr insert-after)))
          (if insert-after
              (setf (cdr insert-after) (nconc (nreverse contents) (cdr insert-after)))
            (nconc targets `(,section ,@(nreverse contents)))))))
    targets))


(ert-deftest emake-test-targets-trivial-project-1 ()
  (emake--test-project-dependencies "trivial-project" () emake--test-targets-trivial-project-main))

(ert-deftest emake-test-targets-trivial-project-2 ()
  (emake--test-project-dependencies "trivial-project" ("main") emake--test-targets-trivial-project-main))

(ert-deftest emake-test-targets-trivial-project-3 ()
  (emake--test-project-dependencies "trivial-project" ("test") emake--test-targets-trivial-project-test))

(ert-deftest emake-test-targets-trivial-project-4 ()
  (emake--test-project-dependencies "trivial-project" ("all") emake--test-targets-trivial-project-main emake--test-targets-trivial-project-test))


(ert-deftest emake-test-targets-project-a-1 ()
  (emake--test-project-dependencies "project-a" () emake--test-targets-project-a-main))

(ert-deftest emake-test-targets-project-a-2 ()
  (emake--test-project-dependencies "project-a" ("main") emake--test-targets-project-a-main))

(ert-deftest emake-test-targets-project-a-3 ()
  (emake--test-project-dependencies "project-a" ("test") emake--test-targets-project-a-test))

(ert-deftest emake-test-targets-project-a-4 ()
  (emake--test-project-dependencies "project-a" ("all") emake--test-targets-project-a-main emake--test-targets-project-a-test))


(ert-deftest emake-test-targets-project-b-1 ()
  (emake--test-project-dependencies "project-b" () emake--test-targets-project-b-main))

(ert-deftest emake-test-targets-project-b-2 ()
  (emake--test-project-dependencies "project-b" ("main") emake--test-targets-project-b-main))

(ert-deftest emake-test-targets-project-b-3 ()
  (emake--test-project-dependencies "project-b" ("test") emake--test-targets-project-b-test))

(ert-deftest emake-test-targets-project-b-4 ()
  (emake--test-project-dependencies "project-b" ("all") emake--test-targets-project-b-main emake--test-targets-project-b-test))


(ert-deftest emake-test-targets-project-c-1 ()
  (emake--test-project-dependencies "project-c" () emake--test-targets-project-c-main))

(ert-deftest emake-test-targets-project-c-2 ()
  (emake--test-project-dependencies "project-c" ("main") emake--test-targets-project-c-main))

(ert-deftest emake-test-targets-project-c-3 ()
  (emake--test-project-dependencies "project-c" ("test") emake--test-targets-project-c-test))

(ert-deftest emake-test-targets-project-c-4 ()
  (emake--test-project-dependencies "project-c" ("all") emake--test-targets-project-c-main emake--test-targets-project-c-test))


(ert-deftest emake-test-targets-project-d-1 ()
  (emake--test-project-dependencies "project-d" () emake--test-targets-project-d-main))

(ert-deftest emake-test-targets-project-d-2 ()
  (emake--test-project-dependencies "project-d" ("main") emake--test-targets-project-d-main))

(ert-deftest emake-test-targets-project-d-3 ()
  (emake--test-project-dependencies "project-d" ("test") emake--test-targets-project-d-test))

(ert-deftest emake-test-targets-project-d-4 ()
  (emake--test-project-dependencies "project-d" ("all") emake--test-targets-project-d-main emake--test-targets-project-d-test))


(ert-deftest emake-test-targets-project-e-1 ()
  (emake--test-project-dependencies "project-e" () emake--test-targets-project-e-main))

(ert-deftest emake-test-targets-project-e-2 ()
  (emake--test-project-dependencies "project-e" ("main") emake--test-targets-project-e-main))

(ert-deftest emake-test-targets-project-e-3 ()
  (emake--test-project-dependencies "project-e" ("test") emake--test-targets-project-e-test))

(ert-deftest emake-test-targets-project-e-4 ()
  (emake--test-project-dependencies "project-e" ("all") emake--test-targets-project-e-main emake--test-targets-project-e-test))


(ert-deftest emake-test-targets-project-f-1 ()
  (emake--test-project-dependencies "project-f" () emake--test-targets-project-f-main))

(ert-deftest emake-test-targets-project-f-2 ()
  (emake--test-project-dependencies "project-f" ("main") emake--test-targets-project-f-main))

(ert-deftest emake-test-targets-project-f-3 ()
  (emake--test-project-dependencies "project-f" ("test") emake--test-targets-project-f-test))

(ert-deftest emake-test-targets-project-f-4 ()
  (emake--test-project-dependencies "project-f" ("all") emake--test-targets-project-f-main emake--test-targets-project-f-test))


(provide 'test/targets)

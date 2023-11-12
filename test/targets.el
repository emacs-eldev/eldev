;;  -*- lexical-binding: t -*-

(require 'test/common)


(defvar eldev--test-targets-trivial-project-main
  '(":default"
    ":package"
    "    dist/trivial-project-1.0.el  [PACK]"
    "        trivial-project.el"
    ":compile"
    "    trivial-project.elc  [ELC]"
    "        trivial-project.el"
    ":package-archive-entry"
    "    dist/trivial-project-1.0.entry  [repeated, see `dist/trivial-project-1.0.el' above]"))

(defvar eldev--test-targets-trivial-project-test
  '(":default"))


(defvar eldev--test-targets-project-a-main
  '(":default"
    ":package"
    "    dist/project-a-1.0.el  [PACK]"
    "        project-a.el"
    ":compile"
    "    project-a.elc  [ELC]"
    "        project-a.el"
    ":package-archive-entry"
    "    dist/project-a-1.0.entry  [repeated, see `dist/project-a-1.0.el' above]"))

(defvar eldev--test-targets-project-a-test
  '(":default"
    ":compile"
    "    test/project-a.elc  [ELC]"
    "        test/project-a.el"
    "        [inh] project-a.elc"))


(defvar eldev--test-targets-project-b-main
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

(defvar eldev--test-targets-project-b-test
  '(":default"
    ":compile"
    "    test/project-b.elc  [ELC]"
    "        test/project-b.el"
    "        [inh] project-b.elc"))


(defvar eldev--test-targets-project-c-main
  '(":default"
    ":package"
    "    dist/project-c-1.0.tar  [PACK]"
    "        project-c-pkg.el"
    "        project-c.el"
    ":compile"
    ;; `project-c-pkg.elc' is now omitted because of `eldev-build-ignored-target-fileset'.
    "    project-c.elc  [ELC]"
    "        project-c.el"
    ":package-archive-entry"
    "    dist/project-c-1.0.entry  [repeated, see `dist/project-c-1.0.tar' above]"))

(defvar eldev--test-targets-project-c-test
  '(":default"
    ":compile"
    "    test/dummy.elc  [ELC]"
    "        test/dummy.el"
    "    test/project-c.elc  [ELC]"
    "        test/project-c.el"
    "        [inh] project-c.elc"
    "        [inh] test/dummy.elc"))


(defvar eldev--test-targets-project-d-main
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

(defvar eldev--test-targets-project-d-test
  '(":default"
    ":compile"
    "    test/project-d.elc  [ELC]"
    "        test/project-d.el"
    "        [inh] project-d.elc"))


(defvar eldev--test-targets-project-e-main
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

(defvar eldev--test-targets-project-e-test
  '(":default"
    ":compile"
    "    test/project-e.elc  [ELC]"
    "        test/project-e.el"
    "        [inh] project-e.elc"))


(defvar eldev--test-targets-project-f-main
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

(defvar eldev--test-targets-project-f-test
  '(":default"))


(defvar eldev--test-targets-project-g-main
  '(":default"
    ":package"
    "    dist/project-g-1.0.tar  [PACK]"
    "        project-g-util.el"
    "        project-g.el"
    ":compile"
    "    project-g-util.elc  [ELC]"
    "        project-g-util.el"
    ;; Note the unusual dependency; however, this is correct, see project files.
    "        [inh] project-g.elc"
    "    project-g.elc  [ELC]"
    "        project-g.el"
    ":package-archive-entry"
    "    dist/project-g-1.0.entry  [repeated, see `dist/project-g-1.0.tar' above]"))

(defvar eldev--test-targets-project-g-test
  '(":default"
    ":compile"
    "    test/test-g-1.elc  [ELC]"
    "        test/test-g-1.el"
    "        [inh] test/test-g-util.elc"
    "    test/test-g-integration.elc  [ELC]"
    "        test/test-g-integration.el"
    "        [inh] test/test-g-util.elc"
    "    test/test-g-util.elc  [ELC]"
    "        test/test-g-util.el"
    "        [inh] project-g.elc"))


(defvar eldev--test-targets-project-l-main
  '(":default"
    "    :autoloads"
    "        src/project-l-autoloads.el  [AUTOLOADS]"
    "            src/project-l-misc.el"
    "            src/project-l-util.el"
    "            src/project-l.el"
    ":package"
    "    dist/project-l-1.0.tar  [PACK]"
    "        resources/project-l/simple-resource.txt"
    "        src/project-l-misc.el"
    "        src/project-l-util.el"
    "        src/project-l.el"
    ":compile"
    "    src/project-l-misc.elc  [ELC]"
    "        src/project-l-misc.el"
    "        [dep] src/project-l-autoloads.el"
    "        [inh] src/project-l-util.elc"
    "    src/project-l-util.elc  [ELC]"
    "        src/project-l-util.el"
    "        [dep] src/project-l-autoloads.el"
    "    src/project-l.elc  [ELC]"
    "        src/project-l.el"
    "        [dep] src/project-l-autoloads.el"
    "        [inh] src/project-l-misc.elc"
    ":package-archive-entry"
    "    dist/project-l-1.0.entry  [repeated, see `dist/project-l-1.0.tar' above]"))

(defvar eldev--test-targets-project-l-test
  '(":default"
    ":compile"
    "    test/dummy.elc  [ELC]"
    "        test/dummy.el"
    "    test/project-l.elc  [ELC]"
    "        test/project-l.el"
    "        [inh] src/project-l.elc"
    "        [inh] test/dummy.elc"))


(defsubst eldev--test-targets-dependency-line-p (line)
  (string-match-p (rx "[" (or "dep" "inh") "]") line))

(defmacro eldev--test-project-dependencies (test-project sets &rest targets)
  `(let ((eldev--test-project (or ,test-project eldev--test-project))
         (expected            ,(if (= (length targets) 1) (car targets) `(eldev--test-targets-combine ,@targets))))
     (eldev--test-run nil ("targets" ,@sets "--no-dependencies")
       (should (string= stdout (eldev--test-lines (eldev-filter (not (eldev--test-targets-dependency-line-p it)) expected))))
       (should (= exit-code 0)))
     (eldev--test-run nil ("targets" ,@sets "--no-dependencies" "--no-sources")
       (should (string= stdout (eldev--test-lines (eldev-filter (and (not (eldev--test-targets-dependency-line-p it))
                                                                     (string-match-p (rx (or ":" "[")) it))
                                                                expected))))
       (should (= exit-code 0)))
     ;; FIXME: Also test `concise' mode.
     ;; Otherwise we could fail when trying to compile tests in
     ;; "projects" that have no tests.
     (when (eldev-any-p (member ":compile" it) (list ,@targets))
       ;; Cleaning Eldev cache also removes all previously collected information about
       ;; target dependencies.
       (eldev--test-run nil ("clean" ".elc" "eldev-cache")
         (should (= exit-code 0)))
       (let (compilation)
         (eldev--test-run nil ("compile" ,@(mapcar (lambda (set) (format "--set=%s" set)) sets))
           ;; In certain test projects compilation fails, so don't test `exit-code' here.
           (setf compilation call-info))
         (eldev--test-run nil ("targets" ,@sets "--dependencies")
           :previous-call "Test project compilation" compilation
           (if (= (plist-get compilation :exit-code) 0)
               (should (string= stdout (eldev--test-lines expected)))
             ;; If compilation hasn't succeeded, don't insist on dependencies having been
             ;; fully discovered.
             (let ((actual   (eldev--test-line-list stdout))
                   ;; To resolve quotes.
                   (expected (eldev--test-line-list (eldev--test-lines expected))))
               (while (or actual expected)
                 (if (and actual expected (string= (car actual) (car expected)))
                     (setf actual   (cdr actual)
                           expected (cdr expected))
                   (if (and expected (eldev--test-targets-dependency-line-p (car expected))
                            (or (null actual) (not (eldev--test-targets-dependency-line-p (car actual)))))
                       (setf expected (cdr expected))
                     (eldev-warn "%S %S" actual expected)
                     (ert-fail "Unexpected output"))))))
           (should (= exit-code 0)))))))

(defun eldev--test-targets-combine (&rest lists)
  (let ((targets (copy-sequence (pop lists))))
    (dolist (list lists)
      (while list
        (let* ((section      (pop list))
               (insert-after (member section targets))
               contents)
          (while (and list (not (eldev-virtual-target-p (car list))))
            (push (pop list) contents))
          (while (and (cdr insert-after) (not (eldev-virtual-target-p (cadr insert-after))))
            (setf insert-after (cdr insert-after)))
          (if insert-after
              (setf (cdr insert-after) (nconc (nreverse contents) (cdr insert-after)))
            (nconc targets `(,section ,@(nreverse contents)))))))
    targets))


(ert-deftest eldev-targets-trivial-project-1 ()
  (eldev--test-project-dependencies "trivial-project" () eldev--test-targets-trivial-project-main))

(ert-deftest eldev-targets-trivial-project-2 ()
  (eldev--test-project-dependencies "trivial-project" ("main") eldev--test-targets-trivial-project-main))

(ert-deftest eldev-targets-trivial-project-3 ()
  (eldev--test-project-dependencies "trivial-project" ("test") eldev--test-targets-trivial-project-test))

(ert-deftest eldev-targets-trivial-project-4 ()
  (eldev--test-project-dependencies "trivial-project" ("all") eldev--test-targets-trivial-project-main eldev--test-targets-trivial-project-test))


(ert-deftest eldev-targets-project-a-1 ()
  (eldev--test-project-dependencies "project-a" () eldev--test-targets-project-a-main))

(ert-deftest eldev-targets-project-a-2 ()
  (eldev--test-project-dependencies "project-a" ("main") eldev--test-targets-project-a-main))

(ert-deftest eldev-targets-project-a-3 ()
  (eldev--test-project-dependencies "project-a" ("test") eldev--test-targets-project-a-test))

(ert-deftest eldev-targets-project-a-4 ()
  (eldev--test-project-dependencies "project-a" ("all") eldev--test-targets-project-a-main eldev--test-targets-project-a-test))


(ert-deftest eldev-targets-project-b-1 ()
  (eldev--test-project-dependencies "project-b" () eldev--test-targets-project-b-main))

(ert-deftest eldev-targets-project-b-2 ()
  (eldev--test-project-dependencies "project-b" ("main") eldev--test-targets-project-b-main))

(ert-deftest eldev-targets-project-b-3 ()
  (eldev--test-project-dependencies "project-b" ("test") eldev--test-targets-project-b-test))

(ert-deftest eldev-targets-project-b-4 ()
  (eldev--test-project-dependencies "project-b" ("all") eldev--test-targets-project-b-main eldev--test-targets-project-b-test))


(ert-deftest eldev-targets-project-c-1 ()
  (eldev--test-project-dependencies "project-c" () eldev--test-targets-project-c-main))

(ert-deftest eldev-targets-project-c-2 ()
  (eldev--test-project-dependencies "project-c" ("main") eldev--test-targets-project-c-main))

(ert-deftest eldev-targets-project-c-3 ()
  (eldev--test-project-dependencies "project-c" ("test") eldev--test-targets-project-c-test))

(ert-deftest eldev-targets-project-c-4 ()
  (eldev--test-project-dependencies "project-c" ("all") eldev--test-targets-project-c-main eldev--test-targets-project-c-test))


(ert-deftest eldev-targets-project-d-1 ()
  (eldev--test-project-dependencies "project-d" () eldev--test-targets-project-d-main))

(ert-deftest eldev-targets-project-d-2 ()
  (eldev--test-project-dependencies "project-d" ("main") eldev--test-targets-project-d-main))

(ert-deftest eldev-targets-project-d-3 ()
  (eldev--test-project-dependencies "project-d" ("test") eldev--test-targets-project-d-test))

(ert-deftest eldev-targets-project-d-4 ()
  (eldev--test-project-dependencies "project-d" ("all") eldev--test-targets-project-d-main eldev--test-targets-project-d-test))


(ert-deftest eldev-targets-project-e-1 ()
  (eldev--test-project-dependencies "project-e" () eldev--test-targets-project-e-main))

(ert-deftest eldev-targets-project-e-2 ()
  (eldev--test-project-dependencies "project-e" ("main") eldev--test-targets-project-e-main))

(ert-deftest eldev-targets-project-e-3 ()
  (eldev--test-project-dependencies "project-e" ("test") eldev--test-targets-project-e-test))

(ert-deftest eldev-targets-project-e-4 ()
  (eldev--test-project-dependencies "project-e" ("all") eldev--test-targets-project-e-main eldev--test-targets-project-e-test))


(ert-deftest eldev-targets-project-f-1 ()
  (eldev--test-project-dependencies "project-f" () eldev--test-targets-project-f-main))

(ert-deftest eldev-targets-project-f-2 ()
  (eldev--test-project-dependencies "project-f" ("main") eldev--test-targets-project-f-main))

(ert-deftest eldev-targets-project-f-3 ()
  (eldev--test-project-dependencies "project-f" ("test") eldev--test-targets-project-f-test))

(ert-deftest eldev-targets-project-f-4 ()
  (eldev--test-project-dependencies "project-f" ("all") eldev--test-targets-project-f-main eldev--test-targets-project-f-test))


(ert-deftest eldev-targets-project-g-1 ()
  (eldev--test-project-dependencies "project-g" () eldev--test-targets-project-g-main))

(ert-deftest eldev-targets-project-g-2 ()
  (eldev--test-project-dependencies "project-g" ("main") eldev--test-targets-project-g-main))

(ert-deftest eldev-targets-project-g-3 ()
  (eldev--test-project-dependencies "project-g" ("test") eldev--test-targets-project-g-test))

(ert-deftest eldev-targets-project-g-4 ()
  (eldev--test-project-dependencies "project-g" ("all") eldev--test-targets-project-g-main eldev--test-targets-project-g-test))


(ert-deftest eldev-targets-project-l-1 ()
  (eldev--test-project-dependencies "project-l" () eldev--test-targets-project-l-main))

(ert-deftest eldev-targets-project-l-2 ()
  (eldev--test-project-dependencies "project-l" ("main") eldev--test-targets-project-l-main))

(ert-deftest eldev-targets-project-l-3 ()
  (eldev--test-project-dependencies "project-l" ("test") eldev--test-targets-project-l-test))

(ert-deftest eldev-targets-project-l-4 ()
  (eldev--test-project-dependencies "project-l" ("all") eldev--test-targets-project-l-main eldev--test-targets-project-l-test))


(provide 'test/targets)

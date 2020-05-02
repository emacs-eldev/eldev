(require 'eldev)
(require 'ert)


(defvar eldev--test-project nil
  "All `test-project' arguments default to this value.")

(defvar eldev--test-eldev-local nil)
(defvar eldev--test-eldev-dir nil)

(defvar eldev--test-shell-command (expand-file-name "bin/eldev" eldev-project-dir))

(defvar eldev--test-packaged-self nil)


(defun eldev--test-dir ()
  (file-name-as-directory (expand-file-name "test" eldev-project-dir)))

(defun eldev--test-tmp-subdir (name)
  (expand-file-name name (expand-file-name "test/.tmp" eldev-project-dir)))

(defun eldev--test-project-dir (&optional test-project)
  (file-name-as-directory (expand-file-name (or test-project eldev--test-project) (eldev--test-dir))))

(defun eldev--test-project-cache-dir (&optional test-project)
  (expand-file-name eldev-cache-dir (eldev--test-project-dir test-project)))

(defun eldev--test-delete-cache (&optional test-project)
  (let ((dir (eldev--test-project-cache-dir)))
    (when (file-exists-p dir)
      (ignore-errors (delete-directory dir t)))))

(defmacro eldev--test-call-process (program-name executable command-line &rest body)
  (declare (indent 3) (debug (sexp body)))
  (let ((no-errors (make-symbol "$no-errors")))
    `(with-temp-buffer
       (let ((stderr-file (make-temp-file "eldev-stderr-")))
         (unwind-protect
             (let* ((exit-code (call-process ,executable nil (list (current-buffer) stderr-file) nil ,@command-line))
                    (stdout    (buffer-string))
                    (stderr    (with-temp-buffer
                                 (insert-file-contents stderr-file)
                                 (buffer-string)))
                    ,no-errors)
               (goto-char 1)
               (unwind-protect
                   (prog1 (progn ,@body)
                     (setf ,no-errors t))
                 (unless ,no-errors
                   (eldev-warn "Ran %s as `%s' in directory `%s'" ,program-name (mapconcat #'eldev-quote-sh-string (list ,executable ,@command-line) " ") default-directory)
                   (eldev-warn "Stdout contents:\n%s" (eldev-colorize stdout 'verbose))
                   (unless (string= stderr "")
                     (eldev-warn "Stderr contents:\n%s" (eldev-colorize stderr 'verbose)))
                   (eldev-warn "Process exit code: %s" exit-code))))
           (delete-file stderr-file))))))

(defmacro eldev--test-run (test-project command-line &rest body)
  "Execute child Eldev process.
BODY is executed with output bound as STDOUT and STDERR
variables.  Stdout output is also available to BODY in the
current (temporary) buffer, with the point positioned at the
beginning.  Exit code of the process is bound as EXIT-CODE."
  (declare (indent 2) (debug (stringp sexp body)))
  `(let* ((eldev--test-project (or ,test-project eldev--test-project))
          (default-directory   (eldev--test-project-dir ,test-project))
          (process-environment `(,(format "ELDEV_LOCAL=%s" (or eldev--test-eldev-local
                                                               ;; When in mode `packaged', also generate a package
                                                               ;; for spawned processes to use.
                                                               (when (and (eq eldev-project-loading-mode 'packaged) (not (eq eldev--test-packaged-self 'in-process)))
                                                                 (unless eldev--test-packaged-self
                                                                   ;; Set to prevent recursive reentry.
                                                                   (setf eldev--test-packaged-self 'in-process)
                                                                   (eldev--test-create-eldev-archive "packaged-self")
                                                                   (setf eldev--test-packaged-self t))
                                                                 (concat ":pa:" (eldev--test-tmp-subdir "packaged-self")))
                                                               ;; Otherwise, let the child processes use the sources
                                                               ;; (maybe byte-compiled) directly.
                                                               eldev-project-dir))
                                 ,(format "ELDEV_DIR=%s"   (or eldev--test-eldev-dir   (eldev--test-tmp-subdir "stdroot")))
                                 ,@process-environment)))
     (eldev--test-call-process "Eldev" eldev--test-shell-command ,command-line
       ,@body)))


(defmacro eldev--test-capture-output (&rest body)
  `(with-temp-buffer
     (let ((standard-output         (current-buffer))
           (eldev-output-time-diffs nil)
           (eldev-coloring-mode     nil))
       ,@body
       (buffer-string))))

(defun eldev--test-first-line (text)
  (replace-regexp-in-string (rx bos (0+ nonl) (group (0+ anything)) eos) "" text t t 1))

(defun eldev--test-lines (&rest arguments)
  (let ((formatter   nil)
        (trailing-lf "\n"))
    (while (keywordp (car arguments))
      (eldev-pcase-exhaustive (pop arguments)
        (:fmt    (setf formatter nil))
        (:nofmt  (setf formatter #'identity))
        (:lf     (setf trailing-lf "\n"))
        (:nolf   (setf trailing-lf ""))))
    (concat (mapconcat (or formatter (lambda (line) (eldev-format-message (replace-regexp-in-string "%" "%%" line)))) (eldev-flatten-tree arguments) "\n")
            trailing-lf)))

(defmacro eldev--test-in-project-environment (&rest body)
  `(let ((eldev-project-dir   (eldev--test-project-dir))
         (eldev-shell-command eldev--test-shell-command))
     ,@body))


(defmacro eldev--test-without-files (test-project files-to-delete &rest body)
  (declare (indent 2)
           (debug (stringp sexp body)))
  `(let ((eldev--test-project (or ,test-project eldev--test-project)))
     (eldev--test-delete-quietly nil ',files-to-delete)
     (let* ((project-dir       (eldev--test-project-dir))
            (preexisting-files (eldev--test-find-files project-dir)))
       (unwind-protect
           (progn ,@body)
         (eldev--test-delete-quietly nil ',files-to-delete)))))

(declare-function directory-files-recursively "subr" t)

(defun eldev--test-find-files (directory)
  ;; Not using `eldev-find-files' if easily possible (Emacs 25 and
  ;; up) to avoid tests depending on its correctness.
  (let (files)
    (if (fboundp #'directory-files-recursively)
        (dolist (file (directory-files-recursively directory (rx (1+ any))))
          (let ((relative-name (file-relative-name file directory)))
            (unless (string-match-p (rx "." (or "eldev" "git") "/") relative-name)
              (push relative-name files))))
      (setf files (eldev-find-files '("!.eldev/" "!.git/") nil directory)))
    (sort files #'string<)))

(defun eldev--test-delete-quietly (test-project files)
  (let ((dir (eldev--test-project-dir test-project)))
    (dolist (file (eldev-listify files))
      (ignore-errors (delete-file (expand-file-name file dir))))))


(defun eldev--test-assert-files (directory &rest arguments)
  (eldev--test-assert-unsorted (eldev--test-find-files directory) (eldev-flatten-tree arguments) (eldev-format-message "Wrong files in `%s'" directory)))

(defun eldev--test-assert-building (stdout targets)
  (eldev--test-assert-unsorted (split-string stdout "\n" t)
                               (mapcar (lambda (target)
                                         (format "%-8s %s" (or (car (cdr-safe target)) "ELC") (if (consp target) (car target) target)))
                                       targets)
                               "Wrong building steps"))

(defun eldev--test-assert-unsorted (actual expected &optional error-header)
  (setf actual   (sort (copy-sequence actual)   #'string<)
        expected (sort (copy-sequence expected) #'string<))
  (let (unexpected
        missing
        problems)
    (while (or actual expected)
      (cond ((and actual expected (string= (car actual) (car expected)))
             (setf actual   (cdr actual)
                   expected (cdr expected)))
            ((or (not expected) (and actual expected (string< (car actual) (car expected))))
             (push (pop actual) unexpected))
            (t
             (push (pop expected) missing))))
    (when missing
      (push (eldev-format-message "%s missing" (eldev-message-enumerate nil (nreverse missing))) problems))
    (when unexpected
      (push (eldev-format-message "%s unexpected" (eldev-message-enumerate nil (nreverse unexpected))) problems))
    (when problems
      (ert-fail (format "%s: %s" (or error-header "Expectations failed") (eldev-message-enumerate nil problems nil t t))))))

(defun eldev--test-create-eldev-archive (archive-name &optional forced-version)
  (let ((archive-dir (eldev--test-tmp-subdir archive-name)))
    (ignore-errors (delete-directory archive-dir t))
    (if forced-version
        (eldev--test-run ".." ("--setup" (prin1-to-string `(setf eldev-dist-dir ,archive-dir))
                               "package" "--entry-file" "--force-version" forced-version)
          (should (= exit-code 0)))
      (eldev--test-run ".." ("--setup" (prin1-to-string `(setf eldev-dist-dir ,archive-dir))
                             "package" "--entry-file")
        (should (= exit-code 0))))
    (with-temp-file (expand-file-name "archive-contents" archive-dir)
      (prin1 `(1 ,(with-temp-buffer
                    (insert-file-contents-literally (expand-file-name (format "eldev-%s.entry" (or forced-version (eldev-message-version (eldev-package-descriptor)))) archive-dir))
                    (read (current-buffer))))
             (current-buffer)))))


(provide 'test/common)

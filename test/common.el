(require 'emake)
(require 'ert)


(defvar emake--test-project nil
  "All `test-project' arguments default to this value.")

(defvar emake--test-emake-local nil)
(defvar emake--test-emake-dir nil)

(defvar emake--test-shell-command (expand-file-name "bin/emake" emake-project-dir))


(defun emake--test-dir ()
  (file-name-as-directory (expand-file-name "test" emake-project-dir)))

(defun emake--test-tmp-subdir (name)
  (expand-file-name name (expand-file-name "test/.tmp" emake-project-dir)))

(defun emake--test-project-dir (&optional test-project)
  (file-name-as-directory (expand-file-name (or test-project emake--test-project) (emake--test-dir))))

(defun emake--test-project-cache-dir (&optional test-project)
  (expand-file-name emake-cache-dir (emake--test-project-dir test-project)))

(defun emake--test-delete-cache (&optional test-project)
  (let ((dir (emake--test-project-cache-dir)))
    (when (file-exists-p dir)
      (ignore-errors (delete-directory dir t)))))

(defmacro emake--test-call-process (program-name executable command-line &rest body)
  (declare (indent 3) (debug (sexp body)))
  (let ((no-errors (make-symbol "$no-errors")))
    `(with-temp-buffer
       (let ((stderr-file (make-temp-file "emake-stderr-")))
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
                   (emake-warn "Ran %s as `%s' in directory `%s'" ,program-name (mapconcat #'emake-quote-sh-string (list ,executable ,@command-line) " ") default-directory)
                   (emake-warn "Stdout contents:\n%s" (emake-colorize stdout 'verbose))
                   (unless (string= stderr "")
                     (emake-warn "Stderr contents:\n%s" (emake-colorize stderr 'verbose)))
                   (emake-warn "Process exit code: %s" exit-code))))
           (delete-file stderr-file))))))

(defmacro emake--test-run (test-project command-line &rest body)
  "Execute child Emake process.
BODY is executed with output bound as STDOUT and STDERR
variables.  Stdout output is also available to BODY in the
current (temporary) buffer, with the point positioned at the
beginning.  Exit code of the process is bound as EXIT-CODE."
  (declare (indent 2) (debug (stringp sexp body)))
  `(let* ((emake--test-project (or ,test-project emake--test-project))
          (default-directory   (emake--test-project-dir ,test-project))
          (process-environment `(,(format "EMAKE_LOCAL=%s" (or emake--test-emake-local emake-project-dir))
                                 ,(format "EMAKE_DIR=%s"   (or emake--test-emake-dir   (emake--test-tmp-subdir "stdroot")))
                                 ,@process-environment)))
     (emake--test-call-process "Emake" emake--test-shell-command ,command-line
       ,@body)))


(defmacro emake--test-capture-output (&rest body)
  `(with-temp-buffer
     (let ((standard-output         (current-buffer))
           (emake-output-time-diffs nil)
           (emake-coloring-mode     nil))
       ,@body
       (buffer-string))))

(defun emake--test-first-line (text)
  (replace-regexp-in-string (rx bos (0+ nonl) (group (0+ anything)) eos) "" text t t 1))

(defun emake--test-lines (&rest arguments)
  (let ((formatter   nil)
        (trailing-lf "\n"))
    (while (keywordp (car arguments))
      (emake-pcase-exhaustive (pop arguments)
        (:fmt    (setf formatter nil))
        (:nofmt  (setf formatter #'identity))
        (:lf     (setf trailing-lf "\n"))
        (:nolf   (setf trailing-lf ""))))
    (concat (mapconcat (or formatter (lambda (line) (emake-format-message (replace-regexp-in-string "%" "%%" line)))) (emake-flatten-tree arguments) "\n")
            trailing-lf)))

(defmacro emake--test-in-project-environment (&rest body)
  `(let ((emake-project-dir   (emake--test-project-dir))
         (emake-shell-command emake--test-shell-command))
     ,@body))


(defmacro emake--test-without-files (test-project files-to-delete &rest body)
  (declare (indent 2)
           (debug (stringp sexp body)))
  `(let ((emake--test-project (or ,test-project emake--test-project)))
     (emake--test-delete-quietly nil ',files-to-delete)
     (let* ((project-dir       (emake--test-project-dir))
            (preexisting-files (emake--test-find-files project-dir)))
       (unwind-protect
           (progn ,@body)
         (emake--test-delete-quietly nil ',files-to-delete)))))

(defun emake--test-find-files (directory)
  ;; Not using `emake-find-files' if easily possible (Emacs 25 and
  ;; up) to avoid tests depending on its correctness.
  (let (files)
    (if (fboundp #'directory-files-recursively)
        (dolist (file (directory-files-recursively directory (rx (1+ any))))
          (let ((relative-name (file-relative-name file directory)))
            (unless (string-match-p (rx "." (or "emake" "git") "/") relative-name)
              (push relative-name files))))
      (setf files (emake-find-files '("!.emake/" "!.git/") nil directory)))
    (sort files #'string<)))

(defun emake--test-delete-quietly (test-project files)
  (let ((dir (emake--test-project-dir test-project)))
    (dolist (file (emake-listify files))
      (ignore-errors (delete-file (expand-file-name file dir))))))


(defun emake--test-assert-files (directory &rest arguments)
  (emake--test-assert-unsorted (emake--test-find-files directory) (emake-flatten-tree arguments) (emake-format-message "Wrong files in `%s'" directory)))

(defun emake--test-assert-building (stdout targets)
  (emake--test-assert-unsorted (split-string stdout "\n" t)
                               (mapcar (lambda (target)
                                         (format "%-8s %s" (or (car (cdr-safe target)) "ELC") (if (consp target) (car target) target)))
                                       targets)
                               "Wrong building steps"))

(defun emake--test-assert-unsorted (actual expected &optional error-header)
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
      (push (emake-format-message "%s missing" (emake-message-enumerate nil (nreverse missing))) problems))
    (when unexpected
      (push (emake-format-message "%s unexpected" (emake-message-enumerate nil (nreverse unexpected))) problems))
    (when problems
      (ert-fail (format "%s: %s" (or error-header "Expectations failed") (emake-message-enumerate nil problems nil t t))))))

(defun emake--test-create-emake-archive (archive-name &optional forced-version)
  (let ((archive-dir (emake--test-tmp-subdir archive-name)))
    (ignore-errors (delete-directory archive-dir t))
    (if forced-version
        (emake--test-run ".." ("--setup" (prin1-to-string `(setf emake-dist-dir ,archive-dir))
                               "package" "--entry-file" "--force-version" forced-version)
          (should (= exit-code 0)))
      (emake--test-run ".." ("--setup" (prin1-to-string `(setf emake-dist-dir ,archive-dir))
                             "package" "--entry-file")
        (should (= exit-code 0))))
    (with-temp-file (expand-file-name "archive-contents" archive-dir)
      (prin1 `(1 ,(with-temp-buffer
                    (insert-file-contents-literally (expand-file-name (format "emake-%s.entry" (or forced-version (emake-message-version (emake-package-descriptor)))) archive-dir))
                    (read (current-buffer))))
             (current-buffer)))))


(provide 'test/common)

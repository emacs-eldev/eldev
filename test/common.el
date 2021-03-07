(require 'eldev)
(require 'eldev-vc)
(require 'ert)
(require 'subr-x)


(defvar eldev--test-project nil
  "All `test-project' arguments default to this value.")

(defvar eldev--test-eldev-local nil)
(defvar eldev--test-eldev-dir nil)

(defvar eldev--test-shell-command (expand-file-name "bin/eldev" eldev-project-dir))

(defvar eldev--test-packaged-self nil)


(defun eldev--test-dir ()
  (file-name-as-directory (expand-file-name "test" eldev-project-dir)))

(defun eldev--test-tmp-subdir (name)
  (expand-file-name name (expand-file-name (format "test/.tmp/%s.%s" emacs-major-version emacs-minor-version) eldev-project-dir)))

(defun eldev--test-eldev-dir ()
  (or eldev--test-eldev-dir (eldev--test-tmp-subdir "stdroot")))


(defun eldev--test-project-dir (&optional test-project)
  (file-name-as-directory (expand-file-name (or test-project eldev--test-project) (eldev--test-dir))))

(defun eldev--test-project-cache-dir (&optional test-project)
  (expand-file-name eldev-cache-dir (eldev--test-project-dir test-project)))

(defun eldev--test-delete-cache (&optional test-project)
  (let ((dir (eldev--test-project-cache-dir test-project)))
    (when (file-exists-p dir)
      (ignore-errors (delete-directory dir t)))))

(defmacro eldev--test-call-process (program-name executable command-line &rest body)
  (declare (indent 3) (debug (sexp body)))
  (let ((actual-command-line `(list ,@command-line))
        (no-errors           (make-symbol "$no-errors"))
        process-input-form
        important-files)
    (pcase command-line
      (`(:eval ,expression) (setf actual-command-line expression)))
    (while (keywordp (car body))
      (eldev-pcase-exhaustive (pop body)
        (:process-input   (setf process-input-form (pop body)))
        (:important-files (setf important-files    (eldev-listify (pop body))))))
    `(let* ((process-input      ,process-input-form)
            (process-input-file (when process-input (make-temp-file "eldev-test")))
            (stderr-file        (make-temp-file "eldev-stderr-")))
       (when process-input-file
         (with-temp-file process-input-file
           (insert process-input)))
       (unwind-protect
           ;; Using `eldev-call-process' here mostly for a bit of testing for it.
           (eldev-call-process ,executable (mapcar (lambda (argument) (if (stringp argument) argument (prin1-to-string argument))) ,actual-command-line)
             :destination `(t ,stderr-file)
             :infile      process-input-file
             (let ((stdout (buffer-string))
                   (stderr (with-temp-buffer
                             (insert-file-contents stderr-file)
                             (buffer-string)))
                   ,no-errors)
               (goto-char 1)
               (unwind-protect
                   (condition-case error
                       (prog1 (progn ,@body)
                         (setf ,no-errors t))
                     (ert-test-skipped (setf ,no-errors t)
                                       (signal (car error) (cdr error))))
                 (unless ,no-errors
                   (eldev-warn "Ran %s as `%s' in directory `%s'" ,program-name (eldev-message-command-line executable command-line) default-directory)
                   (when process-input-file
                     (eldev-warn "Process input:\n%s" (eldev-colorize process-input 'verbose)))
                   (eldev-warn "Stdout contents:\n%s" (eldev-colorize stdout 'verbose))
                   (unless (string= stderr "")
                     (eldev-warn "Stderr contents:\n%s" (eldev-colorize stderr 'verbose)))
                   (eldev-warn "Process exit code: %s" exit-code)
                   ,@(when important-files
                       `((eldev-warn "Important files (oldest first):")
                         (dolist (data (sort (mapcar (lambda (file) (cons file (nth 5 (file-attributes file)))) ',important-files)
                                             (lambda (a b) (< (if (cdr a) (float-time (cdr a)) 0) (if (cdr b) (float-time (cdr b)) 0)))))
                           ;; Not using `current-time-string' as not precise enough.
                           (eldev-warn "    `%s': %s" (car data) (if (cdr data) (float-time (cdr data)) "missing")))))))))
         (when process-input-file
           (ignore-errors (delete-file process-input-file)))
         (ignore-errors (delete-file stderr-file))))))

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
                                                               ;; When in mode `packaged', also generate
                                                               ;; a package for spawned processes to use.
                                                               (when (and (eq eldev-project-loading-mode 'packaged)
                                                                          (not (eq eldev--test-packaged-self 'in-process))
                                                                          (null eldev--test-eldev-dir))
                                                                 (unless eldev--test-packaged-self
                                                                   ;; Set to prevent recursive reentry.
                                                                   (setf eldev--test-packaged-self 'in-process)
                                                                   (eldev--test-create-eldev-archive "packaged-self")
                                                                   (setf eldev--test-packaged-self t)
                                                                   ;; Delete any previously installed in such a way Eldev.
                                                                   (ignore-errors (delete-directory (eldev--test-tmp-subdir (format "stdroot/%s.%s/bootstrap"
                                                                                                                                    emacs-major-version emacs-minor-version))
                                                                                                    t)))
                                                                 (concat ":pa:" (eldev--test-tmp-subdir "packaged-self")))
                                                               ;; Otherwise, let the child processes use the
                                                               ;; sources (maybe byte-compiled) directly.
                                                               eldev-project-dir))
                                 ,(format "ELDEV_DIR=%s"   (eldev--test-eldev-dir))
                                 ,@process-environment)))
     (eldev--test-call-process "Eldev" eldev--test-shell-command ,command-line
       ,@body)))

(defmacro eldev--test-with-temp-copy (test-project vc-backend &rest body)
  (declare (indent 2) (debug (stringp sexp body)))
  (let ((ignores :std))
    (while (keywordp (car body))
      (eldev-pcase-exhaustive (pop body)
        (:ignores (setf ignores (pop body)))))
    `(let* ((vc-backend          ,vc-backend)
            (eldev--test-project (eldev--test-make-temp-copy ,test-project vc-backend ,ignores)))
       (unless eldev--test-project
         (ert-skip (eldev-format-message "%s couldn't be found" vc-backend)))
       (prog1 (progn ,@body)
         ;; Delete the copy if there are no errors.
         (ignore-errors (delete-directory eldev--test-project t))))))

(defun eldev--test-make-temp-copy (test-project vc-backend ignores)
  (unless (memq vc-backend '(nil Git Hg SVN))
    (error "Cannot create temporary VC copy for backend `%s'" vc-backend))
  (let* ((svnadmin   (when (eq vc-backend 'SVN) (eldev-svnadmin-executable t)))
         (executable (if vc-backend
                         (when (or (not (eq vc-backend 'SVN)) svnadmin)
                           (eldev-vc-executable vc-backend t))
                       t)))
    (unless executable
      (ert-skip (eldev-format-message "%s doesn't appear to be installed" (eldev-vc-full-name vc-backend))))
    (let* ((project-dir    (eldev--test-project-dir test-project))
           (copy-dir       (file-name-as-directory (make-temp-file "eldev-" t (if vc-backend (format "-test-%s" (downcase (symbol-name vc-backend))) "-test"))))
           (repository-dir (when svnadmin
                             (file-name-as-directory (make-temp-file "eldev-" t "-test-svnrepo"))))
           (repository-url  (format "file://%s%s"
                                    ;; windows file paths require the
                                    ;; extra root node at the front
                                    (if (eq system-type 'windows-nt) "/" "")
                                    repository-dir)))
      ;; To avoid copying generated files.
      (eldev--test-run test-project ("clean" "everything")
        (should (= exit-code 0)))
      (when svnadmin
        (eldev-call-process svnadmin   `("create" ,repository-dir) :die-on-error t)
        (eldev-call-process executable `("mkdir" ,(format "%s/trunk" repository-url) ,(format "%s/branches" repository-url) "--message" "Init") :die-on-error t)
        (eldev-call-process executable `("checkout" ,(format "%s/trunk" repository-url) ,copy-dir) :die-on-error t))
      (copy-directory project-dir copy-dir t nil t)
      (when vc-backend
        (let ((default-directory copy-dir))
          (unless svnadmin
            (eldev-call-process executable `("init") :die-on-error t))
          (when (eq ignores :std)
            (eldev--vc-ignore-standard-files vc-backend))
          (pcase vc-backend
            (`Git (eldev-call-process executable `("config" "user.name"  "Eldev")             :die-on-error t)
                  (eldev-call-process executable `("config" "user.email" "eldev@example.com") :die-on-error t))
            ;; Mercurial doesn't appear to provide commands for this.
            (`Hg  (with-temp-file ".hg/hgrc"
                    (erase-buffer)
                    (insert "[ui]\nusername = Eldev <eldev@example.com>\n"))))
          ;; Subversion doesn't allow 'add .'.
          (let ((all-files (directory-files copy-dir)))
            (dolist (special-file '("." ".." ".git" ".hg" ".svn"))
              (setf all-files (remove special-file all-files)))
            (eldev-call-process executable `("add" ,@all-files) :die-on-error t))
          (eldev-call-process executable `("commit" "--message" "Copy") :die-on-error t)))
      copy-dir)))


(defmacro eldev--test-with-external-dir (test-project packages &rest body)
  (declare (indent 2) (debug (stringp sexp body)))
  (let ((enabled t))
    (while (keywordp (car body))
      (eldev-pcase-exhaustive (pop body)
        (:enabled (setf enabled (pop body)))))
    `(let* ((eldev--test-project (or ,test-project eldev--test-project))
            (external-dir        (when ,enabled (eldev--test-create-external-dir ,packages)))
            ;; User-accessible variable so that it can be modified.
            (preexisting-files   (when external-dir (eldev--test-find-files external-dir))))
       (prog1 (progn ,@body)
         ;; Eldev must not modify contents of the external directory.
         (when external-dir
           (eldev--test-assert-files external-dir preexisting-files))
         ;; Delete the directory if there are no errors.
         (ignore-errors (when external-dir (delete-directory external-dir t)))))))

(defun eldev--test-create-external-dir (packages)
  (setf packages (eldev-listify packages))
  (let ((external-dir (make-temp-file "eldev-" t "-test")))
    ;; Certainly not the fastest way, but at least robust.  Call a child process to
    ;; install the dependencies and then copy them to the temporary external directory.
    (when packages
      (eldev--test-run nil ("exec" `(dolist (dependency ',(eldev-listify packages))
                                      (copy-directory (package-desc-dir (eldev-find-package-descriptor dependency)) ,(file-name-as-directory external-dir))))
        (unless (= exit-code 0)
          (error "Unable to create external dependency directory"))))
    ;; Just create an empty directory to simplify some tests; in real use this directory
    ;; will be present and even not empty.
    (make-directory (expand-file-name "archives" external-dir))
    external-dir))


(defun eldev--test-file-contents (test-project file &optional not-required)
  (condition-case error
      (with-temp-buffer
        (insert-file-contents (expand-file-name file (eldev--test-project-dir test-project)))
        (buffer-string))
    (file-error (unless not-required
                  (signal (car error) (cdr error))))))

(defmacro eldev--test-with-file-buffer (test-project file &rest body)
  "Execute in a buffer with a FILE from TEST-PROJECT and write results back."
  (declare (indent 2) (debug (stringp sexp body)))
  `(with-temp-buffer
     (ignore-errors (insert-file-contents (expand-file-name ,file (eldev--test-project-dir ,test-project)) t))
     ,@body
     (let ((backup-inhibited t))
       (save-buffer))))

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

(defun eldev--test-line-list (multiline-string)
  (split-string multiline-string "\n" t))

(defmacro eldev--test-canonicalize-bin/eldev-path (_result)
  (let ((result (make-symbol "result")))
    (if (eq system-type 'windows-nt)
        `(let ((,result  ,_result))
           (if (string-match "^\\(.*/bin/eldev\\)" ,result)
               (let* ((matched (match-string 1 ,result))
                      (replacement
                       (replace-regexp-in-string "bin\\\\eldev" "bin\\eldev.bat"
                                                 (replace-regexp-in-string "/" "\\" matched
                                                                           nil 'literal)
                                                 nil 'literal)))
                 (replace-match replacement nil 'literal ,result 1))
             ,result))
      _result)))

(defmacro eldev--test-in-project-environment (&rest body)
  `(let ((eldev-project-dir   (eldev--test-project-dir))
         (eldev-shell-command (eldev--test-canonicalize-bin/eldev-path
                               eldev--test-shell-command)))
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
  (eldev--test-assert-unsorted (eldev--test-line-list stdout)
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
        (eldev--test-run ".." ("--setup" `(setf eldev-dist-dir ,archive-dir)
                               "package" "--entry-file" "--force-version" forced-version)
          (should (= exit-code 0)))
      (eldev--test-run ".." ("--setup" `(setf eldev-dist-dir ,archive-dir)
                             "package" "--entry-file")
        (should (= exit-code 0))))
    (with-temp-file (expand-file-name "archive-contents" archive-dir)
      (prin1 `(1 ,(with-temp-buffer
                    (insert-file-contents-literally (expand-file-name (format "eldev-%s.entry" (or forced-version (eldev-message-version (eldev-package-descriptor)))) archive-dir))
                    (read (current-buffer))))
             (current-buffer)))))


(defun eldev--test-skip-if-missing-linter (exit-code stderr)
  (unless (= exit-code 0)
    (dolist (line (eldev--test-line-list stderr))
      (when (string-match-p (rx "Emacs version" (+ any) "required" (+ any) "cannot use linter") line)
        (ert-skip line)))))


(defmacro eldev-ert-defargtest (name arguments values &rest body)
  (declare (indent 3))
  (let ((function (make-symbol (format "%s:impl" name))))
    `(progn
       ;; Apparently we cannot get away with unnamed lambdas here.
       (defun ,function ,arguments ,@body)
       ,@(mapcar (lambda (arg-values)
                   `(ert-deftest ,(intern (format "%s/%s" name (downcase (replace-regexp-in-string " " "/" (replace-regexp-in-string (rx (not (any word "-" " "))) "" (prin1-to-string arg-values)))))) ()
                      (,function ,@(if (= (length arguments) 1) (list arg-values) arg-values))))
                 values))))


(provide 'test/common)

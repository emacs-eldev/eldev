;;; emake.el --- Emacs `make'  -*- lexical-binding: t -*-

;;; Copyright (C) 2019 Paul Pogonyshev

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Utility and compatibility functions.

(require 'package)



;; Compatibility.

;; To silence compilation warnings on Emacs 24.
(defvar inhibit-message)
(defvar package-archive-priorities)
(defvar byte-compile-log-warning-function)


(defun emake-define-error (name message &optional parent)
  "Same as `define-error', needed for compatibility."
  (if (fboundp #'define-error)
      (define-error name message parent)
    (put name 'error-conditions `(,name ,(or parent 'error)))
    (put name 'error-message    message)))


(if (fboundp 'xor)
    (defalias 'emake-xor 'xor
      "Same as `xor', needed for compatibility.")
  (defsubst emake-xor (a b)
    "Same as `xor', needed for compatibility."
    (if a (not b) b)))


(eval-and-compile
  (if (macrop 'pcase-exhaustive)
      (defalias 'emake-pcase-exhaustive 'pcase-exhaustive
        "Same as `pcase-exhaustive', needed for compatibility.")
    (defmacro emake-pcase-exhaustive (value &rest cases)
      "Same as `pcase-exhaustive', needed for compatibility."
      `(pcase ,value
         ,@cases
         (value  (error "No clause matching `%S'" value)))))

  (if (fboundp 'macroexp-quote)
      (defalias 'emake-macroexp-quote 'macroexp-quote
        "Same as `macroexp-quote', needed for compatibility.")
    (defun emake-macroexp-quote (v)
      "Same as `macroexp-quote', needed for compatibility."
      (if (and (not (consp v))
	       (or (keywordp v) (not (symbolp v)) (memq v '(nil t))))
          v
        (list 'quote v)))))

  (defun emake-macroexp-parse-body (body)
    "Same as `macroexp-parse-body', needed for compatibility."
    (let ((decls ()))
      (while (and (cdr body)
                  (let ((e (car body)))
                    (or (stringp e)
                        (memq (car-safe e)
                              '(:documentation declare interactive cl-declare)))))
        (push (pop body) decls))
      (cons (nreverse decls) body)))



;; General.

;; Replacements for a small parts of `dash'.
(defmacro emake-any-p (form list)
  (let ((values (make-symbol "$values"))
        (result (make-symbol "$result")))
    `(let ((,values ,list)
           ,result)
       (while ,values
         (let ((it (car ,values)))
           (setf ,values (if ,form (progn (setf ,result t) nil) (cdr ,values)))))
       ,result)))

(defmacro emake-all-p (form list)
  (let ((values (make-symbol "$values"))
        (result (make-symbol "$result")))
    `(let ((,values ,list)
           (,result t))
       (while ,values
         (let ((it (car ,values)))
           (setf ,values (if ,form (cdr ,values) (setf ,result nil)))))
       ,result)))

(defmacro emake-filter (form list)
  (let ((values (make-symbol "$values"))
        (result (make-symbol "$result")))
    `(let ((,values ,list)
           ,result)
       (while ,values
         (let ((it (pop ,values)))
           (when ,form
             (push it ,result))))
       (nreverse ,result))))


(defmacro emake-advised (spec &rest body)
  "Execute BODY with given advice installed, then remove it.

Advice function can be nil, in which case it is simply ignored.
This can be used to execute BODY with an advice installed
conditionally.

\(fn (SYMBOL WHERE FUNCTION [PROPS]) BODY...)"
  (declare (indent 1) (debug (sexp body)))
  (let ((symbol   (nth 0 spec))
        (where    (nth 1 spec))
        (function (nth 2 spec))
        (props    (nthcdr 3 spec))
        (fn       (make-symbol "$fn")))
    `(let ((,fn ,function))
       (when ,fn
         (advice-add ,symbol ,where ,fn ,@props))
       (unwind-protect
           ,(macroexp-progn body)
         (when ,fn
           (advice-remove ,symbol ,fn))))))


(defsubst emake-listify (x)
  "Make a list out of X.
If X is already a list (including nil), it is returned
unmodified, else it is wrapped as a single-item list."
  (if (listp x) x `(,x)))

(defun emake-string-list-p (x)
  (let ((result t))
    (while x
      (if (and (consp x) (stringp (car x)))
          (setf x (cdr x))
        (setf result nil
              x      nil)))
    result))

(defun emake-flatten-tree (tree)
  "Like `flatten-tree' in newer Emacs versions."
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(eval-and-compile
  (defmacro emake--assq-set (key value place &optional comparator)
    `(let* ((key      ,key)
            (value    ,value)
            ;; Emacs 24 doesn't support arbitrary comparators.
            (existing ,(emake-pcase-exhaustive (or comparator #'eq)
                         (`eq    `(assq  key ,place))
                         (`equal `(assoc key ,place)))))
       (if existing
           (setf (cdr existing) value)
         (push (cons key value) ,place)
         value))))


;; We use these to avoid accidental name clashes with something else.
(defsubst emake-get (symbol property)
  (plist-get (get symbol 'emake--properties) property))

(defsubst emake-put (symbol property value)
  (put symbol 'emake--properties (plist-put (get symbol 'emake--properties) property value)))


(defun emake-getenv (variable &optional if-empty-or-not-set)
  "Like `getenv', but with default value.
Note that it is impossible to tell an unset variable from one set
to an empty string with this function.  Also lacks `frame'
parameter, but it's not needed in noninteractive use."
  (let ((value (getenv variable)))
    (if (> (length value) 0) value if-empty-or-not-set)))


(defun emake-quote-sh-string (string &optional always-quote)
  (if (and (not always-quote) (string-match-p "\\`[-a-zA-Z0-9,._+:@%/]+\\'" string))
      ;; No quoting necessary.
      string
    (with-temp-buffer
      (insert "'" string)
      (goto-char 2)
      (while (search-forward "'" nil t)
        (insert "\\''"))
      (goto-char (point-max))
      (insert "'")
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun emake-replace-suffix (string old-suffix new-suffix)
  (if (string-suffix-p old-suffix string)
      (concat (substring string 0 (- (length old-suffix))) new-suffix)
    string))

(defsubst emake-external-filename (filename)
  (or (string= filename "..") (string-prefix-p "../" filename)))

(defsubst emake-external-or-absolute-filename (filename)
  (or (emake-external-filename filename) (file-name-absolute-p filename)))


(defun emake-environment-value (variable environment)
  "Retrieve value of VARIABLE from alist ENVIRONMENT.
If there is no such value, fall back to the corresponding Lisp
variable, if it is bound.

This function is intended mostly for `let'-binding variables that
are not necessarily declared, e.g. because of older Emacs or
library version."
  (let ((entry (assq variable environment)))
    (if entry
        (cdr entry)
      (when (boundp variable)
        (symbol-value variable)))))

(defmacro emake-bind-from-environment (environment variables &rest body)
  (declare (indent 2))
  `(let (,@(mapcar (lambda (variable) `(,variable (emake-environment-value ',variable ,environment))) variables))
     ,@body))



;; Output.

(defvar emake-verbosity-level nil
  "How much output Emake generates.
Can be a symbol `quiet', `verbose' or `trace'.  Any other value,
including nil, stands for the default verbosity level.")

(defvar emake-coloring-mode 'auto)
(defvar emake--tty (equal (emake-getenv "EMAKE_TTY") "t"))

(defvar emake-colorizing-schemes (eval-when-compile (let (schemes)
                                                      (dolist (type '((error     ((light-bg "91;1") (dark-bg "91;1")))
                                                                      (warn      ((light-bg 31)     (dark-bg 31)))
                                                                      (verbose   ((light-bg 90)     (dark-bg 90)))
                                                                      (trace     ((light-bg 90)     (dark-bg 90)))
                                                                      (section   ((light-bg  1)     (dark-bg  1)))
                                                                      (default   ((light-bg 34)     (dark-bg 94)))
                                                                      (name      ((light-bg 33)     (dark-bg 93)))
                                                                      (url       ((light-bg 34)     (dark-bg 96)))
                                                                      (details   ((light-bg 90)     (dark-bg 90)))
                                                                      (timestamp ((light-bg 90)     (dark-bg 90)))))
                                                        (dolist (entry (cadr type))
                                                          (puthash (car type) (format "%s" (cadr entry))
                                                                   (or (cdr (assq (car entry) schemes))
                                                                       (emake--assq-set (car entry) (make-hash-table :test #'eq) schemes)))))
                                                      schemes)))

(defvar emake-used-colorizing-scheme nil)

(defvar emake-output-time-diffs nil)
(defvar emake--time-diff-base (float-time))

(defvar emake-disable-message-rerouting nil)
(defvar emake-message-rerouting-destination :stderr)
(defvar emake--output-rerouted nil)
(defvar emake--real-stderr-output nil)


(defalias 'emake-format-message (if (fboundp 'format-message) 'format-message 'format))

(defun emake-message-plural (n singular &optional plural)
  (if (= n 1)
      (emake-format-message "%d %s" n singular)
    (if plural
        (emake-format-message "%d %s" n plural)
      (emake-format-message "%d %ss" n singular))))

(defun emake-message-enumerate (string values &optional converter dont-quote no-and)
  (let (enumerated)
    (unless (listp values)
      (setf values (list values)))
    (when string
      (push (if (cdr values)
                (if (consp string) (cadr string) (format "%ss" string))
              (if (consp string) (car string) string))
            enumerated)
      (push " " enumerated))
    (while values
      (let ((as-string (if converter (funcall converter (pop values)) (pop values))))
        (push (emake-format-message (if dont-quote "%s" "`%s'") as-string) enumerated))
      (when values
        (push (if (or (cdr values) (eq no-and t)) ", " (if no-and (concat " " no-and " ") " and ")) enumerated)))
    (apply #'concat (nreverse enumerated))))

(defun emake-message-enumerate-files (string files)
  (emake-format-message string (if (and files (null (cdr files))) "" "s")
                        (if files (mapconcat (lambda (file) (emake-format-message "`%s'" file)) files ", ") "none")
                        (length files)))

(defun emake-message-version (version &optional colorized)
  (let ((string (cond ((stringp version)                        version)
                      ((and version (not (equal version '(0)))) (package-version-join (if (listp version) version (package-desc-version version))))
                      (t                                        "(any)"))))
    (when colorized
      (setf string (emake-colorize string 'version)))
    string))

(defun emake-message-command-line (executable command-line)
  (concat executable " " (mapconcat #'emake-quote-sh-string command-line " ")))

;; Mainly in case we want to write something better later.
(defun emake-y-or-n-p (prompt)
  (y-or-n-p prompt))


(defun emake-colorize (string &rest types)
  (setf string (copy-sequence (if (symbolp string) (symbol-name string) string string)))
  (when types
    (add-face-text-property 0 (length string) types nil string))
  string)

(defun emake-output (format-string &rest arguments)
  "Unconditionally format and print given message."
  (let (stderr
        nolf
        nocolor)
    (while (keywordp format-string)
      (pcase format-string
        (`:stdout  (setf stderr nil))
        (`:stderr  (setf stderr t))
        (`:nolf    (setf nolf t))
        (`:nocolor (setf nocolor t))
        (_         (error "Unknown option `%s'" format-string)))
      (setf format-string (pop arguments)))
    (let ((message (apply #'emake-format-message format-string arguments)))
      (when emake-output-time-diffs
        (let* ((elapsed         (- (float-time) emake--time-diff-base))
               (elapsed-min     (floor (/ elapsed 60)))
               (elapsed-sec-raw (- elapsed (* elapsed-min 60)))
               (elapsed-sec     (floor elapsed-sec-raw))
               (elapsed-millis  (floor (* (- elapsed-sec-raw elapsed-sec) 1000))))
          (setf message (concat (emake-colorize (format "[%02d:%02d.%03d]" elapsed-min elapsed-sec elapsed-millis) 'timestamp)
                                "  " (replace-regexp-in-string "\n" "\n             " message t t)))))
      (when (and (not nocolor) (if (eq emake-coloring-mode 'auto) emake--tty emake-coloring-mode))
        (let ((colorizing-scheme (emake--get-colorizing-scheme))
              (from 0)
              chunks)
          (while (let ((to (next-property-change from message)))
                   (let ((faces (get-text-property from 'face message)))
                     (when (or to faces)
                       (if faces
                           (dolist (type (emake-listify faces))
                             (let ((ascii-mode (gethash type colorizing-scheme)))
                               (when ascii-mode
                                 (push (format "\033[%sm" ascii-mode) chunks))))
                         (push "\033[0m" chunks))
                       (push (substring-no-properties message from to) chunks)
                       (if to
                           (setf from to)
                         (setf from (length message))
                         nil)))))
          (when chunks
            (push "\033[0m" chunks))
          (push (substring-no-properties message from) chunks)
          (setf message (mapconcat #'identity (nreverse chunks) ""))))
      (if stderr
          (let ((inhibit-message           nil)
                (emake--real-stderr-output t))
            ;; FIXME: Is there a way to support both `:stderr' and `:nolf' in one call?
            (message "%s" message))
        (princ (if nolf message (concat message "\n")))))))

(defun emake--get-colorizing-scheme ()
  ;; Main purpose of this function is to autoguess background, but I
  ;; don't know how to do that (see also comments in `bin/emake.in').
  (unless emake-used-colorizing-scheme
    (setf emake-used-colorizing-scheme 'light-bg))
  (cdr (assq emake-used-colorizing-scheme emake-colorizing-schemes)))

(defun emake--output-wrapper (extra-keywords colorize-as format-string arguments)
  (let (keywords)
    (while (keywordp format-string)
      (push format-string keywords)
      (setf format-string (pop arguments)))
    `(emake-output ,@extra-keywords ,@(nreverse keywords) (emake-colorize ,format-string ',colorize-as) ,@arguments)))

(defmacro emake-error (format-string &rest arguments)
  "Format and print given error message."
  (emake--output-wrapper '(:stderr) 'error format-string arguments))

(defmacro emake-warn (format-string &rest arguments)
  "Format and print given warning message."
  (emake--output-wrapper '(:stderr) 'warn format-string arguments))

(defmacro emake-unless-quiet (&rest body)
  "Execute BODY, unless in quiet mode."
  (declare (indent 0) (debug (body)))
  `(unless (eq emake-verbosity-level 'quiet) ,@body))

(defmacro emake-print (format-string &rest arguments)
  "Format and print given message, unless in quiet mode."
  `(emake-unless-quiet (emake-output ,format-string ,@arguments)))

(defmacro emake-when-verbose (&rest body)
  "Execute BODY if in verbose (or trace) mode."
  (declare (indent 0) (debug (body)))
  `(when (memq emake-verbosity-level '(verbose trace)) ,@body))

(defmacro emake-verbose (format-string &rest arguments)
  "Format and print given message if in verbose (or trace) mode."
  `(emake-when-verbose ,(emake--output-wrapper nil 'verbose format-string arguments)))

(defmacro emake-when-tracing (&rest body)
  "Execute BODY if in trace mode."
  (declare (indent 0) (debug (body)))
  `(when (eq emake-verbosity-level 'trace) ,@body))

(defmacro emake-trace (format-string &rest arguments)
  "Format and print given message if in trace mode."
  `(emake-when-tracing ,(emake--output-wrapper nil 'trace format-string arguments)))


(defun emake-read-wholly (string &optional description)
  (setf description (emake-format-message (or description "Lisp object from `%s'") string))
  (let* ((result (condition-case error
                     (read-from-string string)
                   (error (signal 'emake-error `("When reading %s: %s" ,description ,(error-message-string error))))))
         (tail   (replace-regexp-in-string (rx (or (: bol (1+ whitespace)) (: (1+ whitespace) eol))) "" (substring string (cdr result)) t t)))
    (unless (= (length tail) 0)
      (signal 'emake-error `("Trailing garbage after the expression in %s: `%s'" ,description ,tail)))
    (car result)))


(defmacro emake-output-reroute-messages (&rest body)
  (declare (indent 0) (debug (body)))
  `(emake-advised (#'message :around (unless (or emake-disable-message-rerouting emake--output-rerouted)
                                       (lambda (original &rest args)
                                         (unless (and (boundp 'inhibit-message) inhibit-message)
                                           (if emake--real-stderr-output
                                               (apply original args)
                                             (apply #'emake-output (or emake-message-rerouting-destination :stderr) args))))))
     ,@body))


(defun emake-documentation (function)
  ;; Basically like `help--doc-without-fn', but that is package-private.
  (let ((documentation (documentation function)))
    (when documentation
      (replace-regexp-in-string "\n\n(fn[^)]*?)\\'" "" documentation))))

(defun emake-briefdoc (function)
  (or (emake-get function :briefdoc)
      (when (documentation function)
        (with-temp-buffer
          (insert (emake-documentation function))
          (goto-char 1)
          (forward-sentence)
          ;; `skip-syntax-backward' also skips e.g. quotes.
          (skip-chars-backward ".;")
          (buffer-substring 1 (point))))))



;; Child processes.

(defvar emake--tar-executable t)
(defvar emake--makeinfo-executable t)
(defvar emake--install-info-executable t)
(defvar emake--git-executable t)


(defmacro emake--find-executable (var not-required finder-form error-message &rest error-arguments)
  (declare (indent 2))
  `(progn
     (when (eq ,var t)
       (setf ,var ,finder-form))
     (unless ,var
       (cond ((eq ,not-required 'warn) (emake-warn ,error-message ,@error-arguments))
             ((null ,not-required)     (signal 'emake-error `(,',error-message ,@',error-arguments)))))
     ,var))

(defun emake-tar-executable (&optional not-required)
  (emake--find-executable emake--tar-executable not-required
    (or (executable-find "gtar") (executable-find "tar"))
    "Cannot find `tar' program"))

(defun emake-makeinfo-executable (&optional not-required)
  (emake--find-executable emake--makeinfo-executable not-required
    (executable-find "makeinfo")
    "Cannot find `makeinfo' program"))

(defun emake-install-info-executable (&optional not-required)
  (emake--find-executable emake--install-info-executable not-required
    (executable-find "install-info")
    "Cannot find `install-info' program"))

(defun emake-git-executable (&optional not-required)
  (emake--find-executable emake--git-executable not-required
    (executable-find "git")
    "Git is not installed (cannot find `git' executable)"))

(defun emake-directory-in-exec-path (directory)
  (setf directory (expand-file-name directory))
  (or (member (directory-file-name directory) exec-path) (member (file-name-as-directory directory) exec-path)))


(defmacro emake-call-process (executable command-line &rest body)
  "Execute given process synchronously.
Put output (both stderr and stdout) to a temporary buffer.  Run
BODY with this buffer set as current and variable `exit-code'
bound to the exit code of the process."
  (declare (indent 2) (debug (form sexp body)))
  `(with-temp-buffer
     (let ((exit-code (apply #'call-process ,executable nil t nil ,command-line)))
       (goto-char 1)
       ,@body)))

(defun emake--forward-process-output (&optional header-message header-if-empty-output only-when-verbose)
  (if (= (point-min) (point-max))
      (when header-if-empty-output
        (emake-verbose header-if-empty-output))
    (when header-message
      (emake-verbose header-message))
    (if only-when-verbose
        (emake-verbose "%s" (buffer-string))
      (emake-output "%s" (buffer-string)))))



;; Package basics.

;; This is normally set in `emake-cli'.
(defvar emake-project-dir nil
  "Directory of the project being built.")

(defvar emake--package-descriptors nil
  "Cache for `emake-package-descriptor'.")


(declare-function package-dir-info "package" ())

;; Compatibility function.
(defun emake--package-dir-info ()
  (if (fboundp #'package-dir-info)
      (package-dir-info)
    ;; Not available on Emacs 24.  Copied from a recent Emacs source.
    (let* ((desc-file (package--description-file default-directory)))
      (if (file-readable-p desc-file)
          (with-temp-buffer
            (insert-file-contents desc-file)
              (goto-char (point-min))
              (unwind-protect
                  (let* ((pkg-def-parsed (read (current-buffer)))
                         (pkg-desc
                          (when (eq (car pkg-def-parsed) 'define-package)
                            (apply #'package-desc-from-define
                                   (append (cdr pkg-def-parsed))))))
                    (when pkg-desc
                      (setf (package-desc-kind pkg-desc) 'dir)
                      pkg-desc))))
        (let ((files (directory-files default-directory t "\\.el\\'" t))
              info)
          (while files
            (with-temp-buffer
              (insert-file-contents (pop files))
              (when (setq info (ignore-errors (package-buffer-info)))
                (setq files nil)
                (setf (package-desc-kind info) 'dir))))
          (unless info
            (error "No .el files with package headers in `%s'" default-directory))
          info)))))

(defun emake-package-descriptor (&optional project-dir skip-cache)
  "Return descriptor of the package in PROJECT-DIR.
If PROJECT-DIR is not specified, use `emake-project-dir', i.e.
return the descriptor of the project being built."
  (unless project-dir
    (setf project-dir emake-project-dir))
  (let ((descriptor (unless skip-cache
                      (cdr (assoc project-dir emake--package-descriptors)))))
    (unless descriptor
      (setf descriptor (with-temp-buffer
                         (setf default-directory project-dir)
                         (dired-mode)
                         (emake--package-dir-info)))
      (unless skip-cache
        (push (cons project-dir descriptor) emake--package-descriptors)))
    descriptor))

(defun emake-find-package-descriptor (package-name &optional version only-if-activated)
  "Find descriptor of the package with given name."
  (unless (and only-if-activated (not (memq package-name package-activated-list)))
    (when (stringp version)
      (setf version (version-to-list version)))
    (let* ((this-package  (emake-package-descriptor))
           (found-package (if (equal (package-desc-name this-package) package-name) this-package (cadr (assq package-name package-alist)))))
      (when (and found-package (or (null version) (version-list-<= version (package-desc-version found-package))))
        found-package))))


(defun emake-install-package-file (file)
  "Install given FILE as a package, suppressing messages.
Compilation warnings are not suppressed unless `inhibit-message'
is non-nil when this function is called."
  (let* ((original-warning-function         (when (boundp 'byte-compile-log-warning-function) byte-compile-log-warning-function))
         (byte-compile-log-warning-function (if (and (boundp 'inhibit-message) inhibit-message)
                                                original-warning-function
                                              (lambda (&rest arguments)
                                                (let ((inhibit-message nil))
                                                  (apply original-warning-function arguments)))))
         (inhibit-message                   t))
    (package-install-file file)))



;; Fileset basics.

(defvar emake-fileset-max-iterations 10)

(defun emake-find-files (fileset &optional absolute root)
  "Find files matching given FILESET.
Returns a list of file names relative to ROOT (which defaults to
project root if omitted).  If ABSOLUTE is non-nil, relative paths
are substituted with absolute ones.

Resulting files within one directory are ordered alphabetically
(case sensitively), i.e. as if by `string<'.  Subdirectories
within one directory are ordered similarly.  Files within a
directory come before files in any of its subdirectories.

For example, result list could be something like this:

    Foo
    bar
    foo
    baz/bar
    baz/foo"
  (setf root (file-name-as-directory (if root (expand-file-name root emake-project-dir) emake-project-dir)))
  (save-match-data
    (let* ((case-fold-search     nil)
           (preprocessed-fileset (emake--preprocess-fileset fileset))
           (files                (list nil)))
      (unless (equal preprocessed-fileset '(nil))
        (let ((default-directory root))
          (emake--do-find-files root (if absolute root "") preprocessed-fileset files))
        (nreverse (car files))))))

(defun emake-find-and-trace-files (fileset description &optional absolute root)
  (let ((files (emake-find-files fileset absolute root)))
    (emake-trace "%s" (emake-message-enumerate-files (emake-format-message "Found %s: %%s (%%d)" description) files))
    files))

(defun emake-filter-files (files fileset &optional absolute root)
  (setf root (file-name-as-directory (if root (expand-file-name root emake-project-dir) emake-project-dir)))
  (save-match-data
    (let ((case-fold-search     nil)
          (preprocessed-fileset (emake--preprocess-fileset fileset))
          result)
      (unless (equal preprocessed-fileset '(nil))
        (dolist (file files)
          (let ((relative-name (file-relative-name (expand-file-name file root) root)))
            ;; Drop files outside the root outright.
            (unless (emake-external-or-absolute-filename relative-name)
              (let ((scan relative-name)
                    path)
                (while (progn (push (file-name-nondirectory scan) path)
                              (let ((dir (file-name-directory scan)))
                                (setf scan (when dir (directory-file-name dir))))))
                (when (emake--path-matches path preprocessed-fileset)
                  (push (if absolute (expand-file-name relative-name root) relative-name) result))))))
        (nreverse result)))))

(defun emake--preprocess-fileset (fileset)
  (condition-case-unless-debug error
      (emake--do-preprocess-fileset fileset)
    (error (signal 'emake-error `("Invalid fileset `%S': %s" ,fileset ,(error-message-string error))))))

(defun emake--do-preprocess-fileset (fileset &optional negated)
  (let ((original  fileset)
        (continue  t)
        (iteration 0))
    (while continue
      (setf continue nil)
      (when fileset
        (pcase fileset
          ((pred stringp)
           (setf fileset (emake--preprocess-simple-fileset (list fileset) negated)))
          ((pred symbolp)
           (setf continue t))
          (`(:not ,operand)
           (setf fileset (emake--do-preprocess-fileset operand (not negated))))
          (`(,(and (or :and :or) operator) . ,rest)
           (let ((preprocessed-operator (if negated (if (eq operator :and) :or :and) operator))
                 preprocessed-operands)
             (dolist (operand rest)
               (let ((preprocessed-operand (emake--do-preprocess-fileset operand negated)))
                 (if (eq (car preprocessed-operand) preprocessed-operator)
                     ;; Splice nested `:and' and `:or' into parent where possible.
                     (setf preprocessed-operands (nconc (nreverse (cdr preprocessed-operand)) preprocessed-operands))
                   (push preprocessed-operand preprocessed-operands))))
             (setf fileset `(,preprocessed-operator ,@(nreverse preprocessed-operands)))))
          (`(,(pred stringp) . ,_rest)
           (setf fileset (emake--preprocess-simple-fileset fileset negated)))
          (`(,(pred symbolp) . ,_)
           (setf continue t))
          (_
           (if (= iteration 0)
               (error "unexpected element `%S'" fileset)
             (error "unexpected result `%S' of resolving `%S' after %d iteration(s)" fileset original iteration))))
        (when continue
          (when (> (setf iteration (1+ iteration)) emake-fileset-max-iterations)
            (error "failed to resolve `%S' in %d iterations" original emake-fileset-max-iterations))
          (setf fileset (eval fileset t)))))
    (or fileset `(,negated))))

(defun emake--preprocess-simple-fileset (fileset negated)
  ;; Result: (MATCHES-INITIALLY (MATCHES PATH...)...)
  ;; Each PATH element is either a regexp or nil, the latter stands for `**'.
  (let ((matches-initially 'undecided)
        preprocessed-patterns)
    (dolist (pattern fileset)
      (let* ((original-pattern pattern)
             (matches          (not (when (string-prefix-p "!" pattern)
                                      (setf pattern (substring pattern (length "!"))))))
             path)
        ;; For fixed patterns (those beginning with "/" or "./") remove the prefix;
        ;; otherwise inject "**/" at the beginning.
        (setf pattern (cond ((string-prefix-p "/"  pattern)
                             (substring pattern (length "/")))
                            ((string-prefix-p "./" pattern)
                             (substring pattern (length "./")))
                            (t
                             (concat "**/" pattern))))
        (when (string-suffix-p "/" pattern)
          (setf pattern (concat pattern "**")))
        (dolist (element (split-string (replace-regexp-in-string (rx "\\" (group nonl)) "\\1" pattern t) "/" t))
          (let ((converted (cond ((string= element "**")
                                  nil)
                                 ((member element '("." ".."))
                                  (error "Pattern `%s' contains `.' or `..'" original-pattern))
                                 ((string-match-p (rx "**") element)
                                  (error "Pattern `%s' contains `**' in a wrong position" original-pattern))
                                 (t
                                  (concat (rx bos)
                                          (replace-regexp-in-string (rx "\\*") (rx (0+ anything))
                                                                    (replace-regexp-in-string (rx "\\?") "." (regexp-quote element) t t)
                                                                    t t)
                                          (rx eos))))))
            ;; Avoid two nils one after another in patterns like "**/**".
            (when (or converted (null path) (car path))
              (push converted path))))
        (if (equal path '(nil))
            (setf matches-initially     matches
                  preprocessed-patterns nil)
          (push (cons (emake-xor matches negated) (nreverse path)) preprocessed-patterns))))
    (cons (emake-xor (if (eq matches-initially 'undecided) (emake-all-p (not (emake-xor (car it) negated)) preprocessed-patterns) matches-initially) negated)
          (nreverse preprocessed-patterns))))

;; The following complications are mostly needed to avoid even
;; scanning subdirectories where no matches can be found.  A simpler
;; way would be to find everything under `root' and just filter it.

(defun emake--do-find-files (full-directory directory preprocessed-fileset result)
  ;; Shouldn't be even traced; re-enable for testing if needed.
  (when nil
    (emake-trace "  Scanning `%s' for %S" (if (equal directory "") (if (equal full-directory "") default-directory full-directory) directory) preprocessed-fileset))
  (let (subdirectories)
    (dolist (file (directory-files full-directory))
      (unless (member file '("." ".."))
        (if (file-directory-p (concat full-directory file))
            (push file subdirectories)
          (when (emake--file-matches file preprocessed-fileset)
            (push (concat directory file) (car result))))))
    (dolist (subdirectory (nreverse subdirectories))
      (let ((recurse-fileset (emake--build-recurse-fileset subdirectory preprocessed-fileset)))
        (unless (equal recurse-fileset '(nil))
          (emake--do-find-files (concat full-directory subdirectory "/") (concat directory subdirectory "/")
                                recurse-fileset result))))))

(defun emake--file-matches (file preprocessed-fileset)
  (pcase (car preprocessed-fileset)
    (:and    (emake-all-p (emake--file-matches file it) (cdr preprocessed-fileset)))
    (:or     (emake-any-p (emake--file-matches file it) (cdr preprocessed-fileset)))
    (matches (dolist (pattern (cdr preprocessed-fileset))
               (let ((path (cdr pattern)))
                 (when (null (car path))
                   (setf path (cdr path)))
                 (when (or (null path) (and (null (cdr path)) (string-match-p (car path) file)))
                   (setf matches (car pattern)))))
             matches)))

(defun emake--build-recurse-fileset (subdirectory preprocessed-fileset)
  (let ((matches-initially (pop preprocessed-fileset)))
    (if (keywordp matches-initially)
        ;; I.e. `:and' or `:or'.
        (let* ((and-operator           (eq matches-initially :and))
               (matches-if-no-operands and-operator)
               recurse-operands)
          (while preprocessed-fileset
            (let ((recurse-operand (emake--build-recurse-fileset subdirectory (pop preprocessed-fileset))))
              (if (cdr recurse-operand)
                  (push recurse-operand recurse-operands)
                (unless (eq (car recurse-operand) and-operator)
                  (setf preprocessed-fileset   nil
                        recurse-operands       nil
                        matches-if-no-operands (not and-operator))))))
          (if recurse-operands
              (if (cdr recurse-operands)
                  `(,matches-initially ,@(nreverse recurse-operands))
                (car recurse-operands))
            `(,matches-if-no-operands)))
      (let (subdirectory-patterns)
        (while preprocessed-fileset
          (let* ((pattern       (pop preprocessed-fileset))
                 (path          (cdr pattern))
                 (anchored-path (if (car path) path (cdr path))))
            (unless (car path)
              (push pattern subdirectory-patterns))
            (when (and anchored-path (string-match-p (car anchored-path) subdirectory))
              (let ((matches (car pattern)))
                (if (and (cdr anchored-path) (or (cadr anchored-path) (cddr anchored-path)))
                    (push `(,matches ,@(cdr anchored-path)) subdirectory-patterns)
                  (setf matches-initially     matches
                        subdirectory-patterns nil)
                  ;; Also discard immediately following patterns that
                  ;; have the same `matches' flag as useless now.
                  (while (and preprocessed-fileset (eq (caar preprocessed-fileset) matches))
                    (pop preprocessed-fileset)))))))
        `(,matches-initially ,@(nreverse subdirectory-patterns))))))

(defun emake--path-matches (path preprocessed-fileset)
  (pcase (car preprocessed-fileset)
    (:and    (emake-all-p (emake--path-matches path it) (cdr preprocessed-fileset)))
    (:or     (emake-any-p (emake--path-matches path it) (cdr preprocessed-fileset)))
    (matches (dolist (pattern (cdr preprocessed-fileset))
               (when (emake--do-path-matches path (cdr pattern))
                 (setf matches (car pattern))))
             matches)))

(defun emake--do-path-matches (actual-path pattern-path)
  (let ((element           (car actual-path))
        (actual-path-rest  (cdr actual-path))
        (regexp            (car pattern-path))
        (pattern-path-rest (cdr pattern-path)))
    (if regexp
        (and (string-match-p regexp element)
             (or (null pattern-path-rest) (and actual-path-rest (emake--do-path-matches actual-path-rest pattern-path-rest))))
      (or (null pattern-path-rest)
          (emake--do-path-matches actual-path pattern-path-rest)
          (and actual-path-rest (emake--do-path-matches actual-path-rest pattern-path))))))


(provide 'emake-util)

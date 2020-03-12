;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2019-2020 Paul Pogonyshev

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
;; along with this program.  If not, see https://www.gnu.org/licenses.

;;; Commentary:

;; Utility and compatibility functions.

(require 'package)



;; Compatibility.

;; To silence byte-compilation warnings on Emacs 24-25.
(defvar inhibit-message)
(defvar byte-compile-log-warning-function)


(defun eldev-define-error (name message &optional parent)
  "Same as `define-error', needed for compatibility."
  (if (fboundp #'define-error)
      (define-error name message parent)
    (put name 'error-conditions `(,name ,(or parent 'error)))
    (put name 'error-message    message)))


(eval-and-compile
  (if (fboundp 'xor)
      (defalias 'eldev-xor 'xor
        "Same as `xor', needed for compatibility.")
    (defsubst eldev-xor (a b)
      "Same as `xor', needed for compatibility."
      (if a (not b) b)))

  (if (macrop 'pcase-exhaustive)
      (defalias 'eldev-pcase-exhaustive 'pcase-exhaustive
        "Same as `pcase-exhaustive', needed for compatibility.")
    (defmacro eldev-pcase-exhaustive (value &rest cases)
      "Same as `pcase-exhaustive', needed for compatibility."
      `(pcase ,value
         ,@cases
         (value  (error "No clause matching `%S'" value)))))

  (if (fboundp 'macroexp-quote)
      (defalias 'eldev-macroexp-quote 'macroexp-quote
        "Same as `macroexp-quote', needed for compatibility.")
    (defun eldev-macroexp-quote (v)
      "Same as `macroexp-quote', needed for compatibility."
      (if (and (not (consp v))
	       (or (keywordp v) (not (symbolp v)) (memq v '(nil t))))
          v
        (list 'quote v)))))

  (defun eldev-macroexp-parse-body (body)
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
(defmacro eldev-any-p (form list)
  "Same as `--any' in Dash.
Used to avoid depending on the library."
  (let ((values (make-symbol "$values"))
        (result (make-symbol "$result")))
    `(let ((,values ,list)
           ,result)
       (while ,values
         (let ((it (car ,values)))
           (setf ,values (if ,form (progn (setf ,result t) nil) (cdr ,values)))))
       ,result)))

(defmacro eldev-all-p (form list)
  "Same as `--all' in Dash.
Used to avoid depending on the library."
  (let ((values (make-symbol "$values"))
        (result (make-symbol "$result")))
    `(let ((,values ,list)
           (,result t))
       (while ,values
         (let ((it (car ,values)))
           (setf ,values (if ,form (cdr ,values) (setf ,result nil)))))
       ,result)))

(defmacro eldev-filter (form list)
  "Same as `--filter' in Dash.
Used to avoid depending on the library."
  (let ((values (make-symbol "$values"))
        (result (make-symbol "$result")))
    `(let ((,values ,list)
           ,result)
       (while ,values
         (let ((it (pop ,values)))
           (when ,form
             (push it ,result))))
       (nreverse ,result))))


(defmacro eldev-advised (spec &rest body)
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


(defsubst eldev-listify (x)
  "Make a list out of X.
If X is already a list (including nil), it is returned
unmodified, else it is wrapped as a single-item list."
  (if (listp x) x `(,x)))

(defun eldev-string-list-p (x)
  "Determine if X is a list of strings."
  (let ((result t))
    (while x
      (if (and (consp x) (stringp (car x)))
          (setf x (cdr x))
        (setf result nil
              x      nil)))
    result))

(defun eldev-flatten-tree (tree)
  "Like `flatten-tree' in newer Emacs versions.
Needed for compatibility."
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
  (defmacro eldev--assq-set (key value place &optional comparator)
    `(let* ((key      ,key)
            (value    ,value)
            ;; Emacs 24 doesn't support arbitrary comparators.
            (existing ,(eldev-pcase-exhaustive (or comparator #'eq)
                         (`eq    `(assq  key ,place))
                         (`equal `(assoc key ,place)))))
       (if existing
           (setf (cdr existing) value)
         (push (cons key value) ,place)
         value))))

(defun eldev-valid-regexp-p (regexp)
  "Determine if REGEXP is valid.
Since 0.2."
  (ignore-errors
    (string-match regexp "")
    t))


(defsubst eldev-get (symbol property)
  "Similar to built-in `get', used to avoid accidental name clashes."
  (plist-get (get symbol 'eldev--properties) property))

(defsubst eldev-put (symbol property value)
  "Similar to built-in `put', used to avoid accidental name clashes."
  (put symbol 'eldev--properties (plist-put (get symbol 'eldev--properties) property value)))


(defun eldev-getenv (variable &optional if-empty-or-not-set)
  "Like `getenv', but with default value.
Note that it is impossible to tell an unset variable from one set
to an empty string with this function.  Also lacks `frame'
parameter, but it's not needed in noninteractive use."
  (let ((value (getenv variable)))
    (if (> (length value) 0) value if-empty-or-not-set)))


(defun eldev-quote-sh-string (string &optional always-quote)
  "Quote given STRING for use in a shell command.
Unlike standard `shell-quote-argument', this function uses single
quotes to improve readability in most cases.

If ALWAYS-QUOTE is not specified and STRING doesn't contain any
special characters, it is returned unmodified."
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


(defun eldev-replace-suffix (string old-suffix new-suffix)
  "Replace suffix of given STRING (usually a filename).
If STRING doesn't even end in OLD-SUFFIX, it is returned
unmodified."
  (if (string-suffix-p old-suffix string)
      (concat (substring string 0 (- (length old-suffix))) new-suffix)
    string))

(defsubst eldev-external-filename (filename)
  "Determine if FILENAME specifies a path outside current directory.
This function doesn't handle absolute paths specially."
  (or (string= filename "..") (string-prefix-p "../" filename)))

(defsubst eldev-external-or-absolute-filename (filename)
  "Determine if FILENAME specifies a path outside current directory.
Absolute paths are also considered to point outside."
  (or (eldev-external-filename filename) (file-name-absolute-p filename)))


(defun eldev-environment-value (variable environment)
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

(defmacro eldev-bind-from-environment (environment variables &rest body)
  "Execute BODY with certain VARIABLES set from ENVIRONMENT.
ENVIRONMENT should be an alist (see `eldev-environment-value').
If no value for a variable is specified, current value is not
altered."
  (declare (indent 2))
  `(let (,@(mapcar (lambda (variable) `(,variable (eldev-environment-value ',variable ,environment))) variables))
     ,@body))



;; Output.

(defvar eldev-verbosity-level nil
  "How much output Eldev generates.
Can be a symbol `quiet', `verbose' or `trace'.  Any other value,
including nil, stands for the default verbosity level.")

(defvar eldev-coloring-mode 'auto
  "Whether to use coloring on output.
Special symbol \\='auto means that coloring should be used when
printing to a real terminal, but not when printing to a file.")

(defvar eldev--tty (equal (eldev-getenv "ELDEV_TTY") "t"))

(defvar eldev-colorizing-schemes (eval-when-compile (let (schemes)
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
                                                                       (eldev--assq-set (car entry) (make-hash-table :test #'eq) schemes)))))
                                                      schemes))
  "Alist of colorizing schemes.
Specifies how to convert colorizing types to actual ASCII
terminal colors.")

(defvar eldev-used-colorizing-scheme nil
  "Used colorizing scheme.
If not specified, Eldev will try to pick the best-suited one.")

(defvar eldev-output-time-diffs nil
  "Whether to prepend all output lines with elapsed time.")

(defvar eldev--time-diff-base (float-time))

(defvar eldev-disable-message-rerouting nil
  "Temporarily disable message rerouting.
See `eldev-output-reroute-messages'.")

(defvar eldev-message-rerouting-destination :stderr
  "Rerouted message destination.
Should be either `:stderr' or `:stdout'.")

(defvar eldev-message-rerouting-wrapper nil
  "When set, send rerouted message through this function/macro.
Typical values would be `eldev-warn', `eldev-trace' etc.  Note
that this overrides `eldev-message-rerouting-destination'.")

(defvar eldev--output-rerouted nil)
(defvar eldev--real-stderr-output nil)


(defalias 'eldev-format-message (if (fboundp 'format-message) 'format-message #'format)
  "Like `format-message' if that is defined.
Fall back to `format' on older Emacs versions.")

(defun eldev-message-plural (n singular &optional plural)
  "Return SINGULAR or PLURAL as suitable for the value of N.
If PLURAL is not specified, it is built from SINGULAR by adding a
single ‘s’ (suitable for most, but not all words)."
  (if (= n 1)
      (eldev-format-message "%d %s" n singular)
    (if plural
        (eldev-format-message "%d %s" n plural)
      (eldev-format-message "%d %ss" n singular))))

(defun eldev-message-enumerate (string values &optional converter dont-quote no-and)
  "Enumerate VALUES for use in human-readable messages.
STRING is the common term.  Can be either a simple string, a
two-item list in form (SINGULAR PLURAL) or nil.

If CONVERTER is specified, it should be a function that converts
a value to a string; otherwise VALUES must be a list of strings.

Values are put in single quotes, unless DONT-QUOTE is specified.

Values are separated by commas, but the last two, for better
readability, are separated with word “and”.  However, if NO-AND
is t, a comma is used also between the last two values.  If
NO-AND is a string, it is used in place of “and”."
  (let (enumerated)
    (setf values (eldev-listify values))
    (when string
      (push (if (cdr values)
                (if (consp string) (cadr string) (format "%ss" string))
              (if (consp string) (car string) string))
            enumerated)
      (push " " enumerated))
    (while values
      (let ((as-string (if converter (funcall converter (pop values)) (pop values))))
        (push (eldev-format-message (if dont-quote "%s" "`%s'") as-string) enumerated))
      (when values
        (push (if (or (cdr values) (eq no-and t)) ", " (if no-and (concat " " no-and " ") " and ")) enumerated)))
    (apply #'concat (nreverse enumerated))))

(defun eldev-message-enumerate-files (string files)
  "Enumerate FILES for use in human-readable messages.
See function `eldev-message-enumerate' for details."
  (eldev-format-message string (if (and files (null (cdr files))) "" "s")
                        (if files (mapconcat (lambda (file) (eldev-format-message "`%s'" file)) files ", ") "none")
                        (length files)))

(defun eldev-message-version (version &optional colorized parenthesized)
  "Format VERSION for use in human-readable messages.
VERSION can be a string, a list (see `version-to-list') or a
package descriptor."
  (let ((string (cond ((stringp version)                        version)
                      ((and version (not (equal version '(0)))) (package-version-join (if (listp version) version (package-desc-version version))))
                      (t                                        "any"))))
    (when colorized
      (setf string (eldev-colorize string 'version)))
    (when (or parenthesized (string= string "any"))
      (setf string (format "(%s)" string)))
    string))

(defun eldev-message-command-line (executable command-line)
  "Format given command line for human-readable messages"
  (concat executable " " (mapconcat #'eldev-quote-sh-string command-line " ")))

(defun eldev-y-or-n-p (prompt)
  "Similar to `y-or-n-p'.
Currently works exactly like that built-in, but may be changed
later (preserving semantics)."
  (y-or-n-p prompt))


(defun eldev-colorize (string &rest types)
  "Apply given Eldev colorizing to STRING."
  (setf string (copy-sequence (if (symbolp string) (symbol-name string) string string)))
  (when types
    (add-face-text-property 0 (length string) types nil string))
  string)

(defsubst eldev-output-colorized-p ()
  "Determine if output should be colorized.
Since 0.2."
  (if (eq eldev-coloring-mode 'auto) eldev--tty eldev-coloring-mode))

(defun eldev-output (format-string &rest arguments)
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
    (let ((message (apply #'eldev-format-message format-string arguments)))
      (when eldev-output-time-diffs
        (let* ((elapsed         (- (float-time) eldev--time-diff-base))
               (elapsed-min     (floor (/ elapsed 60)))
               (elapsed-sec-raw (- elapsed (* elapsed-min 60)))
               (elapsed-sec     (floor elapsed-sec-raw))
               (elapsed-millis  (floor (* (- elapsed-sec-raw elapsed-sec) 1000))))
          (setf message (concat (eldev-colorize (format "[%02d:%02d.%03d]" elapsed-min elapsed-sec elapsed-millis) 'timestamp)
                                "  " (replace-regexp-in-string "\n" "\n             " message t t)))))
      (when (and (not nocolor) (eldev-output-colorized-p))
        (let ((colorizing-scheme (eldev--get-colorizing-scheme))
              (from 0)
              chunks)
          (while (let ((to (next-property-change from message)))
                   (let ((faces (get-text-property from 'face message)))
                     (when (or to faces)
                       (if faces
                           (dolist (type (eldev-listify faces))
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
                (eldev--real-stderr-output t))
            ;; FIXME: Is there a way to support both `:stderr' and `:nolf' in one call?
            (message "%s" message))
        (princ (if nolf message (concat message "\n")))))))

(defun eldev--get-colorizing-scheme ()
  ;; Main purpose of this function is to autoguess background, but I
  ;; don't know how to do that (see also comments in `bin/eldev.in').
  (unless eldev-used-colorizing-scheme
    (setf eldev-used-colorizing-scheme 'light-bg))
  (cdr (assq eldev-used-colorizing-scheme eldev-colorizing-schemes)))

(eval-and-compile
  (defun eldev--output-wrapper (extra-keywords colorize-as format-string arguments)
    (let (keywords)
      (while (keywordp format-string)
        (push format-string keywords)
        (setf format-string (pop arguments)))
      `(eldev-output ,@extra-keywords ,@(nreverse keywords) (eldev-colorize ,format-string ',colorize-as) ,@arguments))))

(defmacro eldev-error (format-string &rest arguments)
  "Format and print given error message."
  (eldev--output-wrapper '(:stderr) 'error format-string arguments))

(defmacro eldev-warn (format-string &rest arguments)
  "Format and print given warning message."
  (eldev--output-wrapper '(:stderr) 'warn format-string arguments))

(defmacro eldev-unless-quiet (&rest body)
  "Execute BODY, unless in quiet mode."
  (declare (indent 0) (debug (body)))
  `(unless (eq eldev-verbosity-level 'quiet) ,@body))

(defmacro eldev-print (format-string &rest arguments)
  "Format and print given message, unless in quiet mode."
  `(eldev-unless-quiet (eldev-output ,format-string ,@arguments)))

(defmacro eldev-when-verbose (&rest body)
  "Execute BODY if in verbose (or trace) mode."
  (declare (indent 0) (debug (body)))
  `(when (memq eldev-verbosity-level '(verbose trace)) ,@body))

(defmacro eldev-verbose (format-string &rest arguments)
  "Format and print given message if in verbose (or trace) mode."
  `(eldev-when-verbose ,(eldev--output-wrapper nil 'verbose format-string arguments)))

(defmacro eldev-when-tracing (&rest body)
  "Execute BODY if in trace mode."
  (declare (indent 0) (debug (body)))
  `(when (eq eldev-verbosity-level 'trace) ,@body))

(defmacro eldev-trace (format-string &rest arguments)
  "Format and print given message if in trace mode."
  `(eldev-when-tracing ,(eldev--output-wrapper nil 'trace format-string arguments)))


(defun eldev-read-wholly (string &optional description)
  "Read STRING as Elisp expression.
This is basically a wrapper over `read-from-string', but issues
human-readable errors if there are any problems."
  (setf description (eldev-format-message (or description "Lisp object from `%s'") string))
  (let* ((result (condition-case error
                     (read-from-string string)
                   (error (signal 'eldev-error `("When reading %s: %s" ,description ,(error-message-string error))))))
         (tail   (replace-regexp-in-string (rx (or (: bol (1+ whitespace)) (: (1+ whitespace) eol))) "" (substring string (cdr result)) t t)))
    (unless (= (length tail) 0)
      (signal 'eldev-error `("Trailing garbage after the expression in %s: `%s'" ,description ,tail)))
    (car result)))


(defmacro eldev-output-reroute-messages (&rest body)
  "Execute BODY while rerouting standard Emacs messages.
Messages are reformatted through `eldev-output' and sent either
to `eldev-message-rerouting-destination'.  Can be termporarily
disabled by setting `eldev-disable-message-rerouting' inside
BODY."
  (declare (indent 0) (debug (body)))
  `(eldev-advised (#'message :around
                             (unless eldev--output-rerouted
                               (lambda (original &rest args)
                                 (unless (and (boundp 'inhibit-message) inhibit-message)
                                   (cond ((or eldev--real-stderr-output eldev-disable-message-rerouting)
                                          (apply original args))
                                         (eldev-message-rerouting-wrapper
                                          (if (functionp eldev-message-rerouting-wrapper)
                                              (apply eldev-message-rerouting-wrapper args)
                                            ;; Assume a macro (`eldev-warn' or something like that).
                                            (eval `(,eldev-message-rerouting-wrapper ,@(mapcar #'eldev-macroexp-quote args)) t)))
                                         (t
                                          (apply #'eldev-output (or eldev-message-rerouting-destination :stderr) args)))))))
     ,@body))


(defun eldev-documentation (function)
  "Return Elisp documentation of given FUNCTION.
Strip calling convention from byte-compiled functions, since that
is not meant for humans."
  (let ((documentation (documentation function t)))
    (setf documentation (if documentation (replace-regexp-in-string "\n\n(fn[^)]*?)\\'" "" documentation) ""))
    (dolist (preprocessor (reverse (eldev-get function 'doc-preprocessors)))
      (setf documentation (funcall preprocessor documentation)))
    (when (> (length documentation) 0)
      (substitute-command-keys documentation))))

(defun eldev-briefdoc (function)
  "Return first sentence of Elisp documentation of given FUNCTION."
  (or (eldev-get function :briefdoc)
      (when (documentation function)
        (with-temp-buffer
          (insert (eldev-documentation function))
          (goto-char 1)
          (forward-sentence)
          ;; `skip-syntax-backward' also skips e.g. quotes.
          (skip-chars-backward ".;")
          (buffer-substring 1 (point))))))

(defun eldev-add-documentation-preprocessor (functions preprocessor)
  "Call PREPROCESSOR on documentation string for each of FUNCTIONS.
FUNCTIONS can be a single symbol or a list of those.  Since 0.3."
  (dolist (function (eldev-listify functions))
    (eldev-put function 'doc-preprocessors `(,preprocessor ,@(eldev-get function 'doc-preprocessors)))))


(defmacro eldev-suppress-warning-emacs-text (&rest body)
  "Execute BODY with “Warning (emacs):” from `warn' suppressed.
This macro should be used when this text carries no meaning and
potentially lots of warnings is expected.  A good example is
`checkdoc' linter, which otherwise prepends this noise to any
warning it issues.

Since 0.2."
  (declare (indent 0) (debug (body)))
  `(progn (eval-and-compile (require 'warnings))
          (eldev-advised ('display-warning :around
                                           (lambda (original type message &rest arguments)
                                             (let* ((suppressing    (eq (if (consp type) (car type) type) 'emacs))
                                                    (warning-levels (if suppressing `((:warning "") ,@warning-levels) warning-levels)))
                                               ;; Remove the first newline: it makes no sense with silencing.
                                               (apply original type
                                                      (if (and suppressing (string-prefix-p "\n" message)) (substring message 1) message)
                                                      arguments))))
            ,@body)))



;; Child processes.

(defvar eldev-tar-executable t
  "Tar executable.
Can be set explicitly or left to t, in which case it is located
through `executable-find' if possible.  Autofinder prefers
`gtar' (GNU tar) if that is present.  Since Eldev 0.2.")

(defvar eldev-makeinfo-executable t
  "`makeinfo' executable.
Can be set explicitly or left to t, in which case it is located
through `executable-find' if possible.  Since Eldev 0.2.")

(defvar eldev-install-info-executable t
  "`install-info' executable.
Can be set explicitly or left to t, in which case it is located
through `executable-find' if possible.  Since Eldev 0.2.")

(defvar eldev-git-executable t
  "Git executable.
Can be set explicitly or left to t, in which case it is located
through `executable-find' if possible.  Since Eldev 0.2.")


(defmacro eldev-find-executable (cache-var not-required finder-form error-message &rest error-arguments)
  "Find and executable using FINDER-FORM.
The form will usually call `executable-find'.  Result is cached
in CACHE-VAR, unless that is nil.  If CACHE-VAR is a string,
declare and use variable with that name as a cache.

If executable cannot be found and NOT-REQUIRED is nil,
`eldev-error' is signalled.  If NOT-REQUIRED is equal to
\\='warn, a warning is reported through `eldev-warn'.  Error or
warning is formatted using ERROR-MESSAGE and ERROR-ARGUMENTS.

Since Eldev 0.2."
  (declare (indent 2))
  (when (stringp cache-var)
    (setf cache-var (intern cache-var))
    (unless (special-variable-p cache-var)
      (eval `(defvar ,cache-var t) t)))
  `(or ,(if cache-var
            `(setf ,cache-var (if (eq ,cache-var t) ,finder-form ,cache-var))
          finder-form)
       (pcase ,not-required
         (`warn (eldev-warn ,error-message ,@error-arguments) nil)
         (`nil  (signal 'eldev-error `(,',error-message ,@',error-arguments))))))

(defun eldev-tar-executable (&optional not-required)
  "Find `tar' executable.
See also variable `eldev-tar-executable'."
  (eldev-find-executable eldev-tar-executable not-required
    (or (executable-find "gtar") (executable-find "tar"))
    "Cannot find `tar' program"))

(defun eldev-makeinfo-executable (&optional not-required)
  "Find `makeinfo' executable.
See also variable `eldev-makeinfo-executable'."
  (eldev-find-executable eldev-makeinfo-executable not-required
    (executable-find "makeinfo")
    "Cannot find `makeinfo' program"))

(defun eldev-install-info-executable (&optional not-required)
  "Find `install-info' executable.
See also variable `eldev-install-info-executable'."
  (eldev-find-executable eldev-install-info-executable not-required
    (executable-find "install-info")
    "Cannot find `install-info' program"))

(defun eldev-git-executable (&optional not-required)
  "Find `git' executable.
See also variable `eldev-git-executable'."
  (eldev-find-executable eldev-git-executable not-required
    (executable-find "git")
    "Git is not installed (cannot find `git' executable)"))

(defun eldev-directory-in-exec-path (directory)
  "Determine if DIRECTORY is in $PATH environment variable."
  (setf directory (expand-file-name directory))
  (or (member (directory-file-name directory) exec-path) (member (file-name-as-directory directory) exec-path)))


(defmacro eldev-call-process (executable command-line &rest body)
  "Execute given process synchronously.
Put output (both stderr and stdout) to a temporary buffer.  Run
BODY with this buffer set as current and variable `exit-code'
bound to the exit code of the process."
  (declare (indent 2) (debug (form sexp body)))
  `(with-temp-buffer
     (let ((exit-code (apply #'call-process ,executable nil t nil ,command-line)))
       (goto-char 1)
       ,@body)))

(defun eldev--forward-process-output (&optional header-message header-if-empty-output only-when-verbose)
  (if (= (point-min) (point-max))
      (when header-if-empty-output
        (eldev-verbose header-if-empty-output))
    (when header-message
      (eldev-verbose header-message))
    (if only-when-verbose
        (eldev-verbose "%s" (buffer-string))
      (eldev-output "%s" (buffer-string)))))



;; Package basics.

;; This is normally set in `eldev-cli'.
(defvar eldev-project-dir nil
  "Directory of the project being built.")

(defvar eldev-project-main-file nil
  "Name of the file with project headers.
If there is only one such file in the root directory, this can be
left to nil.  In this case `package-dir-info' will automatically
find the file.

Since 0.2.")

(defvar eldev--package-descriptors nil
  "Cache for `eldev-package-descriptor'.")


(declare-function package-dir-info "package" ())

;; Compatibility function.
(defun eldev--package-dir-info ()
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

(declare-function eldev--cross-project-internal-eval "eldev" (project-dir form &optional use-caching))

(defun eldev-package-descriptor (&optional project-dir skip-cache)
  "Return descriptor of the package in PROJECT-DIR.
If PROJECT-DIR is not specified, use `eldev-project-dir', i.e.
return the descriptor of the project being built."
  (unless project-dir
    (setf project-dir eldev-project-dir))
  (let ((descriptor (unless skip-cache
                      (cdr (assoc project-dir eldev--package-descriptors)))))
    (unless descriptor
      (let ((eldev-project-main-file (eldev--cross-project-internal-eval project-dir 'eldev-project-main-file t)))
        (setf descriptor (with-temp-buffer
                           (if eldev-project-main-file
                               (progn (insert-file-contents (expand-file-name eldev-project-main-file project-dir))
                                      (package-buffer-info))
                             (let ((default-directory project-dir))
                               (dired-mode)
                               (eldev--package-dir-info))))))
      (unless skip-cache
        (push (cons project-dir descriptor) eldev--package-descriptors)))
    descriptor))

(defun eldev-find-package-descriptor (package-name &optional version only-if-activated)
  "Find descriptor of the package with given name."
  (unless (and only-if-activated (not (memq package-name package-activated-list)))
    (when (stringp version)
      (setf version (version-to-list version)))
    (let* ((this-package  (ignore-errors (eldev-package-descriptor)))
           (found-package (if (and this-package (eq (package-desc-name this-package) package-name))
                              this-package
                            (cadr (assq package-name package-alist)))))
      (when (and found-package (or (null version) (version-list-<= version (package-desc-version found-package))))
        found-package))))

;; Of course this is not exposed through a public interface.
(defun eldev-find-built-in-version (package-name)
  (if (eq package-name 'emacs)
      (version-to-list emacs-version)
    (when (and (package-built-in-p package-name) (boundp 'package--builtins) (fboundp 'package--bi-desc-version))
      (let ((data (cdr (assq package-name package--builtins))))
        (when data
          (package--bi-desc-version data))))))


(defun eldev-install-package-file (file)
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

(defvar eldev-fileset-max-iterations 10
  "Fail if computed fileset elements cannot be resolved in this many iterations.")

(defun eldev-find-files (fileset &optional absolute root)
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
  (setf root (file-name-as-directory (if root (expand-file-name root eldev-project-dir) eldev-project-dir)))
  (save-match-data
    (let* ((case-fold-search     nil)
           (preprocessed-fileset (eldev--preprocess-fileset fileset))
           (files                (list nil)))
      (unless (equal preprocessed-fileset '(nil))
        (let ((default-directory root))
          (eldev--do-find-files root (if absolute root "") preprocessed-fileset files))
        (nreverse (car files))))))

(defun eldev-find-and-trace-files (fileset description &optional absolute root)
  "Find files and trace a standard message about them.
See `eldev-find-files' for details."
  (let ((files (eldev-find-files fileset absolute root)))
    (eldev-trace "%s" (eldev-message-enumerate-files (eldev-format-message "Found %s: %%s (%%d)" description) files))
    files))

(defun eldev-filter-files (files fileset &optional absolute root)
  "Return only those FILES that are matched by FILESET."
  (setf root (file-name-as-directory (if root (expand-file-name root eldev-project-dir) eldev-project-dir)))
  (save-match-data
    (let ((case-fold-search     nil)
          (preprocessed-fileset (eldev--preprocess-fileset fileset))
          result)
      (unless (equal preprocessed-fileset '(nil))
        (dolist (file files)
          (let ((relative-name (file-relative-name (expand-file-name file root) root)))
            ;; Drop files outside the root outright.
            (unless (eldev-external-or-absolute-filename relative-name)
              (let ((scan relative-name)
                    path)
                (while (progn (push (file-name-nondirectory scan) path)
                              (let ((dir (file-name-directory scan)))
                                (setf scan (when dir (directory-file-name dir))))))
                (when (eldev--path-matches path preprocessed-fileset)
                  (push (if absolute (expand-file-name relative-name root) relative-name) result))))))
        (nreverse result)))))

(defun eldev--preprocess-fileset (fileset)
  (condition-case-unless-debug error
      (eldev--do-preprocess-fileset fileset)
    (error (signal 'eldev-error `("Invalid fileset `%S': %s" ,fileset ,(error-message-string error))))))

(defun eldev--do-preprocess-fileset (fileset &optional negated)
  (let ((original  fileset)
        (continue  t)
        (iteration 0))
    (while continue
      (setf continue nil)
      (when fileset
        (pcase fileset
          ((pred stringp)
           (setf fileset (eldev--preprocess-simple-fileset (list fileset) negated)))
          ((pred symbolp)
           (setf continue t))
          (`(:not ,operand)
           (setf fileset (eldev--do-preprocess-fileset operand (not negated))))
          (`(,(and (or :and :or) operator) . ,rest)
           (let ((preprocessed-operator (if negated (if (eq operator :and) :or :and) operator))
                 preprocessed-operands)
             (dolist (operand rest)
               (let ((preprocessed-operand (eldev--do-preprocess-fileset operand negated)))
                 (if (eq (car preprocessed-operand) preprocessed-operator)
                     ;; Splice nested `:and' and `:or' into parent where possible.
                     (setf preprocessed-operands (nconc (nreverse (cdr preprocessed-operand)) preprocessed-operands))
                   (push preprocessed-operand preprocessed-operands))))
             (setf fileset `(,preprocessed-operator ,@(nreverse preprocessed-operands)))))
          (`(,(pred stringp) . ,_rest)
           (setf fileset (eldev--preprocess-simple-fileset fileset negated)))
          (`(,(pred symbolp) . ,_)
           (setf continue t))
          (_
           (if (= iteration 0)
               (error "unexpected element `%S'" fileset)
             (error "unexpected result `%S' of resolving `%S' after %d iteration(s)" fileset original iteration))))
        (when continue
          (when (> (setf iteration (1+ iteration)) eldev-fileset-max-iterations)
            (error "failed to resolve `%S' in %d iterations" original eldev-fileset-max-iterations))
          (setf fileset (eval fileset t)))))
    (or fileset `(,negated))))

(defun eldev--preprocess-simple-fileset (fileset negated)
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
          (push (cons (eldev-xor matches negated) (nreverse path)) preprocessed-patterns))))
    (cons (eldev-xor (if (eq matches-initially 'undecided) (eldev-all-p (not (eldev-xor (car it) negated)) preprocessed-patterns) matches-initially) negated)
          (nreverse preprocessed-patterns))))

;; The following complications are mostly needed to avoid even
;; scanning subdirectories where no matches can be found.  A simpler
;; way would be to find everything under `root' and just filter it.

(defun eldev--do-find-files (full-directory directory preprocessed-fileset result)
  ;; Shouldn't be even traced; re-enable for testing if needed.
  (when nil
    (eldev-trace "  Scanning `%s' for %S" (if (equal directory "") (if (equal full-directory "") default-directory full-directory) directory) preprocessed-fileset))
  (let (subdirectories)
    (dolist (file (directory-files full-directory))
      (unless (member file '("." ".."))
        (if (file-directory-p (concat full-directory file))
            (push file subdirectories)
          (when (eldev--file-matches file preprocessed-fileset)
            (push (concat directory file) (car result))))))
    (dolist (subdirectory (nreverse subdirectories))
      (let ((recurse-fileset (eldev--build-recurse-fileset subdirectory preprocessed-fileset)))
        (unless (equal recurse-fileset '(nil))
          (eldev--do-find-files (concat full-directory subdirectory "/") (concat directory subdirectory "/")
                                recurse-fileset result))))))

(defun eldev--file-matches (file preprocessed-fileset)
  (pcase (car preprocessed-fileset)
    (:and    (eldev-all-p (eldev--file-matches file it) (cdr preprocessed-fileset)))
    (:or     (eldev-any-p (eldev--file-matches file it) (cdr preprocessed-fileset)))
    (matches (dolist (pattern (cdr preprocessed-fileset))
               (let ((path (cdr pattern)))
                 (unless (car path)
                   (setf path (cdr path)))
                 (when (or (null path) (and (null (cdr path)) (string-match-p (car path) file)))
                   (setf matches (car pattern)))))
             matches)))

(defun eldev--build-recurse-fileset (subdirectory preprocessed-fileset)
  (let ((matches-initially (pop preprocessed-fileset)))
    (if (keywordp matches-initially)
        ;; I.e. `:and' or `:or'.
        (let* ((and-operator           (eq matches-initially :and))
               (matches-if-no-operands and-operator)
               recurse-operands)
          (while preprocessed-fileset
            (let ((recurse-operand (eldev--build-recurse-fileset subdirectory (pop preprocessed-fileset))))
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

(defun eldev--path-matches (path preprocessed-fileset)
  (pcase (car preprocessed-fileset)
    (:and    (eldev-all-p (eldev--path-matches path it) (cdr preprocessed-fileset)))
    (:or     (eldev-any-p (eldev--path-matches path it) (cdr preprocessed-fileset)))
    (matches (dolist (pattern (cdr preprocessed-fileset))
               (when (eldev--do-path-matches path (cdr pattern))
                 (setf matches (car pattern))))
             matches)))

(defun eldev--do-path-matches (actual-path pattern-path)
  (let ((element           (car actual-path))
        (actual-path-rest  (cdr actual-path))
        (regexp            (car pattern-path))
        (pattern-path-rest (cdr pattern-path)))
    (if regexp
        (and (string-match-p regexp element)
             (or (null pattern-path-rest) (and actual-path-rest (eldev--do-path-matches actual-path-rest pattern-path-rest))))
      (or (null pattern-path-rest)
          (eldev--do-path-matches actual-path pattern-path-rest)
          (and actual-path-rest (eldev--do-path-matches actual-path-rest pattern-path))))))


(provide 'eldev-util)

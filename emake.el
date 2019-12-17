;;; emake.el --- Emacs `make'  -*- lexical-binding: t -*-

;;; Copyright (C) 2019 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.1-beta1
;; Keywords:   elisp
;; Homepage:   https://github.com/doublep/emake
;; Package-Requires: ((emacs "24.1"))

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

;; Emacs-based build system targeted at Elisp projects.


;;; Code:

(require 'emake-util)


(defvar emake-shell-command (or (emake-getenv "EMAKE_CMD") "emake"))
(defvar emake-emacs-executable (or (emake-getenv "EMAKE_EMACS") (emake-getenv "EMACS") "emacs"))

(defconst emake-dir (or (emake-getenv "EMAKE_DIR") "~/.emake"))
(defconst emake-user-config-file (expand-file-name "config" emake-dir))
(defconst emake-file "Emake")
(defconst emake-local-file "Emake-local")
(defconst emake-cache-dir ".emake")

(defconst emake--internal-pseudoarchive "--emake--")

(defvar emake--loading-modes
  '((as-is
     . "Do not alter the project or dependency working directory: add the
directory to `load-path' as-is.")
    (packaged
     . "Generate a Elisp package out of the project/dependency and load it.
This is the recommended mode for continuous integration and other ways
of automated testing, because it loads in exactly the same manner most
users will use the project.")
    (source
     . "Clean project's or dependency's working directory before loading.  As
a result, Elisp functions in it are not byte-compiled.")
    (byte-compiled
     . "Compile `.el' files before loading.  This will make Elisp functions
faster, but backtraces less informative.")
    (built
     . "Build project or dependency before loading.  Only important for those
projects that define custom rules, i.e. where target `:default' is not
empty.")
    (built-and-compiled
     . "Build and byte-compile before loading.  Exactly as `built' and
`compiled' combined.")
    (built-source
     . "Clean the project or dependency, then build target `:default'.  See
`source' and `built' modes for more explanations.")))


(defvar emake-main-fileset '(:not (emake-standard-filesets :or :no-excludes :except 'main))
  "Fileset used to find main project targets.
Default value means “everything that doesn't match any non-main
filesets”.  E.g. if no additional filesets are defined, this
matches everything that is not included in `test' fileset.")

(defvar emake-test-fileset '("./test.el" "./tests.el" "./*-test.el" "./*-tests.el" "./test/" "./tests/")
  "Fileset used to find test targets.
Default value is files `test.el', `tests.el', `*-test.el' or
`*-tests.el' in the project directory and everything within
`test' or `tests' subdirectories.")

(defvar emake-standard-excludes '(:or ".*" (let ((dist-dir (file-relative-name (emake-dist-dir) emake-project-dir)))
                                             (unless (emake-external-or-absolute-filename dist-dir)
                                               (concat "/" dist-dir))))
  "Fileset of targets that should be excluded from all sets.
Default value excludes all files and directories with name
starting with dot (i.e. hidden files by UNIX conventions).  Most
importantly, this excludes `.emake' and `.git' directories.
Additionally, `emake-dist-dir' is excluded.")

(defvar emake-filesets '((main . emake-main-fileset)
                         (test . emake-test-fileset))
  "Alist of target filesets.
The two default filesets `main' and `test' are enough for most
purposes, but you can define more if you need.  Special name
`all' is reserved for union (i.e. as with `:or') of all other
standar filesets and must not be mapped here.

Additionally, whenever any of these filesets is resolved,
`emake-standard-excludes' is always “subtracted” from the
result.")

(defvar emake-files-to-package '("*.el" "./*.info" "./dir" "./doc/*.info" "./doc/dir"))
(defvar emake-makeinfo-sources '("./*.texi" "./*.texinfo" "./doc/*.texi" "./doc/*.texinfo"))
(defvar emake-info-sources '("./*.info" "./*.info"))

(defvar emake-dist-dir "dist"
  "Directory where to generate package tarballs.
This directory can even be located outside the project directory,
which is useful in certain special cases.")

(defvar emake-clean-external-dist nil
  "Whether to allow `emake clean dist' to delete `dist' directory
even if it is outside of the project.  Normally it doesn't do
that as a precaution.")


(defvar emake-project-loading-mode nil)

(defvar emake-setup-forms nil)


(defvar emake-force-override nil
  "Set to non-nil to disable all command and option duplicate checks.")

(defvar emake--commands nil)
(defvar emake--command-aliases nil)

(defvar emake--options nil)


(emake-define-error 'emake-error               "Unhandled Emake error")
(emake-define-error 'emake-too-old             "Emake is too old"    'emake-error)
(emake-define-error 'emake-wrong-command-usage "Wrong command usage" 'emake-error)
(emake-define-error 'emake-wrong-option-usage  "Wrong option usage"  'emake-error)
(emake-define-error 'emake-build-failed        "Build failed"        'emake-error)
(emake-define-error 'emake-build-abort-branch  "Build failed"        'emake-error)
(emake-define-error 'emake-quit                nil)


;; Internal helper for `emake-defcommand'.
(defun emake--register-command (function command keywords)
  (let (override)
    (while keywords
      (emake-pcase-exhaustive (pop keywords)
        (:aliases
         (emake-register-command-aliases command (pop keywords)))
        ((and (or :briefdoc :parameters :custom-parsing :works-on-old-emake) keyword)
         (emake-put function keyword (pop keywords)))
        (:override
         (setf override (pop keywords)))))
    (unless (or override emake-force-override (not (assq command emake--commands)) (eq (cdr (assq command emake--commands)) function))
      (error "Duplicate command `%s'; use `:override' keyword if this is intentional" command))
    (emake--assq-set command function emake--commands)))

(defun emake-register-command-aliases (command aliases)
  (dolist (alias (emake-listify aliases))
    (emake--assq-set alias command emake--command-aliases)))

(defmacro emake-defcommand (name arguments &rest body)
  "Define an Emake command.
BODY can contain the following keywords:

    :command COMMAND

        Command name (a symbol) for the command line.  Default
        value is derived from function name by removing `emake-'
        prefix (or a prefix for any other project).

    :aliases ALIASES

        One (a symbol) or several (list of symbols) aliases for
        the command.

    :hook HOOK

        Run hook (a symbol) before executing this command.
        Default value is derived from function name by appending
        `-hook' to it.

    :briefdoc STRING

        Use STRING as brief (one-line) documentation for the
        command.  By default it is derived from the function
        docstring.

    :parameters STRING

        One-line string for describing command parameters in its
        usage documentation.

Result of BODY is ignored.  If you want Emake to exit with an
error status, signal an error, probably a `emake-error'."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body (emake-macroexp-parse-body body))
        (command     (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-")) "" (symbol-name name))))
        (hook        (intern (concat (symbol-name name) "-hook")))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:command (setf command (pop body)))
        (:hook    (setf hook    (pop body)))
        (keyword  (push keyword keywords) (push (pop body) keywords))))
    (emake-put name :command-hook hook)
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (defvar ,hook nil ,(format "Hook to be executed before command `%s'." command))
            (emake--register-command ',name ',command ',(nreverse keywords)))))

;; Internal helper for `emake-defoption'.
(defun emake--register-option (handler options for-command keywords)
  (let (override)
    (while keywords
      (emake-pcase-exhaustive (pop keywords)
        ((and (or :value :optional-value) keyword)
         (emake-put handler :option-value  `(,(pop keywords) . ,(eq keyword :value))))
        ((and (or :briefdoc :if-default :hidden-if) keyword)
         (emake-put handler keyword (pop keywords)))
        (:default-value
         (emake-put handler :default-value (or (pop keywords) 'nil)))
        (:override
         (setf override (pop keywords)))))
    (dolist (command (if for-command (emake-listify for-command) '(nil)))
      (let ((command-options (or (assq command emake--options) (car (push `(,command . nil) emake--options)))))
        (dolist (option (emake-listify options))
          (unless (or (eq override t) emake-force-override (not (assq option (cdr command-options))) (eq (cdr (assq option (cdr command-options))) handler)
                      (memq command override) (memq option override) (member `(,command ,option) override))
            (error "Duplicate option `%s' for command `%s'; use `:override' keyword if this is intentional" option command))
          (emake--assq-set option handler (cdr command-options)))))))

(defmacro emake-defoption (name arguments &rest body)
  "Define an Emake option (for a command or a global one).
Option's handler function is visible to Elisp under given NAME.
BODY can contain the following keywords:

    :options OPTIONS (required)

        Invokable from command line using any of listed strings.
        OPTIONS can be either a string or a list of strings.
        Each must be either in form \"-X\" (one-letter short option)
        or \"--long-option\".

    :value DESCRIPTION or :optional-value DESCRIPTION

        Make the option accept a value that is described with
        DESCRIPTION (string or symbol) in the documentation.

    :for-command COMMAND

        Declare an option for the specified COMMAND or a list of
        those.  COMMAND must be a symbol or a list of symbols.
        Each command must be defined with `emake-defcommand'
        prior to defining an option for it.

    :briefdoc STRING

        Use STRING as brief (one-line) documentation for the
        option.  By default it is derived from the function
        docstring.

    :if-default FORM

        Evaluate given FORM to determine if the option is the
        default (which can be changed e.g. in `~/.emake/config').

    :default-value FORM

        Evaluate given FORM to determine default value for the
        option.

    :hidden-if FORM or :hidden-if :default

        Certain options are not interesting, unless someone sets
        a really unconvential default value.  Typical examples
        are negation of `--dry-run' options."
  (declare (doc-string 3) (indent 2))
  (let* ((parsed-body (emake-macroexp-parse-body body))
         options
         for-command
         keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:options     (setf options     (pop body)))
        (:for-command (setf for-command (pop body)))
        (keyword      (push keyword keywords) (push (pop body) keywords))))
    (unless options
      (error "`:options' is required for `emake-defoption'"))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (emake--register-option ',name ',options ',for-command ',(nreverse keywords)))))

(defmacro emake-defbooloptions (t-name nil-name variable t-body nil-body &rest common-body)
  (declare (indent 3))
  `(progn (emake-defoption ,t-name ()
            ,@(append t-body   common-body `(:if-default ,variable       (setf ,variable t))))
          (emake-defoption ,nil-name ()
            ,@(append nil-body common-body `(:if-default (not ,variable) (setf ,variable nil))))))


(emake-defbooloptions emake-enable-debug-mode emake-disable-debug-mode debug-on-error
  ("Set `debug-on-error' to t"
   :options       (-d --debug))
  ("Set `debug-on-error' to nil"
   :options       (-D --no-debug)))

(emake-defoption emake-set-loading-mode (mode)
  "Set the project's loading mode"
  :options        (-m --loading)
  :value          MODE
  :default-value  (or emake-project-loading-mode 'as-is)
  ;; Accept both strings and symbols.
  (when (stringp mode)
    (setf mode (intern (downcase mode))))
  (unless (assq mode emake--loading-modes)
    (signal 'emake-wrong-option-usage `("unknown loading mode `%s'" ,mode)))
  (setf emake-project-loading-mode mode))

(emake-defoption emake-list-loading-modes ()
  "List loading modes and exit"
  :options        --list-modes
  (let ((modes emake--loading-modes))
    (while modes
      (let ((mode (pop modes)))
        (emake-output "%s\n\n%s" (emake-colorize (car mode) 'section) (cdr mode))
        (when modes
          (emake-output "\n")))))
  (signal 'emake-quit 0))

(emake-defoption emake-set-loading-mode-as-is ()
  "Shorthand for `--loading=as-is'"
  :options        (-a --as-is)
  :hidden-if      (eq (or emake-project-loading-mode 'as-is) 'as-is)
  (emake-set-loading-mode 'as-is))

(emake-defoption emake-set-loading-mode-packaged ()
  "Shorthand for `--loading=packaged'"
  :options        (-p --packaged)
  :hidden-if      (eq emake-project-loading-mode 'packaged)
  (emake-set-loading-mode 'packaged))

(emake-defoption emake-set-loading-mode-source ()
  "Shorthand for `--loading=source'"
  :options        (-s --source)
  :hidden-if      (eq emake-project-loading-mode 'source)
  (emake-set-loading-mode 'source))

(emake-defoption emake-set-loading-mode-byte-compiled ()
  "Shorthand for `--loading=byte-compiled'"
  :options        (-c --byte-compiled)
  :hidden-if      (eq emake-project-loading-mode 'byte-compiled)
  (emake-set-loading-mode 'byte-compiled))

(emake-defbooloptions emake-load-prefer-newer-mode emake-load-first-mode load-prefer-newer
  ("Set `load-prefer-newer' to t"
   :options       (-N --load-newer)
   :hidden-if     (not (boundp 'load-prefer-newer)))
  ("Set `load-prefer-newer' to nil"
   :options       (-F --load-first)
   :hidden-if     (not (boundp 'load-prefer-newer))))

(emake-defoption emake-quiet-mode ()
  "Restrict output to only essential information"
  :options        (-q --quiet)
  :if-default     (eq emake-verbosity-level 'quiet)
  (setf emake-verbosity-level 'quiet))

(emake-defoption emake-normal-output-mode ()
  "Output normal amount of information"
  :options        --normal-output
  :if-default     (not (memq emake-verbosity-level '(verbose trace quiet)))
  :hidden-if      :default
  (setf emake-verbosity-level nil))

(emake-defoption emake-verbose-mode ()
  "Be more verbose"
  :options        (-v --verbose)
  :if-default     (eq emake-verbosity-level 'verbose)
  (setf emake-verbosity-level 'verbose))

(emake-defoption emake-trace-mode ()
  "Be very verbose"
  :options        (-t --trace)
  :if-default     (eq emake-verbosity-level 'trace)
  (setf emake-verbosity-level 'trace))

(emake-defbooloptions emake-output-time-diffs emake-dont-output-time-diffs emake-output-time-diffs
  ("Prepend elapsed time to every line of output"
   :options       (-T --time))
  ("Don't add time information to output"
   :options       --no-time
   :hidden-if     :default))

(emake-defoption emake-coloring-mode (&optional mode)
  "Whether to colorize output; WHEN can be `always', `auto' or `never'"
  :options        (-C --color)
  :optional-value WHEN
  :default-value  (if emake-coloring-mode (if (eq emake-coloring-mode 'auto) "auto" "always") "never")
  (setf mode                (when mode (downcase mode))
        emake-coloring-mode (cond ((or (null mode) (string= mode "always")) t)
                                  ((string= mode "auto")                    'auto)
                                  ((string= mode "never")                   nil)
                                  (t (signal 'emake-wrong-option-usage `("argument must be `always', `auto' or `never'"))))))

(emake-defoption emake-setup-form (form)
  "Evaluate FORM as the final step of the setup"
  :options        (-S --setup)
  :value          FORM
  (push (emake-read-wholly form "setup form") emake-setup-forms))



;; Filesets.

(defun emake-standard-fileset (name &optional no-excludes)
  (emake-standard-filesets (if no-excludes :no-excludes :std-excludes) name))

(defun emake-standard-filesets (&rest arguments)
  (let ((combination-mode :or)
        except
        no-excludes
        names
        result)
    (dolist (argument arguments)
      (pcase argument
        ((or :and :or)
         (setf combination-mode argument))
        ((or :these :except)
         (setf except (eq argument :except)))
        ((or :no-excludes :std-excludes)
         (setf no-excludes (eq argument :no-excludes)))
        ((pred keywordp)
         (error "Unhandled keyword `%s'" argument))
        (_
         (push argument names))))
    (when (memq 'all names)
      (if except
          (error "Using special name `all' with `:except' makes no sense")
        (if (eq combination-mode :or)
            (setf names (mapcar #'car emake-filesets))
          (when (cdr names)
            (setf names (delq 'all names))))))
    (if except
        (dolist (entry emake-filesets)
          (unless (memq (car entry) names)
            (push (cdr entry) result)))
      (dolist (name names)
        (push (cdr (or (assq name emake-filesets) (error "Unknown fileset `%s'" name))) result)))
    (setf result (if (cdr result)
                     `(,combination-mode ,@(nreverse result))
                   (car result)))
    (unless no-excludes
      (setf result `(:and ,result (:not emake-standard-excludes))))
    result))

(defun emake-validate-standard-fileset (name)
  (when (stringp name)
    (setf name (intern name)))
  (unless (or (eq name 'all) (assq name emake-filesets))
    (signal 'emake-error `("Unknown standard fileset `%s'", name)))
  name)



;; Command line interface.

(defvar emake-executing-command-hook nil)
(defvar emake-too-old nil)


(defun emake-start-up ()
  (setf package-user-dir (expand-file-name "packages" (emake-cache-dir t))
        package-archives nil))

(defun emake-cli (command-line)
  "Emake entry point."
  (let (command
        emake-too-old
        exit-code)
    (unwind-protect
        (condition-case-unless-debug error
            (condition-case error
                (emake-output-reroute-messages
                  (let ((emake-project-dir (or emake-project-dir (expand-file-name default-directory)))
                        ;; Emake uses a different default.
                        (load-prefer-newer t))
                    ;; If `inhibit-message' is t when a signal is raised, Emacs won't
                    ;; print error stacktraces even if `debug-on-error' is t.  Add a
                    ;; workaround.
                    (emake-advised (#'debug :around (lambda (original &rest arguments)
                                                      (let ((inhibit-message nil))
                                                        (apply original arguments))))
                      (emake-parse-options command-line nil t t)
                      ;; Since this is printed before `~/.emake/config' is loaded it can
                      ;; ignore some settings from that file, e.g. colorizing mode.
                      (emake-trace "Started up on %s" (replace-regexp-in-string " +" " " (current-time-string)))
                      (emake-trace "Running on %s" (emacs-version))
                      (emake-trace "Project directory: `%s'" emake-project-dir)
                      (condition-case error
                          (emake--set-up)
                        (emake-too-old (setf emake-too-old (cdr error))))
                      (setf command-line (emake-parse-options command-line nil t))
                      (if command-line
                          (progn
                            (setf command (intern (car command-line)))
                            (let* ((real-command (or (cdr (assq command emake--command-aliases)) command))
                                   (handler      (or (cdr (assq real-command emake--commands)))))
                              (if handler
                                  (let ((hook (emake-get handler :command-hook)))
                                    (when (and emake-too-old (not (emake-get handler :works-on-old-emake)))
                                      (signal 'emake-too-old emake-too-old))
                                    (setf command-line (if (emake-get handler :custom-parsing)
                                                           (cdr command-line)
                                                         (emake-parse-options (cdr command-line) real-command)))
                                    (if (eq real-command command)
                                        (emake-verbose "Executing command `%s'..." command)
                                      (emake-verbose "Executing command `%s' (alias for `%s')..." command real-command))
                                    (when emake-executing-command-hook
                                      (emake-trace "Executing `emake-executing-command-hook'...")
                                      (run-hook-with-args 'emake-executing-command-hook real-command))
                                    (when (symbol-value hook)
                                      (emake-trace "Executing `%s'..." hook)
                                      (run-hooks hook))
                                    ;; We want `message' output on stdout universally, but
                                    ;; older Emacses are very verbose and having additional
                                    ;; unexpected messages in our stdout would screw up
                                    ;; tests.  So we set the target to stdout only now.
                                    (let ((emake-message-rerouting-destination :stdout))
                                      (apply handler command-line))
                                    (setf exit-code 0))
                                (emake-error "Unknown command `%s'" command)
                                (emake-print "Run `%s help' for a list of supported commands" (emake-shell-command t)))))
                        (emake-usage)
                        (emake-print "Run `%s help' for more information" (emake-shell-command t))
                        (setf exit-code 0)))))
              (emake-error (let* ((arguments (cdr error))
                                  hint
                                  hint-about-command)
                             (when (and (eq (car error) 'emake-wrong-command-usage) (not (stringp (car arguments))))
                               (setf arguments `(:command ,(or command t) ,@(cdr arguments))))
                             (when (eq arguments emake-too-old)
                               (setf emake-too-old nil))
                             (pcase (car arguments)
                               (:hint    (pop arguments) (setf hint               (pop arguments)))
                               (:command (pop arguments) (setf hint-about-command (pop arguments))))
                             (emake-error "%s" (apply #'emake-format-message arguments))
                             (when hint
                               (emake-print "%s" (apply #'emake-format-message (emake-listify hint))))
                             (when hint-about-command
                               (emake-print "Run `%s help%s' for more information" (emake-shell-command t) (if (eq hint-about-command t) "" (format " %s" hint-about-command))))))
              (emake-quit  (setf exit-code (cdr error))))
          (error (emake-error "%s" (error-message-string error))))
      (emake-trace "Finished %s on %s" (if (eq exit-code 0) "successfully" "erroneously") (replace-regexp-in-string " +" " " (current-time-string))))
    (when emake-too-old
      (when (eq (car emake-too-old) :hint)
        (setf emake-too-old (cddr emake-too-old)))
      (emake-warn "%s" (apply #'emake-format-message emake-too-old)))
    (or exit-code 1)))

(defun emake--set-up ()
  (dolist (config `((,emake-user-config-file . "No file `%s', not applying user-specific configuration")
                    (,emake-file             . "No file `%s', project building uses only defaults")
                    (,emake-local-file       . "No file `%s', not customizing build")))
    (let ((file (locate-file (car config) (list emake-project-dir))))
      (if file
          (progn (emake-trace "Loading file `%s'..." (car config))
                 (load file nil t t))
        (emake-verbose (cdr config) (car config)))))
  (dolist (form (reverse emake-setup-forms))
    (emake-trace "Evaluating form `%S' specified on the command line..." form)
    (eval form t)))

(defun emake-usage ()
  (emake-output "Usage: %s [OPTION...] COMMAND [...]" (emake-shell-command t)))

(defun emake-shell-command (&optional for-display)
  (or (cond ((and for-display (file-name-absolute-p emake-shell-command))
             (if (emake-directory-in-exec-path (file-name-directory emake-shell-command))
                 (file-name-nondirectory emake-shell-command)
               (let ((command (file-relative-name emake-shell-command emake-project-dir)))
                 (unless (emake-external-or-absolute-filename command)
                   command))))
            ((and (not for-display) (file-name-directory emake-shell-command))
             ;; Can only use absolute filenames: anything else means looking up in
             ;; `exec-path', which is not how shell works.
             (expand-file-name emake-shell-command emake-project-dir)))
      emake-shell-command))

(defun emake-parse-options (command-line &optional command stop-on-non-option allow-unknown)
  (save-match-data
    (let (without-options)
      (while command-line
        (let* ((term     (pop command-line))
               (stop-now (string= term "--")))
          (if (and (string-prefix-p "-" term) (not stop-now) (not (string= term "-")))
              (let ((long-option (string-prefix-p "--" term))
                    rest)
                (if long-option
                    (when (string-match (rx bol (group (1+ (not (any "=")))) "=" (group (0+ anything)) eol) term)
                      (setf rest (match-string 2 term)
                            term (match-string 1 term)))
                  (setf rest (when (> (length term) 2) (substring term (length "--")))
                        term (substring term 0 (length "--"))))
                (while term
                  (let ((handler (cdr (assq (intern term) (cdr (assq command emake--options))))))
                    (if handler
                        (condition-case error
                            (let ((value-mode (emake-get handler :option-value)))
                              (if (and value-mode (or rest (cdr value-mode)))
                                  (progn
                                    (funcall handler (or rest (if command-line
                                                                  (pop command-line)
                                                                (if command
                                                                    (signal 'emake-wrong-command-usage `(t "Option `%s' for command `%s' requires an argument" ,term ,command))
                                                                  (signal 'emake-wrong-command-usage `(t "Option `%s' requires an argument" ,term))))))
                                    (setf rest nil))
                                (funcall handler)))
                          (emake-wrong-option-usage (signal 'emake-error `(:command ,command "For option `%s': %s" ,term ,(apply #'emake-format-message (cdr error))))))
                      (if allow-unknown
                          (setf term nil)
                        ;; The following options are special-cased.  They are not
                        ;; advertised, since normally commands `help' and `version' should
                        ;; be used instead, but still handled as too common.
                        (when (string= term "--help")
                          (when (and (null command) command-line (assq (intern (car command-line)) emake--commands))
                            (setf command (intern (car command-line))))
                          (signal 'emake-quit (if command (emake-help (symbol-name command)) (emake-help))))
                        (when (and (null command) (string= term "--version"))
                          (signal 'emake-quit (apply #'emake-version command-line)))
                        (signal 'emake-wrong-command-usage `(t "Unknown option `%s'" ,term)))))
                  (if long-option
                      (setf term nil)
                    (setf term (when rest (format "-%c" (aref rest 0)))
                          rest (when (> (length rest) 1) (substring rest (length "-")))))))
            (unless stop-now
              (push term without-options))
            (when (or stop-now stop-on-non-option)
              (while command-line
                (push (pop command-line) without-options))))))
      (nreverse without-options))))

(defun emake-cache-dir (emacs-version-specific &optional ensure-exists)
  "Get the directory where various internal caches should be stored.
This is the `.emake' subdirectory of project's root.  When
argument EMACS-VERSION-SPECIFIC is non-nil, path also includes
Emacs version (only major and minor parts, e.g. 26.1).

If ENSURE-EXISTS is non-nil, directory is created if it doesn't
exist yet."
  (let ((cache-dir (file-name-as-directory (expand-file-name emake-cache-dir emake-project-dir))))
    (when emacs-version-specific
      (setf cache-dir (expand-file-name (format "%s.%s" emacs-major-version emacs-minor-version) cache-dir)))
    (when ensure-exists
      (make-directory cache-dir t))
    cache-dir))

(defun emake-dist-dir (&optional ensure-exists)
  (let ((dist-dir (file-name-as-directory (expand-file-name (or emake-dist-dir "") emake-project-dir))))
    (when ensure-exists
      (make-directory dist-dir t))
    dist-dir))



;; Functions for `Emake' and `Emake-local'.

(defvar emake--extra-dependencies nil)
(defvar emake--local-dependencies nil)
(defvar emake--local-dependency-packages nil)

(defvar emake--known-package-archives '((gnu            ("gnu"            . "https://elpa.gnu.org/packages/")    300)
                                        (melpa-stable   ("melpa-stable"   . "http://stable.melpa.org/packages/") 200)
                                        (melpa-unstable ("melpa-unstable" . "http://melpa.org/packages/")        100)))


(defun emake-require-version (version)
  "Fail if Emake is too old."
  (unless (emake-find-package-descriptor 'emake version)
    (signal 'emake-too-old `(:hint ("Run `%s upgrade-self' to install the newest available Emake version" ,(emake-shell-command t))
                                   "Project `%s' requires Emake version %s or newer (this is version %s)" ,(package-desc-name (emake-package-descriptor))
                                   ,(emake-message-version version) ,(emake-message-version (emake-package-descriptor))))))

(defun emake-use-package-archive (archive &optional priority)
  "Use given ARCHIVE to install project dependencies.
ARCHIVE can be one of the predefined symbols below or a cons cell
of (ID . URL), same as you would use in `package-archives'.

Standard archives:

  - gnu            (https://elpa.gnu.org/packages/)
  - melpa-stable   (http://stable.melpa.org/packages/)
  - melpa-unstable (http://melpa.org/packages/)

If PRIORITY is non-nil, ARCHIVE is given this priority (see
`package-archive-priorities').  Standard archives get priorities
300, 200 and 100 in the order they are listed above, unless you
specify something explicitly."
  (unless priority
    (setf priority (nth 2 (assq archive emake--known-package-archives))))
  (cond ((assq archive emake--known-package-archives)
         (setf archive (nth 1 (assq archive emake--known-package-archives))))
        ((not (and (consp archive) (stringp (car archive)) (stringp (cdr archive))))
         (error "Unknown package archive `%S'" archive)))
  (when (string= (car archive) emake--internal-pseudoarchive)
    (error "Package archive name `%s' is reserved for internal use" emake--internal-pseudoarchive))
  (emake-verbose "Using package archive `%s' at `%s' with %s"
                 (car archive) (cdr archive) (if priority (format "priority %s" priority) "default priority"))
  (push archive package-archives)
  (when (and priority (boundp 'package-archive-priorities))
    (push (cons (car archive) priority) package-archive-priorities)))

(defun emake-use-local-dependency (dir &optional loading-mode)
  (if loading-mode
      (unless (assq loading-mode emake--loading-modes)
        (error "Unsupported local dependency mode `%s'; see Emake documentation" loading-mode))
    (setf loading-mode 'as-is))
  (setf dir (file-name-as-directory dir))
  (let* ((absolute-dir (expand-file-name dir emake-project-dir))
         (dependency   (emake-package-descriptor absolute-dir))
         (name         (package-desc-name dependency)))
    (when (eq name (package-desc-name (emake-package-descriptor)))
      (error "Local dependency in directory `%s' is the same package as that being built: `%s'" dir name))
    (when (assq name emake--local-dependencies)
      (error "Duplicate local dependency `%s' in directory `%s': already registered in directory `%s'" name dir (nth 1 (assq name emake--local-dependencies))))
    (push `(,name ,dependency ,dir ,absolute-dir ,loading-mode) emake--local-dependencies)
    (emake-trace "Will use directory `%s' as local dependency `%s' with loading mode `%s'" dir name loading-mode)))

(defun emake-add-extra-dependencies (sets &rest dependencies)
  (dolist (set (emake-listify sets))
    (let ((set-dependencies (or (assq set emake--extra-dependencies) (car (push `(,set . nil) emake--extra-dependencies)))))
      (dolist (dependency dependencies)
        (push (if (symbolp dependency) (list dependency) dependency) (cdr set-dependencies))))))


(defun emake-substitute (source target &optional open-string close-string)
  "Substitute Elisp expressions in given file.
Read text from SOURCE, write the result to TARGET.  All Elisp
expressions between OPEN-STRING and CLOSE-STRING are evaluated
and replaced with evaluation result.  If terminating strings are
not specified, default of \"@[\" and \"]@\" is taken.  (Default
is picked in such a way to avoid clashing with anything used in
other programs that have substitution functions.)

If expression evaluation result is a string, it is inserted
as-is.  Otherwise, it is formatted with `prin1-to-string'."
  (with-temp-buffer
    (insert-file-contents source)
    (emake-substitute-in-buffer open-string close-string)
    (write-region (point-min) (point-max) target nil 'no-message)))

(defun emake-substitute-in-buffer (&optional open-string close-string)
  "Substitute Elisp expressions in the current buffer.
Substitution starts from point and only spans visible buffer
part.  See function `emake-substitute' for more information."
  (save-match-data
    (setf open-string  (or open-string  "@[")
          close-string (or close-string "]@"))
    (while (search-forward open-string nil t)
      (let ((from (- (point) (length open-string)))
            (form (read (current-buffer))))
        (unless (looking-at (regexp-quote close-string))
          (error "Expected `%s' at position %d:%d" close-string (line-number-at-pos) (current-column)))
        (let ((value (eval form t)))
          (delete-region from (+ (point) (length close-string)))
          (insert (if (stringp value) value (prin1-to-string value))))))))

(defun emake-file-contents (file &optional trailing-newlines ignore-first ignore-last)
  "Get contents of given FILE as a string.

TRAILING-NEWLINES must be either a non-negative integer or nil.
If it is an integer, leave exactly that many trailing newlines at
the end (typically only values 0 and 1 make sense).  If there are
too many newlines, they are removed, if too few — added.

Arguments IGNORE-FIRST and IGNORE-LAST can be used to discard
lines from the result.  Only non-negative integers (or nil) are
accepted.  Remember that behavior of IGNORE-LAST indirectly
depends on value of TRAILING-NEWLINES."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file emake-project-dir))
    (when ignore-first
      (cond ((and (integerp ignore-first) (>= ignore-first 0))
             (forward-line ignore-first))
            (t
             (error "Wrong `ignore-first' value: %S" ignore-first))))
    (let ((from (point)))
      (goto-char (point-max))
      (when trailing-newlines
        (cond ((and (integerp trailing-newlines) (>= trailing-newlines 0))
               (while (and (eolp)
                           (when (bolp)
                             (backward-char))))
               (delete-region (point) (point-max))
               (insert (make-string trailing-newlines ?\n)))
              (t
               (error "Wrong `trailing-newlines' value: %S" trailing-newlines))))
      (when ignore-last
        (cond ((and (integerp ignore-last) (>= ignore-last 0))
               (unless (or (bolp) (= ignore-last 0))
                 (beginning-of-line)
                 (setf ignore-last (1- ignore-last)))
               (forward-line (- ignore-last)))
              (t
               (error "Wrong `ignore-last' value: %S" ignore-last))))
      (if (<= (point) from)
          ""
        (buffer-substring-no-properties from (point))))))



;; emake help

(defvar emake-help-show-hidden nil)

(emake-defcommand emake-help (&rest parameters)
  "Print details about Emake invocation.  If COMMAND is specified,
show details about that command instead."
  :parameters     "[COMMAND]"
  :aliases        \?
  :works-on-old-emake t
  (if parameters
      ;; This is an exception: don't signal wrong command usage on excessive parameters.
      (let* ((command      (if (= (length parameters) 1) (intern (car parameters)) 'help))
             (real-command (or (cdr (assq command emake--command-aliases)) command))
             (handler      (cdr (assq real-command emake--commands))))
        (if handler
            (let ((parameters  (emake-get handler :parameters))
                  aliases)
              (dolist (alias emake--command-aliases)
                (when (eq (cdr alias) real-command)
                  (push (car alias) aliases)))
              (emake-output "Usage: %s%s %s" (emake-shell-command t) (if (cdr (assq command emake--options)) " [OPTION...]" "")
                            (if parameters (format "%s %s" real-command parameters) real-command))
              (when aliases
                (emake-output "\n%s" (emake-message-enumerate '("Command alias:" "Command aliases:") aliases #'symbol-name t t)))
              (emake-options-help real-command)
              (emake-output "\n%s" (or (emake-documentation handler) "Not documented")))
          (emake-error "Unknown command `%s'" command)
          (emake-output "Run `%s help' for a list of known commands" (emake-shell-command t))))
    (emake-usage)
    (emake-output "
Options before the command are global for Emake.  Many commands have additional
options specific to them; those must be specified after command name.  See each
command's description for a list of such options.")
    (emake-options-help nil "\nGlobal options:")
    (emake-output "\nCommands:")
    (dolist (command (sort (mapcar #'car emake--commands) #'string<))
      (emake-command-or-option-help command (cdr (assq command emake--commands))))
    (emake-output "
Influential environment variables: `EMAKE_EMACS' (also just `EMACS'),
`EMAKE_LOCAL' and `EMAKE_DIR'.  See documentation for their effects.")
    (emake-print "\nRun `%s help COMMAND' for more information." (emake-shell-command t))))

(emake-defbooloptions emake-help-show-hidden emake-help-omit-hidden emake-help-show-hidden
  ("Show all options, even those not interesting normally"
   :options       (-a --all-options))
  ("Hide non-interesting options"
   :options       (-i --only-interesting)
   :hidden-if     :default)
  :for-command    help)

(defun emake-options-help (&optional command title)
  (let (by-handler)
    (dolist (option (cdr (assq command emake--options)))
      (push (car option) (cdr (or (assq (cdr option) by-handler) (car (push (cons (cdr option) nil) by-handler))))))
    (when by-handler
      (emake-output (or title "\nOptions:"))
      (dolist (group by-handler)
        (let* ((value-mode  (emake-get (car group) :option-value))
               (options     (cdr group))
               (all-strings (mapconcat #'symbol-name options ", ")))
          (when (emake-all-p (string-prefix-p "--" (symbol-name it)) options)
            (setf all-strings (concat "    " all-strings)))
          (when value-mode
            (setf all-strings (format (if (string-prefix-p "--" (symbol-name (car (last options))))
                                          (if (cdr value-mode) "%s=%s" "%s[=%s]")
                                        (if (cdr value-mode) "%s %s" "%s [%s]"))
                                      all-strings (car value-mode))))
          (emake-command-or-option-help all-strings (car group)))))))

(defun emake-command-or-option-help (command-or-option handler)
  (let ((documentation  (emake-briefdoc handler))
        (default        (emake-get handler :if-default))
        (default-value  (emake-get handler :default-value))
        (default-string (emake-colorize "default" 'default))
        (hidden-if      (unless emake-help-show-hidden (emake-get handler :hidden-if))))
    (when default
      (setf default (eval default t)))
    (unless (cond ((eq hidden-if :default) default)
                  (hidden-if               (eval hidden-if t)))
      (when default
        (setf documentation (format (if documentation (format "%s [%%s]" documentation) "[%s]") default-string)))
      (when default-value
        (setf documentation (format (if documentation (format "%s [%%s: %%s]" documentation) "[%s: %s]") default-string (eval default-value t))))
      (when (symbolp command-or-option)
        (setf command-or-option (symbol-name command-or-option)))
      (if documentation
          (emake-output (if (> (length command-or-option) 21) "  %s\n                        %s" "  %-21s %s")
                        command-or-option (emake--wrap-text documentation 55 24))
        (emake-output "  %s" command-or-option)))))

(defun emake--wrap-text (text wrap-width &optional indent-wrapped-lines-by)
  (with-temp-buffer
    (let ((fill-column wrap-width))
      (insert text)
      (fill-region (point-min) (point-max))
      (when indent-wrapped-lines-by
        (goto-char 1)
        (let ((indentation (concat "\n" (make-string indent-wrapped-lines-by ? ))))
          (while (re-search-forward "\n" nil t)
            (replace-match indentation t t))))
      (buffer-string))))



;; Loading dependencies; emake prepare, emake upgrade, emake upgrade-self

(defvar emake-upgrade-dry-run-mode nil)
(defvar emake-upgrade-self-from-stable t)
(defvar emake--upgrade-self-from-forced-pa nil
  "Should remain unset; used for testing.")


(emake-defcommand emake-prepare (&rest parameters)
  "Explicitly install project dependencies.

You don't normally have to run this command, because various
other commands will install dependencies as needed before
executing.  However, `prepare' can occasionally be useful for
writing shell scripts in case you want to easily distinguish
command-specific failures (e.g. `build' can fail because you have
a syntax error somewhere) from general errors with unresolvable
dependencies.

ADDITIONAL-SETs can be used to install extra dependencies added
to those sets (see function `emake-add-extra-dependencies')."
  :aliases        prep
  :parameters     "[ADDITIONAL-SET...]"
  (emake-load-project-dependencies (mapcar #'intern parameters)))

(emake-defcommand emake-upgrade (&rest parameters)
  "Upgrade project dependencies.  If specific packages are not
specified, all project dependencies and those registered with
`emake-add-extra-dependencies' are upgraded.  However, you can
list names of packages to be upgraded.  Requirements of these
package will be upgraded if needed too.

Note that local dependencies are never installed or upgraded from
remote package archives.  If you sometimes use an “official”
release of a package and sometimes its local copy and want the
remote copy to be upgraded, make it non-local first (e.g. by
commenting out `emake-use-local-dependency' in `Emake-local')
before upgrading."
  :parameters     "[PACKAGE...]"
  (emake--install-or-upgrade-dependencies nil (mapcar #'car emake--extra-dependencies) (or (mapcar #'intern parameters) t) emake-upgrade-dry-run-mode nil t))

(emake-defcommand emake-upgrade-self (&rest parameters)
  "Upgrade Emake itself.

This command works even if you installed Emake from sources.
However, once Emake is upgraded this way, your installation from
sources will be replaced with a package downloaded from MELPA."
  :works-on-old-emake t
  (when parameters
    (signal 'emake-wrong-command-usage `(t "Unexpected command parameters")))
  (let ((package-user-dir (expand-file-name (format "%s.%s/bootstrap" emacs-major-version emacs-minor-version) emake-dir)))
    (emake--install-or-upgrade-dependencies t nil t emake-upgrade-dry-run-mode nil t)))

(emake-defbooloptions emake-upgrade-self-from-stable emake-upgrade-self-from-unstable emake-upgrade-self-from-stable
  ("Use MELPA Stable (stable.melpa.org)"
   :options       (-s --stable))
  ("Use MELPA Unstable (melpa.org)"
   :options       --unstable)
  :for-command    upgrade-self)

(emake-defbooloptions emake-upgrade-dry-run-mode emake-upgrade-do-upgrade-mode emake-upgrade-dry-run-mode
  ("Don't actually upgrade anything, just print what would be performed"
   :options       (-n --dry-run))
  ("Do upgrade requested packages"
   :options       --do-upgrade
   :hidden-if     :default)
  :for-command    (upgrade upgrade-self))


(defun emake-load-project-dependencies (&optional additional-sets no-error-if-missing)
  "Load dependencies of the project.
Remember that Emake command functions get invoked with
dependencies not having been loaded yet, because not all commands
need them.

ADDITIONAL-SETS should be a symbol or a list of symbols.  Any
dependency added with `emake-add-extra-dependencies' for one of
the specified sets will be loaded in addition to normal project
dependencies.

Normally, if any dependency cannot be loaded, an error is
signalled.  However, this can be disabled using
NO-ERROR-IF-MISSING."
  ;; Certain commands may sometimes work on too old Emake, but then decide to load project
  ;; dependencies, which of course fails.
  (when emake-too-old
    (signal 'emake-too-old emake-too-old))
  (emake--install-or-upgrade-dependencies nil additional-sets nil nil t nil no-error-if-missing))

;; `package-compute-transaction' and friends are not enough in our case, mostly because of
;; local dependencies that can change unpredictably.  Roll our own.
(defun emake--install-or-upgrade-dependencies (self additional-sets to-be-upgraded dry-run activate main-command-effect &optional no-error-if-missing)
  ;; See comments in `emake-cli'.
  (let* ((emake-message-rerouting-destination :stderr)
         (package-name            (if self 'emake (package-desc-name (emake-package-descriptor))))
         (all-packages            `((,package-name)))
         (package-archives        `(,@(if self
                                          (if emake--upgrade-self-from-forced-pa
                                              `(("bootstrap-pa" . ,(file-name-as-directory emake--upgrade-self-from-forced-pa)))
                                            `(,(nth 1 (assq (if emake-upgrade-self-from-stable 'melpa-stable 'melpa-unstable) emake--known-package-archives))))
                                        `((,emake--internal-pseudoarchive . ,(file-name-as-directory (emake--internal-pseudoarchive-dir)))))
                                    ,@package-archives))
         (package-pinned-packages `(,@(unless self (mapcar (lambda (entry) `(,(car entry) . ,emake--internal-pseudoarchive)) emake--local-dependencies)) ,@package-pinned-packages))
         (plan                    (list nil)))
    (unless self
      (emake--create-internal-pseudoarchive-descriptor))
    (emake--fetch-archive-contents to-be-upgraded)
    (package-read-all-archive-contents)
    (package-load-all-descriptors)
    (unless self
      ;; This removes local dependencies that have been installed as packages
      ;; (e.g. because `Emake-local' used to be different) from `package-alist'.
      ;; Otherwise package manager will prefer the installed versions even if we pin the
      ;; packages to our pseudoarchive.  This doesn't quite feel right, but I don't see a
      ;; better way.
      (setf package-alist (emake-filter (null (emake--loading-mode (car it))) package-alist)))
    (when additional-sets
      (dolist (set (emake-listify additional-sets))
        (let ((extra-dependencies (cdr (assq set emake--extra-dependencies))))
          (when extra-dependencies
            (emake-verbose "Need the following extra dependencies for set `%s': %s" set
                           (emake-message-enumerate nil extra-dependencies
                                                    (lambda (package)
                                                      (emake-format-message "%s %s" (car package) (emake-message-version (cadr package))))
                                                    t))
            (setf all-packages (append all-packages extra-dependencies))))))
    (condition-case error
        (emake--plan-install-or-upgrade self to-be-upgraded all-packages plan)
      (emake-error (cond (no-error-if-missing
                          (emake-verbose "%s" (error-message-string error)))
                         ((null to-be-upgraded)
                          ;; Refetch archive contents and retry.
                          (emake--fetch-archive-contents t)
                          (package-read-all-archive-contents)
                          (setf plan (list nil))
                          (emake--plan-install-or-upgrade self nil all-packages plan)))))
    (let ((planned-packages    (nreverse (car plan)))
          (non-local-plan-size 0)
          (dependency-index    0)
          unknown-packages
          missing-dependency)
      (when (and to-be-upgraded (not (eq to-be-upgraded t)))
        (dolist (package-to-be-upgraded to-be-upgraded)
          (unless (emake-any-p (eq (package-desc-name (car it)) package-to-be-upgraded) planned-packages)
            (push package-to-be-upgraded unknown-packages)))
        (when unknown-packages
          (signal 'emake-error `(:hint ,(unless self `("Check output of `%s dependency-tree'" ,(emake-shell-command t)))
                                       "Cannot upgrade %s: `%s' has no such dependencies" ,(emake-message-enumerate "package" (nreverse unknown-packages)) ,package-name))))
      (dolist (dependency planned-packages)
        (unless (emake--loading-mode (car dependency))
          (setf non-local-plan-size (1+ non-local-plan-size))))
      (when (and dry-run (> non-local-plan-size 0))
        (emake-verbose "In dry-run mode Emake only pretends it is upgrading, installing or deleting dependencies"))
      (dolist (dependency planned-packages)
        (let ((previous-version (cdr dependency))
              (dependency       (car dependency)))
          (if (emake--loading-mode dependency)
              (emake--load-local-dependency dependency)
            (setf dependency-index (1+ dependency-index))
            (if previous-version
                (emake-print :stderr "[%d/%d] Upgrading package `%s' (%s -> %s) from `%s'..."
                             dependency-index non-local-plan-size
                             (emake-colorize (package-desc-name dependency) 'name) (emake-message-version previous-version t) (emake-message-version dependency t)
                             (package-desc-archive dependency))
              (emake-print :stderr "[%d/%d] Installing package `%s' (%s) from `%s'..."
                           dependency-index non-local-plan-size
                           (emake-colorize (package-desc-name dependency) 'name) (emake-message-version dependency t) (package-desc-archive dependency)))
            (unless dry-run
              (let ((inhibit-message t))
                (package-install-from-archive dependency))))))
      (when (= non-local-plan-size 0)
        (if additional-sets
            (emake-verbose "All project dependencies (including those for %s) have been installed already or are local"
                           (emake-message-enumerate "set" additional-sets))
          (emake-verbose "All project dependencies have been installed already or are local")))
      (when main-command-effect
        (unless dry-run
          (let ((inhibit-message t)
                (num-deleted     0))
            (dolist (dependency planned-packages)
              (unless (or (null (cdr dependency)) (emake--loading-mode (car dependency)))
                (package-delete (cdr dependency))
                (setf num-deleted (1+ num-deleted))))
            (when (> num-deleted 0)
              (emake-verbose "Deleted %s" (emake-message-plural num-deleted (if self "obsolete package version" "obsolete dependency version"))))))
        (if (> non-local-plan-size 0)
            (emake-print "Upgraded or installed %s" (emake-message-plural non-local-plan-size (if self "package" "dependency package")))
          (emake-print (if self "Emake is up-to-date" "All dependencies are up-to-date"))))
      (when activate
        (let (recursing-for)
          (emake-advised (#'package-activate-1
                          :around (lambda (original dependency &rest rest)
                                    (let ((inhibit-message nil))
                                      (catch 'exit
                                        (let* ((dependency-name (package-desc-name dependency))
                                               (recursing       (memq dependency-name recursing-for)))
                                          (emake-pcase-exhaustive (unless recursing (emake--loading-mode dependency))
                                            (`nil
                                             (emake-trace (if recursing "Activating local dependency package `%s'..." "Activating dependency package `%s'...")
                                                          dependency-name)
                                             (or (let ((inhibit-message t))
                                                   (apply original dependency rest))
                                                 (progn (setf missing-dependency dependency-name)
                                                        nil)))
                                            ;; In all these modes dependency is activated in exactly the same
                                            ;; way, the difference is in `emake--load-local-dependency'.
                                            ((or `as-is `source `byte-compiled `built `built-and-compiled `built-source)
                                             (dolist (requirement (package-desc-reqs dependency))
                                               (unless (package-activate (car requirement))
                                                 (throw 'exit nil)))
                                             (push (if (eq dependency-name package-name)
                                                       emake-project-dir
                                                     ;; 2 and 3 stand for directory name and its absolute path.
                                                     (emake-trace "Activating local dependency `%s' in directory `%s'"
                                                                  dependency-name (nth 2 (assq dependency-name emake--local-dependencies)))
                                                     (nth 3 (assq dependency-name emake--local-dependencies)))
                                                   load-path)
                                             (push dependency-name package-activated-list)
                                             t)
                                            (`packaged
                                             (let ((generated-package (assq dependency-name emake--local-dependency-packages)))
                                               (unless generated-package
                                                 (error "Package for local dependency `%s' must have been generated by this point" dependency-name))
                                               (push dependency-name recursing-for)
                                               (let* ((package-user-dir (expand-file-name "local/packages" (emake-cache-dir t)))
                                                      (up-to-date-desc  (when (nth 2 generated-package)
                                                                          ;; Package is up-to-date, no need to reinstall it.  At
                                                                          ;; least if we can find the installed copy.
                                                                          (ignore-errors (package-load-descriptor (expand-file-name (package-desc-full-name dependency) package-user-dir))))))
                                                 (if up-to-date-desc
                                                     (progn (emake-trace "Local dependency package `%s' hasn't changed since last installation, no need to reinstall" dependency-name)
                                                            (emake--assq-set dependency-name `(,up-to-date-desc) package-alist)
                                                            (package-activate dependency-name))
                                                   (emake-trace "(Re)installing local dependency package `%s'..." dependency-name)
                                                   (emake-install-package-file (nth 1 generated-package))))
                                               (pop recursing-for)))))))))
            (dolist (package all-packages)
              (unless (package-activate (car package))
                ;; We don't report the required version, but if you look at
                ;; `package-activate-1' (as of 2019-11-24), it also has problems with versions
                (if no-error-if-missing
                    (emake-verbose "Unable to load project dependencies: package `%s' is unavailable" missing-dependency)
                  (signal 'emake-error `("Unable to load project dependencies: package `%s' is unavailable" ,missing-dependency)))))))))))

(defun emake--plan-install-or-upgrade (self to-be-upgraded all-packages plan)
  (let ((visited (make-hash-table :test #'eq)))
    (dolist (package all-packages)
      (emake--do-plan-install-or-upgrade self to-be-upgraded (car package) (cadr package) plan visited))))

(defun emake--do-plan-install-or-upgrade (self to-be-upgraded package-name required-version plan visited)
  (unless (gethash package-name visited)
    (if (package-built-in-p package-name)
        (unless (package-built-in-p package-name required-version)
          (signal 'emake-error `("Dependency `%s' is built-in, but required version %s is too new (only %s available)"
                                 ,package-name ,(emake-message-version required-version) "?")))
      (let* ((local             (and (not self) (emake--loading-mode package-name)))
             (already-installed (unless local (emake-find-package-descriptor package-name required-version nil)))
             (package           (unless (or (eq to-be-upgraded t) (memq package-name to-be-upgraded)) already-installed)))
        (unless package
          ;; Not installed, installed not in the version we need or to be upgraded.
          (let ((available (cdr (assq package-name package-archive-contents)))
                package-disabled
                best-version)
            (while (and available (not package))
              (let* ((candidate (pop available))
                     (version   (package-desc-version candidate))
                     (disabled  (package-disabled-p package-name version)))
                (when (or (not local) (string= (package-desc-archive candidate) emake--internal-pseudoarchive))
                  (cond ((version-list-< version required-version)
                         (when (version-list-< best-version version)
                           (setf best-version version)))
                        (disabled
                         (unless package-disabled
                           (setf package-disabled (if (stringp disabled)
                                                      `("Dependency `%s' is held at version %s, but version %s is required"
                                                        ,package-name ,disabled ,(emake-message-version version))
                                                    `("Dependency `%s' is disabled" ,package-name)))))
                        ((or (null already-installed) (version-list-< (package-desc-version already-installed) version))
                         (setf package candidate))))))
            (unless package
              (setf package already-installed))
            (unless package
              (signal 'emake-error (or package-disabled
                                       (if best-version
                                           `("Dependency `%s' version %s is required, but at most %s is available"
                                             ,package-name ,(emake-message-version required-version) ,(emake-message-version best-version))
                                         `("Dependency `%s' (%s) is not available" ,package-name ,(emake-message-version required-version))))))))
        (dolist (requirement (package-desc-reqs package))
          (emake--do-plan-install-or-upgrade self to-be-upgraded (car requirement) (cadr requirement) plan visited))
        (unless (eq package already-installed)
          (push `(,package . ,already-installed) (car plan)))))
    (puthash package-name t visited)))

(defun emake--internal-pseudoarchive-dir (&optional ensure-exists)
  (let ((dir (expand-file-name emake--internal-pseudoarchive (expand-file-name "archives" package-user-dir))))
    (when ensure-exists
      (make-directory dir t))
    dir))

(defun emake--create-internal-pseudoarchive-descriptor ()
  ;; Create our fake pseudoarchive.  We do each time anew to avoid tracking local
  ;; dependency changes, which is likely slower and certainly harder than just writing a
  ;; single file.
  (with-temp-file (expand-file-name "archive-contents" (emake--internal-pseudoarchive-dir t))
    (let ((package (emake-package-descriptor)))
      (prin1 `(,package-archive-version
               ;; Description, file type and extras are irrelevant for us.
               (,(package-desc-name package) . ,(package-make-ac-desc (package-desc-version package) (package-desc-reqs package) nil 'single nil))
               ,@(mapcar (lambda (entry)
                           (let ((dependency (nth 1 entry)))
                             `(,(package-desc-name dependency) . ,(package-make-ac-desc (package-desc-version dependency) (package-desc-reqs dependency) nil 'single nil))))
                         emake--local-dependencies))
             (current-buffer))
      (insert "\n"))))

(defun emake--fetch-archive-contents (&optional refetch-contents)
  ;; I don't see a way to find if package archive contents is fetched
  ;; already without going into internals.
  (let ((archive-dir (expand-file-name "archives" package-user-dir))
        unfetched-archives)
    (dolist (archive package-archives)
      (unless (string= (car archive) emake--internal-pseudoarchive)
        (if (file-exists-p (expand-file-name "archive-contents" (expand-file-name (car archive) archive-dir)))
            (if refetch-contents
                (progn (emake-trace "Will refetch contents of package archive `%s' in case it has changed" (car archive))
                       (push archive unfetched-archives))
              (emake-trace "Contents of package archive `%s' has been fetched already" (car archive)))
          (push archive unfetched-archives))))
    (when unfetched-archives
      (emake-verbose "Fetching contents of %s..." (emake-message-enumerate "package archive" unfetched-archives #'car))
      ;; See comments in `emake-cli'.
      (let ((emake-message-rerouting-destination :stderr)
            (package-archives                    unfetched-archives)
            (inhibit-message                     t))
        (package-refresh-contents)))))

(defun emake--loading-mode (dependency)
  "Get loading mode of package DEPENDENCY.
DEPENDENCY can be either a name (symbol) or a package
descriptor."
  (let ((dependency-name (if (symbolp dependency) dependency (package-desc-name dependency))))
    (if (eq dependency-name (package-desc-name (emake-package-descriptor)))
        (or emake-project-loading-mode 'as-is)
      (let ((entry (assq dependency-name emake--local-dependencies)))
        (if entry
            (or (nth 4 entry) 'as-is)
          (unless (or (symbolp dependency) (not (string= (package-desc-archive dependency) emake--internal-pseudoarchive)))
            (error "Unexpected local dependency `%s'" dependency-name)))))))

(defun emake--load-local-dependency (dependency)
  (let* ((dependency-name (package-desc-name dependency))
         (package-name    (package-desc-name (emake-package-descriptor)))
         (project-itself  (eq dependency-name package-name))
         (loading-mode    (emake--loading-mode dependency)))
    (when (cdr (assq dependency-name package-alist))
      (error "Local dependency `%s' is already listed in `package-alist'" dependency-name))
    ;; FIXME: Special-case project itself: no need to launch separate process(es) for it.
    ;; I don't want to move this into an alist, to avoid fixing the way it works.
    (let ((commands (emake-pcase-exhaustive loading-mode
                      (`as-is)              ; Nothing to do.
                      (`source              `(("clean" ".elc" "--set" "main" "--delete")))
                      (`byte-compiled       `(("build" ":compile")))
                      (`built               `(("build" ":default")))
                      (`built-and-compiled  `(("build" ":default" ":compile")))
                      (`built-source        `(("clean" ".elc" "--set" "main" "--delete")
                                              ("build" ":default")))
                      (`packaged            `(("package" "--output-dir" ,(expand-file-name "local/generated" (emake-cache-dir t)) "--print-filename"))))))
      (when commands
        (if project-itself
            (emake-verbose "Preparing to load the project in mode `%s'" loading-mode)
          (emake-verbose "Preparing to load local dependency `%s' in mode `%s'" dependency-name loading-mode))
        (let ((default-directory (if project-itself emake-project-dir (nth 3 (assq dependency-name emake--local-dependencies)))))
          (dolist (command commands)
            (emake-trace "Full command line (in directory `%s'):\n  %s" default-directory (emake-message-command-line (emake-shell-command) command))
            (emake-call-process (emake-shell-command) command
              (if (= exit-code 0)
                  (progn
                    (emake--forward-process-output "Output of the child Emake process:" "Child Emake process produced no output" t)
                    (when (string= (car command) "package")
                      (goto-char (point-max))
                      (forward-line -2)
                      (let ((point (point)))
                        (end-of-line)
                        (let ((file (buffer-substring-no-properties point (point))))
                          (forward-line)
                          (push `(,dependency-name ,file ,(looking-at "up-to-date")) emake--local-dependency-packages)))))
                (emake-warn "Output of the child Emake process:\n%s" (buffer-string))
                (signal 'emake-error (if project-itself
                                         `("Child Emake process for exited with error code %d" ,exit-code)
                                       `("Child Emake process for local dependency `%s' exited with error code %d" ,dependency-name ,exit-code))))))))
      (push `(,dependency-name . (,dependency)) package-alist))))



;; emake clean

(defvar emake-clean-dry-run-mode nil)
(defvar emake--cleaners nil)
(defvar emake--cleaner-aliases nil)

(defvar emake-clean-sets nil)


;; Internal helper for `emake-defcleaner'.
(defun emake--register-cleaner (cleaner name keywords)
  (while keywords
    (emake-pcase-exhaustive (pop keywords)
      (:aliases
       (emake-register-cleaner-aliases name (pop keywords)))
      ((and (or :briefdoc :default :superseded-by) keyword)
       (emake-put cleaner keyword (pop keywords)))))
  (emake--assq-set name cleaner emake--cleaners))

(defun emake-register-cleaner-aliases (cleaner aliases)
  (dolist (alias (emake-listify aliases))
    (emake--assq-set alias cleaner emake--cleaner-aliases)))

(defmacro emake-defcleaner (name arguments &rest body)
  "Define an Emake cleaner.
A cleaner *must not* delete anything on its own.  Instead, it
must evaluate to a filename (string) or a list of filenames which
should be deleted.  A filename in the list can also specify a
directory, in which case the directory is removed with its
contents (i.e. recursively).

Filenames should usually be relative, in which case they are
resolved based on project directory.  However, it is allowed to
return absolute paths for special needs (but standard cleaners
never do this).

BODY can contain the following keywords:

    :name NAME

        Cleaner name (a symbol) for the command line.  Default
        value is derived from function name by removing `emake-'
        prefix (or a prefix for any other project) and word
        `cleaner-'.

    :aliases ALIASES

        One (a symbol) or several (list of symbols) aliases for
        the cleaner.

    :briefdoc STRING

        Use STRING as brief (one-line) documentation for the
        cleaner.  By default it is derived from the function
        docstring.

    :default VALUE

        Whether the cleaner should be invoked by default,
        i.e. when `emake clean' is issued without further
        command-line arguments.

    :superseded-by CLEANERS

        Don't invoke this cleaner if one of CLEANERS (which must
        be a symbol or a list of those) is also going to be
        invoked."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body  (emake-macroexp-parse-body body))
        (cleaner-name (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-") (? "cleaner-")) "" (symbol-name name))))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:name   (setf cleaner-name (pop body)))
        (keyword (push keyword keywords) (push (pop body) keywords))))
    (when (memq cleaner-name '(everything all))
      (error "Cleaner names `everything' and `all' are reserved and cannot be registered"))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (emake--register-cleaner ',name ',cleaner-name ',(nreverse keywords)))))

(emake-defcommand emake-clean (&rest parameters)
  "Delete various files produced during building.  Emake provides
several cleaners that delete different types of files.  If the
command is executed without parameters, a set of default cleaners
is executed.  However, you can also specify cleaner names on the
command line.

If special name `everything' or `all' is used, all registered
cleaners are executed (not only the default).

Use option `--list-cleaners' to find out which cleaner names can
be used on the command line."
  :parameters     "[CLEANER...]"
  (let (cleaners
        all
        to-delete
        (num-deleted-dirs  0)
        (num-deleted-files 0))
    (if parameters
        (dolist (name parameters)
          (setf name (intern name))
          (if (memq name '(everything all))
              (setf all t)
            (unless (or (assq name emake--cleaners) (assq name emake--cleaner-aliases))
              (signal 'emake-error `(:hint ("Check output of `%s clean --list-cleaners'" ,(emake-shell-command t)) "Unknown cleaner `%s'" ,name)))
            (push name cleaners)))
      (dolist (entry emake--cleaners)
        (when (emake-get (cdr entry) :default)
          (push (car entry) cleaners))))
    (when all
      (setf cleaners (mapcar #'car emake--cleaners)))
    (unless cleaners
      (signal 'emake-error `("There are no %scleaners registered in this project" ,(if all "" "default "))))
    (setf cleaners (nreverse cleaners))
    (dolist (cleaner cleaners)
      (let ((function (or (cdr (assq cleaner emake--cleaners)) (cdr (assq (cdr (assq cleaner emake--cleaner-aliases)) emake--cleaners))))
            superseded-by)
        (if (emake-any-p (let ((superseded-by-resolved (or (cdr (assq it emake--cleaner-aliases)) it)))
                           (setf superseded-by it)
                           (emake-any-p (eq (or (cdr (assq it emake--cleaner-aliases)) it) superseded-by-resolved) cleaners))
                         (emake-listify (emake-get function :superseded-by)))
            (emake-trace "Skipping cleaner `%s': superseded by `%s'" cleaner superseded-by)
          (emake-verbose "Invoking cleaner `%s'..." cleaner)
          (let ((files (emake-listify (funcall function))))
            (emake-trace "%s" (emake-message-enumerate-files "File%s to be deleted according to this cleaner: %s (%d)" files))
            (setf to-delete (append to-delete files))))))
    (setf to-delete (mapcar (lambda (file) (expand-file-name file emake-project-dir)) to-delete))
    (dolist (file to-delete)
      (let* ((attributes   (file-attributes file))
             (directory    (eq (nth 0 attributes) t))
             (printed-name (file-relative-name file emake-project-dir)))
        (when (emake-external-filename printed-name)
          (setf printed-name file))
        (when directory
          (setf printed-name (file-name-as-directory printed-name)))
        (if attributes
            (progn
              (if emake-clean-dry-run-mode
                  (emake-output "%s" printed-name)
                (emake-verbose (if directory "Recursively deleting directory `%s'..." "Deleting file `%s'...") printed-name)
                (if directory
                    (delete-directory file t)
                  (delete-file file)))
              (if directory
                  (setf num-deleted-dirs (1+ num-deleted-dirs))
                (setf num-deleted-files (1+ num-deleted-files))))
          (emake-trace "Skipping file `%s' as it doesn't exist" printed-name))))
    (unless emake-clean-dry-run-mode
      (let ((deleted-dirs  (emake-message-plural num-deleted-dirs "directory" "directories"))
            (deleted-files (emake-message-plural num-deleted-files "file")))
        (cond ((and (> num-deleted-dirs 0) (> num-deleted-files 0))
               (emake-print "Deleted %s and %s" deleted-dirs deleted-files))
              ((> num-deleted-dirs 0)
               (emake-print "Deleted %s" deleted-dirs))
              ((> num-deleted-files 0)
               (emake-print "Deleted %s" deleted-files)))))
    (when (and (= num-deleted-files 0) (= num-deleted-dirs 0))
      (emake-print "Nothing to delete"))))

(defun emake-clean-fileset ()
  (apply #'emake-standard-filesets :or (or emake-clean-sets '(all))))

;; Similar to `emake-build-set', but the default value is different.
(emake-defoption emake-clean-set (name)
  "Clean targets from this set; special set name `all' can be
used"
  :options        (-s --set)
  :for-command    clean
  :value          NAME
  :default-value  (emake-message-enumerate nil (or emake-build-sets '(all)) nil t)
  (setf emake-clean-sets (append emake-clean-sets (list (emake-validate-standard-fileset name)))))

(emake-defbooloptions emake-clean-dry-run-mode emake-clean-do-clean-mode emake-clean-dry-run-mode
  ("Don't delete anything, only print names of files to be deleted"
   :options       (-n --dont-delete --dry-run))
  ("Delete files and directories as requested"
   :options       (-d --delete)
   :hidden-if     :default)
  :for-command    clean)

(emake-defoption emake-clean-list-cleaners ()
  "List available cleaners and exit"
  :options        (-L --list-cleaners --list)
  :for-command    clean
  (emake-print "Cleaner(s) marked with `*' are default, i.e. executed also when\nno cleaner names are specified on the command line.\n")
  (let ((all-cleaners (reverse emake--cleaners)))
    (while all-cleaners
      (let* ((cleaner  (pop all-cleaners))
             (name     (car cleaner))
             (function (cdr cleaner))
             (header   (if (emake-get function :default) (format "%s [*]" name) (format "%s" name))))
        (if (emake-unless-quiet t)
            (let (aliases)
              (dolist (alias emake--cleaner-aliases)
                (when (eq (cdr alias) name)
                  (push (car alias) aliases)))
              (emake-output "%s\n" (emake-colorize header 'section))
              (when aliases
                (emake-output "    %s\n" (emake-message-enumerate '("Alias:" "Aliases:") aliases #'symbol-name t t)))
              (emake-output "%s" (or (emake-documentation function) "Not documented"))
              (when all-cleaners
                (emake-output "\n")))
          (let ((documentation (emake-briefdoc function)))
            (if documentation
                (emake-output "%-18s  %s" header documentation)
              (emake-output "%s" header)))))))
  (signal 'emake-quit 0))


(emake-defcleaner emake-cleaner-.emake ()
  "Delete `.emake' directory.  This cleans all Emake caches for
this project, for this and other Emacs versions."
  :aliases       (dot-emake)
  (emake-cache-dir nil))

(emake-defcleaner emake-cleaner-emake-cache ()
  "Delete all Emake caches for this Emacs version and project.
This currently includes installed dependencies and test results.
Cached data for other Emacs versions is not affected."
  :superseded-by dot-emake
  :aliases       ecache
  (emake-cache-dir t))

(emake-defcleaner emake-cleaner-dependencies ()
  "Delete installed project dependencies.  Deleted dependencies
will be automatically reinstalled when needed, e.g. if you invoke
`test' command.  This cleaner is a way to indirectly upgrade all
dependencies."
  :superseded-by (ecache dot-emake)
  :aliases       (deps requirements reqs)
  (expand-file-name "packages" (emake-cache-dir t)))

(emake-defcleaner emake-cleaner-distribution ()
  "Delete `dist' directory, where package tarballs are generated."
  :aliases       dist
  (let* ((directory     (emake-dist-dir))
         (relative-name (file-relative-name directory emake-project-dir)))
    (cond ((string= relative-name ".")
           (emake-warn "Refusing to delete `emake-dist-dir' since it matches `emake-project-dir'")
           nil)
          ((emake-external-or-absolute-filename relative-name)
           (if emake-clean-external-dist
               (progn (emake-verbose "Deleting `dist' dir (%s), even though it is not inside the project (see variable `emake-clean-external-dist'" directory)
                      directory)
             (emake-warn "Refusing to delete `emake-dist-dir' since it is not inside `emake-project-dir'")
             nil))
          (t
           directory))))



;; emake archives, emake dependencies, emake dependency-tree

(defvar emake-dependencies-list-built-ins nil
  "Whether to list built-in packages among built-ins.
The most obvious such package is `emacs` used to declare minimum
required version of Emacs itself.")

(emake-defcommand emake-archives (&rest parameters)
  "List package archives used to look up dependencies.  Archives
can be registered using function `emake-use-package-archive' in
project's `Emake' file."
  :aliases        arch
  (when parameters
    (signal 'emake-wrong-command-usage `(t "Unexpected command parameters")))
  (if package-archives
      (let* ((have-priorities (boundp 'package-archive-priorities))
             (priorities      (when have-priorities package-archive-priorities)))
        (dolist (archive (sort package-archives (lambda (a b) (> (or (cdr (assoc (car a) priorities)) 0)
                                                                 (or (cdr (assoc (car b) priorities)) 0)))))
          (emake-output "%s: %s%s"
                        (emake-colorize (car archive) 'name)
                        (emake-colorize (cdr archive) 'url)
                        (if have-priorities
                            (emake-format-message "  (priority: %s)" (or (cdr (assoc (car archive) priorities)) "0, defaulted"))
                          ""))))
    (emake-print "None specified; add form `(emake-use-package-archive ...)' in file `%s'" emake-file)))

(emake-defcommand emake-dependencies (&rest parameters)
  "List dependencies of the current project.

If any additional sets are specified, dependencies from the sets
are listed too.  Sets typically have the same name as commands.
E.g. there might be additional dependencies for testing (`test')
or evaluating (`eval') registered in the project's `Emake' file."
  :aliases        (deps requirements reqs)
  :parameters     "[ADDITIONAL-SET...]"
  :works-on-old-emake t
  (let ((additional-sets (mapcar #'intern parameters))
        have-dependencies
        have-non-built-in-dependencies)
    (dolist (dependency (package-desc-reqs (emake-package-descriptor)))
      (if (and (package-built-in-p (car dependency)) (not emake-dependencies-list-built-ins))
          (emake-trace "Omitting dependency as a built in: %s %s" (car dependency) (emake-message-version (cadr dependency)))
        (emake-output "%s %s" (emake-colorize (car dependency) 'name) (emake-message-version (cadr dependency) t))
        (setf have-non-built-in-dependencies t))
      (setf have-dependencies t))
    (dolist (set additional-sets)
      (dolist (dependency (cdr (assq set emake--extra-dependencies)))
        (if (and (package-built-in-p (car dependency)) (not emake-dependencies-list-built-ins))
            (emake-trace "Omitting extra dependency for set `%s' as a built in: %s %s" set (car dependency) (emake-message-version (cadr dependency)))
          (emake-output "%s %s%s" (emake-colorize (car dependency) 'name) (emake-message-version (cadr dependency) t)
                        (or (emake-unless-quiet (emake-format-message "    [for `%s']" set)) ""))
          (setf have-non-built-in-dependencies t))
        (setf have-dependencies t)))
    (if have-dependencies
        (unless have-non-built-in-dependencies
          (emake-print "Project `%s' has only built-in dependencies; rerun with `--list-built-ins'" (emake-colorize (package-desc-name (emake-package-descriptor)) 'name)))
      (emake-print "Project `%s' has no dependencies" (emake-colorize (package-desc-name (emake-package-descriptor)) 'name)))))

(emake-defcommand emake-dependency-tree (&rest parameters)
  "Show dependencies of the current project recursively.

If any additional sets are specified, dependencies from the sets
are listed too.  Sets typically have the same name as commands.
E.g. there might be additional dependencies for testing (`test')
or evaluating (`eval') registered in the project's `Emake' file."
  :aliases        (dtree deptree requirement-tree rtree reqtree)
  :parameters     "[ADDITIONAL-SET...]"
  (let ((additional-sets (mapcar #'intern parameters)))
    (emake-load-project-dependencies additional-sets t)
    (let ((package (emake-package-descriptor))
          (listed  (make-hash-table :test #'equal)))
      (emake--do-dependency-tree (package-desc-name package) (package-desc-version package) package 0 listed)
      (dolist (set additional-sets)
        (dolist (dependency (cdr (assq set emake--extra-dependencies)))
          (emake--do-dependency-tree (car dependency) (cadr dependency) (emake-find-package-descriptor (car dependency) (cadr dependency)) 0 listed set))))))

(emake-defbooloptions emake-dependencies-list-built-ins emake-dependencies-omit-built-ins emake-dependencies-list-built-ins
  ("Also list dependencies that are Emacs built-ins"
   :options       (-b --list-built-ins))
  ("Omit built-in dependencies"
   :options       (-B --omit-built-ins))
  :for-command    (dependencies dependency-tree))

(defun emake--do-dependency-tree (package-name version descriptor level listed &optional additional-set)
  (let ((repeated (gethash (cons package-name version) listed)))
    (if (and (package-built-in-p package-name) (not emake-dependencies-list-built-ins))
        (emake-trace "Omitting dependency as a built-in: %s %s" package-name (emake-message-version version))
      (let (remarks
            important-remarks)
        (cond ((package-built-in-p package-name)
               (push "built-in" remarks))
              ((null descriptor)
               (push (emake-colorize "UNAVAILABLE" 'warn) important-remarks))
              (repeated
               (push (emake-colorize "repeated, see above" 'details) remarks))
              ((not (equal version (package-desc-version descriptor)))
               (push (emake-format-message "%s installed" (emake-message-version descriptor t)) remarks)))
        (when additional-set
          (push (emake-format-message "for `%s'" additional-set) remarks))
        (setf remarks (nconc (emake-unless-quiet remarks) important-remarks))
        (emake-output "%s%s %s%s" (make-string (* level 4) ? ) (emake-colorize package-name 'name) (emake-message-version version t)
                      (if remarks
                          (emake-format-message "    [%s]" (mapconcat #'identity remarks "; "))
                        "")))
      (when (and descriptor (not repeated))
        (puthash (cons package-name version) t listed)
        (dolist (dependency (package-desc-reqs descriptor))
          (emake--do-dependency-tree (car dependency) (cadr dependency) (emake-find-package-descriptor (car dependency) (cadr dependency)) (1+ level) listed))))))



;; emake version, emake info

(emake-defcommand emake-version (&rest parameters)
  "Display version information and exit.  By default version of
Emake itself is displayed.

You can also specify name of the package(s) you are interested in
on the command line.  These may include the project being built,
any of its dependencies, any dependencies of those (recursively),
anything registered with `emake-add-extra-dependencies', `emake'
itself or `emacs'.  If multiple versions are requested, each is
printed on a separate line.

If command line parameters include project dependencies, they are
installed first if needed.

Normally, package name and version is printed.  However, in quiet
mode output is restricted to just the version."
  :parameters     "[PACKAGE...]"
  :works-on-old-emake t
  (let* ((packages     (if parameters
                           (mapcar #'intern parameters)
                         '(emake)))
         (this-package (emake-package-descriptor)))
    ;; Special handling of the project itself so that its version can
    ;; be queried even if there are unavailable dependencies.
    (when (emake-any-p (not (or (eq it 'emacs) (eq it (package-desc-name this-package)) (emake-find-package-descriptor it nil t))) packages)
      (emake-load-project-dependencies (mapcar #'car emake--extra-dependencies)))
    (dolist (package packages)
      (let ((version (if (eq package 'emacs)
                         emacs-version
                       (let ((descriptor (if (eq package (package-desc-name this-package))
                                             this-package
                                           (emake-find-package-descriptor package nil t))))
                         (unless descriptor
                           (signal 'emake-error `(:hint ("Check output of `%s dependency-tree'" ,(emake-shell-command t))
                                                        "Package `%s' is not among those used in the build" ,package)))
                         (emake-message-version descriptor)))))
        (emake-print :nolf "%s " package)
        (emake-output "%s" version)))))

(emake-defcommand emake-info (&rest parameters)
  "Display information about the project."
  :works-on-old-emake t
  (when parameters
    (signal 'emake-wrong-command-usage `(t "Unexpected command parameters")))
  (let* ((package     (emake-package-descriptor))
         (description (when (fboundp 'package--get-description) (package--get-description package))))
    (unless (> (length description) 0)
      (setf description (package-desc-summary package)))
    (emake-output "%s %s" (emake-colorize (package-desc-name package) 'name) (emake-message-version package))
    (when description
      (setf description (replace-regexp-in-string (rx (| (: bos (1+ (or space "\n"))) (: (1+ (or space "\n")) eos))) "" description))
      (unless (string= description "")
        (emake-output "\n%s" description)))))



;; emake test

(declare-function emake-test-ert-preprocess-selectors "emake-ert" (selectors))
(declare-function emake-test-ert-load-results "emake-ert" ())
(declare-function emake-test-ert-save-results "emake-ert" ())
(declare-function emake-run-ert-tests "emake-ert" (selectors &optional environment))

(defvar emake-test-dwim t
  "Employ some heuristics when parsing command line of `test' command.

* Any selector that ends in `.el' is instead treated as a file
  pattern.

* For ERT: any symbol selector that doesn’t match a test name is
  instead treated as regular expression (i.e. as a string).

These heuristics are aimed at further simplifying test execution.")

(defvar emake-test-stop-on-unexpected nil)
(defvar emake-test-print-backtraces t)
(defvar emake-test-expect-at-least nil)

(defvar emake-test-framework nil)
(defvar emake-test-runner nil)
(defvar emake--test-runners nil)

(defvar emake-test-file-patterns nil)

(defvar emake-test-known-frameworks '((ERT . ((detect               . (lambda () (featurep 'ert)))
                                              (preprocess-selectors . (lambda (selectors)
                                                                        (require 'emake-ert)
                                                                        (emake-test-ert-preprocess-selectors selectors)))
                                              (prepare              . (lambda (_selectors)
                                                                        (require 'emake-ert)
                                                                        (emake-test-ert-load-results)))
                                              (run-tests            . (lambda (selectors _runner environment)
                                                                        (require 'emake-ert)
                                                                        (emake-run-ert-tests selectors environment)))
                                              (finalize             . (lambda (_selectors)
                                                                        (require 'emake-ert)
                                                                        (emake-test-ert-save-results))))))
  "Alist of all test frameworks known to Emake.
While this variable is public and can be modified, you most
likely shouldn't do that.  Leave adding support for more
frameworks to Emake code, instead write a test runner if you need
it.")


(emake-defcommand emake-test (&rest parameters)
  "Run project's regression/unit tests.  By default all tests
defined in the project are executed.  However, you can restrict
their set using SELECTORs and/or `--file' option listed above.

If `--file' is specified, only files that match the PATTERN (and
also project's test fileset) are loaded.  See documentation on
Emake filesets for details of what can be specified as argument.
The option can be used multiple times, in which case all its
arguments are combined into a single fileset.

Remember that files are loaded and executed as normal Elisp, so
`require' forms can result in loading more than you specify.

SELECTORs can be used to choose tests to run among the loaded
ones.  Multiple are combined as `or' operation: all tests that
match any of the specified selectors are executed.  When used in
combination with `--file', they result in `and' operation: only
those tests that are loaded and match the selectors are executed.

If `emake-test-dwim' variable is on (default), Emake employs
additional heuristics when processing SELECTORs.  This variable
is not controllable from command line---change its value in
`~/.emake/config' or `Emake-local'.

This command exits with error code 1 if any test produces an
unexpected result."
  :parameters     "[SELECTOR...]"
  (emake-load-project-dependencies 'test)
  (let ((files (emake-find-and-trace-files `(:and ,(emake-standard-fileset 'test) "*.el") "test `.el' file%s"))
        selectors)
    (when files
      (let ((filter-patterns emake-test-file-patterns))
        (dolist (selector parameters)
          (if (and emake-test-dwim (string-match-p (rx ".el" eol) selector))
              (push selector filter-patterns)
            (push selector selectors)))
        (setf selectors (nreverse selectors))
        (when filter-patterns
          (setf files (emake-filter-files files (reverse filter-patterns)))
          (emake-trace "%s" (emake-message-enumerate-files "Remaining test `.el' file%s after applying `--file' filter(s): %s (%d)" files)))))
    ;; Framework autoguessing can only work if there is at least one file to load, so this
    ;; `if' is important.
    (if files
        (progn
          (dolist (file files)
            (let* ((absolute-without-el (replace-regexp-in-string (rx ".el" eos) "" (expand-file-name file emake-project-dir) t t))
                   (already-loaded      (emake-any-p (assoc (concat absolute-without-el it) load-history) load-suffixes)))
              (if already-loaded
                  (emake-trace "Not loading file `%s': already `require'd by some other file" file)
                (emake-verbose "Loading test file `%s'..." file)
                (load absolute-without-el nil t nil t))))
          (let* ((runner-name (or emake-test-runner 'simple))
                 (runner      (or (cdr (assq runner-name emake--test-runners))
                                  (signal 'emake-error `(:hint ("Check output of `%s test --list-runners'" ,(emake-shell-command t)) "Unknown test runner `%s'" ,runner-name))))
                 (supported   (emake-get runner :frameworks))
                 (framework   (or (emake-test-framework)
                                  (signal 'emake-error `(:hint "Consider setting `emake-test-framework' explicitly" "Unable to guess testing framework")))))
            (unless (or (null supported) (eq framework supported) (and (listp supported) (memq framework supported)))
              (signal 'emake-error `(:hint ("Run `%s test --list-runners' for more information" ,(emake-shell-command t))
                                           "Test runner `%s' doesn't support framework `%s'" ,runner-name ,framework)))
            (setf selectors (emake-test-preprocess-selectors framework selectors))
            (emake-test-prepare-framework framework selectors)
            (unwind-protect
                (funcall runner framework selectors)
              (emake-test-finalize-framework framework selectors))))
      (emake-print "No test files to load"))))

(defun emake-test-get-framework-data (framework)
  (or (cdr (assq framework emake-test-known-frameworks))
      (error "Unknown test framework `%s'" framework)))

(defun emake-test-framework ()
  (or emake-test-framework
      (let ((scan emake-test-known-frameworks)
            detected)
        (while scan
          (let* ((framework-data (pop scan))
                 (detect         (cdr (assq 'detect (cdr framework-data)))))
            (when (and detect (funcall detect))
              (setf detected (car framework-data)
                    scan     nil))))
        detected)))

(defun emake-test-preprocess-selectors (framework selectors)
  (let ((preprocess-selectors (cdr (assq 'preprocess-selectors (emake-test-get-framework-data framework)))))
    (if preprocess-selectors
        (funcall preprocess-selectors selectors)
      selectors)))

(defun emake-test-selectors-to-elisp-values (selectors &optional cons-with-string)
  (mapcar (lambda (selector)
            (let ((as-elisp (emake-read-wholly selector "selector `%s'")))
              (if cons-with-string `(,as-elisp . ,selector) as-elisp)))
          selectors))

(defun emake-test-prepare-framework (framework selectors)
  (let ((prepare (cdr (assq 'prepare (emake-test-get-framework-data framework)))))
    (when prepare
      (funcall prepare selectors))))

(defun emake-test-finalize-framework (framework selectors)
  (condition-case error
      (let ((finalize (cdr (assq 'finalize (emake-test-get-framework-data framework)))))
        (when finalize
          (funcall finalize selectors)))
    (error (emake-warn "When finalizing test framework `%s': %s" framework (error-message-string error)))))

(defmacro emake-do-load-cache-file (file description expected-version &rest body)
  (declare (indent 3) (debug (stringp stringp numberp body)))
  `(condition-case error
       (if (file-exists-p ,file)
           (progn
             (emake-trace "Reading %s from file `%s'..." ,description (file-relative-name ,file emake-project-dir))
             (with-temp-buffer
               (insert-file-contents ,file)
               (let* ((contents (read (current-buffer)))
                      (version  (cdr (assq 'version contents))))
                 (if (> version ,expected-version)
                     (emake-verbose "Ignoring %s in file `%s': version %d must be from the future (expected at most %d)" ,description ,file version ,expected-version)
                   ,@body))))
         (emake-trace "No file `%s', not reading %s" (file-relative-name ,file emake-project-dir) ,description))
     ;; Since this is not overly important, just print a verbose-level message.
     (error (emake-verbose "Failed to load %s: %s" ,description (error-message-string error)))))

(defmacro emake-test-do-load-results (file-extension description expected-version &rest body)
  (declare (indent 3) (debug (stringp stringp numberp body)))
  (let ((file (expand-file-name (format "test-results.%s" file-extension) (emake-cache-dir t))))
    `(emake-do-load-cache-file ,file ,description ,expected-version ,@body)))

(defmacro emake-do-save-cache-file (file description version &rest body)
  (declare (indent 3) (debug (stringp stringp numberp body)))
  `(condition-case error
       (with-temp-file ,file
         (emake-trace "Saving %s to file `%s'..." ,description (file-relative-name ,file emake-project-dir))
         (let ((data                       (progn ,@body))
               (print-circle               nil)
               (print-continuous-numbering nil))
           (prin1 (cons (cons 'version ,version) data) (current-buffer))
           (insert "\n")))
     ;; Since this is not overly important, just print a verbose-level message.
     (error (emake-verbose "Failed to save %s: %s" ,description (error-message-string error)))))

(defmacro emake-test-do-save-results (file-extension description version &rest body)
  (declare (indent 3) (debug (stringp stringp numberp body)))
  (let ((file (expand-file-name (format "test-results.%s" file-extension) (emake-cache-dir t t))))
    `(emake-do-save-cache-file ,file ,description ,version ,@body)))

(defun emake-test-validate-amount (num-tests)
  (when (and emake-test-expect-at-least (< num-tests emake-test-expect-at-least))
    (signal 'emake-error `("Too few tests match the selectors (%d, expected at least %d)" ,num-tests ,emake-test-expect-at-least))))


(emake-defcleaner emake-cleaner-test-results ()
  "Forget all test results for this Emacs version.  All tests
will count as new for frameworks that have appropriate
selectors (e.g. ERT's `:new')."
  :aliases        (tests)
  (emake-find-files (format "./%s/test-results.*" (file-relative-name (emake-cache-dir t) emake-project-dir))))


;; Internal helper for `emake-deftestrunner'.
(defun emake--register-test-runner (runner name keywords)
  (while keywords
    (emake-pcase-exhaustive (pop keywords)
      ((and (or :briefdoc :frameworks) keyword)
       (emake-put runner keyword (pop keywords)))))
  (emake--assq-set name runner emake--test-runners))

(defmacro emake-deftestrunner (name arguments &rest body)
  "Define an Emake test runner."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body (emake-macroexp-parse-body body))
        (runner-name (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-") (? "test-runner-")) "" (symbol-name name))))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:name   (setf runner-name (pop body)))
        (keyword (push keyword keywords) (push (pop body) keywords))))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (emake--register-test-runner ',name ',runner-name ',(nreverse keywords)))))


(emake-deftestrunner emake-test-runner-standard (framework selectors)
  "Invokes test framework without changing anything."
  (funcall (cdr (assq 'run-tests (emake-test-get-framework-data framework))) selectors 'standard nil))

(emake-deftestrunner emake-test-runner-simple (framework selectors)
  "Simple test runner with a few tweaks to the defaults.

For ERT:
  - if Emake is in quiet mode, set `ert-quiet' to t;
  - only backtrace frames inside test functions are printed;
  - long line trimming is disabled."
  ;; Workaround: older Emacs versions don't support setting
  ;; `ert-batch-backtrace-right-margin' to nil.  We assume that if the
  ;; variable is customizable, nil is already supported.
  (let ((right-margin (if (get 'ert-batch-backtrace-right-margin 'custom-type)
                          nil
                        1000000)))
    (funcall (cdr (assq 'run-tests (emake-test-get-framework-data framework))) selectors 'simple
             ;; Non-ERT frameworks are just invoked with empty environment.
             (pcase framework
               (`ERT `((ert-quiet                        . ,(not (emake-unless-quiet t)))
                       (ert-batch-backtrace-right-margin . ,right-margin)
                       (emake--test-ert-short-backtraces . t)))))))

(emake-defoption emake-test-files (pattern)
  "Load only tests in given file(s)"
  :options        (-f --file)
  :for-command    test
  :value          PATTERN
  (push pattern emake-test-file-patterns))

(emake-defbooloptions emake-test-stop-on-unexpected-mode emake-test-continue-mode emake-test-stop-on-unexpected
  ("Stop if any test produces an unexpected result (usually a failure)"
   :options       (-s --stop --stop-on-unexpected))
  ("Execute all scheduled tests regardless of results"
   :options       (-c --continue))
  :for-command    test)

(emake-defbooloptions emake-test-print-backtraces emake-test-omit-backtraces emake-test-print-backtraces
  ("Print failure backtraces"
   :options       (-b --print-backtraces))
  ("Omit failure backtraces for brevity"
   :options       (-B --omit-backtraces))
  :for-command    test)

(emake-defoption emake-test-expect-at-least (n)
  "Fail if fewer than N tests match selectors"
  :options        (-X --expect)
  :for-command    test
  :value          N
  (setf emake-test-expect-at-least (string-to-number n)))

(emake-defoption emake-test-runner (name)
  "Choose test runner"
  :options        (-r --runner)
  :for-command    test
  :value          NAME
  :default-value  (or emake-test-runner 'simple)
  ;; Will be validated when executing the command.
  (setf emake-test-runner (intern name)))

(emake-defoption emake-test-list-runners ()
  "List available test runners and exit"
  :options        --list-runners
  :for-command    test
  (let ((all-runners (reverse emake--test-runners)))
    (while all-runners
      (let* ((runner   (pop all-runners))
             (name     (car runner))
             (function (cdr runner)))
        (if (emake-unless-quiet t)
            (progn (emake-output "%s\n" (emake-colorize name 'section))
                   (emake-output "    %s\n" (emake-message-enumerate '("Supported framework:" "Supported frameworks:")
                                                                     (or (emake-get function :frameworks) (mapcar #'car emake-test-known-frameworks)) nil t))
                   (emake-output "%s" (or (emake-documentation function) "Not documented"))
                   (when all-runners
                     (emake-output "\n")))
          (let ((documentation (emake-briefdoc function)))
            (if documentation
                (emake-output "%-18s  %s" name documentation)
              (emake-output "%s" name)))))))
  (signal 'emake-quit 0))



;; emake eval, emake exec

(defvar emake-default-required-features :project-name
  "Features that certain commands require.
Currently affected commands are `eval', `exec' and `emacs'.
There are also variables `emake-eval-required-features' and
`emake-emacs-required-features' that take precedence if they have
an appropriate value.

If let as `:project-name', defaults to project's name and assumes
that the project provides that feature.  Otherwise should be a
single symbol or a list of symbols (empty list---nil---of course
means not to require anything).")

(defvar emake-eval-lexical t)
(defvar emake-eval-require-main-feature t)
(defvar emake-eval-printer-function #'emake-prin1
  "Default printer function for `eval' command.
Standard value is `emake-prin1', but can be customized.")

(defvar emake-eval-required-features :default
  "Features that `eval' and `exec' commands require.
See `emake-default-required-features' for value description.
Special value `:default' means to use that variable's value
instead.")

(emake-defcommand emake-eval (&rest parameters)
  "Evaluate Lisp expressions and print results.  Expressions are
evaluated in project's environment, with all the dependencies
available.  If option `-r' is specified (or is on by default),
project's main feature will be required before evaluating, so
that you don't have to include `(require ...)' yourself."
  :parameters     "EXPRESSION..."
  :aliases        evaluate
  (emake--do-eval t parameters))

(emake-defcommand emake-exec (&rest parameters)
  "Execute Lisp forms for side effect.  Forms are executed in
project's environment, with all the dependencies available.  If
option `-r' is specified (or is on by default), project's main
feature will be required before evaluating, so that you don't
have to include `(require ...)' yourself.

This is basically like `eval' command, with the only difference
being that it doesn't print form results."
  :parameters     "FORM..."
  :aliases        execute
  (emake--do-eval nil parameters))

(defun emake--do-eval (print-results parameters)
  (unless parameters
    (signal 'emake-wrong-command-usage `(t ,(if print-results "Missing expressions to evaluate" "Missing forms to execute"))))
  (let ((forms (mapcar (lambda (parameter) (cons parameter (emake-read-wholly parameter (if print-results "expression" "form to evaluate")))) parameters)))
    (emake-load-project-dependencies (if print-results 'eval 'exec))
    (when emake-eval-require-main-feature
      (dolist (feature (emake-required-features emake-eval-required-features))
        (emake-verbose "Autorequiring feature `%s' before %s" feature (if print-results "evaluating" "executing"))
        (require feature)))
    (dolist (form forms)
      (emake-verbose (if print-results "Evaluating expression `%s':" "Executing form `%s'...") (car form))
      (let ((result (eval (cdr form) emake-eval-lexical)))
        (when print-results
          (with-temp-buffer
            (funcall (or emake-eval-printer-function #'prin1) result (current-buffer))
            ;; Older Emacs version end some value representations with a linefeed for
            ;; whatever reasons.
            (when (equal (char-before) ?\n)
              (delete-char -1))
            (emake-output "%s" (buffer-string))))))))

(emake-defbooloptions emake-eval-lexical-mode emake-eval-dynamic-mode emake-eval-lexical
  ("Evaluate expressions using lexical scoping"
   :options       (-l --lexical))
  ("Use dynamic (i.e. not lexical) scoping when evaluating"
   :options       (-d --dynamic --not-lexical))
  :for-command    (eval exec))

(emake-defbooloptions emake-eval-require-main-feature emake-eval-dont-require-main-feature emake-eval-require-main-feature
  ("Require project's main feature before evaluating"
   :options       (-r --require))
  ("Don't require project's features before evaluating"
   :options       (-R --dont-require))
  :for-command    (eval exec))

(emake-defoption emake-eval-printer (function)
  "Use given function to print results"
  :options        (-p --printer)
  :for-command    eval
  :value          FUNCTION
  :default-value  (or emake-eval-printer-function #'prin1)
  (let ((function (emake-read-wholly function "printer function")))
    (unless (fboundp function)
      (signal 'emake-wrong-option-usage `("undefined function `%s'" ,function)))
    (setf emake-eval-printer-function function)))

(emake-defoption emake-eval-use-standard-printer ()
  "Use `prin1' (i.e. standard built-in) to print results"
  :options        (-s --standard-printer)
  :for-command    eval
  (setf emake-eval-printer-function #'prin1))

(declare-function cl-prin1 "cl-print" (object &optional stream))

(defun emake-prin1 (object &optional stream)
  "Print an object similarly to `prin1'.
Current implementation uses `cl-prin1' if available (Emacs 26 and
later), or else just calls `prin1'.

This may be changed in later Emake versions: this function is
supposed to provide output useful for humans, not fulfil strict
obligations."
  (if (require 'cl-print nil t)
      (cl-prin1 object stream)
    (pp object stream)))

(defun emake-required-features (feature-names)
  (mapcar (lambda (feature)
            (if (eq feature :project-name) (package-desc-name (emake-package-descriptor)) feature))
          (emake-flatten-tree (mapcar (lambda (feature)
                                        (if (eq feature :default) emake-default-required-features feature))
                                      (emake-listify feature-names)))))



;; emake emacs

(defvar emake-emacs-default-command-line '("--no-init-file" "--no-site-file" "--no-site-lisp" "--no-splash"))

(defvar emake-emacs-required-features :default
  "Features that spawned Emacs processes require automatically.
See `emake-default-required-features' for value description.
Special value `:default' means to use that variable's value
instead.")

(emake-defcommand emake-emacs (&rest parameters)
  "Launch Emacs in a prepared environment.  Emacs will be able to
load the project and all its dependencies.

This command parses contents of command line specially.
Normally, contents of command line used to launch Emacs looks
like this:

  - options from `emake-emacs-default-command-line' (unless
    changed in file `Emake' or elsewhere, these disable all
    configuration files and the splash screen);

  - Elisp `require' forms to load `emake-emacs-required-features'
    (which default to project name);

  - COMMAND-LINE as passed to this command.

However, if COMMAND-LINE has `--' at the first position, Emake
passes the rest of it to Emacs without adding anything on its
own.  Be advised that in this case you will likely need to
specify at least `-q' (`--no-init-file') option to be passed to
Emacs, else it will most likely fail."
  :parameters     "[--] [COMMAND-LINE...]"
  :custom-parsing t
  (emake-load-project-dependencies 'emacs)
  (let* ((command-line        (if (string= (car parameters) "--")
                                  (cdr parameters)
                                (append emake-emacs-default-command-line
                                        (apply #'append (mapcar (lambda (feature) (list "--eval" (format "(require '%s)" feature)))
                                                                (emake-required-features emake-emacs-required-features)))
                                        parameters)))
         (effective-load-path (mapconcat #'identity load-path path-separator))
         (process-environment `(,(format "EMACSLOADPATH=%s" effective-load-path) ,@process-environment)))
    (emake-verbose "Full command line to run child Emacs process:\n  %s" (emake-message-command-line emake-emacs-executable command-line))
    (emake-verbose "Effective load path for it:\n  %s" effective-load-path)
    (emake-call-process emake-emacs-executable command-line
      (emake--forward-process-output "Output of the child Emacs process:" "Child Emacs process produced no output")
      (unless (= exit-code 0)
        (signal 'emake-error `("Child Emacs process exited with error code %d" ,exit-code))))))



;; emake targets, emake build, emake compile, emake package

(defvar emake--builders nil)
(defvar emake--build-targets (make-hash-table :test #'equal))
(defvar emake--targets-prepared-for nil)

(defvar emake-build-sets nil)
(defvar emake-build-keep-going nil)
(defvar emake-build-treat-warnings-as-errors nil)
(defvar emake-build-suppress-warnings nil)

(defvar emake-build-force-rebuilding nil
  "Targets that are to be rebuilt even if they appear up-to-date.
Can be either a list of target names or symbol t, in which case
all targets will be force-built.  However, a target will only
ever be force-built if it is otherwise a part of building plan.")

(defvar emake-build-infinitely-new nil
  "Files (sources or intermediate targets) that are “infinitely new”.
Can be either a list of filenames or symbol t, meaning “all of
them”.  Targets that are built from these sources or depend on
them will never be found up-to-date.")

(defvar emake-build-dry-run-mode nil)

(defvar emake-build-current-targets nil)

(defvar emake-package-generate-entry-file nil)
(defvar emake-package-forced-version nil)
(defvar emake-package-output-dir nil)
(defvar emake-package-print-filename nil)

(defvar emake--build-plan nil)
(defvar emake--build-results nil)

(defvar emake-targets-list-dependencies nil)

(defvar emake--target-dependencies nil)
(defvar emake--target-dependencies-need-saving nil)


(defsubst emake-virtual-target-p (target)
  (string-prefix-p ":" target))

(defun emake-build-find-targets (&rest standard-filesets)
  "Return a hash table of all build targets in given filesets.
STANDARD-FILESETS must be a list of names from `emake-filesets'
variable or `all'.  The hash table might contain targets from
other filesets too.  Caller must not modify contents of the
returned hash table.

Hash table keys are strings, i.e. names of targets.  Entries,
each describing a builder invocations, are alists with at least
the following keys, in no particular order:

  - builder: name (symbol) of the builder that can generate this
    target, see macro `emake-defbuilder';

  - sources: list of files or other targets that serve as sources
    for this one; each entry must be a file or a real target, not
    virtual;

  - targets: all targets that will be build by this invocation;
    will include at least the target used as the key, but may
    include more (depends on the builder).

Returned hash table doesn't contain cross-target dependency
information past the list of sources.  For this, use function
`emake-get-target-dependencies'."
  (when (or (null standard-filesets) (memq 'all standard-filesets))
    (setf standard-filesets (mapcar #'car emake-filesets)))
  (let ((new-filesets (emake-filter (not (memq it emake--targets-prepared-for)) standard-filesets)))
    (when new-filesets
      (setf new-filesets (nreverse new-filesets))
      (emake-trace "Generating build target list for %s" (emake-message-enumerate "standard fileset" new-filesets))
      ;; FIXME: The point of special-casing `package' builder is to let it know all the
      ;;        sources at once, because its target depends on the number of sources
      ;;        (`.el' if one file, `.tar' otherwise).  This is of course a hack, but it
      ;;        is not clear how to generalize that.  Postponed until some real-world
      ;;        needs come up.
      (let* ((special-cased-builder (assq 'package emake--builders))
             (reordered-builders    (if special-cased-builder
                                        (append (remq special-cased-builder emake--builders) (list special-cased-builder))
                                      emake--builders))
             (potential-new-sources (emake-find-files (apply #'emake-standard-filesets new-filesets)))
             (visited-sources       (make-hash-table :test #'equal))
             special-cased-builder-sources
             new-sources)
        (while (setf new-sources (emake-filter-files (emake-filter (not (gethash it visited-sources)) potential-new-sources)
                                                     `(:not ,emake-standard-excludes)))
          (dolist (source new-sources)
            (puthash source t visited-sources))
          (setf potential-new-sources nil)
          (dolist (entry reordered-builders)
            (let* ((builder-name (car entry))
                   (builder      (cdr entry))
                   (source-files (emake-get builder :source-files)))
              (when source-files
                (setf source-files (emake-filter-files new-sources source-files)))
              (when (eq entry special-cased-builder)
                (setf source-files (append special-cased-builder-sources source-files)))
              (when source-files
                (if (eq entry special-cased-builder)
                    (setf special-cased-builder-sources source-files)
                  (dolist (invocation (emake--build-find-builder-invocations source-files builder builder-name))
                    (let ((sources (car invocation))
                          (targets (cdr invocation)))
                      (emake--build-target-entries targets builder builder-name sources)
                      (setf potential-new-sources (append targets potential-new-sources)))))))))
        (when special-cased-builder-sources
          (let ((builder-name (car special-cased-builder))
                (builder      (cdr special-cased-builder)))
            (dolist (invocation (emake--build-find-builder-invocations special-cased-builder-sources builder builder-name))
              (emake--build-target-entries (cdr invocation) builder builder-name (car invocation))))))))
  (unless (gethash ":default" emake--build-targets)
    (emake--build-target-entries '(":default") nil nil nil))
  emake--build-targets)

(defun emake--build-find-builder-invocations (sources builder builder-name)
  (let ((target-rule (emake-get builder :targets)))
    (when (functionp target-rule)
      (setf target-rule (funcall target-rule sources)))
    (pcase target-rule
      ((pred stringp)
       `((,sources . (,target-rule))))
      ((pred emake-string-list-p)
       `((,sources . ,target-rule)))
      (`(,(and (or (pred stringp) (pred emake-string-list-p)) source-suffixes) -> ,(and (pred stringp) target-suffix))
       (setf source-suffixes (emake-listify source-suffixes))
       (let (result)
         (dolist (source sources)
           (let ((scan source-suffixes)
                 found)
             (while scan
               (let ((source-suffix (pop scan)))
                 (when (string-suffix-p source-suffix source)
                   (push `((,source) . (,(emake-replace-suffix source source-suffix target-suffix))) result)
                   (setf scan  nil
                         found t))))
             (unless found
               (error "Builder `%s': name of source file `%s' doesn't end with %s as `:targets' wants"
                      builder-name source (emake-message-enumerate nil source-suffixes nil nil "or")))))
         (nreverse result)))
      (_ (error "Invalid `:targets' (or its return value) %S in builder `%s'" target-rule builder-name)))))

(defun emake--build-target-entries (targets builder builder-name sources)
  (let ((sorted-targets (sort (copy-sequence targets) #'string<))
        shared-entry
        created-new-entry)
    (dolist (target targets)
      (if (emake-virtual-target-p target)
          (when builder-name
            (error "Virtual targets (e.g. `%s') must not have builders" target))
        (unless builder-name
          (error "Real targets (e.g. `%s') must have associated builders" target)))
      (let ((entry (gethash target emake--build-targets)))
        (if shared-entry
            (if (or entry (not created-new-entry))
                (unless (eq entry shared-entry)
                  (error "Unexpected entry %S for target `%s': expected %S" entry target shared-entry))
              (setf entry shared-entry))
          (if entry
              (let ((entry-builder (cdr (assq 'builder entry))))
                (unless (eq entry-builder builder-name)
                  (error "Conflicting builders `%s' and `%s' for target `%s'" entry-builder builder-name target))
                ;; Since both target lists are sorted, we can compare just like this.
                (unless (equal (cdr (assq 'sorted-targets entry)) sorted-targets)
                  (error "Unexpected full target list %s for target `%s': expected %s"
                         (emake-message-enumerate nil (cdr (assq 'targets entry)) nil nil t) target (emake-message-enumerate nil targets nil nil t))))
            (setf entry             `((builder        . ,builder-name)
                                      (targets        . ,targets)
                                      (sorted-targets . ,sorted-targets))
                  created-new-entry t))
          (let* ((existing-sources (cdr (assq 'sources entry)))
                 (new-sources      (if existing-sources (emake-filter (not (member it existing-sources)) sources) sources)))
            (emake--assq-set 'sources (append existing-sources new-sources) entry))
          (setf shared-entry entry))
        (puthash target entry emake--build-targets))))
  (when builder
    (emake--build-collect-targets targets builder)))

(defun emake--build-collect-targets (targets builder)
  (let ((collect (emake-get builder :collect)))
    (when (functionp collect)
      (setf collect (funcall collect targets)))
    (dolist (entry (cond ((or (stringp collect) (emake-string-list-p collect))
                          `((,targets ,collect)))
                         ((emake-all-p (pcase it (`(,(or (pred stringp) (pred emake-string-list-p))
                                                    ,(or (pred stringp) (pred emake-string-list-p)))
                                                  t))
                                       collect)
                          collect)))
      (let ((entry-targets (emake-listify (car entry))))
        (dolist (virtual-target (reverse (emake-listify (cadr entry))))
          (unless (emake-virtual-target-p virtual-target)
            (error "Expected a virtual target, `%s' isn't" virtual-target))
          (emake--build-target-entries (list virtual-target) nil nil entry-targets)
          (setf entry-targets (list virtual-target)))))))


(defun emake-get-target-dependencies (target)
  "Get TARGET's dependencies.
Usually, this function should be of no interest: custom builders
should normally use only `emake-set-target-dependencies'.
However, it is allowed to use this function as described below.

Returned list's elements look like (TYPE DEPENDENCY [...]).
Following types (symbols) are currently defined:

  - depends-on: DEPENDENCY must always be built (or be
    up-to-date) before TARGET is built;

  - inherits: TARGET inherits all sources (which become
    `depends-on' dependencies) and dependencies of DEPENDENCY;
    this is less strict than `depends-on';

For future compatibility, callers should treat any unknown type
as `depends-on'.  It must not assume anything about contents of
the elements past the two described values.

Returned list must not be modified.  Instead, use function
`emake-set-target-dependencies' when needed.

This function may only be called while inside the body of a
`emake-with-target-dependencies' macro."
  (gethash target emake--target-dependencies))

(defun emake-set-target-dependencies (target dependencies)
  "Set the list of TARGET's dependencies.
See documentation of `emake-get-target-dependencies' for list's
elements description.

Evaluates to non-nil if dependencies are changed, to nil if they
are exactly the same as before (possibly in different order).

This function may only be called while inside the body of a
`emake-with-target-dependencies' macro."
  (let ((current-dependencies (emake-get-target-dependencies dependencies)))
    (unless (equal (sort (copy-sequence dependencies)         (lambda (a b) (string< (car a) (car b))))
                   (sort (copy-sequence current-dependencies) (lambda (a b) (string< (car a) (car b)))))
      (puthash target (copy-sequence dependencies) emake--target-dependencies)
      (setf emake--target-dependencies-need-saving t))))

(defmacro emake-with-target-dependencies (&rest body)
  (declare (indent 0) (debug (body)))
  `(progn (emake--load-target-dependencies)
          (unwind-protect
              (progn ,@body)
            (emake--save-target-dependencies))))

(defun emake--load-target-dependencies ()
  (emake-do-load-cache-file (expand-file-name "target-dependencies.build" (emake-cache-dir t)) "target dependencies" 1
    (setf emake--target-dependencies (cdr (assq 'dependencies contents))))
  (unless emake--target-dependencies
    (setf emake--target-dependencies (make-hash-table :test #'equal))))

(defun emake--save-target-dependencies ()
  (if emake--target-dependencies-need-saving
      (emake-do-save-cache-file (expand-file-name "target-dependencies.build" (emake-cache-dir t)) "target dependencies" 1
        `((dependencies . ,emake--target-dependencies)))
    (emake-trace "Target dependency information is up-to-date, not saving...")))


;; Internal helper for `emake-defbuilder'.
(defun emake--register-builder (builder name keywords)
  (let (cleaner-specifications)
    (while keywords
      (emake-pcase-exhaustive (pop keywords)
        ((and (or :type :short-name :message :source-files :targets :collect :briefdoc) keyword)
         (emake-put builder keyword (pop keywords)))
        (:define-cleaner
         (push (pop keywords) cleaner-specifications))))
    (emake--assq-set name builder emake--builders)
    (dolist (specification cleaner-specifications)
      (eval `(emake-defcleaner ,(car specification) ()
               ,@(cdr specification)
               (let (targets)
                 (dolist (entry (emake--build-find-builder-invocations (emake-find-files `(:and (emake-get ',',builder :source-files) (emake-clean-fileset)))
                                                                       ',builder ',name))
                   (setf targets (append targets (cdr entry))))
                 targets))))))


(defmacro emake-defbuilder (name arguments &rest body)
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body  (emake-macroexp-parse-body body))
        (builder-name (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-") (? "builder-")) "" (symbol-name name))))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:name   (setf builder-name (pop body)))
        (keyword (push keyword keywords) (push (pop body) keywords))))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (emake--register-builder ',name ',builder-name ',(nreverse keywords)))))


(emake-defcommand emake-targets (&rest parameters)
  "Show tree of building targets in the project.  Out of the box
Emake defines rules to byte-compile `.el' files, generate `.info'
from `.texi' files and build Elisp packages.  These are combined
into `:compile', `:package' and `:package-archive-entry' virtual
targets.  Each project can additionally define more targets in
its `Emake' file.

Every project will also have virtual target `:default', possibly
empty.

In case `--dependencies' option is specified, lines starting with
[...] show target dependencies:

  [dep]: hard dependency: this must be built before the main
         target;

  [inh]: main target inherits dependencies and sources (which
         also become dependencies) from the one it depends on.

However, dependencies are listed only if they are known.  Usually
this means that you need to compile the project once — then Emake
will know changes in which `.el' files might require
recompilation of seemingly unrelated `.elc'."
  :parameters     "[TARGET-SET...]"
  :aliases        target-tree
  (let ((all-targets (if parameters
                         (apply #'emake-build-find-targets (mapcar #'emake-validate-standard-fileset parameters))
                       (emake-build-find-targets 'main))))
    (if (> (hash-table-count all-targets) 0)
        (let ((sources (make-hash-table :test #'equal))
              toplevel)
          (maphash (lambda (_target entry)
                     (dolist (source (cdr (assq 'sources entry)))
                       (puthash source t sources)))
                   all-targets)
          (maphash (lambda (target _)
                     (unless (gethash target sources)
                       (push target toplevel)))
                   all-targets)
          (emake-with-target-dependencies
            ;; This is done only to detect cyclic dependencies.
            (let ((emake--build-plan (make-hash-table :test #'equal)))
              (maphash (lambda (target _entry) (emake--build-add-to-plan target all-targets)) all-targets))
            (emake--do-targets toplevel 0 all-targets (make-hash-table :test #'eq))))
      (emake-print "There are no targets for %s" (if (> (length parameters) 1) "these filesets" "this fileset")))))

(emake-defbooloptions emake-targets-list-dependencies emake-targets-omit-dependencies emake-targets-list-dependencies
  ("List known target dependencies"
   :options       (-d --dependencies))
  ("Don't show target dependencies, even if known"
   :options       (-D --no-dependencies))
  :for-command    targets)


(defun emake--do-targets (level-targets level all-targets printed-target-entries)
  (when level-targets
    (let ((indentation   (make-string (* level 4) ? ))
          (level-targets (copy-sequence level-targets)))
      (setf level-targets (sort level-targets #'string<))
      (dolist (prioritized-target '(":package" ":default"))
        (when (member prioritized-target level-targets)
          (setf level-targets `(,prioritized-target ,@(delete prioritized-target level-targets)))))
      (dolist (target level-targets)
        (let* ((entry       (gethash target all-targets))
               (repeated    (gethash entry printed-target-entries))
               (builder     (cdr (assq 'builder entry)))
               (sources     (cdr (assq 'sources entry)))
               (target-name (emake-colorize target (cond ((emake-virtual-target-p target) 'section) (sources 'name)) target)))
          (cond ((and repeated builder)
                 (emake-output "%s%s  [%s]" indentation target-name
                               (emake-colorize (if (equal repeated target) "repeated, see above" (emake-format-message "repeated, see `%s' above" repeated)) 'details)))
                ((and builder (emake-unless-quiet t))
                 (emake-output "%s%s  [%s]" indentation target-name (or (emake-get (cdr (assq builder emake--builders)) :short-name) builder)))
                (t
                 (emake-output "%s%s" indentation target-name)))
          (unless repeated
            (puthash entry target printed-target-entries)
            (emake--do-targets sources (1+ level) all-targets printed-target-entries)
            (when emake-targets-list-dependencies
              (dolist (dependency (sort (copy-sequence (emake-get-target-dependencies target)) (lambda (a b) (string< (car a) (car b)))))
                (emake-output "%s    %s %s" indentation
                              (emake-colorize (emake-pcase-exhaustive (car dependency)
                                                (`depends-on "[dep]")
                                                (`inherits   "[inh]"))
                                              'details)
                              (cadr dependency))))))))))


(emake-defcommand emake-build (&rest parameters)
  "Build targets in the project.

TARGETs should form a fileset.  Only those targets within
requested sets (see option `--set' above) that also match the
fileset will be built, together with their dependencies, of
course.  In addition, virtual targets, i.e. with names starting
with `:', can be included.  These are handled separately from
real targets and must be named exactly, i.e. their names are not
considered part of fileset.

If no targets are specified on the command line, build virtual
target `:default'.

Out of the box Emake defines rules to byte-compile `.el' files,
generate `.info' from `.texi' files and build Elisp packages.
However, each project can define more targets in its `Emake'
file.

Also see commands `compile' and `package'."
  :parameters     "[TARGET...]"
  (emake-load-project-dependencies 'build)
  (let ((all-targets (apply #'emake-build-find-targets (or emake-build-sets '(main))))
        target-list
        target-fileset
        virtual-targets)
    (if parameters
        (dolist (parameter parameters)
          (push parameter (if (emake-virtual-target-p parameter) virtual-targets target-fileset)))
      (setf virtual-targets '(":default")))
    (dolist (target (setf virtual-targets (nreverse virtual-targets)))
      (unless (gethash target all-targets)
        (signal 'emake-error `("Unknown virtual target `%s'" ,target))))
    (when target-fileset
      (maphash (lambda (target _entry) (push target target-list)) all-targets)
      (setf target-list (emake-filter-files target-list (nreverse target-fileset))))
    (emake-with-target-dependencies
      (let ((emake--build-plan    (make-hash-table :test #'equal))
            (emake--build-results (make-hash-table :test #'equal))
            build-sequence
            anything-failed)
        ;; We create a plan before even starting to build everything, so that cyclic
        ;; dependencies can be detected earlier.
        (dolist (target (nconc virtual-targets target-list))
          (emake--build-add-to-plan target all-targets))
        (maphash (lambda (target order) (when (numberp order) (push (cons target order) build-sequence))) emake--build-plan)
        ;; FIXME: Really could add nicety by sorting this more intelligently so that
        ;;        e.g. targets go alphabetically and the same builder is used sequentially
        ;;        where this doesn't break dependency ordering.
        (when build-sequence
          (setf build-sequence (sort build-sequence (lambda (a b) (< (cdr a) (cdr b)))))
          (emake-trace "Building plan: %s" (emake-message-enumerate nil build-sequence #'car nil t))
          (dolist (entry build-sequence)
            (if emake-build-keep-going
                ;; Ignore errors here: they will have been reported in `emake-build-target'
                ;; already.
                (condition-case nil
                    (emake-build-target (car entry))
                  (emake-build-abort-branch))
              (emake-build-target (car entry)))))
        (when (= (hash-table-count emake--build-results) 0)
          (emake-print "Nothing to do"))
        (maphash (lambda (_target status) (unless (eq status 'built) (setf anything-failed t))) emake--build-results)
        (when anything-failed
          (signal 'emake-error `("Build failed")))))))

(emake-defcommand emake-compile (&rest parameters)
  "Byte-compile `.el' files.

By default, all `.el' files are compiled.  But you can also
specify SOURCEs, which should be a fileset.

This is basically a different way of invoking `build' command.
However, instead of targets, here you specify sources."
  :parameters     "[SOURCE...]"
  :aliases        byte-compile
  (if parameters
      (let (elc-files)
        (dolist (el-file (emake-find-files `(:and ,(apply #'emake-standard-filesets :or (or emake-build-sets '(main))) "*.el" ,parameters)))
          (push (concat el-file "c") elc-files))
        (apply #'emake-build (nreverse elc-files)))
    (emake-build ":compile")))

(emake-defcommand emake-package (&rest parameters)
  "Build Elisp package tarball or a single-file package.  This is
basically an alias for `build :package' or, if `--entry-file' is
specified, `build :package :package-archive-entry', but with
additional optional features.

Option `--force-version' allows you to override the project's
self-reported version."
  :aliases        pack
  (when parameters
    (signal 'emake-wrong-command-usage `(t "Unexpected command parameters")))
  (if emake-package-generate-entry-file
      (emake-build ":package" ":package-archive-entry")
    (emake-build ":package"))
  (when emake-package-print-filename
    (emake-output "%s" (expand-file-name emake--package-target-file emake-project-dir))
    (emake-output "%s" (if emake--package-target-generated "generated" "up-to-date"))))

(emake-defoption emake-build-set (name)
  "Build targets from this set; special set name `all' can be
used"
  :options        (-s --set)
  :for-command    (build compile)
  :value          NAME
  :default-value  (emake-message-enumerate nil (or emake-build-sets '(main)) nil t)
  (setf emake-build-sets (append emake-build-sets (list (emake-validate-standard-fileset name)))))

(emake-defbooloptions emake-build-keep-going emake-build-stop-on-failure emake-build-keep-going
  ("Keep building even if a target failed"
   :options       (-k --keep-going))
  ("Stop building after the first failure"
   :options       (-S --stop --stop-on-failure --no-keep-going))
  :for-command    (build compile package))

(emake-defbooloptions emake-build-treat-warnings-as-errors emake-build-dont-treat-warnings-as-errors emake-build-treat-warnings-as-errors
  ("Treat all warnings as errors"
   :options       (-W --warnings-as-errors))
  ("Don't treat warnings as errors"
   :options       (--warnings-as-warnings --no-warnings-as-errors)
   :hidden-if     :default)
  :for-command    (build compile package))

(emake-defbooloptions emake-build-suppress-warnings emake-build-show-warnings emake-build-suppress-warnings
  ("Suppress all warnings"
   :options       (-w --suppress-warnings))
  ("Show warnings during building"
   :options       --show-warnings
   :hidden-if     :default)
  :for-command    (build compile package))

(emake-defoption emake-build-force-rebuilding (&optional target)
  "Force building of TARGET even if it appears up-to-date (or all
targets if TARGET is not specified)"
  :options        (-f --force)
  :optional-value TARGET
  :for-command    (build compile package)
  (unless (eq emake-build-force-rebuilding t)
    (if target
        (push target emake-build-force-rebuilding)
      (setf emake-build-force-rebuilding t))))

(emake-defoption emake-build-infinitely-new (&optional file)
  "Consider FILE “infinitely new” (or all files if FILE is not
specified)"
  :options        (-N --new)
  :optional-value FILE
  :for-command    (build compile package)
  (unless (eq emake-build-infinitely-new t)
    (if file
        (push file emake-build-infinitely-new)
      (setf emake-build-infinitely-new t))))

(emake-defbooloptions emake-package-generate-entry-file emake-package-no-entry-file emake-package-generate-entry-file
  ("Generate an entry for package archives"
   :options       (-e --entry-file))
  ("Don't generate an entry file"
   :options       (-E --no-entry-file)
   :hidden-if     :default)
  :for-command    package)

(emake-defoption emake-package-force-version (version)
  "Force given package VERSION instead of what source code
declares"
  :options        (-V --force-version)
  :value          VERSION
  :for-command    package
  (setf emake-package-forced-version (version-to-list version)))

(emake-defoption emake-package-output-dir (dir)
  "Generate package in given directory instead of `dist'"
  :options        --output-dir
  :value          DIR
  :for-command    package
  (setf emake-package-output-dir dir))

(emake-defbooloptions emake-package-print-filename emake-package-dont-print-filename emake-package-print-filename
  ("Print absolute package filename and word “generated” or “up-to-date” as two last lines of output"
   :options       --print-filename)
  ("Don't add special ouput"
   :options       --dont-print-filename
   :hidden-if     :default)
  :for-command    package)

(emake-defbooloptions emake-build-dry-run-mode emake-build-do-build-mode emake-build-dry-run-mode
  ("Don't actually build, just print what would be performed"
   :options       (-n --dry-run))
  ("Do build as requested"
   :options       --do-build
   :hidden-if     :default)
  :for-command    (build compile package))


(defun emake--need-to-build (target source)
  "Determine if we need to build a non-virtual TARGET because of SOURCE."
  (unless (emake-virtual-target-p source) 
    (or (eq emake-build-infinitely-new t)
        (member source emake-build-infinitely-new)
        (file-newer-than-file-p source target))))

(defun emake--need-to-build-full (target dependency all-targets &optional dependency-stack)
  (let ((cycle (member dependency dependency-stack)))
    (when cycle
      (signal 'emake-error `("%s form a dependency cycle" ,(emake-message-enumerate "Target" (reverse dependency-stack))))))
  (let ((entry (gethash dependency all-targets)))
    (when entry
      (push dependency dependency-stack)
      (or (emake-any-p (emake--need-to-build target it) (cdr (assq 'sources entry)))
          (emake-any-p (emake-pcase-exhaustive (car it)
                         (`depends-on (emake--need-to-build target (cadr it)))
                         (`inherits   (emake--need-to-build-full target (cadr it) all-targets dependency-stack)))
                       (emake-get-target-dependencies dependency))))))

(defun emake--build-add-to-plan (target all-targets &optional dependency-stack)
  (let ((already-planned (gethash target emake--build-plan)))
    (when (eq already-planned 'side-effect)
      (puthash target 'planned-side-effect emake--build-plan))
    (or already-planned
        (let ((cycle (member target dependency-stack)))
          (when cycle
            (signal 'emake-error `("%s form a dependency cycle" ,(emake-message-enumerate "Target" (reverse dependency-stack))))))
        (let ((entry         (gethash target all-targets))
              (virtual       (emake-virtual-target-p target))
              (need-to-build (or (eq emake-build-force-rebuilding t)
                                 (member target emake-build-force-rebuilding))))
          (when entry
            (push target dependency-stack)
            (dolist (source (cdr (assq 'sources entry)))
              (when (or (emake--build-add-to-plan source all-targets dependency-stack)
                        (unless virtual
                          (emake--need-to-build target source)))
                (setf need-to-build t)))
            (dolist (dependency-entry (emake-get-target-dependencies target))
              (let ((dependency (cadr dependency-entry)))
                (emake-pcase-exhaustive (car dependency-entry)
                  (`depends-on (when (or (emake--build-add-to-plan dependency all-targets dependency-stack)
                                         (unless virtual
                                           (emake--need-to-build target dependency)))
                                 (setf need-to-build t)))
                  (`inherits   (when (emake--need-to-build-full target dependency all-targets dependency-stack)
                                 (setf need-to-build t))))))
            ;; For the main target it will be overwritten.
            (dolist (other-target (cdr (assq 'targets entry)))
              (puthash other-target 'side-effect emake--build-plan))
            (puthash target (when need-to-build (hash-table-count emake--build-plan)) emake--build-plan))))))

(defun emake-build-target-status (target)
  "Returns TARGET building result.
Return value is one of the following symbols:

  - planned
  - building
  - built
  - failed
  - not-planned"
  (or (gethash target emake--build-results)
      ;; Both numbers and symbol `planned-side-effect' mean that the target is planned for
      ;; building.
      (if (memq (gethash target emake--build-plan) '(nil side-effect)) 'not-planned 'planned)))

(defun emake-build-target (target)
  (emake-pcase-exhaustive (emake-build-target-status target)
    (`building    (error "Trying to build `%s' recursively" target))
    (`built       (emake-trace "Not trying to build `%s' again" target))
    (`failed      (error "Building `%s' failed earlier" target))
    (`not-planned (error "Building `%s' was never planned" target))
    (`planned
     ;; Reset `default-directory', because it can have been changed e.g. when called from
     ;; inside byte-compilation.
     (let* ((default-directory emake-project-dir)
            (entry             (gethash target emake--build-targets))
            (builder-name      (cdr (assq 'builder entry)))
            (targets           (cdr (assq 'targets entry)))
            succeeded)
       (puthash target 'building emake--build-results)
       (unwind-protect
           (if builder-name
               (let* ((builder       (cdr (assq builder-name emake--builders)))
                      (builder-type  (or (emake-get builder :type) 'one-to-one))
                      (sources       (cdr (assq 'sources entry)))
                      ;; Normally such things will not be in the plan to begin with, but
                      ;; also take into account situations where builders don't update
                      ;; their target for whatever reason, e.g. when they can detect that
                      ;; it hasn't changed.
                      (need-to-build (or (eq emake-build-force-rebuilding t)
                                         (member target emake-build-force-rebuilding)
                                         (emake--need-to-build-full target target emake--build-targets))))
                 (if need-to-build
                     (let* ((emake-build-current-targets targets)
                            (source-argument             (if (memq builder-type '(one-to-one one-to-many)) (car sources) sources))
                            (target-argument             (if (memq builder-type '(one-to-one many-to-one)) (car targets) targets)))
                       (emake-unless-quiet
                         (let* ((short-name    (or (emake-get builder :short-name) builder-name))
                                (message       (or (emake-get builder :message) 'sources))
                                (source-string (if (cddr sources)
                                                   (if (emake-when-verbose t)
                                                       (emake-message-enumerate nil sources (lambda (source) (emake-colorize source 'name)) t)
                                                     (emake-format-message "%d files" (length sources)))
                                                 (emake-colorize (car sources) 'name)))
                                (target-string (if (cddr targets)
                                                   (if (emake-when-verbose t)
                                                       (emake-message-enumerate nil targets (lambda (target) (emake-colorize target 'name)) t)
                                                     (emake-format-message "%d files" (length targets)))
                                                 (emake-colorize (car targets) 'name))))
                           (pcase message
                             ((or `source `sources)
                              (emake-output "%-8s %s" short-name source-string))
                             ((or `target `targets)
                              (emake-output "%-8s -> %s" short-name target-string))
                             ((or `source-and-target `sources-and-target `source-and-targets `sources-and-targets)
                              (emake-output "%-8s %s -> %s" short-name source-string target-string))
                             (_
                              (emake-output "%-8s %s" short-name (funcall message sources targets))))))
                       (unless emake-build-dry-run-mode
                         (if emake-build-keep-going
                             (condition-case error
                                 (funcall builder source-argument target-argument)
                               (emake-build-abort-branch
                                (signal (car error) (cdr error)))
                               (emake-error
                                (emake-error "While building `%s': %s" target (apply #'emake-format-message (cdr error)))
                                (signal 'emake-build-abort-branch nil))
                               (error
                                (emake-error "While building `%s': %s" target (error-message-string error))
                                (signal 'emake-build-abort-branch nil)))
                           (funcall builder source-argument target-argument)))
                       (setf succeeded t))
                   (setf succeeded t)
                   (emake-verbose "Not building target `%s': it is up-to-date" target)))
             (setf succeeded t)
             (emake-verbose "Done building “sources” for virtual target `%s'" target))
         (dolist (target targets)
           (puthash target (if succeeded 'built 'failed) emake--build-results)))))))


(defvar emake--recursive-byte-compilation nil)
(defvar emake--feature-providers (make-hash-table :test #'eq))

(emake-defbuilder emake-builder-byte-compile-.el (source target)
  :short-name     "ELC"
  :source-files   "*.el"
  ;; We intentionally don't use `byte-compile-dest-file'.  If you need to customize this
  ;; somehow, define a new builder instead.
  :targets        (".el" -> ".elc")
  :collect        ":compile"
  :define-cleaner (emake-cleaner-.elc
                   "Delete `.elc' files, i.e. results of byte-compilation."
                   :aliases (elc dot-elc)
                   :default t)
  ;; The following complications are mostly to speed up byte-compilation, with result
  ;; noticeable on larger projects with heavy cross-dependencies and many macros.  The
  ;; idea is to byte-compile `require'd files first so that the outer file uses already
  ;; byte-compiled and thus faster definitions.
  (require 'bytecomp)
  (let* ((load-prefer-newer         t)
         (recursive                 emake--recursive-byte-compilation)
         (original-load-source-file load-source-file-function)
         (load-source-file-function (if recursive
                                        original-load-source-file
                                      (lambda (full-name file no-error no-message)
                                        (let* ((dependency-source (file-relative-name full-name emake-project-dir))
                                               ;; See above: not using `byte-compile-dest-file'.
                                               (dependency-target (concat dependency-source "c")))
                                          (emake-trace "Loading file `%s'" dependency-source)
                                          ;; We have to load the file before byte-compiling it,
                                          ;; otherwise e.g. certain macros can fail.
                                          (and (funcall original-load-source-file full-name file no-error no-message)
                                               (or (string= dependency-source source)
                                                   (eq (emake-build-target-status dependency-target) 'not-planned)
                                                   (emake-build-target dependency-target)))))))
         inherits)
    (emake-verbose (if recursive "Byte-compiling file `%s' early as `require'd from another file..." "Byte-compiling file `%s'...")
                   source)
    (let ((emake--recursive-byte-compilation t))
      (let ((failed-source (catch 'emake--byte-compilation-failed
                             ;; When called recursively, let `byte-compile-file' determine this.
                             (let ((skip-byte-compilation (unless recursive
                                                            (with-temp-buffer
                                                              (insert-file-contents source)
                                                              ;; Older versions don't understand `no-mode'.
                                                              (hack-local-variables (when (>= emacs-major-version 26) 'no-mode))
                                                              no-byte-compile))))
                               ;; Don't even load `no-byte-compile' files unless called
                               ;; recursively.  Otherwise we might e.g. attempt loading
                               ;; `define-package' and fail.
                               (unless (and (or recursive skip-byte-compilation (load source nil t t))
                                            (let* ((byte-compile-error-on-warn        emake-build-treat-warnings-as-errors)
                                                   (have-warning-function             (boundp 'byte-compile-log-warning-function))
                                                   (byte-compile-log-warning-function (if emake-build-suppress-warnings
                                                                                          #'ignore
                                                                                        (when have-warning-function
                                                                                          byte-compile-log-warning-function)))
                                                   (result (if skip-byte-compilation
                                                               'no-byte-compile
                                                             (emake-advised (#'byte-compile-log-warning :around
                                                                                                        (unless have-warning-function
                                                                                                          (lambda (original &rest arguments)
                                                                                                            (unless emake-build-suppress-warnings
                                                                                                              (apply original arguments)))))
                                                               ;; Hack: make Emacs 24.x shut up.  We don't do it in various
                                                               ;; other cases, but this one is important for our tests.
                                                               (emake-advised (#'message :around
                                                                                         (when (< emacs-major-version 25)
                                                                                           (lambda (original &rest arguments)
                                                                                             (unless (equal arguments `("Wrote %s" ,(expand-file-name target)))
                                                                                               (apply original arguments)))))
                                                                 (byte-compile-file source))))))
                                              (cond ((eq result 'no-byte-compile)
                                                     (emake-verbose "Cancelled byte-compilation of `%s': it has `no-byte-compile' local variable"
                                                                    source)
                                                     t)
                                                    (result
                                                     ;; Load ourselves, since `byte-compile-file' calls
                                                     ;; `load' without `nomessage' parameter.
                                                     (load target nil t)))))
                                 source)))))
        (when failed-source
          (if recursive
              ;; Normal errors would get caught inside Emacs byte-compilation code.
              (throw 'emake--byte-compilation-failed failed-source)
            (signal 'emake-build-failed `("Failed to byte-compile `%s'" ,failed-source))))
        (dolist (entry (cdr (or (assoc (expand-file-name source emake-project-dir) load-history) (assoc (expand-file-name target emake-project-dir) load-history))))
          (pcase entry
            (`(require . ,feature)
             (let ((provided-by (emake--find-feature-provider feature)))
               (when (stringp provided-by)
                 (push `(inherits ,(emake-replace-suffix provided-by ".el" ".elc")) inherits))))
            (`(provide . ,feature)
             (puthash feature source emake--feature-providers))))
        (emake-set-target-dependencies target inherits)))))

(defun emake--find-feature-provider (feature)
  (or (gethash feature emake--feature-providers)
      (let ((scan     load-history)
            (look-for `(provide . ,feature))
            provider)
        (while scan
          (let ((entry (pop scan)))
            (when (member look-for (cdr entry))
              (setf provider (let ((filename (file-relative-name (car entry) emake-project-dir)))
                               (if (or (emake-external-or-absolute-filename filename)
                                       (not (emake-external-or-absolute-filename (file-relative-name filename emake-cache-dir))))
                                   'external
                                 (setf filename (emake-replace-suffix filename ".elc" ".el"))
                                 (if (file-exists-p (expand-file-name filename emake-project-dir))
                                     filename
                                   'unknown)))
                    scan     nil))))
        (puthash feature (or provider 'built-in) emake--feature-providers))))


(emake-defbuilder emake-builder-makeinfo (source target)
  :short-name     "MKINFO"
  :source-files   emake-makeinfo-sources
  :targets        ((".texi" ".texinfo") -> ".info")
  :define-cleaner (emake-cleaner-.info
                   "Delete `.info' files generated from `.texi' or `.texinfo'."
                   :aliases (info dot-info)
                   :default t)
  (emake-call-process (emake-makeinfo-executable) `("--no-split" ,source "--output" ,target ,@(when emake-build-suppress-warnings '("--no-warn")))
    (emake--forward-process-output)
    (unless (= exit-code 0)
      (signal 'emake-error `("`makeinfo' process exited with error code %d" ,exit-code)))))

(emake-defbuilder emake-builder-info-dir (sources target)
  :type           many-to-one
  :short-name     "INFO-DIR"
  :message        target
  :source-files   emake-info-sources
  :targets        "dir"
  :define-cleaner (emake-cleaner-info-dir
                   "Delete `dir' file generated from `.info'."
                   :default t)
  (emake-call-process (emake-install-info-executable) `("--dir-file" ,target ,@sources ,@(when emake-build-suppress-warnings '("--silent")))
    (emake--forward-process-output)
    (unless (= exit-code 0)
      (signal 'emake-error `("`install-info' process exited with error code %d" ,exit-code)))))


;; FIXME: Maybe find a better way?
(defvar emake--package-target-file nil)
(defvar emake--package-target-generated nil)

(emake-defbuilder emake-builder-package (sources targets)
  :type           many-to-many
  :short-name     "PACK"
  :message        targets
  :source-files   (:and emake-files-to-package (emake-standard-fileset 'main))
  :targets        (lambda (sources)
                    ;; FIXME: We rely on `emake-build-find-targets' special-casing us so
                    ;;        that we know number of sources from the start.  Find a
                    ;;        better solution.
                    (let ((target-base (file-relative-name (expand-file-name (emake--package-name-version) (or emake-package-output-dir (emake-dist-dir)))
                                                           emake-project-dir)))
                      (setf emake--package-target-file (format (if (cdr sources) "%s.tar" "%s.el") target-base))
                      `(,emake--package-target-file ,(format "%s.entry" target-base))))
  :collect        (lambda (targets)
                    `(((,(car targets)) ":package") ((,(cadr targets)) ":package-archive-entry")))
  (unless sources
    (signal 'emake-build-failed `("No sources for packaging")))
  (let* ((package           (emake-package-descriptor))
         (pretended-version (or emake-package-forced-version (package-desc-version package)))
         (package-target    (car targets))
         (entry-target      (cadr targets)))
    ;; Hardly anyone would make an entry without the package, but let's check.
    (when (memq (emake-build-target-status package-target) '(planned building))
      (if (cdr sources)
          ;; Building a tarball.
          (let* ((name-version     (emake--package-name-version))
                 (name-version-dir (file-name-as-directory name-version))
                 (working-dir      (make-temp-file "emake-packaging-" t))
                 (descriptor-file  (format "%s-pkg.el" (package-desc-name package)))
                 temporary-descriptor
                 files-to-tar)
            (condition-case nil
                (make-symbolic-link emake-project-dir (expand-file-name name-version working-dir))
              (file-error (error "FIXME: Copy...")))
            (make-directory (file-name-directory package-target) t)
            (unless (file-exists-p descriptor-file)
              (with-temp-file descriptor-file
                (insert "; -*- no-byte-compile: t -*-\n")
                (pp `(define-package ,(symbol-name (package-desc-name package)) ,(package-version-join pretended-version)
                       ,(package-desc-summary package)
                       ,(emake-macroexp-quote (mapcar (lambda (requirement)
                                                        `(,(car requirement) ,(package-version-join (cadr requirement))))
                                                      (package-desc-reqs package)))
                       ,@(apply #'nconc (mapcar (lambda (extra)
                                                  `(,(emake-macroexp-quote (car extra)) ,(emake-macroexp-quote (cdr extra))))
                                                (package-desc-extras package))))
                    (current-buffer)))
              (setf temporary-descriptor t))
            (when (or temporary-descriptor (not (member descriptor-file sources)))
              (push (concat name-version-dir descriptor-file) files-to-tar))
            (dolist (source sources)
              (push (concat name-version-dir source) files-to-tar))
            ;; Note that `file-name-as-directory' is important on older Emacs versions,
            ;; otherwise `tar' will be executed from `/tmp'.
            (let ((default-directory (file-name-as-directory working-dir))
                  (command-line      `("-cf" ,(expand-file-name package-target emake-project-dir) ,@(nreverse files-to-tar))))
              (emake-verbose "%s" (emake-message-enumerate-files "Packaging the following file%s: %s (%d)" sources))
              (emake-trace "Full command line to create package tarball:\n  %s" (emake-message-command-line (emake-tar-executable) command-line))
              (emake-call-process (emake-tar-executable) command-line
                (unless (= exit-code 0)
                  (emake-warn "`tar' output (ran from directory `%s'):" working-dir))
                (emake--forward-process-output)
                (unless (= exit-code 0)
                  (signal 'emake-build-failed `("Failed to create package tarball `%s'" ,package-target)))))
            ;; Note that if packaging fails, `working-dir' and `descriptor-file' are not
            ;; deleted.  This is intentional.
            (when temporary-descriptor
              (delete-file descriptor-file))
            (delete-directory working-dir t))
        ;; Single-file package.
        ;; FIXME: Validate sanity.
        (make-directory (file-name-directory package-target) t)
        (copy-file (car sources) package-target 'overwrite))
      (setf emake--package-target-generated t))
    (when (memq (emake-build-target-status entry-target) '(planned building))
      (emake-verbose "Generating package archive entry `%s'" entry-target)
      (with-temp-file entry-target
        (prin1 `(,(package-desc-name package)
                 . [,pretended-version ,(package-desc-reqs package) ,(package-desc-summary package)
                    ,(if (cdr sources) 'tar 'single) ,(package-desc-extras package)])
               (current-buffer))
        (insert "\n")))))

(defun emake--package-name-version ()
  (let ((package (emake-package-descriptor)))
    (format "%s-%s" (package-desc-name package) (emake-message-version (or emake-package-forced-version package)))))



;; emake init

(defvar emake-init-interactive t)

(emake-defcommand emake-init (&rest parameters)
  "Initialize project in this directory to use Emake.  This command
will fail if the project already has file named `Emake'."
  (when parameters
    (signal 'emake-wrong-command-usage `(t "Unexpected command parameters")))
  (when (file-exists-p emake-file)
    (signal 'emake-error `("File `%s' already exists in this project" ,emake-file)))
  (let ((archives-to-use t)
        .gitignore)
    (if emake-init-interactive
        (when (emake-y-or-n-p "Try to automatically select package archive for dependency lookup? ")
          (let ((requirements (package-desc-reqs (emake-package-descriptor))))
            (if requirements
                (progn
                  (emake-print "Please wait, this might take a while...")
                  (dolist (archive emake--known-package-archives)
                    (emake-use-package-archive (car archive)))
                  (let ((archive-options (emake--init-all-archive-combinations (mapcar #'car emake--known-package-archives))))
                    (while archive-options
                      (let ((archives (pop archive-options)))
                        (if (let ((package-archives (mapcar (lambda (archive) (nth 1 (assq archive emake--known-package-archives))) archives)))
                              (emake--fetch-archive-contents)
                              (package-read-all-archive-contents)
                              (package-load-all-descriptors)
                              (ignore-errors
                                (let ((inhibit-message t))
                                  (package-compute-transaction nil requirements))))
                            (progn (emake-print "Autoguessed the following %s" (emake-message-enumerate '("package archive:" "package archives:") archives))
                                   (setf archives-to-use archives
                                         archive-options nil))
                          (emake-verbose "Cannot fetch project dependencies from %s" (emake-message-enumerate "package archive" archives))))))
                  (when (eq archives-to-use t)
                    (emake-warn "Failed to autoguess needed package archives; please edit `%s' as appropriate later" emake-file)))
              (emake-print "This project has no dependencies"))))
      (emake-trace "Not in interactive mode, not autodetermining package archives to use"))
    (cond ((file-directory-p ".git")
           (emake-trace "Detected `.git' subdirectory, assuming a Git repository")
           (setf .gitignore (if emake-init-interactive
                                (emake-y-or-n-p (emake-format-message "Usage of Git detected; modify `.gitignore' as appropriate? "))
                              (emake-trace "Not in interactive mode, will modify `.gitignore' by default")
                              t)))
          (t
           (emake-verbose "This doesn't appear to be a supported VCS repository")))
    (with-temp-file emake-file
      (insert "; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-\n\n")
      (cond ((eq archives-to-use t)
             (emake-trace "Adding a few commented-out calls to `emake-use-package-archive' to `%s'" emake-file)
             (insert ";; Uncomment some calls below as needed for your project.  It is not\n"
                     ";; recommended to use `melpa-unstable' unless some dependencies simply\n"
                     ";; cannot be downloaded from another archive.\n")
             (dolist (archive emake--known-package-archives)
               (insert (format ";(emake-use-package-archive '%s)\n" (car archive)))))
            (archives-to-use
             (emake-trace "Adding the autodetermined package archives to `%s'" emake-file)
             (insert ";; Autodetermined by `emake init'.\n")
             (dolist (archive archives-to-use)
               (insert (format "(emake-use-package-archive '%s)\n" archive))))
            (t
             (insert ";; Calls to `emake-use-package-archive' are not needed: no dependencies\n"))))
    (emake-print "Created file `%s' for this project" emake-file)
    (cond (.gitignore
           (when (emake-git-executable 'warn)
             (let ((files-to-ignore `(,emake-cache-dir ,emake-local-file))
                   add-to-ignore)
               (dolist (file files-to-ignore)
                 (if (/= (call-process (emake-git-executable) nil nil nil "check-ignore" "--quiet" file)  0)
                     (progn (push file add-to-ignore)
                            (emake-trace "Git doesn't ignore `%s' currently" file))
                   (emake-trace "Git already ignores `%s'" file)))
               (if add-to-ignore
                   (let (failed)
                     (setf add-to-ignore (nreverse add-to-ignore))
                     (emake-verbose "Adding %s to `.gitignore'" (emake-message-enumerate "file" add-to-ignore))
                     (with-temp-file ".gitignore"
                       (condition-case nil
                           (insert-file-contents ".gitignore" t)
                         (file-missing))
                       (goto-char (point-max))
                       (when (not (eolp))
                         (insert "\n"))
                       (when (looking-back (rx nonl "\n") nil)
                         (insert "\n"))
                       (insert "# Added automatically by `emake init'.\n")
                       (dolist (file add-to-ignore)
                         (insert (format "/%s\n" file))))
                     (dolist (file files-to-ignore)
                       (when (/= (call-process (emake-git-executable) nil nil nil "check-ignore" "--quiet" file) 0)
                         (emake-warn "Failed to convince Git to ignore file `%s'" file)
                         (setf failed t)))
                     (unless failed
                       (emake-print "Modified file `.gitignore'")))
                 (emake-verbose "Git already ignores what Emake thinks it should"))))))))

(defun emake--init-all-archive-combinations (all-archives)
  (let ((combinations (list nil)))
    (dotimes (k (length all-archives))
      (setf k            (- (length all-archives) k 1)
            combinations (append combinations (mapcar (lambda (combination) (cons k combination)) combinations))))
    (mapcar (lambda (combination)
              (mapcar (lambda (k) (nth k all-archives)) combination))
            (sort combinations (lambda (a b)
                                 (let ((length-a (length a))
                                       (length-b (length b)))
                                   (or (< length-a length-b)
                                       (and (= length-a length-b)
                                            (let (before)
                                              (while a
                                                (when (< (pop a) (pop b))
                                                  (setf before t
                                                        a      nil)))
                                              before)))))))))

(emake-defbooloptions emake-init-interactive-mode emake-init-non-interactive-mode emake-init-interactive
  ("Create `Emake' interactively"
   :options       (-i --interactive))
  ("Don't ask any questions"
   :options       (-n --non-interactive))
  :for-command    init)


(provide 'emake)

;;; emake.el ends here

;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2019 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.1
;; Keywords:   maint, tools
;; Homepage:   https://github.com/doublep/eldev
;; Package-Requires: ((emacs "24.4"))

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

;; Eldev (Elisp Development Tool) is an Emacs-based build system,
;; targeted solely at Elisp projects.  It is an alternative to Cask.
;; Unlike Cask, Eldev itself is fully written in Elisp and its
;; configuration files are also Elisp programs.  If you are familiar
;; with Java world, Cask can be seen as a parallel to Maven — it uses
;; project description, while Eldev is sort of a parallel to Gradle —
;; its configuration is a program on its own.

;; Eldev is a command-line utility that runs Emacs in batch mode.
;; Therefore, you should not (or at least need not) install it in your
;; Emacs.  Instead, use one of the following ways.

;; If you have a catch-all directory for executables.
;;
;; 1. From this directory (e.g. `~/bin') execute:
;;
;;      $ curl -fsSL https://raw.github.com/doublep/eldev/master/bin/eldev > eldev && chmod a+x eldev
;;
;; No further steps necessary — Eldev will bootstrap itself as needed
;; on first invocation.

;; If you don't have such a directory and don't care where `eldev'
;; executable is placed.
;;
;; 1. Run:
;;
;;      $ curl -fsSL https://raw.github.com/doublep/eldev/master/webinstall/eldev | sh
;;
;;    This will install eldev script to ~/.eldev/bin.
;;
;; 2. Add the directory to your $PATH; e.g. in ~/.profile add this:
;;
;;      export PATH="$HOME/.eldev/bin:$PATH"
;;
;; Afterwards Eldev will bootstrap itself as needed on first
;; invocation.

;; For further help and more ways to install, please see the homepage:
;;
;;   https://github.com/doublep/eldev


;;; Code:

(require 'eldev-util)


;; To silence byte-compilation warnings on Emacs 24-25.
(defvar inhibit-message)
(defvar package-archive-priorities)
(defvar byte-compile-log-warning-function)


(defvar eldev-shell-command (or (eldev-getenv "ELDEV_CMD") "eldev")
  "Command used to launch Eldev, in raw form.
You should use function `eldev-shell-command' in most cases
instead.")

(defvar eldev-emacs-executable (or (eldev-getenv "ELDEV_EMACS") (eldev-getenv "EMACS") "emacs")
  "Emacs executable used for Eldev.")

(defconst eldev-dir (or (eldev-getenv "ELDEV_DIR") "~/.eldev")
  "Global user's Eldev directory, usually `~/.eldev'.")

(defconst eldev-user-config-file (expand-file-name "config" eldev-dir)
  "Global user's Eldev configuration file, usually `~/.eldev/config'.")

(defconst eldev-file "Eldev"
  "Project's Eldev configuration file, `Eldev'.")

(defconst eldev-local-file "Eldev-local"
  "Working directory Eldev configuration file, `Eldev-local'.")

(defconst eldev-cache-dir ".eldev"
  "Name of Eldev cache subdirectory, `.eldev'.
See also function `eldev-cache-dir'.")

(defconst eldev--internal-pseudoarchive "--eldev--")

(defvar eldev--loading-modes
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


(defvar eldev-main-fileset '(:not (eldev-standard-filesets :or :no-excludes :except 'main))
  "Fileset used to find main project targets.
Default value means “everything that doesn't match any non-main
filesets”.  E.g. if no additional filesets are defined, this
matches everything that is not included in `test' fileset.")

(defvar eldev-test-fileset '("./test.el" "./tests.el" "./*-test.el" "./*-tests.el" "./test/" "./tests/")
  "Fileset used to find test targets.
Default value is files `test.el', `tests.el', `*-test.el' or
`*-tests.el' in the project directory and everything within
`test' or `tests' subdirectories.")

(defvar eldev-standard-excludes '(:or ".*" (let ((dist-dir (file-relative-name (eldev-dist-dir) eldev-project-dir)))
                                             (unless (eldev-external-or-absolute-filename dist-dir)
                                               (concat "/" dist-dir))))
  "Fileset of targets that should be excluded from all sets.
Default value excludes all files and directories with name
starting with dot (i.e. hidden files by UNIX conventions).  Most
importantly, this excludes `.eldev' and `.git' directories.
Additionally, `eldev-dist-dir' is excluded.")

(defvar eldev-filesets '((main . eldev-main-fileset)
                         (test . eldev-test-fileset))
  "Alist of target filesets.
The two default filesets `main' and `test' are enough for most
purposes, but you can define more if you need.  Special name
`all' is reserved for union (i.e. as with `:or') of all other
standar filesets and must not be mapped here.

Additionally, whenever any of these filesets is resolved,
`eldev-standard-excludes' is always “subtracted” from the
result.")

(defvar eldev-files-to-package '("*.el" "./*.info" "./dir" "./doc/*.info" "./doc/dir")
  "Files that are copied to built packages.")

(defvar eldev-makeinfo-sources '("./*.texi" "./*.texinfo" "./doc/*.texi" "./doc/*.texinfo")
  "Files used as `makeinfo' sources.")

(defvar eldev-info-sources '("./*.info" "./*.info")
  "Files used as `install-info' sources to generate target `dir'.")

(defvar eldev-dist-dir "dist"
  "Directory where to generate package tarballs.
This directory can even be located outside the project directory,
which is useful in certain special cases.

See also function `eldev-dist-dir'.")

(defvar eldev-clean-external-dist nil
  "Whether to allow `eldev clean dist' to delete `dist' directory
even if it is outside of the project.  Normally it doesn't do
that as a precaution.")


(defvar eldev-project-loading-mode nil
  "Project loading mode, `as-is' if not specified.")

(defvar eldev-setup-forms nil
  "Forms executed as the last step of Eldev setup.
Should normally be specified only via command line.")


(defvar eldev-force-override nil
  "Set to non-nil to disable all command and option duplicate checks.")

(defvar eldev--commands nil)
(defvar eldev--command-aliases nil)

(defvar eldev--options nil)


(eldev-define-error 'eldev-error               "Unhandled Eldev error")
(eldev-define-error 'eldev-too-old             "Eldev is too old"    'eldev-error)
(eldev-define-error 'eldev-wrong-command-usage "Wrong command usage" 'eldev-error)
(eldev-define-error 'eldev-wrong-option-usage  "Wrong option usage"  'eldev-error)
(eldev-define-error 'eldev-build-failed        "Build failed"        'eldev-error)
(eldev-define-error 'eldev-build-abort-branch  "Build failed"        'eldev-error)
(eldev-define-error 'eldev-quit                nil)


;; Internal helper for `eldev-defcommand'.
(defun eldev--register-command (function command keywords)
  (let (override)
    (while keywords
      (eldev-pcase-exhaustive (pop keywords)
        (:aliases
         (eldev-register-command-aliases command (pop keywords)))
        ((and (or :briefdoc :parameters :custom-parsing :works-on-old-eldev) keyword)
         (eldev-put function keyword (pop keywords)))
        (:override
         (setf override (pop keywords)))))
    (unless (or override eldev-force-override (not (assq command eldev--commands)) (eq (cdr (assq command eldev--commands)) function))
      (error "Duplicate command `%s'; use `:override' keyword if this is intentional" command))
    (eldev--assq-set command function eldev--commands)))

(defun eldev-register-command-aliases (command aliases)
  "Register additional ALIASES for given COMMAND."
  (dolist (alias (eldev-listify aliases))
    (eldev--assq-set alias command eldev--command-aliases)))

(defmacro eldev-defcommand (name arguments &rest body)
  "Define an Eldev command.
BODY can contain the following keywords:

    :command COMMAND

        Command name (a symbol) for the command line.  Default
        value is derived from function name by removing `eldev-'
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

Result of BODY is ignored.  If you want Eldev to exit with an
error status, signal an error, probably a `eldev-error'."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body (eldev-macroexp-parse-body body))
        (command     (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-")) "" (symbol-name name))))
        (hook        (intern (concat (symbol-name name) "-hook")))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:command (setf command (pop body)))
        (:hook    (setf hook    (pop body)))
        (keyword  (push keyword keywords) (push (pop body) keywords))))
    (eldev-put name :command-hook hook)
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (defvar ,hook nil ,(format "Hook to be executed before command `%s'." command))
            (eldev--register-command ',name ',command ',(nreverse keywords)))))

;; Internal helper for `eldev-defoption'.
(defun eldev--register-option (handler options for-command keywords)
  (let (override)
    (while keywords
      (eldev-pcase-exhaustive (pop keywords)
        ((and (or :value :optional-value) keyword)
         (eldev-put handler :option-value  `(,(pop keywords) . ,(eq keyword :value))))
        ((and (or :briefdoc :if-default :hidden-if) keyword)
         (eldev-put handler keyword (pop keywords)))
        (:default-value
         (eldev-put handler :default-value (or (pop keywords) 'nil)))
        (:override
         (setf override (pop keywords)))))
    (dolist (command (if for-command (eldev-listify for-command) '(nil)))
      (let ((command-options (or (assq command eldev--options) (car (push `(,command . nil) eldev--options)))))
        (dolist (option (eldev-listify options))
          (unless (or (eq override t) eldev-force-override (not (assq option (cdr command-options))) (eq (cdr (assq option (cdr command-options))) handler)
                      (memq command override) (memq option override) (member `(,command ,option) override))
            (error "Duplicate option `%s' for command `%s'; use `:override' keyword if this is intentional" option command))
          (eldev--assq-set option handler (cdr command-options)))))))

(defmacro eldev-defoption (name arguments &rest body)
  "Define an Eldev option (for a command or a global one).
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
        Each command must be defined with `eldev-defcommand'
        prior to defining an option for it.

    :briefdoc STRING

        Use STRING as brief (one-line) documentation for the
        option.  By default it is derived from the function
        docstring.

    :if-default FORM

        Evaluate given FORM to determine if the option is the
        default (which can be changed e.g. in `~/.eldev/config').

    :default-value FORM

        Evaluate given FORM to determine default value for the
        option.

    :hidden-if FORM or :hidden-if :default

        Certain options are not interesting, unless someone sets
        a really unconvential default value.  Typical examples
        are negation of `--dry-run' options."
  (declare (doc-string 3) (indent 2))
  (let* ((parsed-body (eldev-macroexp-parse-body body))
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
      (error "`:options' is required for `eldev-defoption'"))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (eldev--register-option ',name ',options ',for-command ',(nreverse keywords)))))

(defmacro eldev-defbooloptions (t-name nil-name variable t-body nil-body &rest common-body)
  "Define a yes/no (boolean) option.
This is only a wrapper over `eldev-defoption'."
  (declare (indent 3))
  `(progn (eldev-defoption ,t-name ()
            ,@(append t-body   common-body `(:if-default ,variable       (setf ,variable t))))
          (eldev-defoption ,nil-name ()
            ,@(append nil-body common-body `(:if-default (not ,variable) (setf ,variable nil))))))


(eldev-defbooloptions eldev-enable-debug-mode eldev-disable-debug-mode debug-on-error
  ("Set `debug-on-error' to t"
   :options       (-d --debug))
  ("Set `debug-on-error' to nil"
   :options       (-D --no-debug)))

(eldev-defoption eldev-set-loading-mode (mode)
  "Set the project's loading mode"
  :options        (-m --loading)
  :value          MODE
  :default-value  (or eldev-project-loading-mode 'as-is)
  ;; Accept both strings and symbols.
  (when (stringp mode)
    (setf mode (intern (downcase mode))))
  (unless (assq mode eldev--loading-modes)
    (signal 'eldev-wrong-option-usage `("unknown loading mode `%s'" ,mode)))
  (setf eldev-project-loading-mode mode))

(eldev-defoption eldev-list-loading-modes ()
  "List loading modes and exit"
  :options        --list-modes
  (let ((modes eldev--loading-modes))
    (while modes
      (let ((mode (pop modes)))
        (eldev-output "%s\n\n%s" (eldev-colorize (car mode) 'section) (cdr mode))
        (when modes
          (eldev-output "\n")))))
  (signal 'eldev-quit 0))

(eldev-defoption eldev-set-loading-mode-as-is ()
  "Shorthand for `--loading=as-is'"
  :options        (-a --as-is)
  :hidden-if      (eq (or eldev-project-loading-mode 'as-is) 'as-is)
  (eldev-set-loading-mode 'as-is))

(eldev-defoption eldev-set-loading-mode-packaged ()
  "Shorthand for `--loading=packaged'"
  :options        (-p --packaged)
  :hidden-if      (eq eldev-project-loading-mode 'packaged)
  (eldev-set-loading-mode 'packaged))

(eldev-defoption eldev-set-loading-mode-source ()
  "Shorthand for `--loading=source'"
  :options        (-s --source)
  :hidden-if      (eq eldev-project-loading-mode 'source)
  (eldev-set-loading-mode 'source))

(eldev-defoption eldev-set-loading-mode-byte-compiled ()
  "Shorthand for `--loading=byte-compiled'"
  :options        (-c --byte-compiled)
  :hidden-if      (eq eldev-project-loading-mode 'byte-compiled)
  (eldev-set-loading-mode 'byte-compiled))

(eldev-defbooloptions eldev-load-prefer-newer-mode eldev-load-first-mode load-prefer-newer
  ("Set `load-prefer-newer' to t"
   :options       (-N --load-newer)
   :hidden-if     (not (boundp 'load-prefer-newer)))
  ("Set `load-prefer-newer' to nil"
   :options       (-F --load-first)
   :hidden-if     (not (boundp 'load-prefer-newer))))

(eldev-defoption eldev-quiet-mode ()
  "Restrict output to only essential information"
  :options        (-q --quiet)
  :if-default     (eq eldev-verbosity-level 'quiet)
  (setf eldev-verbosity-level 'quiet))

(eldev-defoption eldev-normal-output-mode ()
  "Output normal amount of information"
  :options        --normal-output
  :if-default     (not (memq eldev-verbosity-level '(verbose trace quiet)))
  :hidden-if      :default
  (setf eldev-verbosity-level nil))

(eldev-defoption eldev-verbose-mode ()
  "Be more verbose"
  :options        (-v --verbose)
  :if-default     (eq eldev-verbosity-level 'verbose)
  (setf eldev-verbosity-level 'verbose))

(eldev-defoption eldev-trace-mode ()
  "Be very verbose"
  :options        (-t --trace)
  :if-default     (eq eldev-verbosity-level 'trace)
  (setf eldev-verbosity-level 'trace))

(eldev-defbooloptions eldev-output-time-diffs eldev-dont-output-time-diffs eldev-output-time-diffs
  ("Prepend elapsed time to every line of output"
   :options       (-T --time))
  ("Don't add time information to output"
   :options       --no-time
   :hidden-if     :default))

(eldev-defoption eldev-coloring-mode (&optional mode)
  "Whether to colorize output; WHEN can be `always', `auto' or `never'"
  :options        (-C --color)
  :optional-value WHEN
  :default-value  (if eldev-coloring-mode (if (eq eldev-coloring-mode 'auto) "auto" "always") "never")
  (setf mode                (when mode (downcase mode))
        eldev-coloring-mode (cond ((or (null mode) (string= mode "always")) t)
                                  ((string= mode "auto")                    'auto)
                                  ((string= mode "never")                   nil)
                                  (t (signal 'eldev-wrong-option-usage `("argument must be `always', `auto' or `never'"))))))

(eldev-defoption eldev-setup-form (form)
  "Evaluate FORM as the final step of the setup"
  :options        (-S --setup)
  :value          FORM
  (push (eldev-read-wholly form "setup form") eldev-setup-forms))



;; Filesets.

(defun eldev-standard-fileset (name &optional no-excludes)
  "Return a computed fileset for target set with given NAME.
Unless NO-EXCLUDES is specified, all targets from
`eldev-standard-excludes' are ignored."
  (eldev-standard-filesets (if no-excludes :no-excludes :std-excludes) name))

(defun eldev-standard-filesets (&rest arguments)
  "Build a fileset from standard pieces.
ARGUMENTS should be either names of standard filesets
(i.e. usually `main' or `test', but see `eldev-filesets') or any
of the following keywords:

    :and or :or

        How to combine filesets; `:or' is the default.

    :these or :except

        Whether to use given filesets (default) or every standard
        fileset except them.

    :std-excludes or :no-excludes

        Whether to use `eldev-standard-filesets' (default) or not."
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
            (setf names (mapcar #'car eldev-filesets))
          (when (cdr names)
            (setf names (delq 'all names))))))
    (if except
        (dolist (entry eldev-filesets)
          (unless (memq (car entry) names)
            (push (cdr entry) result)))
      (dolist (name names)
        (push (cdr (or (assq name eldev-filesets) (error "Unknown fileset `%s'" name))) result)))
    (setf result (if (cdr result)
                     `(,combination-mode ,@(nreverse result))
                   (car result)))
    (unless no-excludes
      (setf result `(:and ,result (:not eldev-standard-excludes))))
    result))

(defun eldev-validate-standard-fileset (name)
  "Validate that NAME specifies a standard fileset.
See `eldev-filesets'."
  (when (stringp name)
    (setf name (intern name)))
  (unless (or (eq name 'all) (assq name eldev-filesets))
    (signal 'eldev-error `("Unknown standard fileset `%s'", name)))
  name)



;; Command line interface.

(defvar eldev-executing-command-hook nil)
(defvar eldev-too-old nil)


(defun eldev-start-up ()
  "Prepare Eldev.
Used by Eldev startup script."
  (setf package-user-dir (expand-file-name "packages" (eldev-cache-dir t))
        package-archives nil))

(defun eldev-cli (command-line)
  "Eldev entry point."
  (let (command
        eldev-too-old
        exit-code)
    (unwind-protect
        (condition-case-unless-debug error
            (condition-case error
                (eldev-output-reroute-messages
                  (eldev-advised (#'display-warning :around (lambda (original type message &optional level &rest args)
                                                              (let ((eldev-message-rerouting-wrapper (pcase level
                                                                                                       (:error #'eldev-error)
                                                                                                       (:debug #'eldev-verbose)
                                                                                                       (_      #'eldev-warn))))
                                                                (apply original type message level args))))
                    (let ((eldev-project-dir (or eldev-project-dir (expand-file-name default-directory)))
                          ;; Eldev uses a different default.
                          (load-prefer-newer t))
                      ;; If `inhibit-message' is t when a signal is raised, Emacs won't
                      ;; print error stacktraces even if `debug-on-error' is t.  Add a
                      ;; workaround.
                      (eldev-advised (#'debug :around (lambda (original &rest arguments)
                                                        (let ((inhibit-message nil))
                                                          (apply original arguments))))
                        (eldev-parse-options command-line nil t t)
                        ;; Since this is printed before `~/.eldev/config' is loaded it can
                        ;; ignore some settings from that file, e.g. colorizing mode.
                        (eldev-trace "Started up on %s" (replace-regexp-in-string " +" " " (current-time-string)))
                        (eldev-trace "Running on %s" (emacs-version))
                        (eldev-trace "Project directory: `%s'" eldev-project-dir)
                        (condition-case error
                            (eldev--set-up)
                          (eldev-too-old (setf eldev-too-old (cdr error))))
                        (setf command-line (eldev-parse-options command-line nil t))
                        (if command-line
                            (progn
                              (setf command (intern (car command-line)))
                              (let* ((real-command (or (cdr (assq command eldev--command-aliases)) command))
                                     (handler      (or (cdr (assq real-command eldev--commands)))))
                                (if handler
                                    (let ((hook (eldev-get handler :command-hook)))
                                      (when (and eldev-too-old (not (eldev-get handler :works-on-old-eldev)))
                                        (signal 'eldev-too-old eldev-too-old))
                                      (setf command-line (if (eldev-get handler :custom-parsing)
                                                             (cdr command-line)
                                                           (eldev-parse-options (cdr command-line) real-command)))
                                      (if (eq real-command command)
                                          (eldev-verbose "Executing command `%s'..." command)
                                        (eldev-verbose "Executing command `%s' (alias for `%s')..." command real-command))
                                      (when eldev-executing-command-hook
                                        (eldev-trace "Executing `eldev-executing-command-hook'...")
                                        (run-hook-with-args 'eldev-executing-command-hook real-command))
                                      (when (symbol-value hook)
                                        (eldev-trace "Executing `%s'..." hook)
                                        (run-hooks hook))
                                      ;; We want `message' output on stdout universally, but
                                      ;; older Emacses are very verbose and having additional
                                      ;; unexpected messages in our stdout would screw up
                                      ;; tests.  So we set the target to stdout only now.
                                      (let ((eldev-message-rerouting-destination :stdout))
                                        (apply handler command-line))
                                      (setf exit-code 0))
                                  (eldev-error "Unknown command `%s'" command)
                                  (eldev-print "Run `%s help' for a list of supported commands" (eldev-shell-command t)))))
                          (eldev-usage)
                          (eldev-print "Run `%s help' for more information" (eldev-shell-command t))
                          (setf exit-code 0))))))
              (eldev-error (let* ((arguments (cdr error))
                                  hint
                                  hint-about-command)
                             (when (and (eq (car error) 'eldev-wrong-command-usage) (not (stringp (car arguments))))
                               (setf arguments `(:command ,(or command t) ,@(cdr arguments))))
                             (when (eq arguments eldev-too-old)
                               (setf eldev-too-old nil))
                             (pcase (car arguments)
                               (:hint    (pop arguments) (setf hint               (pop arguments)))
                               (:command (pop arguments) (setf hint-about-command (pop arguments))))
                             (eldev-error "%s" (apply #'eldev-format-message arguments))
                             (when hint
                               (eldev-print "%s" (apply #'eldev-format-message (eldev-listify hint))))
                             (when hint-about-command
                               (eldev-print "Run `%s help%s' for more information" (eldev-shell-command t) (if (eq hint-about-command t) "" (format " %s" hint-about-command))))))
              (eldev-quit  (setf exit-code (cdr error))))
          (error (eldev-error "%s" (error-message-string error))))
      (eldev-trace "Finished %s on %s" (if (eq exit-code 0) "successfully" "erroneously") (replace-regexp-in-string " +" " " (current-time-string))))
    (when eldev-too-old
      (when (eq (car eldev-too-old) :hint)
        (setf eldev-too-old (cddr eldev-too-old)))
      (eldev-warn "%s" (apply #'eldev-format-message eldev-too-old)))
    (or exit-code 1)))

(defun eldev--set-up ()
  (dolist (config `((,eldev-user-config-file . "No file `%s', not applying user-specific configuration")
                    (,eldev-file             . "No file `%s', project building uses only defaults")
                    (,eldev-local-file       . "No file `%s', not customizing build")))
    (let ((file (locate-file (car config) (list eldev-project-dir))))
      (if file
          (progn (eldev-trace "Loading file `%s'..." (car config))
                 (load file nil t t))
        (eldev-verbose (cdr config) (car config)))))
  (dolist (form (reverse eldev-setup-forms))
    (eldev-trace "Evaluating form `%S' specified on the command line..." form)
    (eval form t)))

(defun eldev-usage ()
  "Print Eldev usage message."
  (eldev-output "Usage: %s [OPTION...] COMMAND [...]" (eldev-shell-command t)))

(defun eldev-shell-command (&optional for-display)
  "Return shell command used to start Eldev."
  (or (cond ((and for-display (file-name-absolute-p eldev-shell-command))
             (if (eldev-directory-in-exec-path (file-name-directory eldev-shell-command))
                 (file-name-nondirectory eldev-shell-command)
               (let ((command (file-relative-name eldev-shell-command eldev-project-dir)))
                 (unless (eldev-external-or-absolute-filename command)
                   command))))
            ((and (not for-display) (file-name-directory eldev-shell-command))
             ;; Can only use absolute filenames: anything else means looking up in
             ;; `exec-path', which is not how shell works.
             (expand-file-name eldev-shell-command eldev-project-dir)))
      eldev-shell-command))

(defun eldev-parse-options (command-line &optional command stop-on-non-option allow-unknown)
  "Parse global or command-specific options.
Returns COMMAND-LINE with options removed."
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
                  (let ((handler (cdr (assq (intern term) (cdr (assq command eldev--options))))))
                    (if handler
                        (condition-case error
                            (let ((value-mode (eldev-get handler :option-value)))
                              (if (and value-mode (or rest (cdr value-mode)))
                                  (progn
                                    (funcall handler (or rest (if command-line
                                                                  (pop command-line)
                                                                (if command
                                                                    (signal 'eldev-wrong-command-usage `(t "Option `%s' for command `%s' requires an argument" ,term ,command))
                                                                  (signal 'eldev-wrong-command-usage `(t "Option `%s' requires an argument" ,term))))))
                                    (setf rest nil))
                                (funcall handler)))
                          (eldev-wrong-option-usage (signal 'eldev-error `(:command ,command "For option `%s': %s" ,term ,(apply #'eldev-format-message (cdr error))))))
                      (if allow-unknown
                          (setf term nil)
                        ;; The following options are special-cased.  They are not
                        ;; advertised, since normally commands `help' and `version' should
                        ;; be used instead, but still handled as too common.
                        (when (string= term "--help")
                          (when (and (null command) command-line (assq (intern (car command-line)) eldev--commands))
                            (setf command (intern (car command-line))))
                          (signal 'eldev-quit (if command (eldev-help (symbol-name command)) (eldev-help))))
                        (when (and (null command) (string= term "--version"))
                          (signal 'eldev-quit (apply #'eldev-version command-line)))
                        (signal 'eldev-wrong-command-usage `(t "Unknown option `%s'" ,term)))))
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

(defun eldev-cache-dir (emacs-version-specific &optional ensure-exists)
  "Get the directory where various internal caches should be stored.
This is the `.eldev' subdirectory of project's root.  When
argument EMACS-VERSION-SPECIFIC is non-nil, path also includes
Emacs version (only major and minor parts, e.g. 26.1).

If ENSURE-EXISTS is non-nil, directory is created if it doesn't
exist yet."
  (let ((cache-dir (file-name-as-directory (expand-file-name eldev-cache-dir eldev-project-dir))))
    (when emacs-version-specific
      (setf cache-dir (expand-file-name (format "%s.%s" emacs-major-version emacs-minor-version) cache-dir)))
    (when ensure-exists
      (make-directory cache-dir t))
    cache-dir))

(defun eldev-dist-dir (&optional ensure-exists)
  "Get the directory where to generate package tarballs."
  (let ((dist-dir (file-name-as-directory (expand-file-name (or eldev-dist-dir "") eldev-project-dir))))
    (when ensure-exists
      (make-directory dist-dir t))
    dist-dir))

(defmacro eldev-do-load-cache-file (file description expected-version &rest body)
  "Load data from a cache FILE.
Such data is considered non-critical, therefore this function
will never signal an error.  DESCRIPTION is used in
human-readable information.

Data in FILE is supposed to be a single alist (see macro
`eldev-do-save-cache-file').  Its version must not be newer than
EXPECTED-VERSION, else it is discarded.

If loading succeeds, BODY is executed with loaded Elisp object
bound as `contents'.  Version of stored data is bound as
`version'."
  (declare (indent 3) (debug (stringp stringp numberp body)))
  `(condition-case error
       (if (file-exists-p ,file)
           (progn
             (eldev-trace "Reading %s from file `%s'..." ,description (file-relative-name ,file eldev-project-dir))
             (with-temp-buffer
               (insert-file-contents ,file)
               (let* ((contents (read (current-buffer)))
                      (version  (cdr (assq 'version contents))))
                 (if (> version ,expected-version)
                     (eldev-verbose "Ignoring %s in file `%s': version %d must be from the future (expected at most %d)" ,description ,file version ,expected-version)
                   ,@body))))
         (eldev-trace "No file `%s', not reading %s" (file-relative-name ,file eldev-project-dir) ,description))
     ;; Since this is not overly important, just print a verbose-level message.
     (error (eldev-verbose "Failed to load %s: %s" ,description (error-message-string error)))))

(defmacro eldev-do-save-cache-file (file description version &rest body)
  "Save data generated by BODY to a cache FILE.
Such data is considered non-critical, therefore this function
will never signal an error.  DESCRIPTION is used in
human-readable information.

BODY is supposed to generate an alist.  A single entry `(version
. VERSION)' is prepended to it.  It will be used by macro
`eldev-do-load-cache-file' and its body to determine how to parse
the data."
  (declare (indent 3) (debug (stringp stringp numberp body)))
  `(condition-case error
       (with-temp-file ,file
         (eldev-trace "Saving %s to file `%s'..." ,description (file-relative-name ,file eldev-project-dir))
         (let ((data                       (progn ,@body))
               (print-circle               nil)
               (print-continuous-numbering nil))
           (prin1 (cons (cons 'version ,version) data) (current-buffer))
           (insert "\n")))
     ;; Since this is not overly important, just print a verbose-level message.
     (error (eldev-verbose "Failed to save %s: %s" ,description (error-message-string error)))))



;; Functions for `Eldev' and `Eldev-local'.

(defvar eldev--extra-dependencies nil)
(defvar eldev--local-dependencies nil)
(defvar eldev--local-dependency-packages nil)

(defvar eldev--known-package-archives '((gnu            ("gnu"            . "https://elpa.gnu.org/packages/")     300)
                                        (melpa-stable   ("melpa-stable"   . "https://stable.melpa.org/packages/") 200)
                                        (melpa-unstable ("melpa-unstable" . "https://melpa.org/packages/")        100)))


(defun eldev-require-version (version)
  "Fail if Eldev is too old.
VERSION can be either a string or a list (see
`version-to-list')."
  (unless (eldev-find-package-descriptor 'eldev version)
    (signal 'eldev-too-old `(:hint ("Run `%s upgrade-self' to install the newest available Eldev version" ,(eldev-shell-command t))
                                   "Project `%s' requires Eldev version %s or newer (this is version %s)" ,(package-desc-name (eldev-package-descriptor))
                                   ,(eldev-message-version version) ,(eldev-message-version (eldev-package-descriptor))))))

(defun eldev-use-package-archive (archive &optional priority)
  "Use given ARCHIVE to install project dependencies.
ARCHIVE can be one of the predefined symbols below or a cons cell
of (ID . URL), same as you would use in `package-archives'.

Standard archives:

  - gnu            (https://elpa.gnu.org/packages/)
  - melpa-stable   (https://stable.melpa.org/packages/)
  - melpa-unstable (https://melpa.org/packages/)

If PRIORITY is non-nil, ARCHIVE is given this priority (see
`package-archive-priorities').  Standard archives get priorities
300, 200 and 100 in the order they are listed above, unless you
specify something explicitly."
  (unless priority
    (setf priority (nth 2 (assq archive eldev--known-package-archives))))
  (cond ((assq archive eldev--known-package-archives)
         (setf archive (nth 1 (assq archive eldev--known-package-archives))))
        ((not (and (consp archive) (stringp (car archive)) (stringp (cdr archive))))
         (error "Unknown package archive `%S'" archive)))
  (when (string= (car archive) eldev--internal-pseudoarchive)
    (error "Package archive name `%s' is reserved for internal use" eldev--internal-pseudoarchive))
  (eldev-verbose "Using package archive `%s' at `%s' with %s"
                 (car archive) (cdr archive) (if priority (format "priority %s" priority) "default priority"))
  (push archive package-archives)
  (when (and priority (boundp 'package-archive-priorities))
    (push (cons (car archive) priority) package-archive-priorities)))

(defun eldev-use-local-dependency (dir &optional loading-mode)
  "Use local dependency found in DIR.
See the manual for more information about local dependencies."
  (if loading-mode
      (unless (assq loading-mode eldev--loading-modes)
        (error "Unsupported local dependency mode `%s'; see Eldev documentation" loading-mode))
    (setf loading-mode 'as-is))
  (setf dir (file-name-as-directory dir))
  (let* ((absolute-dir (expand-file-name dir eldev-project-dir))
         (dependency   (eldev-package-descriptor absolute-dir))
         (name         (package-desc-name dependency)))
    (when (eq name (package-desc-name (eldev-package-descriptor)))
      (error "Local dependency in directory `%s' is the same package as that being built: `%s'" dir name))
    (when (assq name eldev--local-dependencies)
      (error "Duplicate local dependency `%s' in directory `%s': already registered in directory `%s'" name dir (nth 1 (assq name eldev--local-dependencies))))
    (push `(,name ,dependency ,dir ,absolute-dir ,loading-mode) eldev--local-dependencies)
    (eldev-trace "Will use directory `%s' as local dependency `%s' with loading mode `%s'" dir name loading-mode)))

(defun eldev-add-extra-dependencies (sets &rest dependencies)
  "Additionally use DEPENDENCIES for given SETS.
Sets are typically named after Eldev commands.  See the manual
for details."
  (dolist (set (eldev-listify sets))
    (let ((set-dependencies (or (assq set eldev--extra-dependencies) (car (push `(,set . nil) eldev--extra-dependencies)))))
      (dolist (dependency dependencies)
        (push (if (symbolp dependency) (list dependency) dependency) (cdr set-dependencies))))))


(defun eldev-substitute (source target &optional open-string close-string)
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
    (eldev-substitute-in-buffer open-string close-string)
    (write-region (point-min) (point-max) target nil 'no-message)))

(defun eldev-substitute-in-buffer (&optional open-string close-string)
  "Substitute Elisp expressions in the current buffer.
Substitution starts from point and only spans visible buffer
part.  See function `eldev-substitute' for more information."
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

(defun eldev-file-contents (file &optional trailing-newlines ignore-first ignore-last)
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
    (insert-file-contents (expand-file-name file eldev-project-dir))
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



;; eldev help

(defvar eldev-help-show-hidden nil)

(eldev-defcommand eldev-help (&rest parameters)
  "Print details about Eldev invocation.  If COMMAND is specified,
show details about that command instead."
  :parameters     "[COMMAND]"
  :aliases        \?
  :works-on-old-eldev t
  (if parameters
      ;; This is an exception: don't signal wrong command usage on excessive parameters.
      (let* ((command      (if (= (length parameters) 1) (intern (car parameters)) 'help))
             (real-command (or (cdr (assq command eldev--command-aliases)) command))
             (handler      (cdr (assq real-command eldev--commands))))
        (if handler
            (let ((parameters  (eldev-get handler :parameters))
                  aliases)
              (dolist (alias eldev--command-aliases)
                (when (eq (cdr alias) real-command)
                  (push (car alias) aliases)))
              (eldev-output "Usage: %s%s %s" (eldev-shell-command t) (if (cdr (assq command eldev--options)) " [OPTION...]" "")
                            (if parameters (format "%s %s" real-command parameters) real-command))
              (when aliases
                (eldev-output "\n%s" (eldev-message-enumerate '("Command alias:" "Command aliases:") aliases #'symbol-name t t)))
              (eldev-options-help real-command)
              (eldev-output "\n%s" (or (eldev-documentation handler) "Not documented")))
          (eldev-error "Unknown command `%s'" command)
          (eldev-output "Run `%s help' for a list of known commands" (eldev-shell-command t))))
    (eldev-usage)
    (eldev-output "
Options before the command are global for Eldev.  Many commands have additional
options specific to them; those must be specified after command name.  See each
command's description for a list of such options.")
    (eldev-options-help nil "\nGlobal options:")
    (eldev-output "\nCommands:")
    (dolist (command (sort (mapcar #'car eldev--commands) #'string<))
      (eldev-command-or-option-help command (cdr (assq command eldev--commands))))
    (eldev-output "
Influential environment variables: `ELDEV_EMACS' (also just `EMACS'),
`ELDEV_LOCAL' and `ELDEV_DIR'.  See documentation for their effects.")
    (eldev-print "\nRun `%s help COMMAND' for more information." (eldev-shell-command t))))

(eldev-defbooloptions eldev-help-show-hidden eldev-help-omit-hidden eldev-help-show-hidden
  ("Show all options, even those not interesting normally"
   :options       (-a --all-options))
  ("Hide non-interesting options"
   :options       (-i --only-interesting)
   :hidden-if     :default)
  :for-command    help)

(defun eldev-options-help (&optional command title)
  "List options for given COMMAND.
If COMMAND is nil, list global options instead."
  (let (by-handler)
    (dolist (option (cdr (assq command eldev--options)))
      (push (car option) (cdr (or (assq (cdr option) by-handler) (car (push (cons (cdr option) nil) by-handler))))))
    (when by-handler
      (eldev-output (or title "\nOptions:"))
      (dolist (group by-handler)
        (let* ((value-mode  (eldev-get (car group) :option-value))
               (options     (cdr group))
               (all-strings (mapconcat #'symbol-name options ", ")))
          (when (eldev-all-p (string-prefix-p "--" (symbol-name it)) options)
            (setf all-strings (concat "    " all-strings)))
          (when value-mode
            (setf all-strings (format (if (string-prefix-p "--" (symbol-name (car (last options))))
                                          (if (cdr value-mode) "%s=%s" "%s[=%s]")
                                        (if (cdr value-mode) "%s %s" "%s [%s]"))
                                      all-strings (car value-mode))))
          (eldev-command-or-option-help all-strings (car group)))))))

(defun eldev-command-or-option-help (command-or-option handler)
  "Print help on given command or option."
  (let ((documentation  (eldev-briefdoc handler))
        (default        (eldev-get handler :if-default))
        (default-value  (eldev-get handler :default-value))
        (default-string (eldev-colorize "default" 'default))
        (hidden-if      (unless eldev-help-show-hidden (eldev-get handler :hidden-if))))
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
          (eldev-output (if (> (length command-or-option) 21) "  %s\n                        %s" "  %-21s %s")
                        command-or-option (eldev--wrap-text documentation 55 24))
        (eldev-output "  %s" command-or-option)))))

(defun eldev--wrap-text (text wrap-width &optional indent-wrapped-lines-by)
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



;; Loading dependencies; eldev prepare, eldev upgrade, eldev upgrade-self

(defvar eldev-upgrade-dry-run-mode nil
  "Don't upgrade if non-nil, just pretend to do so.")

(defvar eldev-upgrade-self-from-stable t
  "Use Melpa Stable when upgrading Eldev.")

(defvar eldev--upgrade-self-from-forced-pa nil
  "Should remain unset; used for testing.")


(eldev-defcommand eldev-prepare (&rest parameters)
  "Explicitly install project dependencies.

You don't normally have to run this command, because various
other commands will install dependencies as needed before
executing.  However, `prepare' can occasionally be useful for
writing shell scripts in case you want to easily distinguish
command-specific failures (e.g. `build' can fail because you have
a syntax error somewhere) from general errors with unresolvable
dependencies.

ADDITIONAL-SETs can be used to install extra dependencies added
to those sets (see function `eldev-add-extra-dependencies')."
  :aliases        prep
  :parameters     "[ADDITIONAL-SET...]"
  (eldev-load-project-dependencies (mapcar #'intern parameters)))

(eldev-defcommand eldev-upgrade (&rest parameters)
  "Upgrade project dependencies.  If specific packages are not
specified, all project dependencies and those registered with
`eldev-add-extra-dependencies' are upgraded.  However, you can
list names of packages to be upgraded.  Requirements of these
package will be upgraded if needed too.

Note that local dependencies are never installed or upgraded from
remote package archives.  If you sometimes use an “official”
release of a package and sometimes its local copy and want the
remote copy to be upgraded, make it non-local first (e.g. by
commenting out `eldev-use-local-dependency' in `Eldev-local')
before upgrading."
  :parameters     "[PACKAGE...]"
  (eldev--install-or-upgrade-dependencies nil (mapcar #'car eldev--extra-dependencies) (or (mapcar #'intern parameters) t) eldev-upgrade-dry-run-mode nil t))

(eldev-defcommand eldev-upgrade-self (&rest parameters)
  "Upgrade Eldev itself.

This command works even if you installed Eldev from sources.
However, once Eldev is upgraded this way, your installation from
sources will be replaced with a package downloaded from MELPA."
  :works-on-old-eldev t
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (let ((package-user-dir (expand-file-name (format "%s.%s/bootstrap" emacs-major-version emacs-minor-version) eldev-dir)))
    (eldev--install-or-upgrade-dependencies t nil t eldev-upgrade-dry-run-mode nil t)))

(eldev-defbooloptions eldev-upgrade-self-from-stable eldev-upgrade-self-from-unstable eldev-upgrade-self-from-stable
  ("Use MELPA Stable (stable.melpa.org)"
   :options       (-s --stable))
  ("Use MELPA Unstable (melpa.org)"
   :options       --unstable)
  :for-command    upgrade-self)

(eldev-defbooloptions eldev-upgrade-dry-run-mode eldev-upgrade-do-upgrade-mode eldev-upgrade-dry-run-mode
  ("Don't actually upgrade anything, just print what would be performed"
   :options       (-n --dry-run))
  ("Do upgrade requested packages"
   :options       --do-upgrade
   :hidden-if     :default)
  :for-command    (upgrade upgrade-self))


(defun eldev-load-project-dependencies (&optional additional-sets no-error-if-missing)
  "Load dependencies of the project.
Remember that Eldev command functions get invoked with
dependencies not having been loaded yet, because not all commands
need them.

ADDITIONAL-SETS should be a symbol or a list of symbols.  Any
dependency added with `eldev-add-extra-dependencies' for one of
the specified sets will be loaded in addition to normal project
dependencies.

Normally, if any dependency cannot be loaded, an error is
signalled.  However, this can be disabled using
NO-ERROR-IF-MISSING."
  ;; Certain commands may sometimes work on too old Eldev, but then decide to load project
  ;; dependencies, which of course fails.
  (when eldev-too-old
    (signal 'eldev-too-old eldev-too-old))
  (eldev--install-or-upgrade-dependencies nil additional-sets nil nil t nil no-error-if-missing))

;; `package-compute-transaction' and friends are not enough in our case, mostly because of
;; local dependencies that can change unpredictably.  Roll our own.
(defun eldev--install-or-upgrade-dependencies (self additional-sets to-be-upgraded dry-run activate main-command-effect &optional no-error-if-missing)
  ;; See comments in `eldev-cli'.
  (let* ((eldev-message-rerouting-destination :stderr)
         (package-name            (if self 'eldev (package-desc-name (eldev-package-descriptor))))
         (all-packages            `((,package-name)))
         (package-archives        `(,@(if self
                                          (if eldev--upgrade-self-from-forced-pa
                                              `(("bootstrap-pa" . ,(file-name-as-directory eldev--upgrade-self-from-forced-pa)))
                                            `(,(nth 1 (assq (if eldev-upgrade-self-from-stable 'melpa-stable 'melpa-unstable) eldev--known-package-archives))))
                                        `((,eldev--internal-pseudoarchive . ,(file-name-as-directory (eldev--internal-pseudoarchive-dir)))))
                                    ,@package-archives))
         (package-pinned-packages `(,@(unless self (mapcar (lambda (entry) `(,(car entry) . ,eldev--internal-pseudoarchive)) eldev--local-dependencies)) ,@package-pinned-packages))
         (plan                    (list nil)))
    (unless self
      (eldev--create-internal-pseudoarchive-descriptor))
    (eldev--fetch-archive-contents to-be-upgraded)
    (package-read-all-archive-contents)
    (package-load-all-descriptors)
    (unless self
      ;; This removes local dependencies that have been installed as packages
      ;; (e.g. because `Eldev-local' used to be different) from `package-alist'.
      ;; Otherwise package manager will prefer the installed versions even if we pin the
      ;; packages to our pseudoarchive.  This doesn't quite feel right, but I don't see a
      ;; better way.
      (setf package-alist (eldev-filter (null (eldev--loading-mode (car it))) package-alist)))
    (when additional-sets
      (dolist (set (eldev-listify additional-sets))
        (let ((extra-dependencies (cdr (assq set eldev--extra-dependencies))))
          (when extra-dependencies
            (eldev-verbose "Need the following extra dependencies for set `%s': %s" set
                           (eldev-message-enumerate nil extra-dependencies
                                                    (lambda (package)
                                                      (eldev-format-message "%s %s" (car package) (eldev-message-version (cadr package))))
                                                    t))
            (setf all-packages (append all-packages extra-dependencies))))))
    (condition-case error
        (eldev--plan-install-or-upgrade self to-be-upgraded all-packages plan)
      (eldev-error (cond (no-error-if-missing
                          (eldev-verbose "%s" (error-message-string error)))
                         ((null to-be-upgraded)
                          ;; Refetch archive contents and retry.
                          (eldev--fetch-archive-contents t)
                          (package-read-all-archive-contents)
                          (setf plan (list nil))
                          (eldev--plan-install-or-upgrade self nil all-packages plan)))))
    (let ((planned-packages    (nreverse (car plan)))
          (non-local-plan-size 0)
          (dependency-index    0)
          unknown-packages
          missing-dependency)
      (when (and to-be-upgraded (not (eq to-be-upgraded t)))
        (dolist (package-to-be-upgraded to-be-upgraded)
          (unless (eldev-any-p (eq (package-desc-name (car it)) package-to-be-upgraded) planned-packages)
            (push package-to-be-upgraded unknown-packages)))
        (when unknown-packages
          (signal 'eldev-error `(:hint ,(unless self `("Check output of `%s dependency-tree'" ,(eldev-shell-command t)))
                                       "Cannot upgrade %s: `%s' has no such dependencies" ,(eldev-message-enumerate "package" (nreverse unknown-packages)) ,package-name))))
      (dolist (dependency planned-packages)
        (unless (eldev--loading-mode (car dependency))
          (setf non-local-plan-size (1+ non-local-plan-size))))
      (when (and dry-run (> non-local-plan-size 0))
        (eldev-verbose "In dry-run mode Eldev only pretends it is upgrading, installing or deleting dependencies"))
      (dolist (dependency planned-packages)
        (let ((previous-version (cdr dependency))
              (dependency       (car dependency)))
          (if (eldev--loading-mode dependency)
              (eldev--load-local-dependency dependency)
            (setf dependency-index (1+ dependency-index))
            (if previous-version
                (eldev-print :stderr "[%d/%d] Upgrading package `%s' (%s -> %s) from `%s'..."
                             dependency-index non-local-plan-size
                             (eldev-colorize (package-desc-name dependency) 'name) (eldev-message-version previous-version t) (eldev-message-version dependency t)
                             (package-desc-archive dependency))
              (eldev-print :stderr "[%d/%d] Installing package `%s' (%s) from `%s'..."
                           dependency-index non-local-plan-size
                           (eldev-colorize (package-desc-name dependency) 'name) (eldev-message-version dependency t) (package-desc-archive dependency)))
            (unless dry-run
              (let ((inhibit-message t))
                (package-install-from-archive dependency))))))
      (when (= non-local-plan-size 0)
        (if additional-sets
            (eldev-verbose "All project dependencies (including those for %s) have been installed already or are local"
                           (eldev-message-enumerate "set" additional-sets))
          (eldev-verbose "All project dependencies have been installed already or are local")))
      (when main-command-effect
        (unless dry-run
          (let ((inhibit-message t)
                (num-deleted     0))
            (dolist (dependency planned-packages)
              (unless (or (null (cdr dependency)) (eldev--loading-mode (car dependency)))
                (package-delete (cdr dependency))
                (setf num-deleted (1+ num-deleted))))
            (when (> num-deleted 0)
              (eldev-verbose "Deleted %s" (eldev-message-plural num-deleted (if self "obsolete package version" "obsolete dependency version"))))))
        (if (> non-local-plan-size 0)
            (eldev-print "Upgraded or installed %s" (eldev-message-plural non-local-plan-size (if self "package" "dependency package")))
          (eldev-print (if self "Eldev is up-to-date" "All dependencies are up-to-date"))))
      (when activate
        (let (recursing-for)
          (eldev-advised (#'package-activate-1
                          :around (lambda (original dependency &rest rest)
                                    (let ((inhibit-message nil))
                                      (catch 'exit
                                        (let* ((dependency-name (package-desc-name dependency))
                                               (recursing       (memq dependency-name recursing-for)))
                                          (eldev-pcase-exhaustive (unless recursing (eldev--loading-mode dependency))
                                            (`nil
                                             (eldev-trace (if recursing "Activating local dependency package `%s'..." "Activating dependency package `%s'...")
                                                          dependency-name)
                                             (or (let ((inhibit-message t))
                                                   (apply original dependency rest))
                                                 (progn (setf missing-dependency dependency-name)
                                                        nil)))
                                            ;; In all these modes dependency is activated in exactly the same
                                            ;; way, the difference is in `eldev--load-local-dependency'.
                                            ((or `as-is `source `byte-compiled `built `built-and-compiled `built-source)
                                             (dolist (requirement (package-desc-reqs dependency))
                                               (unless (package-activate (car requirement))
                                                 (throw 'exit nil)))
                                             (push (if (eq dependency-name package-name)
                                                       eldev-project-dir
                                                     ;; 2 and 3 stand for directory name and its absolute path.
                                                     (eldev-trace "Activating local dependency `%s' in directory `%s'"
                                                                  dependency-name (nth 2 (assq dependency-name eldev--local-dependencies)))
                                                     (nth 3 (assq dependency-name eldev--local-dependencies)))
                                                   load-path)
                                             (push dependency-name package-activated-list)
                                             t)
                                            (`packaged
                                             (let ((generated-package (assq dependency-name eldev--local-dependency-packages)))
                                               (unless generated-package
                                                 (error "Package for local dependency `%s' must have been generated by this point" dependency-name))
                                               (push dependency-name recursing-for)
                                               (let* ((package-user-dir (expand-file-name "local/packages" (eldev-cache-dir t)))
                                                      (up-to-date-desc  (when (nth 2 generated-package)
                                                                          ;; Package is up-to-date, no need to reinstall it.  At
                                                                          ;; least if we can find the installed copy.
                                                                          (ignore-errors (package-load-descriptor (expand-file-name (package-desc-full-name dependency) package-user-dir))))))
                                                 (if up-to-date-desc
                                                     (progn (eldev-trace "Local dependency package `%s' hasn't changed since last installation, no need to reinstall" dependency-name)
                                                            (eldev--assq-set dependency-name `(,up-to-date-desc) package-alist)
                                                            (package-activate dependency-name))
                                                   (eldev-trace "(Re)installing local dependency package `%s'..." dependency-name)
                                                   (eldev-install-package-file (nth 1 generated-package))))
                                               (pop recursing-for)))))))))
            (dolist (package all-packages)
              (unless (package-activate (car package))
                ;; We don't report the required version, but if you look at
                ;; `package-activate-1' (as of 2019-11-24), it also has problems with versions
                (if no-error-if-missing
                    (eldev-verbose "Unable to load project dependencies: package `%s' is unavailable" missing-dependency)
                  (signal 'eldev-error `("Unable to load project dependencies: package `%s' is unavailable" ,missing-dependency)))))))))))

(defun eldev--plan-install-or-upgrade (self to-be-upgraded all-packages plan)
  (let ((visited (make-hash-table :test #'eq)))
    (dolist (package all-packages)
      (eldev--do-plan-install-or-upgrade self to-be-upgraded (car package) (cadr package) plan visited))))

(defun eldev--do-plan-install-or-upgrade (self to-be-upgraded package-name required-version plan visited)
  (unless (gethash package-name visited)
    (if (package-built-in-p package-name)
        (unless (package-built-in-p package-name required-version)
          (signal 'eldev-error `("Dependency `%s' is built-in, but required version %s is too new (only %s available)"
                                 ,package-name ,(eldev-message-version required-version) "?")))
      (let* ((local             (and (not self) (eldev--loading-mode package-name)))
             (already-installed (unless local (eldev-find-package-descriptor package-name required-version nil)))
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
                (when (or (not local) (string= (package-desc-archive candidate) eldev--internal-pseudoarchive))
                  (cond ((version-list-< version required-version)
                         (when (version-list-< best-version version)
                           (setf best-version version)))
                        (disabled
                         (unless package-disabled
                           (setf package-disabled (if (stringp disabled)
                                                      `("Dependency `%s' is held at version %s, but version %s is required"
                                                        ,package-name ,disabled ,(eldev-message-version version))
                                                    `("Dependency `%s' is disabled" ,package-name)))))
                        ((or (null already-installed) (version-list-< (package-desc-version already-installed) version))
                         (setf package candidate))))))
            (unless package
              (setf package already-installed))
            (unless package
              (signal 'eldev-error (or package-disabled
                                       (if best-version
                                           `("Dependency `%s' version %s is required, but at most %s is available"
                                             ,package-name ,(eldev-message-version required-version) ,(eldev-message-version best-version))
                                         `("Dependency `%s' (%s) is not available" ,package-name ,(eldev-message-version required-version))))))))
        (dolist (requirement (package-desc-reqs package))
          (eldev--do-plan-install-or-upgrade self to-be-upgraded (car requirement) (cadr requirement) plan visited))
        (unless (eq package already-installed)
          (push `(,package . ,already-installed) (car plan)))))
    (puthash package-name t visited)))

(defun eldev--internal-pseudoarchive-dir (&optional ensure-exists)
  (let ((dir (expand-file-name eldev--internal-pseudoarchive (expand-file-name "archives" package-user-dir))))
    (when ensure-exists
      (make-directory dir t))
    dir))

(defun eldev--create-internal-pseudoarchive-descriptor ()
  ;; Create our fake pseudoarchive.  We do each time anew to avoid tracking local
  ;; dependency changes, which is likely slower and certainly harder than just writing a
  ;; single file.
  (with-temp-file (expand-file-name "archive-contents" (eldev--internal-pseudoarchive-dir t))
    (let ((package (eldev-package-descriptor)))
      (prin1 `(,package-archive-version
               ;; Description, file type and extras are irrelevant for us.
               (,(package-desc-name package) . ,(package-make-ac-desc (package-desc-version package) (package-desc-reqs package) nil 'single nil))
               ,@(mapcar (lambda (entry)
                           (let ((dependency (nth 1 entry)))
                             `(,(package-desc-name dependency) . ,(package-make-ac-desc (package-desc-version dependency) (package-desc-reqs dependency) nil 'single nil))))
                         eldev--local-dependencies))
             (current-buffer))
      (insert "\n"))))

(defun eldev--fetch-archive-contents (&optional refetch-contents)
  ;; I don't see a way to find if package archive contents is fetched
  ;; already without going into internals.
  (let ((archive-dir (expand-file-name "archives" package-user-dir))
        unfetched-archives)
    (dolist (archive package-archives)
      (unless (string= (car archive) eldev--internal-pseudoarchive)
        (if (file-exists-p (expand-file-name "archive-contents" (expand-file-name (car archive) archive-dir)))
            (if refetch-contents
                (progn (eldev-trace "Will refetch contents of package archive `%s' in case it has changed" (car archive))
                       (push archive unfetched-archives))
              (eldev-trace "Contents of package archive `%s' has been fetched already" (car archive)))
          (push archive unfetched-archives))))
    (when unfetched-archives
      (eldev-verbose "Fetching contents of %s..." (eldev-message-enumerate "package archive" unfetched-archives #'car))
      ;; See comments in `eldev-cli'.
      (let ((eldev-message-rerouting-destination :stderr)
            (package-archives                    unfetched-archives)
            (inhibit-message                     t))
        (package-refresh-contents)))))

(defun eldev--loading-mode (dependency)
  "Get loading mode of package DEPENDENCY.
DEPENDENCY can be either a name (symbol) or a package
descriptor."
  (let ((dependency-name (if (symbolp dependency) dependency (package-desc-name dependency))))
    (if (eq dependency-name (package-desc-name (eldev-package-descriptor)))
        (or eldev-project-loading-mode 'as-is)
      (let ((entry (assq dependency-name eldev--local-dependencies)))
        (if entry
            (or (nth 4 entry) 'as-is)
          (unless (or (symbolp dependency) (not (string= (package-desc-archive dependency) eldev--internal-pseudoarchive)))
            (error "Unexpected local dependency `%s'" dependency-name)))))))

(defun eldev--load-local-dependency (dependency)
  (let* ((dependency-name (package-desc-name dependency))
         (package-name    (package-desc-name (eldev-package-descriptor)))
         (project-itself  (eq dependency-name package-name))
         (loading-mode    (eldev--loading-mode dependency)))
    (when (cdr (assq dependency-name package-alist))
      (error "Local dependency `%s' is already listed in `package-alist'" dependency-name))
    ;; FIXME: Special-case project itself: no need to launch separate process(es) for it.
    ;; I don't want to move this into an alist, to avoid fixing the way it works.
    (let ((commands (eldev-pcase-exhaustive loading-mode
                      (`as-is)              ; Nothing to do.
                      (`source              `(("clean" ".elc" "--set" "main" "--delete")))
                      (`byte-compiled       `(("build" ":compile")))
                      (`built               `(("build" ":default")))
                      (`built-and-compiled  `(("build" ":default" ":compile")))
                      (`built-source        `(("clean" ".elc" "--set" "main" "--delete")
                                              ("build" ":default")))
                      (`packaged            `(("package" "--output-dir" ,(expand-file-name "local/generated" (eldev-cache-dir t)) "--print-filename"))))))
      (when commands
        (if project-itself
            (eldev-verbose "Preparing to load the project in mode `%s'" loading-mode)
          (eldev-verbose "Preparing to load local dependency `%s' in mode `%s'" dependency-name loading-mode))
        (let ((default-directory (if project-itself eldev-project-dir (nth 3 (assq dependency-name eldev--local-dependencies)))))
          (dolist (command commands)
            (eldev-trace "Full command line (in directory `%s'):\n  %s" default-directory (eldev-message-command-line (eldev-shell-command) command))
            (eldev-call-process (eldev-shell-command) command
              (if (= exit-code 0)
                  (progn
                    (eldev--forward-process-output "Output of the child Eldev process:" "Child Eldev process produced no output" t)
                    (when (string= (car command) "package")
                      (goto-char (point-max))
                      (forward-line -2)
                      (let ((point (point)))
                        (end-of-line)
                        (let ((file (buffer-substring-no-properties point (point))))
                          (forward-line)
                          (push `(,dependency-name ,file ,(looking-at "up-to-date")) eldev--local-dependency-packages)))))
                (eldev-warn "Output of the child Eldev process:\n%s" (buffer-string))
                (signal 'eldev-error (if project-itself
                                         `("Child Eldev process for exited with error code %d" ,exit-code)
                                       `("Child Eldev process for local dependency `%s' exited with error code %d" ,dependency-name ,exit-code))))))))
      (push `(,dependency-name . (,dependency)) package-alist))))



;; eldev clean

(defvar eldev-clean-dry-run-mode nil
  "Don't delete anything if non-nil, just pretend to do so.")

(defvar eldev--cleaners nil)
(defvar eldev--cleaner-aliases nil)

(defvar eldev-clean-sets nil
  "Target sets to clean.
See `eldev-filesets'.  Default (nil) means to clean everything.")


;; Internal helper for `eldev-defcleaner'.
(defun eldev--register-cleaner (cleaner name keywords)
  (while keywords
    (eldev-pcase-exhaustive (pop keywords)
      (:aliases
       (eldev-register-cleaner-aliases name (pop keywords)))
      ((and (or :briefdoc :default :superseded-by) keyword)
       (eldev-put cleaner keyword (pop keywords)))))
  (eldev--assq-set name cleaner eldev--cleaners))

(defun eldev-register-cleaner-aliases (cleaner aliases)
  (dolist (alias (eldev-listify aliases))
    (eldev--assq-set alias cleaner eldev--cleaner-aliases)))

(defmacro eldev-defcleaner (name arguments &rest body)
  "Define an Eldev cleaner.
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
        value is derived from function name by removing `eldev-'
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
        i.e. when `eldev clean' is issued without further
        command-line arguments.

    :superseded-by CLEANERS

        Don't invoke this cleaner if one of CLEANERS (which must
        be a symbol or a list of those) is also going to be
        invoked."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body  (eldev-macroexp-parse-body body))
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
            (eldev--register-cleaner ',name ',cleaner-name ',(nreverse keywords)))))

(eldev-defcommand eldev-clean (&rest parameters)
  "Delete various files produced during building.  Eldev provides
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
            (unless (or (assq name eldev--cleaners) (assq name eldev--cleaner-aliases))
              (signal 'eldev-error `(:hint ("Check output of `%s clean --list-cleaners'" ,(eldev-shell-command t)) "Unknown cleaner `%s'" ,name)))
            (push name cleaners)))
      (dolist (entry eldev--cleaners)
        (when (eldev-get (cdr entry) :default)
          (push (car entry) cleaners))))
    (when all
      (setf cleaners (mapcar #'car eldev--cleaners)))
    (unless cleaners
      (signal 'eldev-error `("There are no %scleaners registered in this project" ,(if all "" "default "))))
    (setf cleaners (nreverse cleaners))
    (dolist (cleaner cleaners)
      (let ((function (or (cdr (assq cleaner eldev--cleaners)) (cdr (assq (cdr (assq cleaner eldev--cleaner-aliases)) eldev--cleaners))))
            superseded-by)
        (if (eldev-any-p (let ((superseded-by-resolved (or (cdr (assq it eldev--cleaner-aliases)) it)))
                           (setf superseded-by it)
                           (eldev-any-p (eq (or (cdr (assq it eldev--cleaner-aliases)) it) superseded-by-resolved) cleaners))
                         (eldev-listify (eldev-get function :superseded-by)))
            (eldev-trace "Skipping cleaner `%s': superseded by `%s'" cleaner superseded-by)
          (eldev-verbose "Invoking cleaner `%s'..." cleaner)
          (let ((files (eldev-listify (funcall function))))
            (eldev-trace "%s" (eldev-message-enumerate-files "File%s to be deleted according to this cleaner: %s (%d)" files))
            (setf to-delete (append to-delete files))))))
    (setf to-delete (mapcar (lambda (file) (expand-file-name file eldev-project-dir)) to-delete))
    (dolist (file to-delete)
      (let* ((attributes   (file-attributes file))
             (directory    (eq (nth 0 attributes) t))
             (printed-name (file-relative-name file eldev-project-dir)))
        (when (eldev-external-filename printed-name)
          (setf printed-name file))
        (when directory
          (setf printed-name (file-name-as-directory printed-name)))
        (if attributes
            (progn
              (if eldev-clean-dry-run-mode
                  (eldev-output "%s" printed-name)
                (eldev-verbose (if directory "Recursively deleting directory `%s'..." "Deleting file `%s'...") printed-name)
                (if directory
                    (delete-directory file t)
                  (delete-file file)))
              (if directory
                  (setf num-deleted-dirs (1+ num-deleted-dirs))
                (setf num-deleted-files (1+ num-deleted-files))))
          (eldev-trace "Skipping file `%s' as it doesn't exist" printed-name))))
    (unless eldev-clean-dry-run-mode
      (let ((deleted-dirs  (eldev-message-plural num-deleted-dirs "directory" "directories"))
            (deleted-files (eldev-message-plural num-deleted-files "file")))
        (cond ((and (> num-deleted-dirs 0) (> num-deleted-files 0))
               (eldev-print "Deleted %s and %s" deleted-dirs deleted-files))
              ((> num-deleted-dirs 0)
               (eldev-print "Deleted %s" deleted-dirs))
              ((> num-deleted-files 0)
               (eldev-print "Deleted %s" deleted-files)))))
    (when (and (= num-deleted-files 0) (= num-deleted-dirs 0))
      (eldev-print "Nothing to delete"))))

(defun eldev-clean-fileset ()
  "Return effective fileset to clean."
  (apply #'eldev-standard-filesets :or (or eldev-clean-sets '(all))))

;; Similar to `eldev-build-set', but the default value is different.
(eldev-defoption eldev-clean-set (name)
  "Clean targets from this set; special set name `all' can be
used"
  :options        (-s --set)
  :for-command    clean
  :value          NAME
  :default-value  (eldev-message-enumerate nil (or eldev-build-sets '(all)) nil t)
  (setf eldev-clean-sets (append eldev-clean-sets (list (eldev-validate-standard-fileset name)))))

(eldev-defbooloptions eldev-clean-dry-run-mode eldev-clean-do-clean-mode eldev-clean-dry-run-mode
  ("Don't delete anything, only print names of files to be deleted"
   :options       (-n --dont-delete --dry-run))
  ("Delete files and directories as requested"
   :options       (-d --delete)
   :hidden-if     :default)
  :for-command    clean)

(eldev-defoption eldev-clean-list-cleaners ()
  "List available cleaners and exit"
  :options        (-L --list-cleaners --list)
  :for-command    clean
  (eldev-print "Cleaner(s) marked with `*' are default, i.e. executed also when\nno cleaner names are specified on the command line.\n")
  (let ((all-cleaners (reverse eldev--cleaners)))
    (while all-cleaners
      (let* ((cleaner  (pop all-cleaners))
             (name     (car cleaner))
             (function (cdr cleaner))
             (header   (if (eldev-get function :default) (format "%s [*]" name) (format "%s" name))))
        (if (eldev-unless-quiet t)
            (let (aliases)
              (dolist (alias eldev--cleaner-aliases)
                (when (eq (cdr alias) name)
                  (push (car alias) aliases)))
              (eldev-output "%s\n" (eldev-colorize header 'section))
              (when aliases
                (eldev-output "    %s\n" (eldev-message-enumerate '("Alias:" "Aliases:") aliases #'symbol-name t t)))
              (eldev-output "%s" (or (eldev-documentation function) "Not documented"))
              (when all-cleaners
                (eldev-output "\n")))
          (let ((documentation (eldev-briefdoc function)))
            (if documentation
                (eldev-output "%-18s  %s" header documentation)
              (eldev-output "%s" header)))))))
  (signal 'eldev-quit 0))


(eldev-defcleaner eldev-cleaner-.eldev ()
  "Delete `.eldev' directory.  This cleans all Eldev caches for
this project, for this and other Emacs versions."
  :aliases       (dot-eldev)
  (eldev-cache-dir nil))

(eldev-defcleaner eldev-cleaner-eldev-cache ()
  "Delete all Eldev caches for this Emacs version and project.
This currently includes installed dependencies and test results.
Cached data for other Emacs versions is not affected."
  :superseded-by dot-eldev
  :aliases       ecache
  (eldev-cache-dir t))

(eldev-defcleaner eldev-cleaner-dependencies ()
  "Delete installed project dependencies.  Deleted dependencies
will be automatically reinstalled when needed, e.g. if you invoke
`test' command.  This cleaner is a way to indirectly upgrade all
dependencies."
  :superseded-by (ecache dot-eldev)
  :aliases       (deps requirements reqs)
  (expand-file-name "packages" (eldev-cache-dir t)))

(eldev-defcleaner eldev-cleaner-distribution ()
  "Delete `dist' directory, where package tarballs are generated."
  :aliases       dist
  (let* ((directory     (eldev-dist-dir))
         (relative-name (file-relative-name directory eldev-project-dir)))
    (cond ((string= relative-name ".")
           (eldev-warn "Refusing to delete `eldev-dist-dir' since it matches `eldev-project-dir'")
           nil)
          ((eldev-external-or-absolute-filename relative-name)
           (if eldev-clean-external-dist
               (progn (eldev-verbose "Deleting `dist' dir (%s), even though it is not inside the project (see variable `eldev-clean-external-dist'" directory)
                      directory)
             (eldev-warn "Refusing to delete `eldev-dist-dir' since it is not inside `eldev-project-dir'")
             nil))
          (t
           directory))))



;; eldev archives, eldev dependencies, eldev dependency-tree

(defvar eldev-dependencies-list-built-ins nil
  "Whether to list built-in packages among built-ins.
The most obvious such package is `emacs` used to declare minimum
required version of Emacs itself.")

(eldev-defcommand eldev-archives (&rest parameters)
  "List package archives used to look up dependencies.  Archives
can be registered using function `eldev-use-package-archive' in
project's `Eldev' file."
  :aliases        arch
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (if package-archives
      (let* ((have-priorities (boundp 'package-archive-priorities))
             (priorities      (when have-priorities package-archive-priorities)))
        (dolist (archive (sort package-archives (lambda (a b) (> (or (cdr (assoc (car a) priorities)) 0)
                                                                 (or (cdr (assoc (car b) priorities)) 0)))))
          (eldev-output "%s: %s%s"
                        (eldev-colorize (car archive) 'name)
                        (eldev-colorize (cdr archive) 'url)
                        (if have-priorities
                            (eldev-format-message "  (priority: %s)" (or (cdr (assoc (car archive) priorities)) "0, defaulted"))
                          ""))))
    (eldev-print "None specified; add form `(eldev-use-package-archive ...)' in file `%s'" eldev-file)))

(eldev-defcommand eldev-dependencies (&rest parameters)
  "List dependencies of the current project.

If any additional sets are specified, dependencies from the sets
are listed too.  Sets typically have the same name as commands.
E.g. there might be additional dependencies for testing (`test')
or evaluating (`eval') registered in the project's `Eldev' file."
  :aliases        (deps requirements reqs)
  :parameters     "[ADDITIONAL-SET...]"
  :works-on-old-eldev t
  (let ((additional-sets (mapcar #'intern parameters))
        have-dependencies
        have-non-built-in-dependencies)
    (dolist (dependency (package-desc-reqs (eldev-package-descriptor)))
      (if (and (package-built-in-p (car dependency)) (not eldev-dependencies-list-built-ins))
          (eldev-trace "Omitting dependency as a built in: %s %s" (car dependency) (eldev-message-version (cadr dependency)))
        (eldev-output "%s %s" (eldev-colorize (car dependency) 'name) (eldev-message-version (cadr dependency) t))
        (setf have-non-built-in-dependencies t))
      (setf have-dependencies t))
    (dolist (set additional-sets)
      (dolist (dependency (cdr (assq set eldev--extra-dependencies)))
        (if (and (package-built-in-p (car dependency)) (not eldev-dependencies-list-built-ins))
            (eldev-trace "Omitting extra dependency for set `%s' as a built in: %s %s" set (car dependency) (eldev-message-version (cadr dependency)))
          (eldev-output "%s %s%s" (eldev-colorize (car dependency) 'name) (eldev-message-version (cadr dependency) t)
                        (or (eldev-unless-quiet (eldev-format-message "    [for `%s']" set)) ""))
          (setf have-non-built-in-dependencies t))
        (setf have-dependencies t)))
    (if have-dependencies
        (unless have-non-built-in-dependencies
          (eldev-print "Project `%s' has only built-in dependencies; rerun with `--list-built-ins'" (eldev-colorize (package-desc-name (eldev-package-descriptor)) 'name)))
      (eldev-print "Project `%s' has no dependencies" (eldev-colorize (package-desc-name (eldev-package-descriptor)) 'name)))))

(eldev-defcommand eldev-dependency-tree (&rest parameters)
  "Show dependencies of the current project recursively.

If any additional sets are specified, dependencies from the sets
are listed too.  Sets typically have the same name as commands.
E.g. there might be additional dependencies for testing (`test')
or evaluating (`eval') registered in the project's `Eldev' file."
  :aliases        (dtree deptree requirement-tree rtree reqtree)
  :parameters     "[ADDITIONAL-SET...]"
  (let ((additional-sets (mapcar #'intern parameters)))
    (eldev-load-project-dependencies additional-sets t)
    (let ((package (eldev-package-descriptor))
          (listed  (make-hash-table :test #'equal)))
      (eldev--do-dependency-tree (package-desc-name package) (package-desc-version package) package 0 listed)
      (dolist (set additional-sets)
        (dolist (dependency (cdr (assq set eldev--extra-dependencies)))
          (eldev--do-dependency-tree (car dependency) (cadr dependency) (eldev-find-package-descriptor (car dependency) (cadr dependency)) 0 listed set))))))

(eldev-defbooloptions eldev-dependencies-list-built-ins eldev-dependencies-omit-built-ins eldev-dependencies-list-built-ins
  ("Also list dependencies that are Emacs built-ins"
   :options       (-b --list-built-ins))
  ("Omit built-in dependencies"
   :options       (-B --omit-built-ins))
  :for-command    (dependencies dependency-tree))

(defun eldev--do-dependency-tree (package-name version descriptor level listed &optional additional-set)
  (let ((repeated (gethash (cons package-name version) listed)))
    (if (and (package-built-in-p package-name) (not eldev-dependencies-list-built-ins))
        (eldev-trace "Omitting dependency as a built-in: %s %s" package-name (eldev-message-version version))
      (let (remarks
            important-remarks)
        (cond ((package-built-in-p package-name)
               (push "built-in" remarks))
              ((null descriptor)
               (push (eldev-colorize "UNAVAILABLE" 'warn) important-remarks))
              (repeated
               (push (eldev-colorize "repeated, see above" 'details) remarks))
              ((not (equal version (package-desc-version descriptor)))
               (push (eldev-format-message "%s installed" (eldev-message-version descriptor t)) remarks)))
        (when additional-set
          (push (eldev-format-message "for `%s'" additional-set) remarks))
        (setf remarks (nconc (eldev-unless-quiet remarks) important-remarks))
        (eldev-output "%s%s %s%s" (make-string (* level 4) ? ) (eldev-colorize package-name 'name) (eldev-message-version version t)
                      (if remarks
                          (eldev-format-message "    [%s]" (mapconcat #'identity remarks "; "))
                        "")))
      (when (and descriptor (not repeated))
        (puthash (cons package-name version) t listed)
        (dolist (dependency (package-desc-reqs descriptor))
          (eldev--do-dependency-tree (car dependency) (cadr dependency) (eldev-find-package-descriptor (car dependency) (cadr dependency)) (1+ level) listed))))))



;; eldev version, eldev info

(eldev-defcommand eldev-version (&rest parameters)
  "Display version information and exit.  By default version of
Eldev itself is displayed.

You can also specify name of the package(s) you are interested in
on the command line.  These may include the project being built,
any of its dependencies, any dependencies of those (recursively),
anything registered with `eldev-add-extra-dependencies', `eldev'
itself or `emacs'.  If multiple versions are requested, each is
printed on a separate line.

If command line parameters include project dependencies, they are
installed first if needed.

Normally, package name and version is printed.  However, in quiet
mode output is restricted to just the version."
  :parameters     "[PACKAGE...]"
  :works-on-old-eldev t
  (let* ((packages     (if parameters
                           (mapcar #'intern parameters)
                         '(eldev)))
         (this-package (eldev-package-descriptor)))
    ;; Special handling of the project itself so that its version can
    ;; be queried even if there are unavailable dependencies.
    (when (eldev-any-p (not (or (eq it 'emacs) (eq it (package-desc-name this-package)) (eldev-find-package-descriptor it nil t))) packages)
      (eldev-load-project-dependencies (mapcar #'car eldev--extra-dependencies)))
    (dolist (package packages)
      (let ((version (if (eq package 'emacs)
                         emacs-version
                       (let ((descriptor (if (eq package (package-desc-name this-package))
                                             this-package
                                           (eldev-find-package-descriptor package nil t))))
                         (unless descriptor
                           (signal 'eldev-error `(:hint ("Check output of `%s dependency-tree'" ,(eldev-shell-command t))
                                                        "Package `%s' is not among those used in the build" ,package)))
                         (eldev-message-version descriptor)))))
        (eldev-print :nolf "%s " package)
        (eldev-output "%s" version)))))

(eldev-defcommand eldev-info (&rest parameters)
  "Display information about the project."
  :works-on-old-eldev t
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (let* ((package     (eldev-package-descriptor))
         (description (when (fboundp 'package--get-description) (package--get-description package))))
    (unless (> (length description) 0)
      (setf description (package-desc-summary package)))
    (eldev-output "%s %s" (eldev-colorize (package-desc-name package) 'name) (eldev-message-version package))
    (when description
      (setf description (replace-regexp-in-string (rx (| (: bos (1+ (or space "\n"))) (: (1+ (or space "\n")) eos))) "" description))
      (unless (string= description "")
        (eldev-output "\n%s" description)))))



;; eldev test

(declare-function eldev-test-ert-preprocess-selectors "eldev-ert" (selectors))
(declare-function eldev-test-ert-load-results "eldev-ert" ())
(declare-function eldev-test-ert-save-results "eldev-ert" ())
(declare-function eldev-run-ert-tests "eldev-ert" (selectors &optional environment))

(defvar eldev-test-dwim t
  "Employ some heuristics when parsing command line of `test' command.

* Any selector that ends in `.el' is instead treated as a file
  pattern.

* For ERT: any symbol selector that doesn’t match a test name is
  instead treated as regular expression (i.e. as a string).

These heuristics are aimed at further simplifying test execution.")

(defvar eldev-test-stop-on-unexpected nil
  "If non-nil, stop as soon as a test produces an unexpected result.")

(defvar eldev-test-print-backtraces t
  "If non-nil (default), print assertion backtraces.")

(defvar eldev-test-expect-at-least nil
  "Fail if there are fewer than this many tests.
This is mostly meant for automated tests to prevent accidental
situations where success is claimed only because no/few tests
were executed.")

(defvar eldev-test-framework nil
  "Test framework to use.
If left nil (default value), Eldev will try to autodetect.")

(defvar eldev-test-runner nil
  "Test runner to use.")

(defvar eldev--test-runners nil)

(defvar eldev-test-file-patterns nil
  "Load only those test files that match one of these patterns.
Should normally be specified only from command line.")

(defvar eldev-test-known-frameworks '((ERT . ((detect               . (lambda () (featurep 'ert)))
                                              (preprocess-selectors . (lambda (selectors)
                                                                        (require 'eldev-ert)
                                                                        (eldev-test-ert-preprocess-selectors selectors)))
                                              (prepare              . (lambda (_selectors)
                                                                        (require 'eldev-ert)
                                                                        (eldev-test-ert-load-results)))
                                              (run-tests            . (lambda (selectors _runner environment)
                                                                        (require 'eldev-ert)
                                                                        (eldev-run-ert-tests selectors environment)))
                                              (finalize             . (lambda (_selectors)
                                                                        (require 'eldev-ert)
                                                                        (eldev-test-ert-save-results))))))
  "Alist of all test frameworks known to Eldev.
While this variable is public and can be modified, you most
likely shouldn't do that.  Leave adding support for more
frameworks to Eldev code, instead write a test runner if you need
it.")


(eldev-defcommand eldev-test (&rest parameters)
  "Run project's regression/unit tests.  By default all tests
defined in the project are executed.  However, you can restrict
their set using SELECTORs and/or `--file' option listed above.

If `--file' is specified, only files that match the PATTERN (and
also project's test fileset) are loaded.  See documentation on
Eldev filesets for details of what can be specified as argument.
The option can be used multiple times, in which case all its
arguments are combined into a single fileset.

Remember that files are loaded and executed as normal Elisp, so
`require' forms can result in loading more than you specify.

SELECTORs can be used to choose tests to run among the loaded
ones.  Multiple are combined as `or' operation: all tests that
match any of the specified selectors are executed.  When used in
combination with `--file', they result in `and' operation: only
those tests that are loaded and match the selectors are executed.

If `eldev-test-dwim' variable is on (default), Eldev employs
additional heuristics when processing SELECTORs.  This variable
is not controllable from command line---change its value in
`~/.eldev/config' or `Eldev-local'.

This command exits with error code 1 if any test produces an
unexpected result."
  :parameters     "[SELECTOR...]"
  (eldev-load-project-dependencies 'test)
  (let ((files (eldev-find-and-trace-files `(:and ,(eldev-standard-fileset 'test) "*.el") "test `.el' file%s"))
        selectors)
    (when files
      (let ((filter-patterns eldev-test-file-patterns))
        (dolist (selector parameters)
          (if (and eldev-test-dwim (string-match-p (rx ".el" eol) selector))
              (push selector filter-patterns)
            (push selector selectors)))
        (setf selectors (nreverse selectors))
        (when filter-patterns
          (setf files (eldev-filter-files files (reverse filter-patterns)))
          (eldev-trace "%s" (eldev-message-enumerate-files "Remaining test `.el' file%s after applying `--file' filter(s): %s (%d)" files)))))
    ;; Framework autoguessing can only work if there is at least one file to load, so this
    ;; `if' is important.
    (if files
        (progn
          (dolist (file files)
            (let* ((absolute-without-el (replace-regexp-in-string (rx ".el" eos) "" (expand-file-name file eldev-project-dir) t t))
                   (already-loaded      (eldev-any-p (assoc (concat absolute-without-el it) load-history) load-suffixes)))
              (if already-loaded
                  (eldev-trace "Not loading file `%s': already `require'd by some other file" file)
                (eldev-verbose "Loading test file `%s'..." file)
                (load absolute-without-el nil t nil t))))
          (let* ((runner-name (or eldev-test-runner 'simple))
                 (runner      (or (cdr (assq runner-name eldev--test-runners))
                                  (signal 'eldev-error `(:hint ("Check output of `%s test --list-runners'" ,(eldev-shell-command t)) "Unknown test runner `%s'" ,runner-name))))
                 (supported   (eldev-get runner :frameworks))
                 (framework   (or (eldev-test-framework)
                                  (signal 'eldev-error `(:hint "Consider setting `eldev-test-framework' explicitly" "Unable to guess testing framework")))))
            (unless (or (null supported) (eq framework supported) (and (listp supported) (memq framework supported)))
              (signal 'eldev-error `(:hint ("Run `%s test --list-runners' for more information" ,(eldev-shell-command t))
                                           "Test runner `%s' doesn't support framework `%s'" ,runner-name ,framework)))
            (setf selectors (eldev-test-preprocess-selectors framework selectors))
            (eldev-test-prepare-framework framework selectors)
            (unwind-protect
                (funcall runner framework selectors)
              (eldev-test-finalize-framework framework selectors))))
      (eldev-print "No test files to load"))))

(defun eldev-test-get-framework-data (framework)
  "Get handlers for given FRAMEWORK."
  (or (cdr (assq framework eldev-test-known-frameworks))
      (error "Unknown test framework `%s'" framework)))

(defun eldev-test-framework ()
  "Get used test framework.
If variable `eldev-test-framework' has nil value, framework is
autodetected if possible."
  (or eldev-test-framework
      (let ((scan eldev-test-known-frameworks)
            detected)
        (while scan
          (let* ((framework-data (pop scan))
                 (detect         (cdr (assq 'detect (cdr framework-data)))))
            (when (and detect (funcall detect))
              (setf detected (car framework-data)
                    scan     nil))))
        detected)))

(defun eldev-test-preprocess-selectors (framework selectors)
  "Convert specified SELECTORS for use by given FRAMEWORK."
  (let ((preprocess-selectors (cdr (assq 'preprocess-selectors (eldev-test-get-framework-data framework)))))
    (if preprocess-selectors
        (funcall preprocess-selectors selectors)
      selectors)))

(defun eldev-test-selectors-to-elisp-values (selectors &optional cons-with-string)
  "Read SELECTORS as Elisp expressions.
This function is meant for use in framework support
implementations."
  (mapcar (lambda (selector)
            (let ((as-elisp (eldev-read-wholly selector "selector `%s'")))
              (if cons-with-string `(,as-elisp . ,selector) as-elisp)))
          selectors))

(defun eldev-test-prepare-framework (framework selectors)
  "Prepare given test FRAMEWORK."
  (let ((prepare (cdr (assq 'prepare (eldev-test-get-framework-data framework)))))
    (when prepare
      (funcall prepare selectors))))

(defun eldev-test-finalize-framework (framework selectors)
  "Finalize given test FRAMEWORK after executing tests."
  (condition-case error
      (let ((finalize (cdr (assq 'finalize (eldev-test-get-framework-data framework)))))
        (when finalize
          (funcall finalize selectors)))
    (error (eldev-warn "When finalizing test framework `%s': %s" framework (error-message-string error)))))

(defmacro eldev-test-do-load-results (file-extension description expected-version &rest body)
  "Load previous test results.
This macro is meant for use in framework support
implementations."
  (declare (indent 3) (debug (stringp stringp numberp body)))
  (let ((file (expand-file-name (format "test-results.%s" file-extension) (eldev-cache-dir t))))
    `(eldev-do-load-cache-file ,file ,description ,expected-version ,@body)))

(defmacro eldev-test-do-save-results (file-extension description version &rest body)
  "Save test results.
This macro is meant for use in framework support
implementations."
  (declare (indent 3) (debug (stringp stringp numberp body)))
  (let ((file (expand-file-name (format "test-results.%s" file-extension) (eldev-cache-dir t t))))
    `(eldev-do-save-cache-file ,file ,description ,version ,@body)))

(defun eldev-test-validate-amount (num-tests)
  "Fail if NUM-TESTS are not enough.
See `eldev-test-expect-at-least'."
  (when (and eldev-test-expect-at-least (< num-tests eldev-test-expect-at-least))
    (signal 'eldev-error `("Too few tests match the selectors (%d, expected at least %d)" ,num-tests ,eldev-test-expect-at-least))))


(eldev-defcleaner eldev-cleaner-test-results ()
  "Forget all test results for this Emacs version.  All tests
will count as new for frameworks that have appropriate
selectors (e.g. ERT's `:new')."
  :aliases        (tests)
  (eldev-find-files (format "./%s/test-results.*" (file-relative-name (eldev-cache-dir t) eldev-project-dir))))


;; Internal helper for `eldev-deftestrunner'.
(defun eldev--register-test-runner (runner name keywords)
  (while keywords
    (eldev-pcase-exhaustive (pop keywords)
      ((and (or :briefdoc :frameworks) keyword)
       (eldev-put runner keyword (pop keywords)))))
  (eldev--assq-set name runner eldev--test-runners))

(defmacro eldev-deftestrunner (name arguments &rest body)
  "Define an Eldev test runner."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body (eldev-macroexp-parse-body body))
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
            (eldev--register-test-runner ',name ',runner-name ',(nreverse keywords)))))


(eldev-deftestrunner eldev-test-runner-standard (framework selectors)
  "Invokes test framework without changing anything."
  (funcall (cdr (assq 'run-tests (eldev-test-get-framework-data framework))) selectors 'standard nil))

(eldev-deftestrunner eldev-test-runner-simple (framework selectors)
  "Simple test runner with a few tweaks to the defaults.

For ERT:
  - if Eldev is in quiet mode, set `ert-quiet' to t;
  - only backtrace frames inside test functions are printed;
  - long line trimming is disabled."
  ;; Workaround: older Emacs versions don't support setting
  ;; `ert-batch-backtrace-right-margin' to nil.  We assume that if the
  ;; variable is customizable, nil is already supported.
  (let ((right-margin (if (get 'ert-batch-backtrace-right-margin 'custom-type)
                          nil
                        1000000)))
    (funcall (cdr (assq 'run-tests (eldev-test-get-framework-data framework))) selectors 'simple
             ;; Non-ERT frameworks are just invoked with empty environment.
             (pcase framework
               (`ERT `((ert-quiet                        . ,(not (eldev-unless-quiet t)))
                       (ert-batch-backtrace-right-margin . ,right-margin)
                       (eldev--test-ert-short-backtraces . t)))))))

(eldev-defoption eldev-test-files (pattern)
  "Load only tests in given file(s)"
  :options        (-f --file)
  :for-command    test
  :value          PATTERN
  (push pattern eldev-test-file-patterns))

(eldev-defbooloptions eldev-test-stop-on-unexpected-mode eldev-test-continue-mode eldev-test-stop-on-unexpected
  ("Stop if any test produces an unexpected result (usually a failure)"
   :options       (-s --stop --stop-on-unexpected))
  ("Execute all scheduled tests regardless of results"
   :options       (-c --continue))
  :for-command    test)

(eldev-defbooloptions eldev-test-print-backtraces eldev-test-omit-backtraces eldev-test-print-backtraces
  ("Print failure backtraces"
   :options       (-b --print-backtraces))
  ("Omit failure backtraces for brevity"
   :options       (-B --omit-backtraces))
  :for-command    test)

(eldev-defoption eldev-test-expect-at-least (n)
  "Fail if fewer than N tests match selectors"
  :options        (-X --expect)
  :for-command    test
  :value          N
  (setf eldev-test-expect-at-least (string-to-number n)))

(eldev-defoption eldev-test-runner (name)
  "Choose test runner"
  :options        (-r --runner)
  :for-command    test
  :value          NAME
  :default-value  (or eldev-test-runner 'simple)
  ;; Will be validated when executing the command.
  (setf eldev-test-runner (intern name)))

(eldev-defoption eldev-test-list-runners ()
  "List available test runners and exit"
  :options        --list-runners
  :for-command    test
  (let ((all-runners (reverse eldev--test-runners)))
    (while all-runners
      (let* ((runner   (pop all-runners))
             (name     (car runner))
             (function (cdr runner)))
        (if (eldev-unless-quiet t)
            (progn (eldev-output "%s\n" (eldev-colorize name 'section))
                   (eldev-output "    %s\n" (eldev-message-enumerate '("Supported framework:" "Supported frameworks:")
                                                                     (or (eldev-get function :frameworks) (mapcar #'car eldev-test-known-frameworks)) nil t))
                   (eldev-output "%s" (or (eldev-documentation function) "Not documented"))
                   (when all-runners
                     (eldev-output "\n")))
          (let ((documentation (eldev-briefdoc function)))
            (if documentation
                (eldev-output "%-18s  %s" name documentation)
              (eldev-output "%s" name)))))))
  (signal 'eldev-quit 0))



;; eldev eval, eldev exec

(defvar eldev-default-required-features :project-name
  "Features that certain commands require.
Currently affected commands are `eval', `exec' and `emacs'.
There are also variables `eldev-eval-required-features' and
`eldev-emacs-required-features' that take precedence if they have
an appropriate value.

If let as `:project-name', defaults to project's name and assumes
that the project provides that feature.  Otherwise should be a
single symbol or a list of symbols (empty list---nil---of course
means not to require anything).")

(defvar eldev-eval-lexical t
  "Whether to evaluate expressions in lexical or dynamic scope.")

(defvar eldev-eval-require-main-feature t
  "Whether to autorequire main project features.")

(defvar eldev-eval-printer-function #'eldev-prin1
  "Default printer function for `eval' command.
Standard value is `eldev-prin1', but can be customized.")

(defvar eldev-eval-required-features :default
  "Features that `eval' and `exec' commands require.
See `eldev-default-required-features' for value description.
Special value `:default' means to use that variable's value
instead.")

(eldev-defcommand eldev-eval (&rest parameters)
  "Evaluate Lisp expressions and print results.  Expressions are
evaluated in project's environment, with all the dependencies
available.  If option `-r' is specified (or is on by default),
project's main feature will be required before evaluating, so
that you don't have to include `(require ...)' yourself."
  :parameters     "EXPRESSION..."
  :aliases        evaluate
  (eldev--do-eval t parameters))

(eldev-defcommand eldev-exec (&rest parameters)
  "Execute Lisp forms for side effect.  Forms are executed in
project's environment, with all the dependencies available.  If
option `-r' is specified (or is on by default), project's main
feature will be required before evaluating, so that you don't
have to include `(require ...)' yourself.

This is basically like `eval' command, with the only difference
being that it doesn't print form results."
  :parameters     "FORM..."
  :aliases        execute
  (eldev--do-eval nil parameters))

(defun eldev--do-eval (print-results parameters)
  (unless parameters
    (signal 'eldev-wrong-command-usage `(t ,(if print-results "Missing expressions to evaluate" "Missing forms to execute"))))
  (let ((forms (mapcar (lambda (parameter) (cons parameter (eldev-read-wholly parameter (if print-results "expression" "form to evaluate")))) parameters)))
    (eldev-load-project-dependencies (if print-results 'eval 'exec))
    (when eldev-eval-require-main-feature
      (dolist (feature (eldev-required-features eldev-eval-required-features))
        (eldev-verbose "Autorequiring feature `%s' before %s" feature (if print-results "evaluating" "executing"))
        (require feature)))
    (dolist (form forms)
      (eldev-verbose (if print-results "Evaluating expression `%s':" "Executing form `%s'...") (car form))
      (let ((result (eval (cdr form) eldev-eval-lexical)))
        (when print-results
          (with-temp-buffer
            (funcall (or eldev-eval-printer-function #'prin1) result (current-buffer))
            ;; Older Emacs version end some value representations with a linefeed for
            ;; whatever reasons.
            (when (equal (char-before) ?\n)
              (delete-char -1))
            (eldev-output "%s" (buffer-string))))))))

(eldev-defbooloptions eldev-eval-lexical-mode eldev-eval-dynamic-mode eldev-eval-lexical
  ("Evaluate expressions using lexical scoping"
   :options       (-l --lexical))
  ("Use dynamic (i.e. not lexical) scoping when evaluating"
   :options       (-d --dynamic --not-lexical))
  :for-command    (eval exec))

(eldev-defbooloptions eldev-eval-require-main-feature eldev-eval-dont-require-main-feature eldev-eval-require-main-feature
  ("Require project's main feature before evaluating"
   :options       (-r --require))
  ("Don't require project's features before evaluating"
   :options       (-R --dont-require))
  :for-command    (eval exec))

(eldev-defoption eldev-eval-printer (function)
  "Use given function to print results"
  :options        (-p --printer)
  :for-command    eval
  :value          FUNCTION
  :default-value  (or eldev-eval-printer-function #'prin1)
  (let ((function (eldev-read-wholly function "printer function")))
    (unless (fboundp function)
      (signal 'eldev-wrong-option-usage `("undefined function `%s'" ,function)))
    (setf eldev-eval-printer-function function)))

(eldev-defoption eldev-eval-use-standard-printer ()
  "Use `prin1' (i.e. standard built-in) to print results"
  :options        (-s --standard-printer)
  :for-command    eval
  (setf eldev-eval-printer-function #'prin1))

(declare-function cl-prin1 "cl-print" (object &optional stream))

(defun eldev-prin1 (object &optional stream)
  "Print an object similarly to `prin1'.
Current implementation uses `cl-prin1' if available (Emacs 26 and
later), or else just calls `prin1'.

This may be changed in later Eldev versions: this function is
supposed to provide output useful for humans, not fulfil strict
obligations."
  (if (require 'cl-print nil t)
      (cl-prin1 object stream)
    (pp object stream)))

(defun eldev-required-features (feature-names)
  "Return a list of features to autorequire."
  (mapcar (lambda (feature)
            (if (eq feature :project-name) (package-desc-name (eldev-package-descriptor)) feature))
          (eldev-flatten-tree (mapcar (lambda (feature)
                                        (if (eq feature :default) eldev-default-required-features feature))
                                      (eldev-listify feature-names)))))



;; eldev emacs

(defvar eldev-emacs-default-command-line '("--no-init-file" "--no-site-file" "--no-site-lisp" "--no-splash")
  "Options added to Emacs command line by default.")

(defvar eldev-emacs-required-features :default
  "Features that spawned Emacs processes require automatically.
See `eldev-default-required-features' for value description.
Special value `:default' means to use that variable's value
instead.")

(eldev-defcommand eldev-emacs (&rest parameters)
  "Launch Emacs in a prepared environment.  Emacs will be able to
load the project and all its dependencies.

This command parses contents of command line specially.
Normally, contents of command line used to launch Emacs looks
like this:

  - options from `eldev-emacs-default-command-line' (unless
    changed in file `Eldev' or elsewhere, these disable all
    configuration files and the splash screen);

  - Elisp `require' forms to load `eldev-emacs-required-features'
    (which default to project name);

  - COMMAND-LINE as passed to this command.

However, if COMMAND-LINE has `--' at the first position, Eldev
passes the rest of it to Emacs without adding anything on its
own.  Be advised that in this case you will likely need to
specify at least `-q' (`--no-init-file') option to be passed to
Emacs, else it will most likely fail."
  :parameters     "[--] [COMMAND-LINE...]"
  :custom-parsing t
  (eldev-load-project-dependencies 'emacs)
  (let* ((command-line        (if (string= (car parameters) "--")
                                  (cdr parameters)
                                (append eldev-emacs-default-command-line
                                        (apply #'append (mapcar (lambda (feature) (list "--eval" (format "(require '%s)" feature)))
                                                                (eldev-required-features eldev-emacs-required-features)))
                                        parameters)))
         (effective-load-path (mapconcat #'identity load-path path-separator))
         (process-environment `(,(format "EMACSLOADPATH=%s" effective-load-path) ,@process-environment)))
    (eldev-verbose "Full command line to run child Emacs process:\n  %s" (eldev-message-command-line eldev-emacs-executable command-line))
    (eldev-verbose "Effective load path for it:\n  %s" effective-load-path)
    (eldev-call-process eldev-emacs-executable command-line
      (eldev--forward-process-output "Output of the child Emacs process:" "Child Emacs process produced no output")
      (unless (= exit-code 0)
        (signal 'eldev-error `("Child Emacs process exited with error code %d" ,exit-code))))))



;; eldev targets, eldev build, eldev compile, eldev package

(defvar eldev-build-system-hook nil
  "Hook executed whenever build system is used.")

(defvar eldev--builders nil)
(defvar eldev--build-targets (make-hash-table :test #'equal))
(defvar eldev--targets-prepared-for nil)

(defvar eldev-build-sets nil
  "Target sets to build.
If left nil, only set `main' is built.")

(defvar eldev-build-load-before-byte-compiling nil
  "Whether to load `.el' files before byte-compiling them.

Ideally, files should be byte-compilable without loading them
first.  This might require some careful application of
`eval-and-compile' and similar forms, especially when complicated
macros are involved, but is achievable.  Such compilation without
loading first is also somewhat faster.

However, since Emacs packaging system always loads before
byte-compiling, many projects are not prepared for this.  You can
set this to non-nil to avoid warnings and errors you will not
typically see anyway.")

(defvar eldev-build-keep-going nil
  "Whether to continue even if building a target fails.")

(defvar eldev-build-treat-warnings-as-errors nil
  "Whether to treat all building warnings as errors.")

(defvar eldev-build-suppress-warnings nil
  "Whether to ignore all building warnings (but not errors).")

(defvar eldev-build-force-rebuilding nil
  "Targets that are to be rebuilt even if they appear up-to-date.
Can be either a list of target names or symbol t, in which case
all targets will be force-built.  However, a target will only
ever be force-built if it is otherwise a part of building plan.")

(defvar eldev-build-infinitely-new nil
  "Files (sources or intermediate targets) that are “infinitely new”.
Can be either a list of filenames or symbol t, meaning “all of
them”.  Targets that are built from these sources or depend on
them will never be found up-to-date.")

(defvar eldev-build-dry-run-mode nil
  "If non-nil, don't build anything, just pretend to do so.")

(defvar eldev-build-current-targets nil
  "When building, bound to list of targets being built now.")

(defvar eldev-package-generate-entry-file nil
  "If non-nil, generate an entry file for package archives.")

(defvar eldev-package-forced-version nil
  "Use given version instead of project's self-reported version.")

(defvar eldev-package-output-dir nil
  "Put generated packages to given directory.
If left unspecified, value of function `eldev-dist-dir' is used.")

(defvar eldev-package-print-filename nil
  "Print resulting filename after generating a package.
This is mostly for interfacing with other tools.")

(defvar eldev--build-plan nil)
(defvar eldev--build-results nil)

(defvar eldev-targets-list-dependencies nil
  "Whether to print known target dependencies.")

(defvar eldev--target-dependencies nil)
(defvar eldev--target-dependencies-need-saving nil)

;; FIXME: Maybe find a better way?
(defvar eldev--package-target-file nil)
(defvar eldev--package-target-generated nil)


(defsubst eldev-virtual-target-p (target)
  (string-prefix-p ":" target))

(defun eldev-build-find-targets (&rest standard-filesets)
  "Return a hash table of all build targets in given filesets.
STANDARD-FILESETS must be a list of names from `eldev-filesets'
variable or `all'.  The hash table might contain targets from
other filesets too.  Caller must not modify contents of the
returned hash table.

Hash table keys are strings, i.e. names of targets.  Entries,
each describing a builder invocations, are alists with at least
the following keys, in no particular order:

  - builder: name (symbol) of the builder that can generate this
    target, see macro `eldev-defbuilder';

  - sources: list of files or other targets that serve as sources
    for this one; each entry must be a file or a real target, not
    virtual;

  - targets: all targets that will be build by this invocation;
    will include at least the target used as the key, but may
    include more (depends on the builder).

Returned hash table doesn't contain cross-target dependency
information past the list of sources.  For this, use function
`eldev-get-target-dependencies'."
  (when (or (null standard-filesets) (memq 'all standard-filesets))
    (setf standard-filesets (mapcar #'car eldev-filesets)))
  (let ((new-filesets (eldev-filter (not (memq it eldev--targets-prepared-for)) standard-filesets)))
    (when new-filesets
      (setf new-filesets (nreverse new-filesets))
      (eldev-trace "Generating build target list for %s" (eldev-message-enumerate "standard fileset" new-filesets))
      ;; FIXME: The point of special-casing `package' builder is to let it know all the
      ;;        sources at once, because its target depends on the number of sources
      ;;        (`.el' if one file, `.tar' otherwise).  This is of course a hack, but it
      ;;        is not clear how to generalize that.  Postponed until some real-world
      ;;        needs come up.
      (let* ((special-cased-builder (assq 'package eldev--builders))
             (reordered-builders    (if special-cased-builder
                                        (append (remq special-cased-builder eldev--builders) (list special-cased-builder))
                                      eldev--builders))
             (potential-new-sources (eldev-find-files (apply #'eldev-standard-filesets new-filesets)))
             (visited-sources       (make-hash-table :test #'equal))
             special-cased-builder-sources
             new-sources)
        (while (setf new-sources (eldev-filter-files (eldev-filter (not (gethash it visited-sources)) potential-new-sources)
                                                     `(:not ,eldev-standard-excludes)))
          (dolist (source new-sources)
            (puthash source t visited-sources))
          (setf potential-new-sources nil)
          (dolist (entry reordered-builders)
            (let* ((builder-name (car entry))
                   (builder      (cdr entry))
                   (source-files (eldev-get builder :source-files)))
              (when source-files
                (setf source-files (eldev-filter-files new-sources source-files)))
              (when (eq entry special-cased-builder)
                (setf source-files (append special-cased-builder-sources source-files)))
              (when source-files
                (if (eq entry special-cased-builder)
                    (setf special-cased-builder-sources source-files)
                  (dolist (invocation (eldev--build-find-builder-invocations source-files builder builder-name))
                    (let ((sources (car invocation))
                          (targets (cdr invocation)))
                      (eldev--build-target-entries targets builder builder-name sources)
                      (setf potential-new-sources (append targets potential-new-sources)))))))))
        (when special-cased-builder-sources
          (let ((builder-name (car special-cased-builder))
                (builder      (cdr special-cased-builder)))
            (dolist (invocation (eldev--build-find-builder-invocations special-cased-builder-sources builder builder-name))
              (eldev--build-target-entries (cdr invocation) builder builder-name (car invocation))))))))
  (unless (gethash ":default" eldev--build-targets)
    (eldev--build-target-entries '(":default") nil nil nil))
  eldev--build-targets)

(defun eldev--build-find-builder-invocations (sources builder builder-name)
  (let ((target-rule (eldev-get builder :targets)))
    (when (functionp target-rule)
      (setf target-rule (funcall target-rule sources)))
    (pcase target-rule
      ((pred stringp)
       `((,sources . (,target-rule))))
      ((pred eldev-string-list-p)
       `((,sources . ,target-rule)))
      (`(,(and (or (pred stringp) (pred eldev-string-list-p)) source-suffixes) -> ,(and (pred stringp) target-suffix))
       (setf source-suffixes (eldev-listify source-suffixes))
       (let (result)
         (dolist (source sources)
           (let ((scan source-suffixes)
                 found)
             (while scan
               (let ((source-suffix (pop scan)))
                 (when (string-suffix-p source-suffix source)
                   (push `((,source) . (,(eldev-replace-suffix source source-suffix target-suffix))) result)
                   (setf scan  nil
                         found t))))
             (unless found
               (error "Builder `%s': name of source file `%s' doesn't end with %s as `:targets' wants"
                      builder-name source (eldev-message-enumerate nil source-suffixes nil nil "or")))))
         (nreverse result)))
      (_ (error "Invalid `:targets' (or its return value) %S in builder `%s'" target-rule builder-name)))))

(defun eldev--build-target-entries (targets builder builder-name sources)
  (let ((sorted-targets (sort (copy-sequence targets) #'string<))
        shared-entry
        created-new-entry)
    (dolist (target targets)
      (if (eldev-virtual-target-p target)
          (when builder-name
            (error "Virtual targets (e.g. `%s') must not have builders" target))
        (unless builder-name
          (error "Real targets (e.g. `%s') must have associated builders" target)))
      (let ((entry (gethash target eldev--build-targets)))
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
                         (eldev-message-enumerate nil (cdr (assq 'targets entry)) nil nil t) target (eldev-message-enumerate nil targets nil nil t))))
            (setf entry             `((builder        . ,builder-name)
                                      (targets        . ,targets)
                                      (sorted-targets . ,sorted-targets))
                  created-new-entry t))
          (let* ((existing-sources (cdr (assq 'sources entry)))
                 (new-sources      (if existing-sources (eldev-filter (not (member it existing-sources)) sources) sources)))
            (eldev--assq-set 'sources (append existing-sources new-sources) entry))
          (setf shared-entry entry))
        (puthash target entry eldev--build-targets))))
  (when builder
    (eldev--build-collect-targets targets builder)))

(defun eldev--build-collect-targets (targets builder)
  (let ((collect (eldev-get builder :collect)))
    (when (functionp collect)
      (setf collect (funcall collect targets)))
    (dolist (entry (cond ((or (stringp collect) (eldev-string-list-p collect))
                          `((,targets ,collect)))
                         ((eldev-all-p (pcase it (`(,(or (pred stringp) (pred eldev-string-list-p))
                                                    ,(or (pred stringp) (pred eldev-string-list-p)))
                                                  t))
                                       collect)
                          collect)))
      (let ((entry-targets (eldev-listify (car entry))))
        (dolist (virtual-target (reverse (eldev-listify (cadr entry))))
          (unless (eldev-virtual-target-p virtual-target)
            (error "Expected a virtual target, `%s' isn't" virtual-target))
          (eldev--build-target-entries (list virtual-target) nil nil entry-targets)
          (setf entry-targets (list virtual-target)))))))


(defun eldev-get-target-dependencies (target)
  "Get TARGET's dependencies.
Usually, this function should be of no interest: custom builders
should normally use only `eldev-set-target-dependencies'.
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
`eldev-set-target-dependencies' when needed.

This function may only be called while inside the body of a
`eldev-with-target-dependencies' macro."
  (gethash target eldev--target-dependencies))

(defun eldev-set-target-dependencies (target dependencies)
  "Set the list of TARGET's dependencies.
See documentation of `eldev-get-target-dependencies' for list's
elements description.

Evaluates to non-nil if dependencies are changed, to nil if they
are exactly the same as before (possibly in different order).

This function may only be called while inside the body of a
`eldev-with-target-dependencies' macro."
  (let ((current-dependencies (eldev-get-target-dependencies dependencies)))
    (unless (equal (sort (copy-sequence dependencies)         (lambda (a b) (string< (car a) (car b))))
                   (sort (copy-sequence current-dependencies) (lambda (a b) (string< (car a) (car b)))))
      (puthash target (copy-sequence dependencies) eldev--target-dependencies)
      (setf eldev--target-dependencies-need-saving t))))

(defmacro eldev-with-target-dependencies (&rest body)
  "Execute BODY with target dependency mechanism set up."
  (declare (indent 0) (debug (body)))
  `(progn (eldev--load-target-dependencies)
          (unwind-protect
              (progn ,@body)
            (eldev--save-target-dependencies))))

(defun eldev--load-target-dependencies ()
  (eldev-do-load-cache-file (expand-file-name "target-dependencies.build" (eldev-cache-dir t)) "target dependencies" 1
    (setf eldev--target-dependencies (cdr (assq 'dependencies contents))))
  (unless eldev--target-dependencies
    (setf eldev--target-dependencies (make-hash-table :test #'equal))))

(defun eldev--save-target-dependencies ()
  (if eldev--target-dependencies-need-saving
      (eldev-do-save-cache-file (expand-file-name "target-dependencies.build" (eldev-cache-dir t)) "target dependencies" 1
        `((dependencies . ,eldev--target-dependencies)))
    (eldev-trace "Target dependency information is up-to-date, not saving...")))


;; Internal helper for `eldev-defbuilder'.
(defun eldev--register-builder (builder name keywords)
  (let (cleaner-specifications)
    (while keywords
      (eldev-pcase-exhaustive (pop keywords)
        ((and (or :type :short-name :message :source-files :targets :collect :briefdoc) keyword)
         (eldev-put builder keyword (pop keywords)))
        (:define-cleaner
         (push (pop keywords) cleaner-specifications))))
    (eldev--assq-set name builder eldev--builders)
    (dolist (specification cleaner-specifications)
      (eval `(eldev-defcleaner ,(car specification) ()
               ,@(cdr specification)
               (let (targets)
                 (dolist (entry (eldev--build-find-builder-invocations (eldev-find-files `(:and (eldev-get ',',builder :source-files) (eldev-clean-fileset)))
                                                                       ',builder ',name))
                   (setf targets (append targets (cdr entry))))
                 targets))))))


(defmacro eldev-defbuilder (name arguments &rest body)
  "Define a target builder."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body  (eldev-macroexp-parse-body body))
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
            (eldev--register-builder ',name ',builder-name ',(nreverse keywords)))))


(eldev-defcommand eldev-targets (&rest parameters)
  "Show tree of building targets in the project.  Out of the box
Eldev defines rules to byte-compile `.el' files, generate `.info'
from `.texi' files and build Elisp packages.  These are combined
into `:compile', `:package' and `:package-archive-entry' virtual
targets.  Each project can additionally define more targets in
its `Eldev' file.

Every project will also have virtual target `:default', possibly
empty.

In case `--dependencies' option is specified, lines starting with
[...] show target dependencies:

  [dep]: hard dependency: this must be built before the main
         target;

  [inh]: main target inherits dependencies and sources (which
         also become dependencies) from the one it depends on.

However, dependencies are listed only if they are known.  Usually
this means that you need to compile the project once — then Eldev
will know changes in which `.el' files might require
recompilation of seemingly unrelated `.elc'."
  :parameters     "[TARGET-SET...]"
  :aliases        target-tree
  (let ((all-targets (if parameters
                         (apply #'eldev-build-find-targets (mapcar #'eldev-validate-standard-fileset parameters))
                       (eldev-build-find-targets 'main))))
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
          (eldev-with-target-dependencies
            ;; This is done only to detect cyclic dependencies.
            (let ((eldev--build-plan (make-hash-table :test #'equal)))
              (maphash (lambda (target _entry) (eldev--build-add-to-plan target all-targets)) all-targets))
            (eldev--do-targets toplevel 0 all-targets (make-hash-table :test #'eq))))
      (eldev-print "There are no targets for %s" (if (> (length parameters) 1) "these filesets" "this fileset")))))

(eldev-defbooloptions eldev-targets-list-dependencies eldev-targets-omit-dependencies eldev-targets-list-dependencies
  ("List known target dependencies"
   :options       (-d --dependencies))
  ("Don't show target dependencies, even if known"
   :options       (-D --no-dependencies))
  :for-command    targets)


(defun eldev--do-targets (level-targets level all-targets printed-target-entries)
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
               (target-name (eldev-colorize target (cond ((eldev-virtual-target-p target) 'section) (sources 'name)) target)))
          (cond ((and repeated builder)
                 (eldev-output "%s%s  [%s]" indentation target-name
                               (eldev-colorize (if (equal repeated target) "repeated, see above" (eldev-format-message "repeated, see `%s' above" repeated)) 'details)))
                ((and builder (eldev-unless-quiet t))
                 (eldev-output "%s%s  [%s]" indentation target-name (or (eldev-get (cdr (assq builder eldev--builders)) :short-name) builder)))
                (t
                 (eldev-output "%s%s" indentation target-name)))
          (unless repeated
            (puthash entry target printed-target-entries)
            (eldev--do-targets sources (1+ level) all-targets printed-target-entries)
            (when eldev-targets-list-dependencies
              (dolist (dependency (sort (copy-sequence (eldev-get-target-dependencies target)) (lambda (a b) (string< (car a) (car b)))))
                (eldev-output "%s    %s %s" indentation
                              (eldev-colorize (eldev-pcase-exhaustive (car dependency)
                                                (`depends-on "[dep]")
                                                (`inherits   "[inh]"))
                                              'details)
                              (cadr dependency))))))))))


(eldev-defcommand eldev-build (&rest parameters)
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

Out of the box Eldev defines rules to byte-compile `.el' files,
generate `.info' from `.texi' files and build Elisp packages.
However, each project can define more targets in its `Eldev'
file.

Also see commands `compile' and `package'."
  :parameters     "[TARGET...]"
  ;; When building, project loading mode is ignored.  The reason is that building itself
  ;; can involve compiling or packaging.
  (run-hooks 'eldev-build-system-hook)
  (let ((eldev-project-loading-mode 'as-is))
    (eldev-load-project-dependencies 'build))
  (let ((all-targets (apply #'eldev-build-find-targets (or eldev-build-sets '(main))))
        target-list
        target-fileset
        virtual-targets)
    (if parameters
        (dolist (parameter parameters)
          (push parameter (if (eldev-virtual-target-p parameter) virtual-targets target-fileset)))
      (setf virtual-targets '(":default")))
    (dolist (target (setf virtual-targets (nreverse virtual-targets)))
      (unless (gethash target all-targets)
        (signal 'eldev-error `("Unknown virtual target `%s'" ,target))))
    (when target-fileset
      (maphash (lambda (target _entry) (push target target-list)) all-targets)
      (setf target-list (eldev-filter-files target-list (nreverse target-fileset))))
    (eldev-with-target-dependencies
      (let ((eldev--build-plan    (make-hash-table :test #'equal))
            (eldev--build-results (make-hash-table :test #'equal))
            build-sequence
            anything-failed)
        ;; We create a plan before even starting to build everything, so that cyclic
        ;; dependencies can be detected earlier.
        (dolist (target (nconc virtual-targets target-list))
          (eldev--build-add-to-plan target all-targets))
        (maphash (lambda (target order) (when (numberp order) (push (cons target order) build-sequence))) eldev--build-plan)
        ;; FIXME: Really could add nicety by sorting this more intelligently so that
        ;;        e.g. targets go alphabetically and the same builder is used sequentially
        ;;        where this doesn't break dependency ordering.
        (when build-sequence
          (setf build-sequence (sort build-sequence (lambda (a b) (< (cdr a) (cdr b)))))
          (eldev-trace "Building plan: %s" (eldev-message-enumerate nil build-sequence #'car nil t))
          (dolist (entry build-sequence)
            (if eldev-build-keep-going
                ;; Ignore errors here: they will have been reported in `eldev-build-target'
                ;; already.
                (condition-case nil
                    (eldev-build-target (car entry))
                  (eldev-build-abort-branch))
              (eldev-build-target (car entry)))))
        (when (= (hash-table-count eldev--build-results) 0)
          (eldev-print "Nothing to do"))
        (maphash (lambda (_target status) (unless (eq status 'built) (setf anything-failed t))) eldev--build-results)
        (when anything-failed
          (signal 'eldev-error `("Build failed")))))))

(eldev-defcommand eldev-compile (&rest parameters)
  "Byte-compile `.el' files.

By default, all `.el' files are compiled.  But you can also
specify SOURCEs, which should be a fileset.

This is basically a different way of invoking `build' command.
However, instead of targets, here you specify sources."
  :parameters     "[SOURCE...]"
  :aliases        byte-compile
  (if parameters
      (let (elc-files)
        (dolist (el-file (eldev-find-files `(:and ,(apply #'eldev-standard-filesets :or (or eldev-build-sets '(main))) "*.el" ,parameters)))
          (push (concat el-file "c") elc-files))
        (apply #'eldev-build (nreverse elc-files)))
    (eldev-build ":compile")))

(eldev-defcommand eldev-package (&rest parameters)
  "Build Elisp package tarball or a single-file package.  This is
basically an alias for `build :package' or, if `--entry-file' is
specified, `build :package :package-archive-entry', but with
additional optional features.

Option `--force-version' allows you to override the project's
self-reported version."
  :aliases        pack
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (if eldev-package-generate-entry-file
      (eldev-build ":package" ":package-archive-entry")
    (eldev-build ":package"))
  (when eldev-package-print-filename
    (eldev-output "%s" (expand-file-name eldev--package-target-file eldev-project-dir))
    (eldev-output "%s" (if eldev--package-target-generated "generated" "up-to-date"))))

(eldev-defoption eldev-build-set (name)
  "Build targets from this set; special set name `all' can be
used"
  :options        (-s --set)
  :for-command    (build compile)
  :value          NAME
  :default-value  (eldev-message-enumerate nil (or eldev-build-sets '(main)) nil t)
  (setf eldev-build-sets (append eldev-build-sets (list (eldev-validate-standard-fileset name)))))

(eldev-defbooloptions eldev-build-load-before-byte-compiling eldev-build-byte-compile-straight-away eldev-build-load-before-byte-compiling
  ("Load `.el' files before byte-compiling them; this is how Emacs packaging system behaves"
   :options       (-l --load-before-compiling))
  ("Byte-compile `.el' without loading them first; this might require adding some `eval-and-compile' forms in your code"
   :options       (-L --dont-load-before-compiling --byte-compile-straight-away))
  :for-command    (build compile package))

(eldev-defbooloptions eldev-build-keep-going eldev-build-stop-on-failure eldev-build-keep-going
  ("Keep building even if a target failed"
   :options       (-k --keep-going))
  ("Stop building after the first failure"
   :options       (-S --stop --stop-on-failure --no-keep-going))
  :for-command    (build compile package))

(eldev-defbooloptions eldev-build-treat-warnings-as-errors eldev-build-dont-treat-warnings-as-errors eldev-build-treat-warnings-as-errors
  ("Treat all warnings as errors"
   :options       (-W --warnings-as-errors))
  ("Don't treat warnings as errors"
   :options       (--warnings-as-warnings --no-warnings-as-errors)
   :hidden-if     :default)
  :for-command    (build compile package))

(eldev-defbooloptions eldev-build-suppress-warnings eldev-build-show-warnings eldev-build-suppress-warnings
  ("Suppress all warnings"
   :options       (-w --suppress-warnings))
  ("Show warnings during building"
   :options       --show-warnings
   :hidden-if     :default)
  :for-command    (build compile package))

(eldev-defoption eldev-build-force-rebuilding (&optional target)
  "Force building of TARGET even if it appears up-to-date (or all
targets if TARGET is not specified)"
  :options        (-f --force)
  :optional-value TARGET
  :for-command    (build compile package)
  (unless (eq eldev-build-force-rebuilding t)
    (if target
        (push target eldev-build-force-rebuilding)
      (setf eldev-build-force-rebuilding t))))

(eldev-defoption eldev-build-infinitely-new (&optional file)
  "Consider FILE “infinitely new” (or all files if FILE is not
specified)"
  :options        (-N --new)
  :optional-value FILE
  :for-command    (build compile package)
  (unless (eq eldev-build-infinitely-new t)
    (if file
        (push file eldev-build-infinitely-new)
      (setf eldev-build-infinitely-new t))))

(eldev-defbooloptions eldev-package-generate-entry-file eldev-package-no-entry-file eldev-package-generate-entry-file
  ("Generate an entry for package archives"
   :options       (-e --entry-file))
  ("Don't generate an entry file"
   :options       (-E --no-entry-file)
   :hidden-if     :default)
  :for-command    package)

(eldev-defoption eldev-package-force-version (version)
  "Force given package VERSION instead of what source code
declares"
  :options        (-V --force-version)
  :value          VERSION
  :for-command    package
  (setf eldev-package-forced-version (version-to-list version)))

(eldev-defoption eldev-package-output-dir (dir)
  "Generate package in given directory instead of `dist'"
  :options        --output-dir
  :value          DIR
  :for-command    package
  (setf eldev-package-output-dir dir))

(eldev-defbooloptions eldev-package-print-filename eldev-package-dont-print-filename eldev-package-print-filename
  ("Print absolute package filename and word “generated” or “up-to-date” as two last lines of output"
   :options       --print-filename)
  ("Don't add special ouput"
   :options       --dont-print-filename
   :hidden-if     :default)
  :for-command    package)

(eldev-defbooloptions eldev-build-dry-run-mode eldev-build-do-build-mode eldev-build-dry-run-mode
  ("Don't actually build, just print what would be performed"
   :options       (-n --dry-run))
  ("Do build as requested"
   :options       --do-build
   :hidden-if     :default)
  :for-command    (build compile package))


(defun eldev--need-to-build (target source)
  "Determine if we need to build a non-virtual TARGET because of SOURCE."
  (unless (eldev-virtual-target-p source)
    (or (eq eldev-build-infinitely-new t)
        (member source eldev-build-infinitely-new)
        (file-newer-than-file-p source target))))

(defun eldev--need-to-build-full (target dependency all-targets &optional dependency-stack)
  (let ((cycle (member dependency dependency-stack)))
    (when cycle
      (signal 'eldev-error `("%s form a dependency cycle" ,(eldev-message-enumerate "Target" (reverse dependency-stack))))))
  (let ((entry (gethash dependency all-targets)))
    (when entry
      (push dependency dependency-stack)
      (or (eldev-any-p (eldev--need-to-build target it) (cdr (assq 'sources entry)))
          (eldev-any-p (eldev-pcase-exhaustive (car it)
                         (`depends-on (eldev--need-to-build target (cadr it)))
                         (`inherits   (eldev--need-to-build-full target (cadr it) all-targets dependency-stack)))
                       (eldev-get-target-dependencies dependency))))))

(defun eldev--build-add-to-plan (target all-targets &optional dependency-stack)
  (let ((already-planned (gethash target eldev--build-plan)))
    (when (eq already-planned 'side-effect)
      (puthash target 'planned-side-effect eldev--build-plan))
    (or already-planned
        (let ((cycle (member target dependency-stack)))
          (when cycle
            (signal 'eldev-error `("%s form a dependency cycle" ,(eldev-message-enumerate "Target" (reverse dependency-stack))))))
        (let ((entry         (gethash target all-targets))
              (virtual       (eldev-virtual-target-p target))
              (need-to-build (or (eq eldev-build-force-rebuilding t)
                                 (member target eldev-build-force-rebuilding))))
          (when entry
            (push target dependency-stack)
            (dolist (source (cdr (assq 'sources entry)))
              (when (or (eldev--build-add-to-plan source all-targets dependency-stack)
                        (unless virtual
                          (eldev--need-to-build target source)))
                (setf need-to-build t)))
            (dolist (dependency-entry (eldev-get-target-dependencies target))
              (let ((dependency (cadr dependency-entry)))
                (eldev-pcase-exhaustive (car dependency-entry)
                  (`depends-on (when (or (eldev--build-add-to-plan dependency all-targets dependency-stack)
                                         (unless virtual
                                           (eldev--need-to-build target dependency)))
                                 (setf need-to-build t)))
                  (`inherits   (when (eldev--need-to-build-full target dependency all-targets dependency-stack)
                                 (setf need-to-build t))))))
            ;; For the main target it will be overwritten.
            (dolist (other-target (cdr (assq 'targets entry)))
              (puthash other-target 'side-effect eldev--build-plan))
            (puthash target (when need-to-build (hash-table-count eldev--build-plan)) eldev--build-plan))))))

(defun eldev-build-target-status (target)
  "Returns TARGET building result.
Return value is one of the following symbols:

  - planned
  - building
  - built
  - failed
  - not-planned"
  (or (gethash target eldev--build-results)
      ;; Both numbers and symbol `planned-side-effect' mean that the target is planned for
      ;; building.
      (if (memq (gethash target eldev--build-plan) '(nil side-effect)) 'not-planned 'planned)))

(defun eldev-build-target (target)
  "Build given TARGET.
The TARGET must be previously planned for building: it is not
possible to build arbitrary targets this way."
  (eldev-pcase-exhaustive (eldev-build-target-status target)
    (`building    (error "Trying to build `%s' recursively" target))
    (`built       (eldev-trace "Not trying to build `%s' again" target))
    (`failed      (error "Building `%s' failed earlier" target))
    (`not-planned (error "Building `%s' was never planned" target))
    (`planned
     ;; Reset `default-directory', because it can have been changed e.g. when called from
     ;; inside byte-compilation.
     (let* ((default-directory eldev-project-dir)
            (entry             (gethash target eldev--build-targets))
            (builder-name      (cdr (assq 'builder entry)))
            (targets           (cdr (assq 'targets entry)))
            succeeded)
       (puthash target 'building eldev--build-results)
       (unwind-protect
           (if builder-name
               (let* ((builder       (cdr (assq builder-name eldev--builders)))
                      (builder-type  (or (eldev-get builder :type) 'one-to-one))
                      (sources       (cdr (assq 'sources entry)))
                      ;; Normally such things will not be in the plan to begin with, but
                      ;; also take into account situations where builders don't update
                      ;; their target for whatever reason, e.g. when they can detect that
                      ;; it hasn't changed.
                      (need-to-build (or (eq eldev-build-force-rebuilding t)
                                         (member target eldev-build-force-rebuilding)
                                         (eldev--need-to-build-full target target eldev--build-targets))))
                 (if need-to-build
                     (let* ((eldev-build-current-targets targets)
                            (source-argument             (if (memq builder-type '(one-to-one one-to-many)) (car sources) sources))
                            (target-argument             (if (memq builder-type '(one-to-one many-to-one)) (car targets) targets)))
                       (eldev-unless-quiet
                         (let* ((short-name    (or (eldev-get builder :short-name) builder-name))
                                (message       (or (eldev-get builder :message) 'sources))
                                (source-string (if (cddr sources)
                                                   (if (eldev-when-verbose t)
                                                       (eldev-message-enumerate nil sources (lambda (source) (eldev-colorize source 'name)) t)
                                                     (eldev-format-message "%d files" (length sources)))
                                                 (eldev-colorize (car sources) 'name)))
                                (target-string (if (cddr targets)
                                                   (if (eldev-when-verbose t)
                                                       (eldev-message-enumerate nil targets (lambda (target) (eldev-colorize target 'name)) t)
                                                     (eldev-format-message "%d files" (length targets)))
                                                 (eldev-colorize (car targets) 'name))))
                           (pcase message
                             ((or `source `sources)
                              (eldev-output "%-8s %s" short-name source-string))
                             ((or `target `targets)
                              (eldev-output "%-8s -> %s" short-name target-string))
                             ((or `source-and-target `sources-and-target `source-and-targets `sources-and-targets)
                              (eldev-output "%-8s %s -> %s" short-name source-string target-string))
                             (_
                              (eldev-output "%-8s %s" short-name (funcall message sources targets))))))
                       (unless eldev-build-dry-run-mode
                         (if eldev-build-keep-going
                             (condition-case error
                                 (funcall builder source-argument target-argument)
                               (eldev-build-abort-branch
                                (signal (car error) (cdr error)))
                               (eldev-error
                                (eldev-error "While building `%s': %s" target (apply #'eldev-format-message (cdr error)))
                                (signal 'eldev-build-abort-branch nil))
                               (error
                                (eldev-error "While building `%s': %s" target (error-message-string error))
                                (signal 'eldev-build-abort-branch nil)))
                           (funcall builder source-argument target-argument)))
                       (setf succeeded t))
                   (setf succeeded t)
                   (eldev-verbose "Not building target `%s': it is up-to-date" target)))
             (setf succeeded t)
             (eldev-verbose "Done building “sources” for virtual target `%s'" target))
         (dolist (target targets)
           (puthash target (if succeeded 'built 'failed) eldev--build-results)))))))


(defvar eldev--recursive-byte-compilation nil)
(defvar eldev--feature-providers (make-hash-table :test #'eq))

(eldev-defbuilder eldev-builder-byte-compile-.el (source target)
  :short-name     "ELC"
  :source-files   "*.el"
  ;; We intentionally don't use `byte-compile-dest-file'.  If you need to customize this
  ;; somehow, define a new builder instead.
  :targets        (".el" -> ".elc")
  :collect        ":compile"
  :define-cleaner (eldev-cleaner-.elc
                   "Delete `.elc' files, i.e. results of byte-compilation."
                   :aliases (elc dot-elc)
                   :default t)
  ;; The following complications are mostly to speed up byte-compilation, with result
  ;; noticeable on larger projects with heavy cross-dependencies and many macros.  The
  ;; idea is to byte-compile `require'd files first so that the outer file uses already
  ;; byte-compiled and thus faster definitions.
  (require 'bytecomp)
  (let* ((recursive                         eldev--recursive-byte-compilation)
         (eldev--recursive-byte-compilation t)
         (load-prefer-newer                 t)
         ;; When called recursively, let `byte-compile-file' determine this.
         (skip-byte-compilation             (unless recursive
                                              (with-temp-buffer
                                                (insert-file-contents source)
                                                ;; Older versions don't understand `no-mode'.
                                                (hack-local-variables (when (>= emacs-major-version 26) 'no-mode))))))
    ;; Don't do anything with `no-byte-compile' files (not even load) unless called
    ;; recursively.  Otherwise we might e.g. attempt loading `define-package' and fail.
    (unless skip-byte-compilation
      (eldev-verbose (if recursive "Byte-compiling file `%s' early as `require'd from another file..." "Byte-compiling file `%s'...")
                     source)
      (eldev-advised ('load :before
                            (unless recursive
                              (lambda (file &rest _ignored)
                                (eldev--trigger-early-byte-compilation file))))
        ;; The advice for `load' is, unfortunately, not enough since `require' calls
        ;; C-level function directly, bypassing advice machinery.
        (eldev-advised ('require :before
                                 (unless recursive
                                   (lambda (feature &optional filename &rest _ignored)
                                     (eldev--trigger-early-byte-compilation (or filename (eldev--find-feature-provider feature))))))
          (let* (result
                 (failed-source
                  (catch 'eldev--byte-compilation-failed
                    ;; Must be within the `catch', because it can trigger byte-compilation
                    ;; of other targets.
                    (when eldev-build-load-before-byte-compiling
                      (eldev-trace "Loading file `%s' before byte-compiling it..." source)
                      (load source nil t t))
                    (setf result (if skip-byte-compilation
                                     'no-byte-compile
                                   (let* ((byte-compile-error-on-warn        eldev-build-treat-warnings-as-errors)
                                          (have-warning-function             (boundp 'byte-compile-log-warning-function))
                                          (byte-compile-log-warning-function (if eldev-build-suppress-warnings
                                                                                 #'ignore
                                                                               (when have-warning-function
                                                                                 byte-compile-log-warning-function))))
                                     (eldev-advised (#'byte-compile-log-warning :around
                                                                                (unless have-warning-function
                                                                                  (lambda (original &rest arguments)
                                                                                    (unless eldev-build-suppress-warnings
                                                                                      (apply original arguments)))))
                                       ;; Hack: make Emacs 24.x shut up.  We don't do it in various
                                       ;; other places, but this one is important for our tests.
                                       (eldev-advised (#'message :around
                                                                 (when (< emacs-major-version 25)
                                                                   (lambda (original &rest arguments)
                                                                     (unless (equal arguments `("Wrote %s" ,(expand-file-name target)))
                                                                       (apply original arguments)))))
                                         (byte-compile-file source))))))
                    (cond ((eq result 'no-byte-compile)
                           (eldev-verbose "Cancelled byte-compilation of `%s': it has `no-byte-compile' local variable" source)
                           nil)
                          (result
                           (when eldev-build-load-before-byte-compiling
                             ;; Load ourselves, since `byte-compile-file' calls `load'
                             ;; without `nomessage' parameter.  Byte-compiled file should
                             ;; be loaded to replace its slower source we loaded above.
                             (eldev-trace "Loading the byte-compiled file `%s'..." target)
                             (load target nil t t))
                           nil)
                          (t
                           source)))))
            (when failed-source
              (if recursive
                  ;; Normal errors would get caught inside Emacs byte-compilation code, so
                  ;; we use tag throwing/catching instead.
                  (throw 'eldev--byte-compilation-failed failed-source)
                (signal 'eldev-build-failed `("Failed to byte-compile `%s'" ,failed-source))))
            (unless (eq result 'no-byte-compile)
              (let ((history-entry (or (assoc (expand-file-name target eldev-project-dir) load-history)
                                       (assoc (expand-file-name source eldev-project-dir) load-history)))
                    inherited-targets
                    provided-feature)
                (unless history-entry
                  ;; This would not be needed if we could determine dependencies in any
                  ;; other way.  At least we load already byte-compiled file.
                  (eldev-trace "Loading file `%s' in order to find dependencies..." target)
                  (load (expand-file-name target eldev-project-dir) nil t t)
                  (setf history-entry (assoc (expand-file-name target eldev-project-dir) load-history)))
                (dolist (entry (cdr history-entry))
                  (pcase entry
                    (`(require . ,feature)
                     (unless provided-feature
                       (let ((provided-by (eldev--find-feature-provider feature)))
                         (when (stringp provided-by)
                           (push `(,feature . ,provided-by) inherited-targets)))))
                    (`(provide . ,feature)
                     ;; See e.g. `eldev-test-compile-circular-requires-1': after `provide' form
                     ;; ignore all remaining `require's for dependency purposes.  Also remove
                     ;; self-dependency entry if it has been added.
                     (puthash feature source eldev--feature-providers)
                     (setf inherited-targets (delete `(,feature . ,source) inherited-targets)
                           provided-feature  t))))
                (eldev-set-target-dependencies target (mapcar (lambda (entry) `(inherits ,(eldev-replace-suffix (cdr entry) ".el" ".elc")))
                                                              inherited-targets))))))))))

(defun eldev--trigger-early-byte-compilation (file)
  (when (stringp file)
    ;; Reset `default-directory', because it can have been changed e.g. when called from
    ;; inside byte-compilation.
    (let ((default-directory eldev-project-dir))
      (setf file (file-relative-name file eldev-project-dir))
      (unless (or (eldev-external-or-absolute-filename file)
                  (not (eldev-external-or-absolute-filename (file-relative-name file eldev-cache-dir))))
        (setf file (eldev-replace-suffix file ".el" ".elc"))
        (let ((status (eldev-build-target-status file)))
          (when (eq status 'planned)
            (eldev-build-target file)))))))

(defun eldev--find-feature-provider (feature)
  (or (gethash feature eldev--feature-providers)
      (puthash feature
               (if (featurep feature)
                   (let ((scan     load-history)
                         (look-for `(provide . ,feature))
                         (provider 'built-in))
                     (while scan
                       (let ((entry (pop scan)))
                         (when (member look-for (cdr entry))
                           (setf provider (eldev--validate-el-feature-source (car entry))
                                 scan     nil))))
                     provider)
                 (eldev--validate-el-feature-source (locate-file (symbol-name feature) load-path '(".el" ".elc"))))
               eldev--feature-providers)))

(defun eldev--validate-el-feature-source (filename)
  (setf filename (file-relative-name filename eldev-project-dir))
  (if (or (eldev-external-or-absolute-filename filename)
          (not (eldev-external-or-absolute-filename (file-relative-name filename eldev-cache-dir))))
      'external
    (setf filename (eldev-replace-suffix filename ".elc" ".el"))
    (if (file-exists-p (expand-file-name filename eldev-project-dir))
        filename
      'unknown)))


(eldev-defbuilder eldev-builder-makeinfo (source target)
  :short-name     "MKINFO"
  :source-files   eldev-makeinfo-sources
  :targets        ((".texi" ".texinfo") -> ".info")
  :define-cleaner (eldev-cleaner-.info
                   "Delete `.info' files generated from `.texi' or `.texinfo'."
                   :aliases (info dot-info)
                   :default t)
  (eldev-call-process (eldev-makeinfo-executable) `("--no-split" ,source "--output" ,target ,@(when eldev-build-suppress-warnings '("--no-warn")))
    (eldev--forward-process-output)
    (unless (= exit-code 0)
      (signal 'eldev-error `("`makeinfo' process exited with error code %d" ,exit-code)))))

(eldev-defbuilder eldev-builder-info-dir (sources target)
  :type           many-to-one
  :short-name     "INFO-DIR"
  :message        target
  :source-files   eldev-info-sources
  :targets        "dir"
  :define-cleaner (eldev-cleaner-info-dir
                   "Delete `dir' file generated from `.info'."
                   :default t)
  (eldev-call-process (eldev-install-info-executable) `("--dir-file" ,target ,@sources ,@(when eldev-build-suppress-warnings '("--silent")))
    (eldev--forward-process-output)
    (unless (= exit-code 0)
      (signal 'eldev-error `("`install-info' process exited with error code %d" ,exit-code)))))


(eldev-defbuilder eldev-builder-package (sources targets)
  :type           many-to-many
  :short-name     "PACK"
  :message        targets
  :source-files   (:and eldev-files-to-package (eldev-standard-fileset 'main))
  :targets        (lambda (sources)
                    ;; FIXME: We rely on `eldev-build-find-targets' special-casing us so
                    ;;        that we know number of sources from the start.  Find a
                    ;;        better solution.
                    (let ((target-base (file-relative-name (expand-file-name (eldev--package-name-version) (or eldev-package-output-dir (eldev-dist-dir)))
                                                           eldev-project-dir)))
                      (setf eldev--package-target-file (format (if (cdr sources) "%s.tar" "%s.el") target-base))
                      `(,eldev--package-target-file ,(format "%s.entry" target-base))))
  :collect        (lambda (targets)
                    `(((,(car targets)) ":package") ((,(cadr targets)) ":package-archive-entry")))
  (unless sources
    (signal 'eldev-build-failed `("No sources for packaging")))
  (let* ((package           (eldev-package-descriptor))
         (pretended-version (or eldev-package-forced-version (package-desc-version package)))
         (package-target    (car targets))
         (entry-target      (cadr targets)))
    ;; Hardly anyone would make an entry without the package, but let's check.
    (when (memq (eldev-build-target-status package-target) '(planned building))
      (if (cdr sources)
          ;; Building a tarball.
          (let* ((name-version     (eldev--package-name-version))
                 (name-version-dir (file-name-as-directory name-version))
                 (working-dir      (make-temp-file "eldev-packaging-" t))
                 (descriptor-file  (format "%s-pkg.el" (package-desc-name package)))
                 temporary-descriptor
                 files-to-tar)
            (condition-case nil
                (make-symbolic-link eldev-project-dir (expand-file-name name-version working-dir))
              (file-error (error "FIXME: Copy")))
            (make-directory (file-name-directory package-target) t)
            (unless (file-exists-p descriptor-file)
              (with-temp-file descriptor-file
                (insert "; -*- no-byte-compile: t -*-\n")
                (pp `(define-package ,(symbol-name (package-desc-name package)) ,(package-version-join pretended-version)
                       ,(package-desc-summary package)
                       ,(eldev-macroexp-quote (mapcar (lambda (requirement)
                                                        `(,(car requirement) ,(package-version-join (cadr requirement))))
                                                      (package-desc-reqs package)))
                       ,@(apply #'nconc (mapcar (lambda (extra)
                                                  `(,(eldev-macroexp-quote (car extra)) ,(eldev-macroexp-quote (cdr extra))))
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
                  (command-line      `("-cf" ,(expand-file-name package-target eldev-project-dir) ,@(nreverse files-to-tar))))
              (eldev-verbose "%s" (eldev-message-enumerate-files "Packaging the following file%s: %s (%d)" sources))
              (eldev-trace "Full command line to create package tarball:\n  %s" (eldev-message-command-line (eldev-tar-executable) command-line))
              (eldev-call-process (eldev-tar-executable) command-line
                (unless (= exit-code 0)
                  (eldev-warn "`tar' output (ran from directory `%s'):" working-dir))
                (eldev--forward-process-output)
                (unless (= exit-code 0)
                  (signal 'eldev-build-failed `("Failed to create package tarball `%s'" ,package-target)))))
            ;; Note that if packaging fails, `working-dir' and `descriptor-file' are not
            ;; deleted.  This is intentional.
            (when temporary-descriptor
              (delete-file descriptor-file))
            (delete-directory working-dir t))
        ;; Single-file package.
        ;; FIXME: Validate sanity.
        (make-directory (file-name-directory package-target) t)
        (copy-file (car sources) package-target 'overwrite))
      (setf eldev--package-target-generated t))
    (when (memq (eldev-build-target-status entry-target) '(planned building))
      (eldev-verbose "Generating package archive entry `%s'" entry-target)
      (with-temp-file entry-target
        (prin1 `(,(package-desc-name package)
                 . [,pretended-version ,(package-desc-reqs package) ,(package-desc-summary package)
                    ,(if (cdr sources) 'tar 'single) ,(package-desc-extras package)])
               (current-buffer))
        (insert "\n")))))

(defun eldev--package-name-version ()
  (let ((package (eldev-package-descriptor)))
    (format "%s-%s" (package-desc-name package) (eldev-message-version (or eldev-package-forced-version package)))))



;; eldev init

(defvar eldev-init-interactive t)

(eldev-defcommand eldev-init (&rest parameters)
  "Initialize project in this directory to use Eldev.  This command
will fail if the project already has file named `Eldev'."
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (when (file-exists-p eldev-file)
    (signal 'eldev-error `("File `%s' already exists in this project" ,eldev-file)))
  (let ((archives-to-use t)
        .gitignore)
    (if eldev-init-interactive
        (when (eldev-y-or-n-p "Try to automatically select package archive for dependency lookup? ")
          (let ((requirements (package-desc-reqs (eldev-package-descriptor))))
            (if requirements
                (progn
                  (eldev-print "Please wait, this might take a while...")
                  (dolist (archive eldev--known-package-archives)
                    (eldev-use-package-archive (car archive)))
                  (let ((archive-options (eldev--init-all-archive-combinations (mapcar #'car eldev--known-package-archives))))
                    (while archive-options
                      (let ((archives (pop archive-options)))
                        (if (let ((package-archives (mapcar (lambda (archive) (nth 1 (assq archive eldev--known-package-archives))) archives)))
                              (eldev--fetch-archive-contents)
                              (package-read-all-archive-contents)
                              (package-load-all-descriptors)
                              (ignore-errors
                                (let ((inhibit-message t))
                                  (package-compute-transaction nil requirements))))
                            (progn (eldev-print "Autoguessed the following %s" (eldev-message-enumerate '("package archive:" "package archives:") archives))
                                   (setf archives-to-use archives
                                         archive-options nil))
                          (eldev-verbose "Cannot fetch project dependencies from %s" (eldev-message-enumerate "package archive" archives))))))
                  (when (eq archives-to-use t)
                    (eldev-warn "Failed to autoguess needed package archives; please edit `%s' as appropriate later" eldev-file)))
              (eldev-print "This project has no dependencies"))))
      (eldev-trace "Not in interactive mode, not autodetermining package archives to use"))
    (cond ((file-directory-p ".git")
           (eldev-trace "Detected `.git' subdirectory, assuming a Git repository")
           (setf .gitignore (if eldev-init-interactive
                                (eldev-y-or-n-p (eldev-format-message "Usage of Git detected; modify `.gitignore' as appropriate? "))
                              (eldev-trace "Not in interactive mode, will modify `.gitignore' by default")
                              t)))
          (t
           (eldev-verbose "This doesn't appear to be a supported VCS repository")))
    (with-temp-file eldev-file
      (insert "; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-\n\n")
      (cond ((eq archives-to-use t)
             (eldev-trace "Adding a few commented-out calls to `eldev-use-package-archive' to `%s'" eldev-file)
             (insert ";; Uncomment some calls below as needed for your project.  It is not\n"
                     ";; recommended to use `melpa-unstable' unless some dependencies simply\n"
                     ";; cannot be downloaded from another archive.\n")
             (dolist (archive eldev--known-package-archives)
               (insert (format ";(eldev-use-package-archive '%s)\n" (car archive)))))
            (archives-to-use
             (eldev-trace "Adding the autodetermined package archives to `%s'" eldev-file)
             (insert ";; Autodetermined by `eldev init'.\n")
             (dolist (archive archives-to-use)
               (insert (format "(eldev-use-package-archive '%s)\n" archive))))
            (t
             (insert ";; Calls to `eldev-use-package-archive' are not needed: no dependencies\n"))))
    (eldev-print "Created file `%s' for this project" eldev-file)
    (cond (.gitignore
           (when (eldev-git-executable 'warn)
             (let ((files-to-ignore `(,eldev-cache-dir ,eldev-local-file))
                   add-to-ignore)
               (dolist (file files-to-ignore)
                 (if (/= (call-process (eldev-git-executable) nil nil nil "check-ignore" "--quiet" file)  0)
                     (progn (push file add-to-ignore)
                            (eldev-trace "Git doesn't ignore `%s' currently" file))
                   (eldev-trace "Git already ignores `%s'" file)))
               (if add-to-ignore
                   (let (failed)
                     (setf add-to-ignore (nreverse add-to-ignore))
                     (eldev-verbose "Adding %s to `.gitignore'" (eldev-message-enumerate "file" add-to-ignore))
                     (with-temp-file ".gitignore"
                       (condition-case nil
                           (insert-file-contents ".gitignore" t)
                         (file-missing))
                       (goto-char (point-max))
                       (unless (eolp)
                         (insert "\n"))
                       (when (looking-back (rx nonl "\n") nil)
                         (insert "\n"))
                       (insert "# Added automatically by `eldev init'.\n")
                       (dolist (file add-to-ignore)
                         (insert (format "/%s\n" file))))
                     (dolist (file files-to-ignore)
                       (when (/= (call-process (eldev-git-executable) nil nil nil "check-ignore" "--quiet" file) 0)
                         (eldev-warn "Failed to convince Git to ignore file `%s'" file)
                         (setf failed t)))
                     (unless failed
                       (eldev-print "Modified file `.gitignore'")))
                 (eldev-verbose "Git already ignores what Eldev thinks it should"))))))))

(defun eldev--init-all-archive-combinations (all-archives)
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

(eldev-defbooloptions eldev-init-interactive-mode eldev-init-non-interactive-mode eldev-init-interactive
  ("Create `Eldev' interactively"
   :options       (-i --interactive))
  ("Don't ask any questions"
   :options       (-n --non-interactive))
  :for-command    init)


(provide 'eldev)

;;; eldev.el ends here

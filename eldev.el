;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2019-2021 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.10.1
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

;; Using `autoload' function directly instead of ;;;###autoload cookies since the latter
;; won't work with ELDEV_LOCAL.  This must be at the top _and_ in `eval-and-compile', else
;; byte-compilation gives warnings...
(eval-and-compile
  (dolist (autoloads '(("eldev-plugins" eldev-active-plugins eldev-use-plugin)
                       ("eldev-vc"      eldev-vc-root-dir eldev-vc-executable eldev-vc-full-name eldev-with-vc eldev-vc-detect)))
    (dolist (function (cdr autoloads))
      (autoload function (car autoloads)))))


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

(defconst eldev-global-cache-dir "global-cache"
  "Name of the global cache directory (a subdirectory of `eldev-dir').")

(defvar eldev--internal-pseudoarchive "--eldev--")

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

(defvar eldev-test-fileset '("./test.el" "./tests.el" "./*-test.el" "./*-tests.el" "./test/" "./tests/" "./features/")  ; the last is for Ecukes
  "Fileset used to find test targets.
Default value is files `test.el', `tests.el', `*-test.el' or
`*-tests.el' in the project directory and everything within
`test' or `tests' subdirectories.")

(defvar eldev-standard-excludes '(:or ".*"
                                      "./features/step-definitions/" "./features/support/"  ; for Ecukes
                                      (let ((dist-dir (file-relative-name (eldev-dist-dir) eldev-project-dir)))
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

(defvar eldev-external-package-dir nil
  "Use given directory instead of managing dependencies separately.
If this value is nil (default), Eldev will manage and install
dependencies as needed.  If a directory is specified, Eldev never
installs anything and will fail if a dependency is missing.
Local dependencies will still be loaded from their directories.

If value of this variable is t, load from the standard Emacs
location, i.e. `~/.emacs.d/elpa'.

Since 0.8.")

(defvar eldev-prefer-stable-archives t
  "Prefer stable archives (e.g. MELPA Stable) whenever possible.
Since 0.5.")

(defvar eldev-print-backtrace-on-abort nil
  "If Eldev is aborted with C-c, print a backtrace.")

(defvar eldev-setup-first-forms nil
  "Forms executed as the first step of Eldev setup.
In particular, these forms are evaluated before reading `Eldev'
or `Eldev-local'.

Since 0.5")

(defvar eldev-setup-forms nil
  "Forms executed as the last step of Eldev setup.
Should normally be specified only via command line.")

(defvar eldev-skip-global-config nil
  "Whether to skip file `~/.eldev/config'.
Occasionally useful to some external tools.  Not exposed through
normal interface, but can be set in a `--setup-first' form.

Since 0.8")

(defvar eldev-skip-project-config nil
  "Whether to skip both files `Eldev' and `Eldev-local'.
Occasionally useful to some external tools.  Not exposed through
normal interface, but can be set in a `--setup-first' form.

Since 0.5")

(defvar eldev-skip-local-project-config nil
  "Whether to skip file `Eldev-local'.
Occasionally useful to some external tools.  Not exposed through
normal interface, but can be set in a `--setup-first' form.

Since 0.5")

(defvar eldev-known-tool-packages
  '((buttercup    :version "1.24"   :archive  melpa)
    (ecukes       :version "0.6.18" :archive  melpa)
    (package-lint :version "0.14"   :archives (melpa gnu))
    (relint       :version "1.18"   :archive  gnu)
    (elisp-lint                     :archives (melpa gnu))
    ;; This is for a plugin, but probably no reason not to have it unconditionally.
    (undercover   :version "0.8"    :archive  melpa))
  "Alist of known external tool packages.
Packages listed here can be referred to as `(:tool PACKAGE-NAME)'
in places where package alists are understood.  Users can
customize this variable to influence how Eldev picks up external
tools.

Since 0.9.")


(defvar eldev-force-override nil
  "Set to non-nil to disable all command and option duplicate checks.")

(defvar eldev--commands nil)
(defvar eldev--command-aliases nil)

(defvar eldev--options nil)


(eldev-define-error 'eldev-error               "Unhandled Eldev error")
(eldev-define-error 'eldev-missing-dependency  "Dependency is missing" 'eldev-error)
(eldev-define-error 'eldev-too-old             "Eldev is too old"      'eldev-error)
(eldev-define-error 'eldev-wrong-command-usage "Wrong command usage"   'eldev-error)
(eldev-define-error 'eldev-wrong-option-usage  "Wrong option usage"    'eldev-error)
(eldev-define-error 'eldev-build-failed        "Build failed"          'eldev-error)
(eldev-define-error 'eldev-build-abort-branch  "Build failed"          'eldev-error)
(eldev-define-error 'eldev-quit                nil)


;; Internal helper for `eldev-defcommand'.
(defun eldev--register-command (function command keywords)
  (let (override)
    (while keywords
      (eldev-pcase-exhaustive (pop keywords)
        (:aliases
         (eldev-register-command-aliases command (pop keywords)))
        ((and (or :command-hook :briefdoc :parameters :custom-parsing :hidden-if :works-on-old-eldev) keyword)
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
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (defvar ,hook nil ,(format "Hook to be executed before command `%s'." command))
            (eldev--register-command ',name ',command ',`(:command-hook ,hook ,@(nreverse keywords))))))

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
        option.  Special case: if the form evaluates to symbol
        `:no-default', don't print anything.

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

(eldev-defbooloptions eldev-print-backtrace-on-abort eldev-no-backtrace-on-abort eldev-print-backtrace-on-abort
  ("Print backtrace if aborted with C-c"
   :options       (-Q --backtrace-on-abort))
  ("Don't print backtrace if aborted with C-c"
   :options       --no-backtrace-on-abort
   :hidden-if     :default))

(eldev-defoption eldev-backtrace-style (&optional width)
  "Print backtraces for given screen WIDTH; if omitted, don't
truncate backtrace lines"
  :options        (-b --backtrace --backtrace-style)
  :optional-value WIDTH
  :default-value  (if (and (integerp eldev-backtrace-style) (> eldev-backtrace-style 1))
                      eldev-backtrace-style
                    "untruncated")
  (setf eldev-backtrace-style (if width (string-to-number width) t)))

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
        eldev-coloring-mode (cond ((or (null mode) (member mode '("always" "on"))) t)
                                  ((string= mode "auto")                           'auto)
                                  ((member mode '("never" "off"))                   nil)
                                  (t (signal 'eldev-wrong-option-usage `("argument must be `always', `auto' or `never'"))))))

;; Not advertised, this is mostly for external tools.
(eldev-defoption eldev-setup-first-form (form)
  "Evaluate FORM as the first step of the setup"
  :options        --setup-first
  :value          FORM
  :hidden-if      t
  (push (eldev-read-wholly form "setup form") eldev-setup-first-forms))

(eldev-defoption eldev-setup-form (form)
  "Evaluate FORM as the final step of the setup"
  :options        (-S --setup)
  :value          FORM
  (push (eldev-read-wholly form "setup form") eldev-setup-forms))

(eldev-defbooloptions eldev-prefer-stable-archives eldev-prefer-unstable-archives eldev-prefer-stable-archives
  ("Prefer stable archives (e.g. MELPA Stable: stable.melpa.org)"
   :options       --stable
   :hidden-if     :default)
  ("Prefer bleeding-edge archives (e.g. MELPA [Unstable]: melpa.org)"
   :options       --unstable))

(eldev-defoption eldev-enable-project-isolation ()
  "Manage and install project dependencies normally"
  :options        (-I --isolated)
  :if-default     (null eldev-external-package-dir)
  :hidden-if      :default
  (setf eldev-external-package-dir nil))

(eldev-defoption eldev-use-external-package-dir (&optional dir)
  "Use preinstalled dependencies from given directory"
  :options        (-X --external --external-deps)
  :optional-value DIR
  :default-value  (let ((dir (abbreviate-file-name (eldev-external-package-dir t))))
                    (if eldev-external-package-dir
                        dir
                      `(:instead-of-default ,(eldev-format-message "(default would be `%s')" dir))))
  (setf eldev-external-package-dir (or dir t)))



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

(defconst eldev-real-user-emacs-directory user-emacs-directory
  "Original value of variable `user-emacs-directory'.
During startup, Eldev changes `user-emacs-directory' to point
inside its cache directory.  This is done to further isolate the
project being built from the “normal” Emacs, like is done with
e.g. dependency installation.  However, original value is stored
in this variable and can be restored in file `Eldev' if certain
project needs it that way.

Since 0.2.1.")

(defvar eldev-executing-command-hook nil)
(defvar eldev-too-old nil)
(defvar eldev--setup-step nil)


(defun eldev-start-up ()
  "Prepare Eldev.
Used by Eldev startup script."
  (setf package-user-dir     (expand-file-name "packages" (eldev-cache-dir t))
        package-archives     nil
        user-emacs-directory (eldev-user-emacs-dir))
  ;; The idea of postponing directory creation is to avoid littering random directories
  ;; with `.eldev' in case Eldev is executed there by error, for example.
  (add-hook 'eldev--project-validated-hook (lambda () (eldev-user-emacs-dir t))))

(defun eldev-cli (command-line)
  "Eldev entry point."
  ;; We parse command line in a separate from `command-line-args' and
  ;; `command-line-args-left' way, but whatever code we execute from here should believe
  ;; there are no arguments left.
  (let (command-line-args-left
        command
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
                                                        ;; FIXME: Maybe also try to highlight this?
                                                        (let ((inhibit-message nil))
                                                          (apply original arguments))))
                        (eldev-parse-options command-line nil t t)
                        (when eldev-print-backtrace-on-abort
                          (add-hook 'kill-emacs-hook #'eldev-backtrace))
                        ;; Since this is printed before `~/.eldev/config' is loaded it can
                        ;; ignore some settings from that file, e.g. colorizing mode.
                        (eldev-trace "Started up on %s" (replace-regexp-in-string " +" " " (current-time-string)))
                        (eldev-trace "Running on %s" (emacs-version))
                        (eldev-trace "Project directory: `%s'" eldev-project-dir)
                        (condition-case error
                            (eldev--set-up)
                          (eldev-too-old (setf eldev-too-old (cdr error))))
                        (let* ((external-dir     (eldev-external-package-dir))
                               (package-user-dir (or external-dir package-user-dir)))
                          (setf command-line (eldev-parse-options command-line nil t))
                          (if command-line
                              (let (archive-files-to-delete)
                                ;; If we are using external directory, maybe rename
                                ;; archives so that they don't clash with what is
                                ;; retrieved in that directory already.
                                (when external-dir
                                  (dolist (entry package-archives)
                                    (push (setf (car entry) (eldev--maybe-rename-archive (car entry) external-dir)) archive-files-to-delete))
                                  (push (setf eldev--internal-pseudoarchive (eldev--maybe-rename-archive eldev--internal-pseudoarchive external-dir)) archive-files-to-delete))
                                (unwind-protect
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
                                  (when (setf archive-files-to-delete (eldev-filter (file-exists-p (eldev--package-archive-dir it external-dir)) archive-files-to-delete))
                                    (eldev-trace "Deleting package archive contents to avoid polluting the external directory: %s"
                                                 (eldev-message-enumerate nil archive-files-to-delete))
                                    (dolist (archive archive-files-to-delete)
                                      (ignore-errors (delete-directory (eldev--package-archive-dir archive external-dir) t))))))
                            (eldev-usage)
                            (eldev-print "Run `%s help' for more information" (eldev-shell-command t))
                            (setf exit-code 0)))))))
              (eldev-error (eldev--print-eldev-error error command))
              (eldev-quit  (setf exit-code (cdr error))))
          (error (eldev-error "%s" (error-message-string error))
                 (eldev--maybe-inform-about-setup-step)
                 (eldev-print :stderr "Run with `--debug' (`-d') option to see error backtrace")))
      (remove-hook 'kill-emacs-hook #'eldev-backtrace)
      (eldev-trace "Finished %s on %s" (if (eq exit-code 0) "successfully" "erroneously") (replace-regexp-in-string " +" " " (current-time-string))))
    (when eldev-too-old
      (when (eq (car eldev-too-old) :hint)
        (setf eldev-too-old (cddr eldev-too-old)))
      (eldev-warn "%s" (apply #'eldev-format-message eldev-too-old)))
    (or exit-code 1)))

(defun eldev--maybe-rename-archive (archive external-dir)
  (catch 'renamed-as
    (let (k)
      (while t
        (let* ((new-name (if k (format "%s-%03d" archive k) archive))
               (filename (eldev--package-archive-dir new-name external-dir)))
          (unless (file-exists-p filename)
            (throw 'renamed-as new-name))
          (setf k (if k (1+ k) 1)))))))

(defun eldev--print-eldev-error (error &optional command)
  (let* ((arguments (cdr error))
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
      (eldev-print :stderr "%s" (apply #'eldev-format-message (eldev-listify hint))))
    (when hint-about-command
      (eldev-print :stderr "Run `%s help%s' for more information" (eldev-shell-command t) (if (eq hint-about-command t) "" (format " %s" hint-about-command))))
    (eldev--maybe-inform-about-setup-step)))

(defun eldev--maybe-inform-about-setup-step ()
  (when eldev--setup-step
    (eldev-print :stderr "Failed setup step: %s" eldev--setup-step)))

(defun eldev-extract-error-message (error)
  "Extract the message from an `eldev-error'.
Since 0.2."
  (let ((arguments (cdr error)))
    (pcase (car arguments)
      ((or :hint :command) (pop arguments) (pop arguments)))
    (apply #'eldev-format-message arguments)))

(declare-function file-name-case-insensitive-p nil (filename))

(defun eldev--set-up ()
  (let (loaded-project-config)
    (dolist (form (reverse eldev-setup-first-forms))
      (setf eldev--setup-step (eldev-format-message "evaluating form `%S' specified on the command line" form))
      (eldev-trace "%s..." (eldev-message-upcase-first eldev--setup-step))
      (eval form t))
    (dolist (config '((eldev-user-config-file . "No file `%s', not applying user-specific configuration")
                      (eldev-file             . "No file `%s', project building uses only defaults")
                      (eldev-local-file       . "No file `%s', not customizing build")))
      (let* ((symbol           (car config))
             (filename         (symbol-value symbol))
             (file             (locate-file filename (list eldev-project-dir)))
             (skipping-because (or (when (and eldev-skip-global-config (eq symbol 'eldev-user-config-file))
                                     'eldev-skip-global-config)
                                   (when (and eldev-skip-project-config (memq symbol '(eldev-file eldev-local-file)))
                                     'eldev-skip-project-config)
                                   (when (and eldev-skip-local-project-config (eq symbol 'eldev-local-file))
                                     'eldev-skip-local-project-config))))
        (if skipping-because
            (eldev-verbose "Skipping file `%s' because of `%s'" filename skipping-because)
          (if file
              ;; See issue 9: this is for Mac OS.
              (if (and (equal filename "Eldev")
                       (or (not (fboundp 'file-name-case-insensitive-p)) (file-name-case-insensitive-p file))
                       (with-temp-buffer
                         (insert-file-contents file nil 0 100)
                         (looking-at (rx "#!"))))
                  (eldev-verbose "File `%s' appears to be a script on a case-insensitive file system, ignoring" file)
                (progn (setf eldev--setup-step (eldev-format-message "loading file `%s'" filename))
                       (eldev-trace "%s..." (eldev-message-upcase-first eldev--setup-step))
                       (load file nil t t)
                       (when (memq symbol '(eldev-file eldev-local-file))
                         (setf loaded-project-config t))))
            (eldev-verbose (cdr config) filename)))))
    (dolist (form (reverse eldev-setup-forms))
      (setf eldev--setup-step (eldev-format-message "evaluating form `%S' specified on the command line" form))
      (eldev-trace "%s..." (eldev-message-upcase-first eldev--setup-step))
      (eval form t))
    (setf eldev--setup-step nil)
    (when loaded-project-config
      ;; This is an undocumented flag file indicating that `Eldev' or `Eldev-local' have
      ;; been loaded at least once in this project.
      (with-temp-file (expand-file-name "ever-initialized" (eldev-cache-dir nil t))))))

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
                          (if command
                              (eldev-help (symbol-name command))
                            (eldev-help))
                          (signal 'eldev-quit 0))
                        (when (and (null command) (string= term "--version"))
                          (apply #'eldev-version command-line)
                          (signal 'eldev-quit 0))
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

(defun eldev-global-package-archive-cache-dir ()
  (expand-file-name eldev-global-cache-dir eldev-dir))

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

(defun eldev-user-emacs-dir (&optional ensure-exists)
  ;; Intentionally not called `.emacs.d' as an additional test for projects.
  (let ((emacs-dir (file-name-as-directory (expand-file-name "emacs-dir" (eldev-cache-dir t)))))
    (when ensure-exists
      (make-directory emacs-dir t))
    emacs-dir))

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
`version'.  Returned value is that of BODY, or nil if the file is
not loaded."
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
                     (progn (eldev-verbose "Ignoring %s in file `%s': version %d must be from the future (expected at most %d)" ,description ,file version ,expected-version)
                            nil)
                   ,@body))))
         (eldev-trace "No file `%s', not reading %s" (file-relative-name ,file eldev-project-dir) ,description)
         nil)
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

(defun eldev--shell-script-name ()
  (if (eq system-type 'windows-nt)
      "eldev.bat"
    "eldev"))



;; Functions for `Eldev' and `Eldev-local'.

(defvar eldev--extra-dependencies nil)
(defvar eldev--local-dependencies nil)
(defvar eldev--local-dependency-packages nil)
(defvar eldev--loading-roots nil)

(eval-and-compile
  (defvar eldev--known-package-archives '((gnu            ("gnu"            . "https://elpa.gnu.org/packages/")     300)
                                          (nongnu         ("nongnu"         . "https://elpa.nongnu.org/nongnu/")    250)
                                          (melpa-stable   ("melpa-stable"   . "https://stable.melpa.org/packages/") 200)
                                          (melpa-unstable ("melpa-unstable" . "https://melpa.org/packages/")        100)
                                          (melpa          (:stable melpa-stable :unstable melpa-unstable)))))

;; Initial value means that even if `melpa-stable' and `melpa-unstable' are added
;; separately, they will be swappable with relevant options.  For other archives calling
;; `eldev-use-package-archive' with a stable/unstable pair is required.
(defvar eldev--stable/unstable-archives (eval-when-compile `((,(cdr   (cadr (assq 'melpa-stable   eldev--known-package-archives)))
                                                              . ,(cdr (cadr (assq 'melpa-unstable eldev--known-package-archives)))))))

;; This variable is used only on early Emacses: archive priorities are important at least
;; for our tests.  But let's not define `package-archive-priorities' variable to avoid
;; breaking something that might test for it.  If this variable's value is t, use
;; `package-archive-priorities' instead.
(defvar eldev--package-archive-priorities (boundp 'package-archive-priorities))


(defun eldev-require-version (version)
  "Fail if Eldev is too old.
VERSION can be either a string or a list (see
`version-to-list')."
  (unless (eldev-find-package-descriptor 'eldev version)
    (signal 'eldev-too-old `(:hint ("Run `%s upgrade-self' to install the newest available Eldev version" ,(eldev-shell-command t))
                                   "Project `%s' requires Eldev version %s or newer (this is version %s)" ,(package-desc-name (eldev-package-descriptor))
                                   ,(eldev-message-version version) ,(eldev-message-version (eldev-find-package-descriptor 'eldev))))))

(defun eldev-use-package-archive (archive &optional priority)
  "Use given ARCHIVE to install project dependencies.
ARCHIVE can be one of the predefined symbols below or a cons cell
of (ID . URL), same as you would use in `package-archives'.

Standard archives:

  - gnu            (https://elpa.gnu.org/packages/)
  - nongnu         (https://elpa.nongnu.org/nongnu/, since 0.10)
  - melpa-stable   (https://stable.melpa.org/packages/)
  - melpa-unstable (https://melpa.org/packages/)

Since 0.5 an archive can also be a plist with properties
`:stable' and `:unstable'.  Standard archives of this type:

  - melpa          (:stable melpa-stable :unstable melpa-unstable)

If PRIORITY is non-nil, ARCHIVE is given this priority (see
`package-archive-priorities').  Standard archives get priorities
300, 250, 200 and 100 in the order they are listed above, unless
you specify something explicitly.

If archive is stable/unstable plist, given PRIORITY is used for
the unstable variant, stable receives priority 100 higher (these
values can be swapped by using `--unstable' option on the command
line).  You can also specify a cons cell of two integers for the
two variants."
  (setf archive (eldev--resolve-package-archive archive nil))
  (if (eldev--stable/unstable-archive-p archive)
      (let ((stable   (plist-get archive :stable))
            (unstable (plist-get archive :unstable)))
        (eldev--do-use-package-archive stable   (if (consp priority)
                                                    (car priority)
                                                  (when (or priority (not (eldev-any-p (unless (eldev--stable/unstable-archive-p (nth 1 it))
                                                                                         (string= (cdr stable) (cdr (nth 1 it))))
                                                                                       eldev--known-package-archives)))
                                                    (+ (or priority 0) 100))))
        (eldev--do-use-package-archive unstable (if (consp priority) (cdr priority) priority))
        (add-to-list 'eldev--stable/unstable-archives `(,(cdr stable) . ,(cdr unstable))))
    (eldev--do-use-package-archive archive priority)))

(defun eldev--resolve-package-archive (archive &optional only-simple)
  (let ((standard (assq archive eldev--known-package-archives)))
    (when standard
      (setf archive (nth 1 standard))))
  (cond ((and (consp archive) (stringp (car archive)) (stringp (cdr archive)))
         archive)
        ((and (not only-simple) (eldev--stable/unstable-archive-p archive))
         `(:stable   ,(eldev--resolve-package-archive (plist-get archive :stable)   t)
           :unstable ,(eldev--resolve-package-archive (plist-get archive :unstable) t)))
        (t
         (error "Unknown package archive `%S'" archive))))

(defun eldev--find-simple-archive (archives name)
  (catch 'found
    (dolist (archive archives)
      (if (eldev--stable/unstable-archive-p archive)
          (let ((stable   (plist-get archive :stable))
                (unstable (plist-get archive :unstable)))
            (when (string= (car stable) name)
              (throw 'found stable))
            (when (string= (car unstable) name)
              (throw 'found unstable)))
        (when (string= (car archive) name)
          (throw 'found archive))))))

(defun eldev--stable/unstable-archive-p (archive)
  (and (listp archive) (plist-get archive :stable) (plist-get archive :unstable)))

(defun eldev--do-use-package-archive (archive priority)
  (when (string= (car archive) eldev--internal-pseudoarchive)
    (error "Package archive name `%s' is reserved for internal use" eldev--internal-pseudoarchive))
  (let ((existing (assoc (car archive) package-archives)))
    (if existing
        (progn
          (unless (equal (cdr archive) (cdr existing))
            (error "Conflicting URLs for package archive `%s': `%s' and `%s'" (car archive) (cdr existing) (cdr archive)))
          (when priority
            (eldev--assq-set (car archive) priority
                             (if (eq eldev--package-archive-priorities t) package-archive-priorities eldev--package-archive-priorities)
                             #'equal)))
      (unless priority
        (dolist (standard eldev--known-package-archives)
          (when (and (not (eldev--stable/unstable-archive-p (nth 1 standard))) (string= (cdr archive) (cdr (nth 1 standard))))
            (setf priority (nth 2 standard)))))
      (eldev-verbose "Using package archive `%s' at `%s' with %s"
                     (car archive) (cdr archive) (if priority (format "priority %s" priority) "default priority"))
      (push archive package-archives)
      (when priority
        (push (cons (car archive) priority)
              (if (eq eldev--package-archive-priorities t) package-archive-priorities eldev--package-archive-priorities))))))

(defun eldev--stable/unstable-preferred-archive (archive)
  (if (eldev--stable/unstable-archive-counterpart archive)
      (eq (null (rassoc (cdr archive) eldev--stable/unstable-archives))
          (when eldev-prefer-stable-archives t))
    ;; Archives without stable/unstable counterpart are always "preferred".
    t))

(defun eldev--stable/unstable-archive-counterpart (archive &optional value-if-not-used)
  (let ((match (or (cdr (assoc  (cdr archive) eldev--stable/unstable-archives))
                   (car (rassoc (cdr archive) eldev--stable/unstable-archives)))))
    (when match
      (catch 'counterpart
        (dolist (candidate package-archives)
          (when (string= (cdr candidate) match)
            (throw 'counterpart (car candidate))))
        value-if-not-used))))

(defun eldev--adjust-stable/unstable-archive-priorities ()
  (let ((priorities (if (eq eldev--package-archive-priorities t) package-archive-priorities eldev--package-archive-priorities)))
    (dolist (archive package-archives)
      (let ((name        (car archive))
            (counterpart (eldev--stable/unstable-archive-counterpart archive)))
        (when (and counterpart (eldev--stable/unstable-preferred-archive archive))
          (let ((priority1 (cdr (assoc name        priorities)))
                (priority2 (cdr (assoc counterpart priorities))))
            (cond ((and priority1 priority2)
                   (setf (cdr (assoc name        priorities)) (max priority1 priority2)
                         (cdr (assoc counterpart priorities)) (min priority1 priority2)))
                  ((and priority1 (< priority1 0))
                   (setf (car (assoc name        priorities)) counterpart))
                  ((and priority2 (> priority2 0))
                   (setf (car (assoc counterpart priorities)) name)))))))))


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

(defun eldev-add-loading-roots (sets &rest roots)
  "Additionally use loading ROOTS for given SETS.
This would typically be used for `test' command in case your
project has `require' forms in its test files that expect to load
features from test directory without using qualified names
relative to project root.  E.g. like this:

    (eldev-add-loading-roots 'test \"test\")

Since 0.5."
  (dolist (set (eldev-listify sets))
    (let ((set-roots (or (assq set eldev--loading-roots) (car (push `(,set . nil) eldev--loading-roots)))))
      (dolist (root roots)
        (push root (cdr set-roots))))))

(defun eldev--inject-loading-roots (sets)
  (dolist (set (eldev-listify sets))
    (dolist (loading-root (cdr (assq set eldev--loading-roots)))
      (add-to-list 'load-path (file-name-as-directory (expand-file-name loading-root eldev-project-dir))))))


(defvar eldev--know-installed-runtime-dependencies nil)

(defun eldev--load-installed-runtime-dependencies ()
  (eldev-do-load-cache-file (expand-file-name "runtime-dependency.set" (eldev-cache-dir t)) "list of installed runtime dependencies" 1
    (apply #'eldev-add-extra-dependencies 'runtime (cdr (assq 'dependencies contents))))
  (setf eldev--know-installed-runtime-dependencies t))

(defun eldev--save-installed-runtime-dependencies ()
  (let ((eldev--extra-dependencies eldev--extra-dependencies))
    (unless eldev--know-installed-runtime-dependencies
      (eldev--load-installed-runtime-dependencies))
    (eldev-do-save-cache-file (expand-file-name "runtime-dependency.set" (eldev-cache-dir t)) "list of installed runtime dependencies" 1
      `((dependencies . ,(cdr (assq 'runtime eldev--extra-dependencies)))))))


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
    (write-region nil nil target nil 'no-message)))

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
            (let ((parameters (eldev-get handler :parameters)))
              (eldev-output "Usage: %s%s %s" (eldev-shell-command t) (if (cdr (assq command eldev--options)) " [OPTION...]" "")
                            (if parameters (format "%s %s" real-command parameters) real-command))
              (eldev-help-list-aliases real-command eldev--command-aliases "\n%s" '("Command alias:" "Command aliases:"))
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

(defun eldev-help-list-aliases (name alias-alist &optional format-string label-string)
  (let (aliases)
    (dolist (alias alias-alist)
      (when (eq (cdr alias) name)
        (push (car alias) aliases)))
    (when aliases
      (eldev-output (or format-string "%s") (eldev-message-enumerate (or label-string '("Alias:" "Aliases:")) aliases #'symbol-name t t)))))

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
        (setf documentation (if documentation (format "%s [%s]" documentation default-string) (format "[%s]" default-string))))
      (when default-value
        (setf default-value (eval default-value t))
        (unless (eq default-value :no-default)
          (setf default-string (pcase default-value
                                 (`(:instead-of-default ,value) value)
                                 (_                             (format "[%s: %s]" default-string default-value))))
          (setf documentation (if documentation (format "%s %s" documentation default-string) default-string))))
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

(defvar eldev-before-loading-dependencies-hook nil
  "Hook executed before dependencies are loaded.
See `eldev-load-dependencies-hook' for details.  Since 0.6.")

(defvar eldev-load-dependencies-hook nil
  "Hook executed whenever dependencies are loaded.
Functions are called with arguments TYPE and ADDITIONAL-SETS.
TYPE is either t if the project is being loaded for actual use,
symbol `load-only' if it is loaded only for side effect (e.g. to
build a tree of its dependencies), and nil if invoked from
`eldev-load-extra-dependencies' (i.e. if the project is not being
loaded at all: only some additional sets).  The second is a list
of additional dependency sets (see `eldev-add-extra-dependencies').

Since Eldev 0.2.  `load-only' is since 0.6.")

(defvar eldev-upgrade-dry-run-mode nil
  "Don't upgrade if non-nil, just pretend to do so.")

(defvar eldev-upgrade-downgrade-mode nil
  "Downgrade installed packages if necessary.
E.g. if an unstable package has been installed at some point and
you execute `upgrade' command in this mode, the package will be
replaced with a stable version even if it is older.

Since 0.5.")

(defvar eldev-upgrade-self-script t
  "Upgrade the shell script used to start Eldev.
Script is upgraded after upgrading the package.

Since 0.9.2.")

(defvar eldev--upgrade-self-from-forced-pa nil
  "Should remain unset; used for testing.")

(defvar eldev-enable-global-package-archive-cache t
  "Whether to use global package archive cache.
Global cache is shared between all projects and Emacs versions on
your machine.  It allows to speed up project preparation by
avoiding repeated HTTP(S) queries to remote package archives to
retrieve dependency packages and common tools such as testing
frameworks or linters.")

(defvar eldev-global-cache-archive-contents-max-age 1.0
  "Expire cached `archive-contents' files after this timeout.
Value should be a number of hours, possibly fractional.  Negative
numbers mean to never use cache for `archive-contents'.  If the
value is nil, never redownload the file (except for `upgrade' or
`upgrade-self' command).")

(defvar eldev--loaded-autoloads-files nil)

(defvar eldev--package-load-paths nil)


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
  (eldev-load-project-dependencies (mapcar #'intern parameters) nil t))

(eldev-defcommand eldev-upgrade (&rest parameters)
  "Upgrade project dependencies.  If specific packages are not
requested, all project dependencies, those registered with
`eldev-add-extra-dependencies' and all additional tools used
during development (e.g. `buttercup' and various linters) are
upgraded.  However, you can list names of packages to be
upgraded.  Requirements of these package will be upgraded if
needed too.

Note that local dependencies are never installed or upgraded from
remote package archives.  If you sometimes use an “official”
release of a package and sometimes its local copy and want the
remote copy to be upgraded, make it non-local first (e.g. by
commenting out `eldev-use-local-dependency' in `Eldev-local')
before upgrading."
  :parameters     "[PACKAGE...]"
  (when (eldev-external-package-dir)
    (signal 'eldev-error `(:hint "Use global option `--isolated' (`-I')"
                                 "Cannot upgrade when using external package directory")))
  (eldev--load-installed-runtime-dependencies)
  (eldev--install-or-upgrade-dependencies 'project (mapcar #'car eldev--extra-dependencies) (or (mapcar #'intern parameters) t) eldev-upgrade-dry-run-mode nil t))

(eldev-defcommand eldev-upgrade-self (&rest parameters)
  "Upgrade Eldev itself.

This command works even if you installed Eldev from sources.
However, once Eldev is upgraded this way, your installation from
sources will be replaced with a package downloaded from MELPA."
  :works-on-old-eldev t
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (let ((package-user-dir (expand-file-name (format "%s.%s/bootstrap" emacs-major-version emacs-minor-version) eldev-dir)))
    (eldev--install-or-upgrade-dependencies 'eldev nil t eldev-upgrade-dry-run-mode nil t))
  (when eldev-upgrade-self-script
    (eldev--upgrade-self-script)))

(eldev-defbooloptions eldev-upgrade-downgrade-mode eldev-upgrade-keep-installed-mode eldev-upgrade-downgrade-mode
  ("Downgrade installed packages if necessary to use a higher priority archive"
   :options       (-d --downgrade))
  ("Keep installed packages if no higher version is available"
   :options       (-k --keep-installed))
  :for-command    (upgrade upgrade-self))

(eldev-defbooloptions eldev-upgrade-self-script eldev-upgrade-self-keep-script eldev-upgrade-self-script
  ("Also upgrade the script used to start Eldev"
   :options       (-s --upgrade-script))
  ("Don't upgrade the script"
   :options       --keep-script)
  :for-command    upgrade-self)

(eldev-defbooloptions eldev-upgrade-dry-run-mode eldev-upgrade-do-upgrade-mode eldev-upgrade-dry-run-mode
  ("Don't actually install anything, just print what would be performed"
   :options       (-n --dry-run))
  ("Do upgrade requested packages"
   :options       --do-upgrade
   :hidden-if     :default)
  :for-command    (upgrade upgrade-self))


(defun eldev-external-package-dir (&optional force)
  (let ((user-emacs-directory eldev-real-user-emacs-directory))
    (if (stringp eldev-external-package-dir)
        eldev-external-package-dir
      (when (or eldev-external-package-dir force)
        ;; Check if we can get original value like this first.
        (or (let ((package-user-dir package-user-dir))
              (require 'custom)
              (custom-theme-recalc-variable 'package-user-dir)
              package-user-dir)
            (locate-user-emacs-file "elpa"))))))


(defun eldev-load-project-dependencies (&optional additional-sets no-error-if-missing load-only)
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
NO-ERROR-IF-MISSING.

If LOAD-ONLY (since 0.6) is non-nil, project is being loaded for
side-effects only, e.g. to build a tree of its dependencies.
Otherwise it is assumed that some code of the project is going to
be executed."
  ;; Certain commands may sometimes work on too old Eldev, but then decide to load project
  ;; dependencies, which of course fails.
  (when eldev-too-old
    (signal 'eldev-too-old eldev-too-old))
  (setf additional-sets (eldev-listify additional-sets))
  (run-hook-with-args 'eldev-before-loading-dependencies-hook (if load-only 'load-only t) additional-sets)
  (eldev--install-or-upgrade-dependencies 'project additional-sets nil nil t nil no-error-if-missing)
  (run-hook-with-args 'eldev-load-dependencies-hook (if load-only 'load-only t) additional-sets))

(defun eldev-load-extra-dependencies (sets &optional no-error-if-missing)
  "Load extra dependencies, but without normal project's dependencies.
This is exactly like `eldev-load-project-dependencies' except
that the project itself and its normal dependencies are not
loaded.  Mostly useful to load runtime dependencies.

Since 0.2."
  (setf sets (eldev-listify sets))
  (run-hook-with-args 'eldev-before-loading-dependencies-hook nil sets)
  (eldev--install-or-upgrade-dependencies nil sets nil nil t nil no-error-if-missing)
  (run-hook-with-args 'eldev-load-dependencies-hook nil sets))

(defmacro eldev-using-global-package-archive-cache (&rest body)
  (declare (indent 0) (debug (body)))
  `(eldev-advised (#'url-retrieve-synchronously :around #'eldev--global-cache-url-retrieve-synchronously)
     ,@body))

(defvar url-request-method)
(defvar url-http-end-of-headers)

;; We currently cache not the packages themselves, but HTTP responses (only 200 OK) from
;; package archive servers.  Caching packages would be saner, but harder to plug into the
;; package manager.  There is also `url-cache' built-in feature, but it doesn't seem easy
;; to hack into `package'.  Easier to roll out our own.
(defun eldev--global-cache-url-retrieve-synchronously (original url &rest arguments)
  (if (and eldev-enable-global-package-archive-cache (member url-request-method '(nil "GET")))
      (let* (filename
             archive-url
             (cache-dir  (when (string-match-p (rx bos (or "http://" "https://")) url)
                           (catch 'dir
                             (dolist (archive package-archives)
                               (setf archive-url (cdr archive))
                               (when (string-prefix-p archive-url url)
                                 (setf filename (substring url (length archive-url)))
                                 ;; Don't try to cache funny names, especially if it
                                 ;; somehow includes a slash.
                                 (when (string-match-p (rx bos (1+ (any "a-zA-Z0-9" "-" ".+_@")) eos) filename)
                                   (dolist (known eldev--known-package-archives)
                                     (let ((known-archive-url (cdr (nth 1 known))))
                                       ;; Because of stable/unstable archive, URL is, in
                                       ;; fact, not necessarily an URL.
                                       (when (and (stringp known-archive-url) (string= archive-url known-archive-url))
                                         (throw 'dir (symbol-name (car known))))))
                                   ;; Contents of other archives also gets cached, but
                                   ;; with encoded URL as directory name in case different
                                   ;; projects use the same archive under different names
                                   ;; or different archives under the same name.
                                   (throw 'dir (url-hexify-string archive-url))))))))
             (cache-path (when cache-dir (expand-file-name (format "%s/%s" cache-dir filename) (eldev-global-package-archive-cache-dir)))))
        (if cache-path
            ;; Modification time; mnemonic-name functions are too new.
            (let ((modification-time (nth 5 (file-attributes cache-path)))
                  (max-age           (when (member filename '("archive-contents" "archive-contents.sig"))
                                       eldev-global-cache-archive-contents-max-age))
                  cached)
              (cond ((null modification-time)
                     (eldev-trace "File `%s' is not cached, retrieving..." url))
                    ((and max-age (<= max-age 0))
                     (eldev-trace "Updating cached file `%s'..." url))
                    ;; `max-age' is in hours.
                    ((and max-age (> (- (float-time) (float-time modification-time)) (* max-age 3600)))
                     (eldev-trace "File `%s' is cached, but is too old, refreshing..." url))
                    (t
                     (eldev-trace "Using file `%s' from the global cache (at `%s')..." url cache-path)
                     (setf cached t)))
              (if cached
                  ;; This name is not exactly the same as that generated by
                  ;; `url-retrieve-synchronously', but it shouldn't matter.
                  (with-current-buffer (generate-new-buffer (format " *%s" url))
                    (insert-file-contents-literally cache-path)
                    ;; Hacks!
                    (eval-and-compile (require 'url-http))
                    (set (make-local-variable 'url-http-end-of-headers) (point-max))
                    (make-local-variable 'url-http-response-version)
                    (make-local-variable 'url-http-response-status)
                    (url-http-parse-response)
                    (current-buffer))
                (let ((buffer (apply original url arguments)))
                  (when buffer
                    (with-current-buffer buffer
                      ;; Only cache successful replies.  In particular, if the archive
                      ;; doesn't provide `.sig' files (e.g. MELPA doesn't), we don't cache
                      ;; 404 responses.  Emacs will keep retrying, but its default is to
                      ;; allow unsigned packages.
                      (when (and (boundp 'url-http-response-status) (eq url-http-response-status 200))
                        (make-directory (file-name-directory cache-path) t)
                        (let ((coding-system-for-write 'binary))
                          (write-region nil nil cache-path nil 'no-message))))
                    ;; Discard any cached signatures.
                    (unless (string-suffix-p ".sig" filename)
                      (ignore-errors (delete-file (concat cache-path ".sig")))))
                  buffer)))
          (eldev-trace "Accessing unexpected URL `%s': not caching" url)
          (apply original url arguments)))
    (apply original url arguments)))

;; `package-compute-transaction' and friends are not enough in our case, mostly because of
;; local dependencies that can change unpredictably and also requirement that certain
;; dependencies are installed only from certain archives.  Roll our own.
(defun eldev--install-or-upgrade-dependencies (core additional-sets to-be-upgraded dry-run activate main-command-effect &optional no-error-if-missing)
  ;; See comments in `eldev-cli'.
  (let* ((eldev-message-rerouting-destination :stderr)
         (self                              (eq core 'eldev))
         (external-dir                      (unless self (eldev-external-package-dir)))
         (package-name                      (if self 'eldev (package-desc-name (eldev-package-descriptor))))
         ;; The following global variables will be altered inside the `let' form.
         (package-archives                  package-archives)
         (package-archive-priorities        (when (boundp 'package-archive-priorities) package-archive-priorities))
         (eldev--package-archive-priorities eldev--package-archive-priorities)
         (package-pinned-packages           package-pinned-packages)
         (uninstallable-cache-file          (expand-file-name "uninstallable.list" (eldev-cache-dir t)))
         plan
         default-archives
         all-packages
         uninstallable-list-changed)
    (eldev--lazy-scope (uninstallable
                        (eldev-do-load-cache-file uninstallable-cache-file "list of uninstallable optional packages" 1
                          (assq 'packages contents))
                        (when uninstallable-list-changed
                          (eldev-do-save-cache-file uninstallable-cache-file "list of uninstallable optional packages" 1
                            `((packages . ,(funcall uninstallable))))))
      (if self
          (setf package-archives `(,(if eldev--upgrade-self-from-forced-pa
                                        `("bootstrap-pa" . ,(file-name-as-directory eldev--upgrade-self-from-forced-pa))
                                      (eldev--resolve-package-archive (if eldev-prefer-stable-archives 'melpa-stable 'melpa-unstable))))
                all-packages     '((:package eldev)))
        (eldev--create-internal-pseudoarchive-descriptor)
        (push `(,eldev--internal-pseudoarchive . ,(file-name-as-directory (eldev--internal-pseudoarchive-dir))) package-archives)
        (setf default-archives package-archives)
        (when (eq core 'project)
          (push `(:package ,package-name :archives ,default-archives) all-packages))
        (dolist (set (eldev-listify additional-sets))
          (let ((extra-dependencies (cdr (assq set eldev--extra-dependencies)))
                not-retrying-optionals)
            (setf extra-dependencies
                  (eldev-filter (let ((dependency-name (plist-get it :package)))
                                  (not (when (and (not (or (eq to-be-upgraded t) (memq dependency-name to-be-upgraded)))
                                                  (plist-get it :optional) (memq dependency-name (funcall uninstallable)))
                                         (eldev-verbose "Dependency `%s' is optional and has been found uninstallable before, skipping" dependency-name)
                                         (setf not-retrying-optionals t))))
                                (mapcar #'eldev--create-package-plist extra-dependencies)))
            (when not-retrying-optionals
              (eldev-trace "Use `eldev upgrade' to force checking if the skipped dependencies are still uninstallable"))
            (when extra-dependencies
              (eldev-verbose "Need the following extra dependencies for set `%s': %s" set
                             (eldev-message-enumerate nil extra-dependencies
                                                      (lambda (plist)
                                                        (eldev-format-message "%s %s" (plist-get plist :package) (eldev-message-version (plist-get plist :version))))
                                                      t))
              (setf all-packages (append all-packages extra-dependencies))
              (let ((eldev-verbosity-level nil))
                (dolist (plist extra-dependencies)
                  (dolist (archive (eldev--package-plist-get-archives plist))
                    (eldev-use-package-archive archive))))))))
      (eldev--adjust-stable/unstable-archive-priorities)
      (let ((archive-statuses     (or (unless external-dir (eldev--determine-archives-to-fetch to-be-upgraded t)) '((nil . t))))
            (all-package-archives package-archives))
        (while (catch 'refetch-archives
                 (package-load-all-descriptors)
                 (unless self
                   ;; This removes local dependencies that have been installed as packages
                   ;; (e.g. because `Eldev-local' used to be different) from
                   ;; `package-alist'.  Otherwise package manager will prefer the installed
                   ;; versions even if we pin the packages to our pseudoarchive.  This
                   ;; doesn't quite feel right, but I don't see a better way.
                   (setf package-alist (eldev-filter (null (eldev--loading-mode (car it))) package-alist)))
                 ;; Retry for as long as we have archives to fetch contents of.
                 (let ((pass-archive-statuses archive-statuses))
                   (while pass-archive-statuses
                     (let ((next-archive (pop pass-archive-statuses)))
                       (unless (cdr next-archive)
                         (eldev--fetch-archive-contents `(,(car next-archive)) to-be-upgraded)
                         (setf (cdr next-archive) 'fetched-now))
                       ;; Don't use archives we haven't fetched yet.
                       (setf package-archives (eldev-filter (not (assoc it pass-archive-statuses)) all-package-archives))
                       (package-read-all-archive-contents)
                       (condition-case error
                           (let ((considered           (make-hash-table :test #'eq))
                                 (highest-requirements (make-hash-table :test #'eq)))
                             ;; If during planning we find a situation where package X is
                             ;; required at a higher version than what has been considered
                             ;; already, remember this and restart planning.  This looks
                             ;; like a cleaner way than trying to rewrite what has been
                             ;; planned already.  See test `eldev-issue-18' for an example
                             ;; of such situation.
                             (while (catch 'restart-planning
                                      (setf plan (cons nil nil))
                                      (dolist (package-plist all-packages)
                                        (let ((dependency-name  (plist-get package-plist :package))
                                              (can-be-installed (catch 'skip-uninstallable-optionals
                                                                  (eldev--plan-install-or-upgrade self to-be-upgraded package-plist default-archives plan
                                                                                                  (and pass-archive-statuses (not eldev-upgrade-downgrade-mode))
                                                                                                  considered highest-requirements))))
                                          (when (plist-get package-plist :optional)
                                            (let* ((list-of-uninstallable (funcall uninstallable))
                                                   (was-uninstallable     (memq dependency-name list-of-uninstallable)))
                                              (when (eq (not was-uninstallable) (not can-be-installed))
                                                (funcall uninstallable (if can-be-installed
                                                                           (delq dependency-name (funcall uninstallable))
                                                                         `(,dependency-name . ,list-of-uninstallable)))
                                                (setf uninstallable-list-changed t))))))))
                             (when pass-archive-statuses
                               (eldev-trace "Not fetching contents of other archive(s) as redundant")
                               (setf pass-archive-statuses nil)))
                         (eldev-error (unless pass-archive-statuses
                                        ;; If we don't have anything more to fetch, give up.
                                        (if no-error-if-missing
                                            (eldev-verbose "%s" (error-message-string error))
                                          (signal (car error) (cdr error)))))))))
                 (let ((planned-packages    (nreverse (car plan)))
                       (up-to-date-packages (cdr plan))
                       (non-local-plan-size 0)
                       (dependency-index    0)
                       unknown-packages
                       refetching-wanted)
                   (when (and to-be-upgraded (not (eq to-be-upgraded t)))
                     (dolist (package-to-be-upgraded to-be-upgraded)
                       (unless (or (eldev-any-p (eq (package-desc-name (car it)) package-to-be-upgraded) planned-packages)
                                   (memq package-to-be-upgraded up-to-date-packages))
                         (push package-to-be-upgraded unknown-packages)))
                     (when unknown-packages
                       (signal 'eldev-error `(:hint ,(unless self `("Check output of `%s dependency-tree'" ,(eldev-shell-command t)))
                                                    "Cannot upgrade %s: `%s' has no such dependencies" ,(eldev-message-enumerate "package" (nreverse unknown-packages)) ,package-name))))
                   (dolist (dependency planned-packages)
                     (when (or self (null (eldev--loading-mode (car dependency))))
                       (setf non-local-plan-size (1+ non-local-plan-size))
                       ;; If we determine that archive X has to be used for installing or
                       ;; upgrading, always fetch more prioritized archives first: maybe we
                       ;; skipped them only because cached contents files are too old.
                       (let ((used-archive (package-desc-archive (car dependency)))
                             (archive-scan archive-statuses))
                         (while archive-scan
                           (let ((archive (pop archive-scan)))
                             (cond ((string= used-archive (caar archive))
                                    (setf archive-scan nil))
                                   ((not (eq (cdr archive) 'fetched-now))
                                    (setf (cdr archive) nil
                                          refetching-wanted t)
                                    (eldev-verbose "Will refetch contents of package archive `%s' to make sure that `%s' really needs to be used" (caar archive) used-archive))))))))
                   (when refetching-wanted
                     (throw 'refetch-archives t))
                   (when (and dry-run (> non-local-plan-size 0))
                     (eldev-verbose "In dry-run mode Eldev only pretends it is upgrading, installing or deleting dependencies"))
                   (dolist (dependency planned-packages)
                     (let* ((previous-version (cdr dependency))
                            (dependency       (car dependency))
                            (dependency-name  (package-desc-name dependency)))
                       (if (and (not self) (eldev--loading-mode dependency))
                           (eldev--load-local-dependency dependency)
                         (setf dependency-index (1+ dependency-index))
                         (if previous-version
                             (eldev-print :stderr (if (version-list-< (package-desc-version dependency) (package-desc-version previous-version))
                                                      "[%d/%d] Downgrading package `%s' (%s -> %s) from `%s' (to use a better package archive)..."
                                                    "[%d/%d] Upgrading package `%s' (%s -> %s) from `%s'...")
                                          dependency-index non-local-plan-size
                                          (eldev-colorize dependency-name 'name) (eldev-message-version previous-version t) (eldev-message-version dependency t)
                                          (package-desc-archive dependency))
                           (eldev-print :stderr "[%d/%d] Installing package `%s' %s from `%s'..."
                                        dependency-index non-local-plan-size
                                        (eldev-colorize dependency-name 'name) (eldev-message-version dependency t t) (package-desc-archive dependency)))
                         (unless dry-run
                           (let ((inhibit-message t))
                             (eldev--with-pa-access-workarounds (lambda ()
                                                                  (eldev-using-global-package-archive-cache
                                                                    (condition-case error
                                                                        (let ((original-load-path load-path))
                                                                          (unwind-protect
                                                                              (progn
                                                                                ;; See `eldev-upgrade-self-new-macros-1'.
                                                                                (when (eq dependency-name 'eldev)
                                                                                  (eldev--unload-self))
                                                                                ;; Ugly workaround for receiving 400 Bad Request responses from GNU
                                                                                ;; ELPA in poorly understood circumstances, see issue #52.
                                                                                ;; FIXME: Find a better workaround?
                                                                                (eval-and-compile (require 'url-http))
                                                                                (when (and (< emacs-major-version 27)
                                                                                           (boundp 'url-http-open-connections)
                                                                                           (hash-table-p url-http-open-connections))
                                                                                  (clrhash url-http-open-connections))
                                                                                (package-install-from-archive dependency))
                                                                            (when (eq dependency-name 'eldev)
                                                                              ;; Reload the current package again, so that we
                                                                              ;; don't mix old and new function invocations.
                                                                              (eldev--unload-self)
                                                                              (let ((load-path original-load-path))
                                                                                (require 'eldev)))))
                                                                      (file-error (let ((url (eldev--guess-url-from-file-error error)))
                                                                                    (when (stringp url)
                                                                                      (dolist (archive archive-statuses)
                                                                                        (when (and (string= (caar archive) (package-desc-archive dependency))
                                                                                                   (string-prefix-p (cdar archive) url))
                                                                                          ;; It is possible that package archive contents is cached and is deemed
                                                                                          ;; up-to-date, but some packages cannot be fetched.  In this case, refresh the
                                                                                          ;; contents file (but don't do it more than once).
                                                                                          (when (eq (cdr archive) 'fetched-now)
                                                                                            (signal 'eldev-error `(:hint ,(eldev-format-message "When accessing package archive `%s'" (caar archive))
                                                                                                                         "%s" ,(error-message-string error))))
                                                                                          (eldev-print :stderr "Contents of package archive `%s' seems out-of-date, will refetch..." (caar archive))
                                                                                          (setf (cdr archive) nil)
                                                                                          (throw 'refetch-archives t)))))
                                                                                  (signal (car error) (cdr error))))))
                                                                t))))))
                   (if (> non-local-plan-size 0)
                       (when (memq 'runtime (eldev-listify additional-sets))
                         (eldev--save-installed-runtime-dependencies))
                     (let ((installed-how (if external-dir (eldev-format-message "in `%s'" external-dir) "already")))
                       (if additional-sets
                           (eldev-verbose "All project dependencies (including those for %s) have been installed %s or are local"
                                          (eldev-message-enumerate "set" additional-sets) installed-how)
                         (eldev-verbose "All project dependencies have been installed %s or are local" installed-how))))
                   (when main-command-effect
                     (unless dry-run
                       (let ((inhibit-message t)
                             (num-deleted     0))
                         (dolist (dependency planned-packages)
                           (unless (or (null (cdr dependency)) (and (not self) (eldev--loading-mode (car dependency))))
                             ;; Argument FORCE was added only in 25.x.  Always force package
                             ;; deletion, otherwise package manager won't let us downgrade
                             ;; dependencies when we choose to do so.
                             (apply #'package-delete (cdr dependency) (when (>= emacs-major-version 25) '(t)))
                             (setf num-deleted (1+ num-deleted))))
                         (when (> num-deleted 0)
                           (eldev-verbose "Deleted %s" (eldev-message-plural num-deleted (if self "obsolete package version" "obsolete dependency version"))))))
                     (if (> non-local-plan-size 0)
                         (eldev-print "Upgraded or installed %s" (eldev-message-plural non-local-plan-size (if self "package" "dependency package")))
                       (eldev-print (if self "Eldev is up-to-date" "All dependencies are up-to-date")))))
                 nil))))
    (when activate
      (eldev--do-activate-dependencies package-name all-packages additional-sets no-error-if-missing)
      ;; See e.g. `eldev-test-utility-files-in-package-mode-1'.  Basically, test files
      ;; cannot be `require'd if `eldev-project-dir' is not in `load-path', which happens
      ;; if the project is loaded in packaged mode.  Always add the directory, regardless
      ;; of command (e.g. it is feasible that someone would require a test utility feature
      ;; from `eval' command), but at _the end_.
      (when (eq core 'project)
        (add-to-list 'load-path eldev-project-dir t)))))

(defun eldev--unload-self ()
  ;; We don't really unload ourselves, because this causes hard to understand problems due
  ;; to hooks etc., even if we don't directly call any functions before loading again.
  ;; However, the intent is not really to unload, but rather to _reload_ the package, so
  ;; just removing the features from the list of loaded ones should be enough.
  (setf features (eldev-filter (not (string-match-p (rx bol "eldev" (? "-" (1+ any)) eol) (symbol-name it))) features)))

(defun eldev--do-activate-dependencies (package-name dependencies additional-sets no-error-if-missing)
  ;; Also add the additional loading roots here.
  (eldev--inject-loading-roots additional-sets)
  (let (recursing-for
        missing-dependency)
    (eldev-advised (#'package-activate-1
                    :around (lambda (original dependency &rest rest)
                              (let ((inhibit-message nil))
                                (catch 'exit
                                  (let* ((dependency-name (package-desc-name dependency))
                                         (recursing       (memq dependency-name recursing-for))
                                         (description     (eldev-format-message "%s package `%s'"
                                                                                (cond ((eq dependency-name package-name)               "project")
                                                                                      ((or recursing (eldev--loading-mode dependency)) "local dependency")
                                                                                      (t                                               "dependency"))
                                                                                dependency-name)))
                                    (eldev-pcase-exhaustive (unless recursing (eldev--loading-mode dependency))
                                      (`nil
                                       (eldev-trace "Activating %s..." description)
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
                                       (let* ((load-path-before load-path)
                                              (package-dir      (if (eq dependency-name package-name)
                                                                    eldev-project-dir
                                                                  ;; 2 and 3 stand for directory name and its absolute path.
                                                                  (eldev-trace "Activating %s in directory `%s'" description (nth 2 (assq dependency-name eldev--local-dependencies)))
                                                                  (nth 3 (assq dependency-name eldev--local-dependencies)))))
                                         ;; Use package's autoloads file if it is present.  At this
                                         ;; stage we never generate anything: only use existing files.
                                         (eldev--load-autoloads-file (expand-file-name (format "%s-autoloads.el" dependency-name) package-dir))
                                         ;; For non-ancient packages, autoloads file is supposed to
                                         ;; modify `load-path'.  But if there is no such file, or it
                                         ;; doesn't do that for whatever reason, do it ourselves.
                                         (when (and (eq load-path load-path-before) (not (member package-dir load-path)))
                                           (push package-dir load-path))
                                         (eldev--assq-set dependency-name package-dir eldev--package-load-paths))
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
                                               (progn (eldev-trace "%s hasn't changed since last installation, no need to reinstall" (eldev-message-upcase-first description))
                                                      (eldev--assq-set dependency-name `(,up-to-date-desc) package-alist)
                                                      (package-activate dependency-name))
                                             (eldev-trace "(Re)installing %s..." description)
                                             (eldev-install-package-file (nth 1 generated-package))))
                                         (pop recursing-for)))))))))
      (dolist (plist dependencies)
        (unless (package-activate (plist-get plist :package))
          (if (plist-get plist :optional)
              (eldev-trace "Failed to activate `%s', but as the package is optional, just continuing" (plist-get plist :package))
            ;; We don't report the required version, but if you look at
            ;; `package-activate-1' (as of 2019-11-24), it also has problems with
            ;; versions.
            (if no-error-if-missing
                (eldev-verbose "Unable to load project dependencies: package `%s' is unavailable" missing-dependency)
              (signal 'eldev-missing-dependency `("Unable to load project dependencies: package `%s' is unavailable" ,missing-dependency)))))))))

(defun eldev--create-package-plist (package &optional default-archives optional)
  (setf package (pcase package
                  ((pred symbolp)                   `(:package ,package))
                  (`(,(pred symbolp))               `(:package . ,package))
                  (`(,(pred symbolp) ,(pred listp)) `(:package ,(car package) :version ,(cadr package)))
                  (_                                (copy-sequence package))))
  (let ((tool (plist-get package :tool)))
    (when tool
      (unless (equal package `(:tool ,tool))
        (signal 'eldev-error `("Invalid package specification `%S': `:tool' may not be combined with other keywords" ,package)))
      (let ((specification (cdr (assq tool eldev-known-tool-packages))))
        (unless specification
          (signal 'eldev-error `("Unknown tool `%s'" ,tool)))
        (setf package `(:package ,tool ,@specification)))))
  (unless (and (listp package) (plist-get package :package))
    (signal 'eldev-error `("Invalid package specification `%S': there is no package name in it" ,package)))
  (unless (or (null default-archives) (plist-member package :archive) (plist-member package :archives))
    (setf package (plist-put package :archives default-archives)))
  (when optional
    (setf package (plist-put package :optional t)))
  package)

(defun eldev--package-plist-get-archives (package-plist &optional resolved)
  (mapcar (if resolved #'eldev--resolve-package-archive #'identity)
          (or (eldev-listify (plist-get package-plist :archives))
              (let ((archive (plist-get package-plist :archive)))
                (when archive
                  (list archive))))))

;; Returns non-nil.
(defun eldev--plan-install-or-upgrade (self to-be-upgraded package-plist default-archives plan fail-if-too-new considered highest-requirements &optional required-by)
  (let* ((package-name        (plist-get package-plist :package))
         (required-version    (plist-get package-plist :version))
         (optional            (plist-get package-plist :optional))
         (highest-requirement (gethash package-name highest-requirements))
         (real-required-by    required-by)
         (required-by-hint    (lambda (&optional upcase)
                                (when required-by
                                  (let ((hint (eldev-format-message "required by package %s"
                                                                    (mapconcat (lambda (package) (eldev-format-message "`%s'" package)) real-required-by " <- "))))
                                    (if upcase
                                        (eldev-message-upcase-first hint)
                                      hint)))))
         (considered-version  (gethash package-name considered)))
    (when (stringp required-version)
      (setf required-version (version-to-list required-version)))
    ;; Elevate requirement if needed.
    (when (version-list-< required-version (car highest-requirement))
      (setf required-version (car highest-requirement)
            real-required-by (cdr highest-requirement)))
    (when (and considered-version (version-list-< considered-version required-version))
      (puthash package-name (cons required-version real-required-by) highest-requirements)
      ;; Unlike `considered', `highest-requirements' must not be cleared between passes.
      (clrhash considered)
      (throw 'restart-planning t))
    (unless (and considered-version (version-list-<= required-version considered-version))
      (unless (package-built-in-p package-name required-version)
        (when (eq package-name 'emacs)
          (when optional
            (eldev-verbose "Emacs version %s (this is %s) is %s; skipping the optional dependency"
                           (eldev-message-version required-version) emacs-version (funcall required-by-hint))
            (throw 'skip-uninstallable-optionals nil))
          (signal 'eldev-missing-dependency `(:hint ,(funcall required-by-hint t)
                                                    "Emacs version %s is required (this is version %s)" ,(eldev-message-version required-version) ,emacs-version)))
        (let* ((local                     (and (not self) (eldev--loading-mode package-name)))
               (already-installed         (unless local (eldev-find-package-descriptor package-name)))
               (already-installed-version (when already-installed (package-desc-version already-installed)))
               (already-installed-too-old (version-list-< already-installed-version required-version))
               (upgrading                 (or (eq to-be-upgraded t) (memq package-name to-be-upgraded)))
               (package                   (unless (or upgrading already-installed-too-old) already-installed))
               (archives                  (eldev--package-plist-get-archives package-plist t)))
          (unless package
            ;; Not installed, installed not in the version we need or to be upgraded.
            (let* ((best-version     (unless (or eldev-upgrade-downgrade-mode fail-if-too-new) already-installed-version))
                   (best-preferred   nil)
                   (best-priority    most-negative-fixnum)
                   (built-in-version (eldev-find-built-in-version package-name))
                   (available        (cdr (assq package-name package-archive-contents)))
                   package-disabled)
              (when (version-list-< best-version built-in-version)
                (setf best-version built-in-version))
              (while available
                (let* ((candidate (pop available))
                       (archive   (package-desc-archive candidate))
                       (version   (package-desc-version candidate))
                       (preferred (eldev--stable/unstable-preferred-archive (eldev--find-simple-archive archives archive)))
                       (priority  (eldev-package-archive-priority archive))
                       (disabled  (package-disabled-p package-name version)))
                  ;; Make sure we don't install a package from a wrong archive.
                  (when (if local
                            (string= archive eldev--internal-pseudoarchive)
                          (or (null archives) (eldev--find-simple-archive archives archive)))
                    (cond ((version-list-< version required-version)
                           (when (version-list-< best-version version)
                             (setf best-version version)))
                          (disabled
                           (unless package-disabled
                             (setf package-disabled (if (stringp disabled)
                                                        `("Dependency `%s' is held at version %s, but version %s is required"
                                                          ,package-name ,disabled ,(eldev-message-version version))
                                                      `("Dependency `%s' is disabled" ,package-name)))))
                          ;; On Emacs 24 candidates are not sorted by archive priority, so
                          ;; the comparison must not assume any particular order.
                          ;;
                          ;; Conditions:
                          ;; - use preferred (stable or unstable, as requested) archives if possible;
                          ;; - use higher priority archives among preferred ones;
                          ;; - use the highest available version from archives determined as above.
                          ((or (and preferred (not best-preferred))
                               (< best-priority priority)
                               (and (= best-priority priority) (version-list-< best-version version)))
                           (setf package        candidate
                                 best-version   version
                                 best-priority  priority
                                 best-preferred preferred))))))
              (when (and already-installed-version fail-if-too-new (version-list-< best-version already-installed-version))
                ;; This error should be caught at an upper level.
                (signal 'eldev-missing-dependency `("Installed version of dependency `%s' is suspiciously new" ,package-name)))
              (when (and upgrading (null best-version) already-installed-version)
                ;; Should be caught at an upper level, unless the package archive is gone.
                (signal 'eldev-missing-dependency `("Dependency `%s' was once installed, but is not available anymore" ,package-name)))
              (unless (or (version-list-< already-installed-version best-version)
                          already-installed-too-old
                          (and eldev-upgrade-downgrade-mode best-version (not (version-list-= already-installed-version best-version))))
                (setf package already-installed))
              (unless package
                (when optional
                  (eldev-verbose "Dependency `%s' is not available, but as it is optional, just continuing" package-name)
                  (let ((hint (funcall required-by-hint)))
                    (when hint
                      (eldev-trace "Note: it is %s" hint)))
                  (throw 'skip-uninstallable-optionals nil))
                (let* ((version-string (eldev-message-version required-version t))
                       (external-dir   (unless self (eldev-external-package-dir)))
                       (hint           (funcall required-by-hint t))
                       (message        (or package-disabled
                                           (cond ((and best-version (not (eq best-version built-in-version)))
                                                  (if external-dir
                                                      `("Dependency `%s' version %s is required, but only %s is installed in `%s'"
                                                        ,package-name ,version-string ,(eldev-message-version best-version) ,external-dir)
                                                    `("Dependency `%s' version %s is required, but at most %s is available"
                                                      ,package-name ,version-string ,(eldev-message-version best-version))))
                                                 (built-in-version
                                                  `("Dependency `%s' is built-in, but required version %s is too new (only %s available)"
                                                    ,package-name ,version-string ,(eldev-message-version built-in-version)))
                                                 (external-dir
                                                  `("Dependency `%s' is not installed in `%s'" ,package-name ,external-dir))
                                                 (t
                                                  `("Dependency `%s' is not available" ,package-name))))))
                  (when external-dir
                    (setf hint (concat (eldev-format-message "Either install it there, or use global option `--isolated' (`-I')") (if hint (concat "\n" hint) ""))))
                  (signal 'eldev-missing-dependency `(:hint ,hint ,@message))))))
          (dolist (requirement (package-desc-reqs package))
            (eldev--plan-install-or-upgrade self to-be-upgraded (eldev--create-package-plist requirement (or archives default-archives) optional)
                                            default-archives plan fail-if-too-new considered highest-requirements (cons package-name required-by)))
          (if (eq package already-installed)
              (push package-name (cdr plan))
            (push `(,package . ,already-installed) (car plan)))))
      (puthash package-name (or required-version `(,most-negative-fixnum)) considered))))

(defun eldev--upgrade-self-script ()
  (let ((scripts (list (eldev--shell-script-name))))
    ;; Special case for Windows, where we actually have _two_ scripts.
    (when (string= (car scripts) "eldev.bat")
      (push "eldev.ps1" scripts))
    (dolist (script scripts)
      (catch 'continue
        (let* ((installed      (eldev-shell-command))
               (installed-name (file-name-nondirectory installed)))
          (unless (string= installed-name script)
            (if (member installed-name scripts)
                (setf installed (expand-file-name script (file-name-directory installed)))
              (eldev-warn "Cannot locate installed script `%s', skipping" script)
              (throw 'continue nil)))
          (let* ((abbreviated    (abbreviate-file-name installed))
                 (updated-file   (locate-file (format "bin/%s" script) load-path))
                 (updated-script (with-temp-buffer
                                   (eldev-trace "Reading updated script from file `%s'" updated-file)
                                   (insert-file-contents updated-file)
                                   (buffer-string))))
            (with-temp-buffer
              (insert-file-contents installed t)
              (if (string= (buffer-string) updated-script)
                  (eldev-verbose "Installed script `%s' appears to be up-to-date" abbreviated)
                (erase-buffer)
                (insert updated-script)
                (condition-case error
                    (eldev--silence-file-writing-message installed
                      (save-buffer))
                  (error (signal 'eldev-error (append (unless (file-writable-p installed) '(:hint "You probably need to log in as root"))
                                                      (list (eldev-format-message "When updating script `%s': %s" abbreviated (error-message-string error)))))))
                (eldev-print "Upgraded script `%s'" abbreviated)))))))))

(defun eldev--internal-pseudoarchive-dir (&optional ensure-exists)
  (let ((dir (eldev--package-archive-dir eldev--internal-pseudoarchive)))
    (when ensure-exists
      (make-directory dir t))
    dir))

(defun eldev--package-archive-dir (archive &optional base-dir)
  (expand-file-name archive (expand-file-name "archives" (or base-dir package-user-dir))))

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

(defun eldev--determine-archives-to-fetch (&optional refetch-contents return-all)
  "Return a list of archives that need to be (re)fetched.
If RETURN-ALL is non-nil, return a list of (ARCHIVE . UP-TO-DATE)
for all archives instead."
  (let (result)
    (dolist (archive (sort (copy-sequence package-archives)
                           (lambda (a b) (or (and (eldev--stable/unstable-preferred-archive a)
                                                  (not (eldev--stable/unstable-preferred-archive b)))
                                             (> (eldev-package-archive-priority (car a)) (eldev-package-archive-priority (car b)))))))
      (unless (string= (car archive) eldev--internal-pseudoarchive)
        (let (up-to-date)
          ;; I don't see a way to find if package archive contents is fetched already
          ;; without going into internals.
          (when (file-exists-p (expand-file-name "archive-contents" (eldev--package-archive-dir (car archive))))
            (if refetch-contents
                (eldev-trace "Will refetch contents of package archive `%s' in case it has changed" (car archive))
              (setf up-to-date t)
              (eldev-trace "Contents of package archive `%s' has been fetched already" (car archive))))
          (if return-all
              (push `(,archive . ,up-to-date) result)
            (unless up-to-date
              (push archive result))))))
    (nreverse result)))

(defun eldev--fetch-archive-contents (archives &optional refetch-contents)
  (when archives
    (eldev-verbose "Fetching contents of %s..." (eldev-message-enumerate "package archive" archives #'car))
    ;; See comments in `eldev-cli'.
    (let ((eldev-message-rerouting-destination         :stderr)
          (eldev-global-cache-archive-contents-max-age (if refetch-contents -1 eldev-global-cache-archive-contents-max-age))
          (package-archives                            archives)
          (inhibit-message                             t)
          failure)
      (eldev--with-pa-access-workarounds (lambda ()
                                           (eldev-using-global-package-archive-cache
                                             ;; Emacs package system eats up all errors in `package-refresh-contents',
                                             ;; unless in debug mode.  Try to work around that and issue a nice error.
                                             (eldev-advised (#'package--download-one-archive
                                                             :around (lambda (original archive &rest arguments)
                                                                       (unless failure
                                                                         (condition-case-unless-debug error
                                                                             (apply original archive arguments)
                                                                           (error (setf failure (cons error (if (consp archive) (car archive) archive))))))))
                                               (package-refresh-contents)
                                               (when failure
                                                 (signal 'eldev-error `(:hint ,(eldev-format-message "When updating contents of package archive `%s'" (cdr failure))
                                                                              ,(error-message-string (car failure))))))))))))

(defun eldev--with-pa-access-workarounds (callback &optional call-after-working-around)
  ;; This is only to reduce noise from old Emacs versions a bit.
  (eldev-advised (#'write-region :around (when (< emacs-major-version 25)
                                           (lambda (original start end filename &optional append visit &rest arguments)
                                             (apply original start end filename append
                                                    (or visit (when (file-in-directory-p filename (eldev-cache-dir nil)) 'no-message))
                                                    arguments))))
    (eval-and-compile (require 'gnutls))
    ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36749#8 and
    ;; https://github.com/magit/ghub/pull/90/files.  Basically, this is a workaround for a
    ;; bug in older Emacs versions for TLS1.3 support, but it can be activated only if
    ;; GnuTLS is new enough to know about this TLS version.
    (let ((gnutls-algorithm-priority (or gnutls-algorithm-priority
                                         (when (and (version<= emacs-version "26.2") (boundp 'libgnutls-version) (>= libgnutls-version 30603))
                                           "NORMAL:-VERS-TLS1.3"))))
      (when (catch 'eldev--bad-signature
              (eldev-advised ('package--check-signature-content :around
                                                                (lambda (original &rest arguments)
                                                                  (condition-case nil
                                                                      (apply original arguments)
                                                                    ;; If we don't convert it into a thrown tag, the error will be
                                                                    ;; eaten e.g. in `package--download-and-read-archives'.
                                                                    (bad-signature (throw 'eldev--bad-signature t)))))
                (funcall callback))
              nil)
        ;; This probably largely defeats the purpose of signatures, but it is basically what
        ;; `gnu-elpa-keyring-update' itself proposes (only perhaps not in an automated way).
        (eldev-trace "Installing package `gnu-elpa-keyring-update' to hopefully solve `bad-signature' problem...")
        (let ((package-check-signature nil))
          (eldev-using-global-package-archive-cache
            (package-refresh-contents))
          (package-install 'gnu-elpa-keyring-update))
        (when call-after-working-around
          (funcall callback))))))

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

(defun eldev--load-autoloads-file (file)
  (when (file-exists-p file)
    (eldev-trace "Loading file `%s'" (file-relative-name file eldev-project-dir))
    (with-demoted-errors "Error loading autoloads: %s"
      (load file nil t)
      (push file eldev--loaded-autoloads-files))))

(defun eldev--load-local-dependency (dependency)
  (let* ((loading-mode    (eldev--loading-mode dependency))
         (dependency-name (package-desc-name dependency))
         (package-name    (package-desc-name (eldev-package-descriptor)))
         (project-itself  (eq dependency-name package-name))
         (dependency-dir  (if project-itself eldev-project-dir (nth 3 (assq dependency-name eldev--local-dependencies))))
         ;; For project itself autoloads are handled differently.  For other loading mode
         ;; they get built without special care.
         (build-autoloads (when (and (not project-itself) (memq loading-mode '(as-is source byte-compiled)))
                            (eldev--cross-project-internal-eval dependency-dir '(not (null (memq 'autoloads (eldev-active-plugins)))) t))))
    (when (cdr (assq dependency-name package-alist))
      (error "Local dependency `%s' is already listed in `package-alist'" dependency-name))
    ;; FIXME: Special-case project itself: no need to launch separate process(es) for it.
    ;; I don't want to move this into an alist, to avoid fixing the way it works.
    (let ((commands (eldev-pcase-exhaustive loading-mode
                      (`as-is               (when build-autoloads `(("build" ":autoloads"))))
                      (`source              `(("clean" ".elc" "--set" "main" "--delete")
                                              ,@(when build-autoloads `(("build" ":autoloads")))))
                      (`byte-compiled       `(("build" ":compile" ,@(when build-autoloads `(":autoloads")))))
                      (`built               `(("build" ":default")))
                      (`built-and-compiled  `(("build" ":default" ":compile")))
                      (`built-source        `(("clean" ".elc" "--set" "main" "--delete")
                                              ("build" ":default")))
                      (`packaged            `(("package" "--output-dir" ,(expand-file-name "local/generated" (eldev-cache-dir t)) "--print-filename"))))))
      (when commands
        (if project-itself
            (eldev-verbose "Preparing to load the project in mode `%s'" loading-mode)
          (eldev-verbose "Preparing to load local dependency `%s' in mode `%s'" dependency-name loading-mode))
        (let ((default-directory dependency-dir))
          (dolist (command commands)
            (eldev-call-process (eldev-shell-command) command
              :trace-command-line (eldev-format-message "Full command line (in directory `%s')" default-directory)
              :die-on-error       (if project-itself
                                      "child Eldev process"
                                    (eldev-format-message "child Eldev process for local dependency `%s'" dependency-name))
              (eldev--forward-process-output "Output of the child Eldev process:" "Child Eldev process produced no output" t)
              (when (string= (car command) "package")
                (goto-char (point-max))
                (forward-line -2)
                (let ((point (point)))
                  (end-of-line)
                  (let ((file (buffer-substring-no-properties point (point))))
                    (forward-line)
                    (push `(,dependency-name ,file ,(looking-at "up-to-date")) eldev--local-dependency-packages))))))))
      (push `(,dependency-name . (,dependency)) package-alist))))

;; This is a hackish function only working for packages loaded in `as-is' and similar
;; mode.  See how `eldev--package-load-paths' is filled.
(defun eldev--unload-package (package-name)
  (unless (eq package-name 'eldev)
    (let ((load-path-element (cdr (assq package-name eldev--package-load-paths))))
      (unless load-path-element
        (error "Unable to unload package `%s'" package-name))
      (setf load-path              (delete load-path-element load-path)
            package-activated-list (delq package-name package-activated-list)))))

(defmacro eldev-autoinstalling-implicit-dependencies (enabled &rest body)
  "Evaluate BODY, autoinstalling implicit dependencies when needed.
If ENABLED is nil, autoinstallation is disabled (this allows to
keep BODY the same in both cases).  Currently “implicit
dependencies” include only testing frameworks other than ERT, but
that could be expanded later.

Since 0.3."
  (declare (indent 1) (debug (form body)))
  `(eldev--test-autoinstalling-framework ,enabled (lambda () ,@body)))



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
      ((and (or :briefdoc :default :only-explicit :superseded-by) keyword)
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

    :only-explicit VALUE

        If non-nil, this cleaner is not invoked by `clean all',
        i.e. it must be explicitly named.

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
      (setf cleaners (mapcar #'car (eldev-filter (eldev-get (cdr it) :default) eldev--cleaners))))
    (when all
      ;; It is important to _append_ to the list of cleaners, to handle e.g. `clean all
      ;; global-cache' properly.
      (setf cleaners (append cleaners (mapcar #'car (eldev-filter (not (eldev-get (cdr it) :only-explicit)) eldev--cleaners)))))
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
            (progn
              (eldev-output "%s\n" (eldev-colorize header 'section))
              (eldev-help-list-aliases name eldev--cleaner-aliases "    %s\n")
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
  (eldev--adjust-stable/unstable-archive-priorities)
  (if package-archives
      (dolist (archive (sort package-archives (lambda (a b) (> (eldev-package-archive-priority (car a))
                                                               (eldev-package-archive-priority (car b))))))
        (eldev-output "%s: %s%s"
                      (eldev-colorize (car archive) 'name)
                      (eldev-colorize (cdr archive) 'url)
                      (eldev-format-message "  (priority: %s)" (eldev-package-archive-priority (car archive) "0, defaulted"))))
    (eldev-print "None specified; add form `(eldev-use-package-archive ...)' in file `%s'" eldev-file)))

(defun eldev-package-archive-priority (archive &optional default)
  (or (cdr (assoc archive (if (eq eldev--package-archive-priorities t) package-archive-priorities eldev--package-archive-priorities)))
      default 0))

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
    (eldev-load-project-dependencies additional-sets t t)
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
               (push (if descriptor
                         (eldev-format-message "built-in is overriden by installed %s" (eldev-message-version descriptor t))
                       (let ((available (eldev-find-built-in-version package-name)))
                         (if available
                             (let ((message (eldev-format-message "built-in version is %s" (eldev-message-version available))))
                               (if (version-list-< available version)
                                   (eldev-colorize (upcase message) 'warn)
                                 message))
                           "built-in")))
                     remarks))
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
anything registered with `eldev-add-extra-dependencies', tools
used in your project during development (e.g. `buttercup' or
various linters), `eldev' itself or `emacs'.  If multiple
versions are requested, each is printed on a separate line.

If command line parameters include project dependencies, they are
installed first if needed.

Normally, package name and version is printed.  However, in quiet
mode output is restricted to just the version."
  :parameters     "[PACKAGE...]"
  :works-on-old-eldev t
  (let* ((packages          (if parameters
                                (mapcar #'intern parameters)
                              '(eldev)))
         ;; Try to work even if invoked from a non-project directory.
         (this-package      (ignore-errors (eldev-package-descriptor)))
         (this-package-name (when this-package (package-desc-name this-package))))
    ;; Special handling of the project itself so that its version can
    ;; be queried even if there are unavailable dependencies.
    (when (eldev-any-p (not (or (eq it 'emacs) (eq it this-package-name) (eldev-find-package-descriptor it nil t))) packages)
      (eldev--load-installed-runtime-dependencies)
      (eldev-load-project-dependencies (mapcar #'car eldev--extra-dependencies) nil t))
    (dolist (package packages)
      (let ((version (if (eq package 'emacs)
                         emacs-version
                       (let ((descriptor (if (eq package this-package-name)
                                             this-package
                                           ;; Note that all built-in packages can be queried because of the
                                           ;; below code.  However, we do not promise that, maybe later we'll
                                           ;; filter for only declared dependencies.  Also, Emacs 24 and 25
                                           ;; don't know versions of their own built-ins, apparently.
                                           (or (eldev-find-package-descriptor package nil t) (eldev-find-built-in-version package t)))))
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
(declare-function eldev-count-ert-tests "eldev-ert" (selectors))
(declare-function eldev-run-ert-tests "eldev-ert" (selectors &optional environment))

(declare-function eldev-test-buttercup-preprocess-selectors "eldev-buttercup" (selectors))
(declare-function eldev-count-buttercup-tests "eldev-buttercup" (selectors))
(declare-function eldev-run-buttercup-tests "eldev-buttercup" (selectors &optional environment))

(declare-function eldev-test-ecukes-preprocess-selectors "eldev-ecukes" (selectors))
(declare-function eldev-run-ecukes-tests "eldev-ecukes" (feature-files selectors &optional environment))

(defvaralias 'eldev-test-dwim 'eldev-dwim)

(defvar eldev-dwim t
  "Employ some heuristics when parsing command line of certain commands.

For `test':

* Any selector that ends in `.el' is instead treated as a file
  pattern.

* For ERT: any symbol selector that doesn’t match a test name is
  instead treated as regular expression (i.e. as a string).

For `lint':

* Any linter name that ends in `.el' is instead treated as a file
  pattern.

These heuristics are aimed at further simplifying of test
execution and other common commands.")

(defvar eldev-test-stop-on-unexpected nil
  "If non-nil, stop as soon as a test produces an unexpected result.
Can also be an integer, in which case stop after that many tests
produce unexpected results.")

(defvar eldev-test-print-backtraces t
  "If non-nil (default), print assertion backtraces.
By default, style of backtraces is determined by variable
`eldev-backtrace-style' (also settable by global command line
option), as long as this is supported by the testing framework.
However, if value of this variable is an integer, it overrides
the value of `eldev-backtrace-style'.")

(defvar eldev-test-expect-at-least nil
  "Fail if there are fewer than this many tests.
This is mostly meant for automated tests to prevent accidental
situations where success is claimed only because no/few tests
were executed.")

(defvar eldev-test-framework nil
  "Test framework(s) to use.
Can be a list of supported framework names in case the project
includes tests of different kinds, e.g. \\='(ert buttercup)
or \\='(buttercup ecukes).

If left nil (default value), Eldev will try to autodetect.
Autodetection will not produce correct result if you use
different kinds of tests in files of same type, i.e. currently
ERT and Buttercup: both use `.el' files.")

(defvar eldev-test-runner nil
  "Test runner to use.")

(defvar eldev-test-ert-hook nil
  "Hook executed before running ERT tests.
Functions are called with SELECTORS as argument.

Since Eldev 0.2.")

(defvar eldev-test-buttercup-hook nil
  "Hook executed before running Buttercup tests.
Functions are called with SELECTORS as argument.

Since Eldev 0.2.")

(defvar eldev-test-ecukes-hook nil
  "Hook executed before running Ecukes tests.
Functions are called with SELECTORS as argument.

Since Eldev 0.10.")

(defvar eldev--test-runners nil)

(defvar eldev-test-file-patterns nil
  "Load only those test files that match one of these patterns.
Should normally be specified only from command line.")

(defvar eldev-test-num-passed  0)
(defvar eldev-test-num-failed  0)
(defvar eldev-test-num-skipped 0)

(defvar eldev-test-known-frameworks
  (eval-when-compile `((ert .       ((detect               . (lambda () (and (featurep 'ert)
                                                                             ;; Only checking for feature is not enough.
                                                                             ;; E.g. Buttercup includes it too.
                                                                             (catch 'found
                                                                               (mapatoms (lambda (symbol)
                                                                                           (when (ert-test-boundp symbol)
                                                                                             (throw 'found t))))
                                                                               nil))))
                                     (require              . eldev-ert)
                                     (preprocess-selectors . eldev-test-ert-preprocess-selectors)
                                     (prepare              . (lambda (_selectors)
                                                               (eldev-test-ert-load-results)))
                                     (count-tests          . (lambda (selectors _files)
                                                               (eldev-count-ert-tests selectors)))
                                     (run-tests            . (lambda (selectors _files _runner environment)
                                                               (eldev-run-ert-tests selectors environment)))
                                     (finalize             . (lambda (_selectors)
                                                               (eldev-test-ert-save-results)))))
                       (buttercup . ((detect               . (lambda () (featurep 'buttercup)))
                                     (features             . buttercup)
                                     (packages             . ((:tool buttercup)))
                                     (require              . eldev-buttercup)
                                     (preprocess-selectors . eldev-test-buttercup-preprocess-selectors)
                                     (count-tests          . (lambda (selectors _files)
                                                               (eldev-count-buttercup-tests selectors)))
                                     (run-tests            . (lambda (selectors _files _runner environment)
                                                               (eldev-run-buttercup-tests selectors environment)))))
                       (ecukes    . ((detect               . (lambda () t))  ; if `.feature' files are found, then they must be for Ecukes
                                     (fileset              . "*.feature")
                                     (file-description     . "test `.feature' file%s")
                                     (dwim-regexp          . ,(rx ".feature" eol))
                                     (packages             . ((:tool ecukes)))
                                     (require              . eldev-ecukes)
                                     (preprocess-selectors . eldev-test-ecukes-preprocess-selectors)
                                     (run-tests            . (lambda (selectors files _runner environment)
                                                               (eldev-run-ecukes-tests files selectors environment)))))))
  "Alist of all test frameworks known to Eldev.
While this variable is public and can be modified, you most
likely shouldn't do that.  Leave adding support for more
frameworks to Eldev code, instead write a test runner if you need
it.")


(eldev-defcommand eldev-test (&rest parameters)
  "Run project's regression/unit tests.  By default all tests
defined in the project are executed.  However, you can restrict
their set using SELECTORs (to be interpreted by the used test
framework) and/or `--file' option listed above.

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

If `eldev-dwim' variable is on (default), Eldev employs
additional heuristics when processing SELECTORs.  This variable
is not controllable from command line---change its value in
`~/.eldev/config' or `Eldev-local'.

This command exits with error code 1 if any test produces an
unexpected result."
  :parameters     "[SELECTOR...]"
  (eldev--do-test eldev-test-framework parameters))

(eldev-defcommand eldev-test-ert (&rest parameters)
  "Run project's ERT regression/unit tests.  See command `test'
for details."
  :parameters     "[SELECTOR...]"
  :aliases        ert
  :hidden-if      (or (<= (length (eldev-listify eldev-test-framework)) 1) (not (memq 'ert eldev-test-framework)))
  (eldev--do-test 'ert parameters))

(eldev-defcommand eldev-test-buttercup (&rest parameters)
  "Run project's Buttercup regression/unit tests.  See command
`test' for details."
  :parameters     "[SELECTOR...]"
  :aliases        (buttercup test-bcup bcup)
  :hidden-if      (or (<= (length (eldev-listify eldev-test-framework)) 1) (not (memq 'buttercup eldev-test-framework)))
  (eldev--do-test 'buttercup parameters))

(eldev-defcommand eldev-test-ecukes (&rest parameters)
  "Run project's Ecukes regression/unit tests.  See command
`test' for details."
  :parameters     "[SELECTOR...]"
  :aliases        ecukes
  :hidden-if      (or (<= (length (eldev-listify eldev-test-framework)) 1) (not (memq 'ecukes eldev-test-framework)))
  (eldev--do-test 'ecukes parameters))

(defun eldev--do-test (possible-frameworks parameters)
  (eldev-load-project-dependencies 'test)
  (if possible-frameworks
      (let (unused)
        (setf possible-frameworks (eldev-listify possible-frameworks))
        (when eldev-test-framework
          (dolist (framework possible-frameworks)
            (unless (memq framework (eldev-listify eldev-test-framework))
              (push framework unused)))
          (when unused
            (signal 'eldev-error `(:hint ,(eldev-format-message "%s" (eldev-message-enumerate '("Used framework:" "Used frameworks:") (eldev-listify eldev-test-framework)))
                                         "This project doesn't use %s" ,(eldev-message-enumerate "test framework" (nreverse unused)))))))
    (setf possible-frameworks (mapcar #'car eldev-test-known-frameworks)))
  ;; Prepare `eldev-test-stop-on-unexpected' for the actual test functions.
  (let ((eldev-test-stop-on-unexpected (when eldev-test-stop-on-unexpected
                                         (if (and (integerp eldev-test-stop-on-unexpected) (> eldev-test-stop-on-unexpected 0))
                                             eldev-test-stop-on-unexpected
                                           1)))
        (filter-patterns               eldev-test-file-patterns)
        (pass                          (if eldev-test-expect-at-least 'count 'run))
        (num-matched-tests             0)
        selectors
        filesets
        found-any-files
        any-frameworks-failed
        used-frameworks
        noncounting-frameworks)
    (dolist (selector parameters)
      (if (and eldev-dwim (eldev-any-p (string-match-p (or (eldev-test-get-framework-entry it 'dwim-regexp) (rx ".el" eol)) selector) possible-frameworks))
          (push selector filter-patterns)
        (push selector selectors)))
    (dolist (framework possible-frameworks)
      (let* ((fileset (or (eldev-test-get-framework-entry framework 'fileset) "*.el"))
             (entry   (cdr (assoc fileset filesets))))
        (if entry
            (push framework (car entry))
          (push (cons fileset (list `(,framework) (or (eldev-test-get-framework-entry framework 'file-description) "test `.el' file%s") t)) filesets))))
    (setf selectors (nreverse selectors)
          filesets  (nreverse filesets))
    (unwind-protect
        (while (progn
                 (dolist (entry filesets)
                   ;; Skip further frameworks if previous failed too many times or enough
                   ;; tests have been found.
                   (unless (or (and eldev-test-stop-on-unexpected (< eldev-test-stop-on-unexpected 0))
                               (and (eq pass 'count) (>= num-matched-tests eldev-test-expect-at-least)))
                     (let* ((fileset            (car  entry))
                            (used-by-frameworks (nth 0 (cdr entry)))
                            (file-description   (nth 1 (cdr entry)))
                            (files              (nth 2 (cdr entry)))
                            (files-looked-up    (listp files)))
                       (unless files-looked-up
                         (setf files (eldev-find-and-trace-files `(:and ,(eldev-standard-fileset 'test) ,fileset) file-description))
                         (when filter-patterns
                           (setf files (eldev-filter-files files (reverse filter-patterns)))
                           (eldev-trace "%s" (eldev-message-enumerate-files (format "Remaining %s after applying `--file' filter(s): %%s (%%d)" file-description) files)))
                         (setf (nth 2 (cdr entry)) files))
                       ;; Framework autoguessing can only work if there is at least one file to load, so
                       ;; this `when' is important.
                       (when files
                         (when (and (equal fileset "*.el") (not files-looked-up))
                           (eldev-autoinstalling-implicit-dependencies t
                             (dolist (file files)
                               (let* ((absolute-without-el (replace-regexp-in-string (rx ".el" eos) "" (expand-file-name file eldev-project-dir) t t))
                                      (already-loaded      (eldev-any-p (assoc (concat absolute-without-el it) load-history) load-suffixes)))
                                 (if already-loaded
                                     (eldev-trace "Not loading file `%s': already `require'd by some other file" file)
                                   (eldev-verbose "Loading test file `%s'..." file)
                                   (load absolute-without-el nil t nil t))))))
                         (setf found-any-files t)
                         (let* ((runner-name (or eldev-test-runner 'simple))
                                (runner      (or (cdr (assq runner-name eldev--test-runners))
                                                 (signal 'eldev-error `(:hint ("Check output of `%s test --list-runners'" ,(eldev-shell-command t)) "Unknown test runner `%s'" ,runner-name))))
                                (supported   (eldev-get runner :frameworks)))
                           (dolist (framework (or (eldev-listify (eldev-test-framework used-by-frameworks))
                                                  (if (cdr possible-frameworks)
                                                      (signal 'eldev-error `(:hint "Consider setting `eldev-test-framework' explicitly" "Unable to guess testing framework(s)"))
                                                    (signal 'eldev-error `("This project doesn't appear to use testing framework `%s'" ,(car possible-frameworks))))))
                             (unless (or (and eldev-test-stop-on-unexpected (< eldev-test-stop-on-unexpected 0))
                                         (and (eq pass 'count) (>= num-matched-tests eldev-test-expect-at-least)))
                               (let ((already-prepared (memq framework used-frameworks)))
                                 (unless already-prepared
                                   (unless (equal fileset "*.el")
                                     (eldev--test-maybe-install-framework framework "Installing package(s) of testing framework `%s'..."))
                                   (unless (or (null supported) (memq framework (eldev-listify supported)))
                                     (signal 'eldev-error `(:hint ("Run `%s test --list-runners' for more information" ,(eldev-shell-command t))
                                                                  "Test runner `%s' doesn't support framework `%s'" ,runner-name ,framework)))
                                   (push framework used-frameworks))
                                 (let ((actual-selectors (eldev-test-preprocess-selectors framework selectors)))
                                   (unless already-prepared
                                     (run-hook-with-args (intern (format "eldev-test-%s-hook" framework)) actual-selectors)
                                     (eldev-test-prepare-framework framework actual-selectors))
                                   (condition-case error
                                       (if (eq pass 'run)
                                           (unwind-protect
                                               (funcall runner framework actual-selectors files)
                                             (eldev-test-finalize-framework framework actual-selectors))
                                         (let ((count-tests (eldev-test-get-framework-entry framework 'count-tests)))
                                           (if count-tests
                                               (setf num-matched-tests (+ num-matched-tests (funcall count-tests actual-selectors files)))
                                             (push framework noncounting-frameworks))))
                                     (eldev-error
                                      ;; Normally we just print and continue for other frameworks.
                                      (eldev--print-eldev-error error 'test)
                                      (setf any-frameworks-failed t))))))))))))
                 (when (eq pass 'count)
                   (if (>= num-matched-tests eldev-test-expect-at-least)
                       (setf pass 'run)
                     (when noncounting-frameworks
                       (eldev-warn "%s %s't support test counting" (eldev-message-enumerate "Framework" (reverse noncounting-frameworks)) (if (cdr noncounting-frameworks) "do" "does")))
                     (eldev-test-validate-amount num-matched-tests)))))
      (if (cdr used-frameworks)
          ;; Print summary over all test types if we have run more than one.
          (unless (eq pass 'count)
            ;; Using "(s)" because if replaced with proper singular/plural form of word
            ;; "test", "of them" becomes weird.
            (let ((summary (eldev-format-message "\nTesting summary: %d test(s), of them %d passed, %d failed, %d skipped"
                                                 (+ eldev-test-num-passed eldev-test-num-failed eldev-test-num-skipped)
                                                 eldev-test-num-passed eldev-test-num-failed eldev-test-num-skipped)))
              (if (= eldev-test-num-failed 0)
                  (eldev-print "%s" summary)
                (eldev-error "%s" summary))))
        (unless found-any-files
          (eldev-print "No test files to use")))
      (when any-frameworks-failed
        (signal 'eldev-quit 1)))))

(defvar byte-compiler-error-flag)
(defun eldev--test-autoinstalling-framework (enabled callback)
  ;; If framework is specified explicitly, ensure it is installed first.  Otherwise
  ;; appropriate framework will be installed from `require' advice below.
  (when (and enabled eldev-test-framework)
    (dolist (framework (eldev-listify eldev-test-framework))
      (unless (eldev-test-get-framework-entry framework 'fileset)
        (eldev--test-maybe-install-framework framework "Preparing testing framework `%s' as specified by variable `eldev-test-framework'..."))))
  (eldev-advised ('require :before (when (and enabled (null eldev-test-framework))
                                     (lambda (feature &optional filename no-error)
                                       ;; Only perform autoinstallation for simple `(require 'x)' forms.
                                       (unless (or filename no-error (featurep feature))
                                         (let (autoinstall)
                                           (dolist (framework eldev-test-known-frameworks)
                                             (when (memq feature (eldev-listify (eldev-test-get-framework-entry (cdr framework) 'features)))
                                               (setf autoinstall (nconc autoinstall (eldev-test-get-framework-entry (cdr framework) 'packages)))))
                                           (when autoinstall
                                             (eldev-verbose "Installing testing framework package(s) as required...")
                                             (apply #'eldev-add-extra-dependencies 'runtime autoinstall)
                                             ;; Backporting the fix from https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41065
                                             ;; for this specific case: Emacs would attribute results of inner byte-compilation
                                             ;; (dependency package installation) to outer (byte-compiling the project itself).
                                             (let (byte-compiler-error-flag)
                                               (eldev-load-extra-dependencies 'runtime))))))))
    (funcall callback)))

(defun eldev--test-maybe-install-framework (framework message)
  (let ((to-install (eldev-test-get-framework-entry framework 'packages)))
    (when to-install
      (eldev-verbose message framework)
      (apply #'eldev-add-extra-dependencies 'runtime to-install)
      (eldev-load-extra-dependencies 'runtime))))

(defun eldev-test-get-framework-data (framework)
  "Get all data for given FRAMEWORK."
 (or (cdr (assq framework eldev-test-known-frameworks))
     (error "Unknown test framework `%s'" framework)))

(defun eldev-test-get-framework-entry (framework key &optional require-features)
  "Get an entry for given FRAMEWORK."
  (when (symbolp framework)
    (setf framework (eldev-test-get-framework-data framework)))
  (let* ((entry      (cdr (assq key framework)))
         (to-require (eldev-listify (cdr (assq 'require framework)))))
    (when (if (eq require-features 'non-nil) entry require-features)
      (dolist (feature to-require)
        (require feature)))
    entry))

(defun eldev-test-framework (&optional possible-frameworks)
  "Get used test framework(s).
For compatibility, return value is a symbol if only one framework
is used.  If variable `eldev-test-framework' has nil value,
framework(s) are autodetected where possible, among those allowed
by parameter POSSIBLE-FRAMEWORKS (if that is not specified, only
frameworks based on `.el' files, i.e. ERT and Buttercup, are
considered allowed)."
  (let ((frameworks (eldev-listify eldev-test-framework)))
    (setf frameworks (if frameworks
                         (eldev-filter (if possible-frameworks
                                           (memq it possible-frameworks)
                                         (null (eldev-test-get-framework-entry it 'fileset)))
                                       frameworks)
                       (mapcar #'car (eldev-filter (let* ((framework (car it))
                                                          (detect    (when (if possible-frameworks
                                                                               (memq framework possible-frameworks)
                                                                             (null (cdr (assq 'fileset (cdr it)))))
                                                                       (cdr (assq 'detect (cdr it))))))
                                                     (and detect (funcall detect)))
                                                   eldev-test-known-frameworks))))
    (if (cdr frameworks) frameworks (car frameworks))))

(defun eldev-test-preprocess-selectors (framework selectors)
  "Convert specified SELECTORS for use by given FRAMEWORK."
  (let ((preprocess-selectors (eldev-test-get-framework-entry framework 'preprocess-selectors 'non-nil)))
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
  (let ((prepare (eldev-test-get-framework-entry framework 'prepare 'non-nil)))
    (when prepare
      (funcall prepare selectors))))

(defun eldev-test-finalize-framework (framework selectors)
  "Finalize given test FRAMEWORK after executing tests."
  (condition-case error
      (let ((finalize (eldev-test-get-framework-entry framework 'finalize 'non-nil)))
        (when finalize
          (funcall finalize selectors)))
    (error (eldev-warn "When finalizing test framework `%s': %s" framework (error-message-string error)))))

(defmacro eldev-test-do-load-results (file-extension description expected-version &rest body)
  "Load previous test results.
This macro is meant for use in framework support
implementations."
  (declare (indent 3) (debug (stringp stringp numberp body)))
  (let ((file (format "test-results.%s" file-extension)))
    `(eldev-do-load-cache-file (expand-file-name ,file (eldev-cache-dir t)) ,description ,expected-version ,@body)))

(defmacro eldev-test-do-save-results (file-extension description version &rest body)
  "Save test results.
This macro is meant for use in framework support
implementations."
  (declare (indent 3) (debug (stringp stringp numberp body)))
  (let ((file (format "test-results.%s" file-extension)))
    `(eldev-do-save-cache-file (expand-file-name ,file (eldev-cache-dir t t)) ,description ,version ,@body)))

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


(eldev-defcleaner eldev-cleaner-global-cache ()
  "Delete Eldev's global package archive cache directory.  This
doesn't affect any projects, but if Eldev has to prepare a new
project (or an existing project for a different Emacs version),
it will have to redownload all dependency packages."
  :only-explicit  t
  (eldev-global-package-archive-cache-dir))


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


(eldev-deftestrunner eldev-test-runner-standard (framework selectors files)
  "Invokes test framework without changing anything.  However,
various options to command `test' and global option `-b'
(variable `eldev-backtrace-style') are still respected if
possible."
  (funcall (eldev-test-get-framework-entry framework 'run-tests t) selectors files 'standard
           (pcase framework
             ;; For Buttercup we still pass through our color setting.
             (`buttercup `((buttercup-color . ,(eldev-output-colorized-p)))))))

(eldev-deftestrunner eldev-test-runner-simple (framework selectors files)
  "Simple test runner with a few tweaks to the defaults.

For ERT:
  - if Eldev is in quiet mode, set `ert-quiet' to t;
  - only backtrace frames inside test functions are printed.

For Buttercup:
  - output for skipped tests (e.g. because of selectors) is omitted;
  - if Eldev is in quiet mode, only failed tests are mentioned.

Backtraces are formatted according to `eldev-test-print-backtraces'
and `eldev-backtrace-style'.  For Buttercup, any truncation results
in `crop' stack frame style."
  (funcall (eldev-test-get-framework-entry framework 'run-tests t) selectors files 'simple
             ;; Other frameworks are just invoked with empty environment.
             (pcase framework
               (`ert
                `((ert-quiet                        . ,(not (eldev-unless-quiet t)))
                  (eldev--test-ert-short-backtraces . t)))
               (`buttercup
                `((buttercup-color                         . ,(eldev-output-colorized-p))
                  (buttercup-reporter-batch-quiet-statuses . ,`(skipped disabled ,@(unless (eldev-unless-quiet t) '(pending passed))))))
               (`ecukes
                `((ecukes-verbose . (not (eldev-unless-quiet t))))))))

(eldev-defoption eldev-test-files (pattern)
  "Load only tests in given file(s)"
  :options        (-f --file)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :value          PATTERN
  (push pattern eldev-test-file-patterns))

(eldev-defoption eldev-test-stop-on-unexpected-mode (&optional num-failures)
  "Stop after this many unexpected results (usually failures), 1 if omitted"
  :options        (-s --stop --stop-on-unexpected)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :optional-value N
  :default-value  (if eldev-test-stop-on-unexpected
                      (if (and (integerp eldev-test-stop-on-unexpected) (> eldev-test-stop-on-unexpected 1))
                          eldev-test-stop-on-unexpected
                        "right away")
                    :no-default)
  (setf eldev-test-stop-on-unexpected (if num-failures (string-to-number num-failures) t)))

(eldev-defoption eldev-test-continue-mode ()
  "Execute all scheduled tests regardless of results"
  :options        (-c --continue)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :if-default     (null eldev-test-stop-on-unexpected)
  (setf eldev-test-stop-on-unexpected nil))

(eldev-defoption eldev-test-print-backtraces (&optional width)
  "Print test failure backtraces; WIDTH overrides value of the
global option `-b'"
  :options        (-b --print-backtraces)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :optional-value WIDTH
  :default-value  (pcase eldev-test-print-backtraces
                    (`nil            :no-default)
                    ((pred integerp) (if (> eldev-test-print-backtraces 0) eldev-test-print-backtraces "untruncated"))
                    (_               "use global style"))
  (setf eldev-test-print-backtraces (if width (string-to-number width) t)))

(eldev-defoption eldev-test-omit-backtraces ()
  "Omit failure backtraces for brevity"
  :options        (-B --omit-backtraces)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :if-default     (not eldev-test-print-backtraces)
  (setf eldev-test-print-backtraces nil))

(eldev-defoption eldev-test-expect-at-least (n)
  "Fail if fewer than N tests match selectors"
  :options        (-X --expect)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :value          N
  (setf eldev-test-expect-at-least (string-to-number n)))

(eldev-defoption eldev-test-runner (name)
  "Choose test runner"
  :options        (-r --runner)
  :for-command    (test test-ert test-buttercup test-ecukes)
  :value          NAME
  :default-value  (or eldev-test-runner 'simple)
  ;; Will be validated when executing the command.
  (setf eldev-test-runner (intern name)))

(eldev-defoption eldev-test-list-runners ()
  "List available test runners and exit"
  :options        --list-runners
  :for-command    (test test-ert test-buttercup test-ecukes)
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



;; eldev lint

(defvar eldev-lint-default :default
  "Linters to run by default.
`:default' means all linters defined with :default t, t means all
linters known to Eldev. Variables `eldev-lint-default-excluded'
and `eldev-lint-disabled' take precedence.")

(defvar eldev-lint-default-excluded nil
  "Linters to be excluded from those run by default.")

(defvar eldev-lint-disabled nil
  "Linters that are disabled for the current project.")

(defvar eldev-lint-file-patterns nil
  "Lint only files that match one of these patterns.
Should normally be specified only from command line.")

(defvar eldev-lint-ignored-fileset '(eldev-package-descriptor-file-name)
  "Never lint files matching this fileset.
Default value includes only the package descriptor file,
i.e. `PACKAGE-pkg.el'.

Since 0.9.")

(defvar eldev-lint-stop-mode nil
  "Whether and when Eldev should stop early when linting.
Possible modes:

  - nil: don't stop early;
  - linter: run a linter that issued a warnings to the end, but
    don't execute any further linters;
  - file: if a warning is issued, run current linter on that file
    to the end and then stop;
  - warning: immediately stop after the first warning.")

(defvar eldev-lint-optional t
  "Whether to fail on non-installable linters.
Linters can have differing requirements and some may not be
installable on older Emacs versions.  This option tells how to
handle such situation: treat linters as optional (a warning is
still printed) or fail as if non-installable linter issued an
error.")


(defvar eldev--linters nil)
(defvar eldev--linter-aliases nil)

(defvar eldev--lint-num-warnings)


;; Internal helper for `eldev-deflinter'.
(defun eldev--register-linter (linter name keywords)
  (while keywords
    (eldev-pcase-exhaustive (pop keywords)
      (:aliases
       (eldev-register-linter-aliases name (pop keywords)))
      ((and (or :default) keyword)
       (eldev-put linter keyword (pop keywords)))))
  (eldev--assq-set name linter eldev--linters))

(defun eldev-register-linter-aliases (linter aliases)
  (dolist (alias (eldev-listify aliases))
    (eldev--assq-set alias linter eldev--linter-aliases)))

(defmacro eldev-deflinter (name arguments &rest body)
  "Register a linter in Eldev.

BODY can contain the following keywords:

    :name NAME

        Linter name (a symbol) for ther command line.  Default
        value is derived from function name by removing `eldev-'
        prefix (or a prefix for any other project) and word
        `linter-'.

    :aliases ALIASES

        One (a symbol) or several (list of symbols) aliases for
        the linter.

    :default VALUE

        Whether the linter should be invoked by default,
        i.e. when `eldev lint' is called without further
        command line arguments."
  (declare (doc-string 3) (indent 2))
  (let ((parsed-body (eldev-macroexp-parse-body body))
        (linter-name (intern (replace-regexp-in-string (rx bol (1+ (not (any "-"))) (1+ "-") (? "linter-")) "" (symbol-name name))))
        keywords)
    (setf body (cdr parsed-body))
    (while (keywordp (car body))
      (pcase (pop body)
        (:name   (setf linter-name (pop body)))
        (keyword (push keyword keywords) (push (pop body) keywords))))
    (when (eq linter-name 'all)
      (error "Linter name `all' is reserved and cannot be registered"))
    `(progn (defun ,name ,arguments
              ,@(car parsed-body)
              ,@body)
            (eldev--register-linter ',name ',linter-name ',(nreverse keywords)))))


(eldev-defcommand eldev-lint (&rest parameters)
  "Run standard linters on the project's source code.  By default,
linters listed in variable `eldev-lint-default' and not specified
in `eldev-lint-default-excluded' or `eldev-lint-disabled' are
executed.  Default values of these variables mean that all
supported linters will be run.

You can also request specific linters by name.  Use option
`--list-linters' to find all supported by Eldev.  Special name
`all' runs all non-disabled linters, which is useful if value of
`eldev-lint-default-excluded' is not empty.

If `eldev-dwim' variable is on (default), Eldev employs
additional heuristics when processing LINTERs.  Any name that
ends in `.el' is instead treated as a file pattern.

This command exits with error code 1 if any linter issues at
least one warning."
  :parameters     "[LINTER...]"
  :aliases        linter
  (let ((eldev-lint-file-patterns eldev-lint-file-patterns)
        linters
        all)
    (dolist (name parameters)
      (if (and eldev-dwim (string-match-p (rx ".el" eol) name))
          (setf eldev-lint-file-patterns (nconc eldev-lint-file-patterns `(,name)))
        (setf name (intern name))
        (if (eq name 'all)
            (setf all t)
          (unless (or (assq name eldev--linters) (assq name eldev--linter-aliases))
            (signal 'eldev-error `(:hint ("Check output of `%s lint --list-linters'" ,(eldev-shell-command t)) "Unknown linter `%s'" ,name)))
          (push name linters))))
    (unless linters
      (setf linters (eldev-filter (eldev-lint-default-p it) (mapcar #'car eldev--linters))))
    (when all
      (setf linters (eldev-filter (not (memq it eldev-lint-disabled)) (mapcar #'car eldev--linters))))
    (setf linters (nreverse linters))
    (if linters
        (let ((eldev--lint-num-warnings 0))
          (eldev-trace "Going to run the following %s" (eldev-message-enumerate '("linter:" "linters:") linters))
          (catch 'eldev--lint-stop
            (dolist (linter linters)
              (let ((canonical-name (or (cdr (assq linter eldev--linter-aliases)) linter)))
                (when (or (memq linter eldev-lint-disabled) (memq canonical-name eldev-lint-disabled))
                  ;; This can only happen if a linter is requested explicitly.
                  (signal 'eldev-error `("Linter `%s' is disabled (see variable `eldev-lint-disabled')" ,linter)))
                (eldev-verbose "Running linter `%s'..." linter)
                (condition-case error
                    (funcall (cdr (assq canonical-name eldev--linters)))
                  (eldev-missing-dependency (if eldev-lint-optional
                                                (eldev-warn "%s; skipping linter `%s'" (eldev-extract-error-message error) linter)
                                              (signal 'eldev-error `("%s; cannot use linter `%s'" ,(eldev-extract-error-message error) ,linter)))))
                (when (and (eq eldev-lint-stop-mode 'linter) (> eldev--lint-num-warnings 0))
                  (eldev-trace "Stopping after the linter that issued warnings")
                  (throw 'eldev--lint-stop nil)))))
          (if (> eldev--lint-num-warnings 0)
              (signal 'eldev-error `("Linting produced warnings"))
            (eldev-print (if (cdr linters) "Linters have no complaints" "Linter has no complaints"))))
      (eldev-print "Nothing to do"))))

(defun eldev-lint-default-p (linter)
  (and (pcase eldev-lint-default
         (`t       t)
         (:default (eldev-get (cdr-safe (assq linter eldev--linters)) :default))
         (_        (memq linter eldev-lint-default)))
       (not (memq linter eldev-lint-default-excluded))
       (not (memq linter eldev-lint-disabled))))


(eldev-defoption eldev-lint-files (pattern)
  "Lint only given file(s)"
  :options        (-f --file)
  :for-command    lint
  :value          PATTERN
  (push pattern eldev-lint-file-patterns))

(eldev-defoption eldev-lint-continue-mode ()
  "Execute all scheduled linters regardless of any warnings"
  :options        (-c --continue)
  :for-command    lint
  :if-default     (null eldev-lint-stop-mode)
  (setf eldev-lint-stop-mode nil))

(eldev-defoption eldev-lint-finish-linter-mode ()
  "Don't run further linters if one issues a warning"
  :options        (-S --stop-on-linter)
  :for-command    lint
  :if-default     (eq eldev-lint-stop-mode 'linter)
  (setf eldev-lint-stop-mode 'linter))

(eldev-defoption eldev-lint-finish-file-mode ()
  "Stop after the first file with warnings"
  :options        (-s --stop-on-file)
  :for-command    lint
  :if-default     (eq eldev-lint-stop-mode 'file)
  (setf eldev-lint-stop-mode 'file))

(eldev-defoption eldev-lint-stop-on-warning-mode ()
  "Immediately stop if a warning is issued"
  :options        (-w --stop-on-warning)
  :for-command    lint
  :if-default     (eq eldev-lint-stop-mode 'warning)
  (setf eldev-lint-stop-mode 'warning))

(eldev-defbooloptions eldev-lint-allow-skipping-linters eldev-lint-require-linters eldev-lint-optional
  ("Skip linters that cannot be installed (e.g. because Emacs is too old)"
   :options       (-O --optional))
  ("Fail if a linter cannot be installed"
   :options       (-R --required))
  :for-command    lint)

(eldev-defoption eldev-lint-list-linters ()
  "List known linters and exit"
  :options        (-L --list-linters --list)
  :for-command    lint
  (eldev-print "Linter(s) marked with `*' are default, i.e. executed also when\nno names are specified on the command line.\n")
  (let ((all-linters (reverse eldev--linters)))
    (while all-linters
      (let* ((linter   (pop all-linters))
             (name     (car linter))
             (function (cdr linter))
             (header   (if (eldev-lint-default-p name) (format "%s [*]" name) (format "%s" name))))
        (if (eldev-unless-quiet t)
            (progn
              (eldev-output "%s\n" (eldev-colorize header 'section))
              (eldev-help-list-aliases name eldev--linter-aliases "    %s\n")
              (eldev-output "%s" (or (eldev-documentation function) "Not documented"))
              (when all-linters
                (eldev-output "\n")))
          (let ((documentation (eldev-briefdoc function)))
            (if documentation
                (eldev-output "%-18s  %s" header documentation)
              (eldev-output "%s" header)))))))
  (signal 'eldev-quit 0))


(defmacro eldev-lint-printing-warning (level &rest body)
  (declare (indent 1) (debug (body)))
  (unless level
    (setf level :warning))
  `(prog1 (let ((eldev-message-rerouting-wrapper ,(if (keywordp level)
                                                      (if (eq level :error) '#'eldev-error '#'eldev-warn)
                                                    `(if (eq ,level :error) #'eldev-error #'eldev-warn))))
            ,@body)
     (eldev-lint-note-warning)))

(defmacro eldev-lint-linting-file (file &rest body)
  (declare (indent 1) (debug (body)))
  (let ((num-warnings (make-symbol "$num-warnings")))
  `(let ((,num-warnings eldev--lint-num-warnings))
     (eldev-verbose "Linting file `%s'" ,file)
     ,@body
     (if (= eldev--lint-num-warnings ,num-warnings)
         (eldev-print "File `%s': no warnings" ,file)
       (eldev-warn "Found %s in file `%s'" (eldev-message-plural (- eldev--lint-num-warnings ,num-warnings) "warning") ,file))
     (eldev-lint-note-file-finished))))

(defun eldev-lint-note-warning ()
  (setf eldev--lint-num-warnings (1+ eldev--lint-num-warnings))
  (when (eq eldev-lint-stop-mode 'warning)
    (eldev-trace "Stopping after the first linter warning")
    (throw 'eldev--lint-stop nil)))

(defun eldev-lint-note-file-finished ()
  (when (and (eq eldev-lint-stop-mode 'file) (> eldev--lint-num-warnings 0))
    (eldev-trace "Stopping after the first file with linter warnings")
    (throw 'eldev--lint-stop nil)))


(defvar checkdoc-diagnostic-buffer)
(declare-function checkdoc-file "checkdoc")

(eldev-deflinter eldev-linter-doc ()
  "Check documentation for style errors."
  :aliases        (checkdoc documentation)
  :default        t
  ;; Old Emacs version don't have `checkdoc-file' and also don't use `warn'.
  (let ((newer-emacs (fboundp 'checkdoc-file)))
    (eldev-advised ('checkdoc-error :around (lambda (original &rest arguments)
                                              (eldev-suppress-warning-emacs-text
                                                (let ((from (with-current-buffer (get-buffer checkdoc-diagnostic-buffer) (point-max))))
                                                  (apply original arguments)
                                                  (unless newer-emacs
                                                    (warn "%s" (with-current-buffer (get-buffer checkdoc-diagnostic-buffer)
                                                                 (buffer-substring from (point-max)))))
                                                  (eldev-lint-note-warning)))))
      (dolist (file (eldev-lint-find-files "*.el"))
        (eldev-lint-linting-file file
          (if newer-emacs
              (checkdoc-file file)
            (with-current-buffer (find-file-noselect file)
              (checkdoc-current-buffer t))))))))


(defvar package-lint-main-file)
(declare-function package-lint-buffer "package-lint")

(eldev-deflinter eldev-linter-package ()
  "Check package metadata, e.g. correctness of its dependencies."
  :aliases        (package-lint pack)
  :default        t
  ;; Need GNU ELPA for `let-alist' on older Emacs versions.
  (eldev-add-extra-dependencies 'runtime '(:tool package-lint))
  (eldev-load-extra-dependencies 'runtime)
  ;; This linter needs access to package archive contents, so at least fetch all archives
  ;; we have never fetched yet.
  (let ((eldev-global-cache-archive-contents-max-age nil))
    ;; Need to make sure that all archive contents is loaded
    ;; (`eldev--install-or-upgrade-dependencies' loads as few archives as required to
    ;; install the runtime dependencies).
    (eldev--fetch-archive-contents (eldev--determine-archives-to-fetch)))
  (eldev--linter-package-present-archives)
  (require 'package-lint)
  ;; Avoid overriding it if it is already set (presumably in project's `Eldev').
  (unless package-lint-main-file
    (setf package-lint-main-file (eldev-project-main-file)))
  (dolist (file (eldev-lint-find-files "*.el"))
    (eldev-lint-linting-file file
      ;; Adapted `package-lint-batch-and-exit-1'.
      (with-temp-buffer
        (insert-file-contents file t)
        (emacs-lisp-mode)
        (pcase-dolist (`(,line ,col ,type ,message) (package-lint-buffer))
          (eldev-lint-printing-warning (if (eq type 'error) :error :warning)
            (message "%s:%d:%d: %s: %s" file line col type message)))))))

(defun eldev--linter-package-present-archives ()
  ;; Issue #19: otherwise the linter will complain about local dependencies that are not
  ;; available from "normal" package archives.
  (push `(,eldev--internal-pseudoarchive . ,(file-name-as-directory (eldev--internal-pseudoarchive-dir))) package-archives)
  (package-read-all-archive-contents))


(declare-function relint-file "relint")

(eldev-deflinter eldev-linter-re ()
  "Find errors, deprecated syntax etc. in regular expressions."
  :aliases        (relint regex regexp)
  :default        t
  (eldev-add-extra-dependencies 'runtime '(:tool relint))
  (eldev-load-extra-dependencies 'runtime)
  (require 'relint)
  ;; I see no way to avoid diving into internals.
  (eldev-advised ('relint--output-report :around (lambda (original error-buffer file complaint &rest etc)
                                                   (eldev-lint-printing-warning (if (eq (nth 5 complaint) 'error) :error :warning)
                                                     (apply original error-buffer file complaint etc))))
    ;; Suppress totals from `relint' as we print them ourselves.
    (eldev-advised ('relint--finish :around (lambda (original &rest arguments)
                                              (let ((inhibit-message t))
                                                (apply original arguments))))
      (dolist (file (eldev-lint-find-files "*.el"))
        (eldev-lint-linting-file file
          (relint-file file))))))


(declare-function elisp-lint-file "elisp-lint")

(eldev-deflinter eldev-linter-elisp ()
  "Find various problems in Elisp code.  By default this linter
also runs `package-lint', so you either shouldn't run both or
change `elisp-lint's configuration."
  :aliases        (elisp-lint el)
  ;; Need GNU ELPA for `let-alist' on older Emacs versions.
  (eldev-add-extra-dependencies 'runtime '(:tool elisp-lint))
  ;; This linter might need access to package dependencies for byte-compilation.
  (eldev-load-project-dependencies 'runtime nil t)
  ;; This linter might invoke `package-lint' that needs access to package archive
  ;; contents.  Unlike `eldev-linter-package' (where we load _only_ extra dependencies,
  ;; which might not be enough), here we don't need to fetch archive contents, as it
  ;; should have been done by `eldev-load-project-dependencies' already.
  (eldev--linter-package-present-archives)
  (with-eval-after-load 'package-lint
    (unless package-lint-main-file
      (setf package-lint-main-file (eldev-project-main-file))))
  (require 'elisp-lint)
  ;; I don't see a better way than just replacing its output function completely.
  (eldev-advised ('elisp-lint--print :override (lambda (_color format-string &rest arguments)
                                                 (eval `(eldev-warn ,format-string ,@arguments) t)
                                                 (eldev-lint-note-warning)))
    (dolist (file (eldev-lint-find-files "*.el"))
      (eldev-lint-linting-file file
        (elisp-lint-file file)))))


(defun eldev-lint-fileset ()
  ;; Target set is hardcoded for now.
  (eldev-standard-fileset 'main))

(defun eldev-lint-find-files (filter &optional fileset filter-description)
  "Find files to lint matching given FILTER in the FILESET."
  (let ((files (eldev-find-and-trace-files `(:and ,(or fileset (eldev-lint-fileset)) ,filter) (or filter-description "file%s to lint"))))
    (when files
      (when eldev-lint-file-patterns
        (setf files (eldev-filter-files files eldev-lint-file-patterns))
        (eldev-trace "%s" (eldev-message-enumerate-files "Remaining file%s to lint after applying `--file' filter(s): %s (%d)" files)))
      (when eldev-lint-ignored-fileset
        (let ((ignored (eldev-filter-files files eldev-lint-ignored-fileset)))
          (when ignored
            (setf files (eldev-filter (not (member it ignored)) files))
            (eldev-trace "%s" (eldev-message-enumerate-files "Ignored potential file%s to lint: %s (%d)" ignored))))))
    files))


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

(defvar eldev-eval-load-project t
  "Whether to load project and its dependencies.
Not loading means that `eldev-eval-require-main-feature' is also
ignored.  This is available from command line, but the options
are not advertised, since they are rarely useful.")

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
    (when eldev-eval-load-project
      (eldev-load-project-dependencies (if print-results 'eval 'exec)))
    (eldev-autoinstalling-implicit-dependencies eldev-eval-load-project
      (when (and eldev-eval-load-project eldev-eval-require-main-feature)
        (dolist (feature (eldev-required-features eldev-eval-required-features))
          (eldev-verbose "Autorequiring feature `%s' before %s" feature (if print-results "evaluating" "executing"))
          (require feature)))
      (dolist (form forms)
        (eldev-verbose (if print-results "Evaluating expression `%s':" "Executing form `%s'...") (car form))
        ;; For these commands we restore the standard behavior of `message' of writing to stderr,
        (let ((result (let ((eldev-message-rerouting-destination :stderr))
                        (eval (cdr form) eldev-eval-lexical))))
          (when print-results
            (with-temp-buffer
              (funcall (or eldev-eval-printer-function #'prin1) result (current-buffer))
              ;; Older Emacs version end some value representations with a linefeed for
              ;; whatever reasons.
              (when (equal (char-before) ?\n)
                (delete-char -1))
              (eldev-output "%s" (buffer-string)))))))))

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

;; Uncommon `:hidden-if' values mean these options are usually hidden.  They are rarely
;; useful to normal users.
(eldev-defbooloptions eldev-eval-load-project eldev-eval-dont-load-project eldev-eval-load-project
  ("Load project and its dependencies before evaluating"
   :options       (--load)
   :hidden-if     :default)
  ("Don't load project and its dependencies; only Eldev itself and Emacs built-ins will be available"
   :options       (--dont-load)
   :hidden-if     eldev-eval-load-project)
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


(defvar eldev--internal-eval-cache nil)
(defvar eldev--internal-eval-cache-modified nil)

(defun eldev--cross-project-internal-eval (project-dir form &optional use-caching)
  (setf project-dir (expand-file-name project-dir eldev-project-dir))
  (if (string= (file-name-as-directory project-dir) (file-name-as-directory eldev-project-dir))
      (eval form t)
    (when (and use-caching (null eldev--internal-eval-cache))
      (eldev-do-load-cache-file (expand-file-name "internal-eval.cache" (eldev-cache-dir t)) "internal evaluation cache" 1
        (setf eldev--internal-eval-cache (cdr (assq 'cache contents)))))
    (catch 'result
      (let ((cache-key  (when use-caching `(,form . ,project-dir)))
            (timestamps (when use-caching
                          ;; Modification time; mnemonic-name functions are too new.
                          `(,(nth 5 (file-attributes (expand-file-name eldev-file       project-dir)))
                            ,(nth 5 (file-attributes (expand-file-name eldev-local-file project-dir)))))))
        (when (and use-caching eldev--internal-eval-cache)
          (let ((cached (gethash cache-key eldev--internal-eval-cache)))
            (when cached
              (when (equal (car cached) timestamps)
                (eldev-trace "Using cached value for form `%S' in directory `%s': %S" form project-dir (cdr cached))
                (throw 'result (cdr cached)))
              (remhash cache-key eldev--internal-eval-cache)
              (setf eldev--internal-eval-cache-modified t)
              (eldev-trace "Discarded cached value for form `%S' in directory `%s' as `Eldev' and/or `Eldev-local' are newer" form project-dir))))
        (let ((default-directory project-dir))
          (eldev-call-process (eldev-shell-command) `("--quiet" "exec" "--dont-load" ,(prin1-to-string `(prin1 ,form)))
            :destination   '(t nil)
            :pre-execution (eldev-trace "Starting a child Eldev process to evaluate form `%S' in directory `%s'" form project-dir)
            ;; Not using `:die-on-error' because we need a custom message.
            (unless (= exit-code 0)
              (eldev-warn "Output of the child Eldev process:\n%s" (buffer-string))
              (signal 'eldev-error `("Failed to evaluate Eldev expression in directory `%s'" ,project-dir)))
            (let ((result (read (current-buffer))))
              (eldev-trace "Evaluated to `%S'" result)
              (when use-caching
                (unless eldev--internal-eval-cache
                  (setf eldev--internal-eval-cache (make-hash-table :test #'equal)))
                (puthash cache-key `(,timestamps . ,result) eldev--internal-eval-cache)
                (setf eldev--internal-eval-cache-modified t))
              result)))))))

(defun eldev--save-internal-eval-cache ()
  (when eldev--internal-eval-cache-modified
    (eldev-do-save-cache-file (expand-file-name "internal-eval.cache" (eldev-cache-dir t t)) "internal evaluation cache" 1
      `((cache . ,eldev--internal-eval-cache)))))

(add-hook 'kill-emacs-hook #'eldev--save-internal-eval-cache)



;; eldev emacs

(defvar eldev-emacs-default-command-line '("--no-init-file" "--no-site-file" "--no-site-lisp" "--no-splash")
  "Options added to Emacs command line by default.")

;; Because `package-user-dir' became autoloaded in Emacs 28, we need to forward it too:
;; `user-emacs-directory' is set too late for the default value to be correct.
(defvar eldev-emacs-forward-variables '(user-emacs-directory package-user-dir package-archives package-archive-priorities)
  "Variables to forward to Emacs launched by `emacs' command.")

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

  - `setf' form to forward values of variables listed in
    `eldev-emacs-forward-variables';

  - Elisp `require' forms to load `eldev-emacs-required-features'
    (which default to project name);

  - COMMAND-LINE as passed to this command.

However, if COMMAND-LINE has `--' at the first position, Eldev
passes the rest of it to Emacs without adding anything on its own
with the exception of still forwarding variables enumerated in
`eldev-emacs-forward-variables' list.  Be advised that you will
likely need to specify at least `-q' (`--no-init-file') option to
be passed to Emacs, else it will most likely fail."
  :parameters     "[--] [COMMAND-LINE...]"
  :custom-parsing t
  (eldev-load-project-dependencies 'emacs)
  (let (forwarding)
    (dolist (variable eldev-emacs-forward-variables)
      (when (boundp variable)
        (push variable forwarding)
        (push (eldev-macroexp-quote (symbol-value variable)) forwarding)))
    (let* ((autoloads           (apply #'nconc (mapcar (lambda (file) `("--load" ,file)) eldev--loaded-autoloads-files)))
           (value-forwarding    (when forwarding `("--eval" ,(prin1-to-string `(setf ,@(nreverse forwarding))))))
           (effective-load-path (mapconcat #'identity load-path path-separator))
           (process-environment `(,(format "EMACSLOADPATH=%s" effective-load-path) ,@process-environment)))
      (eldev-call-process eldev-emacs-executable
          (if (string= (car parameters) "--")
              (append autoloads value-forwarding (cdr parameters))
            (append eldev-emacs-default-command-line
                    autoloads
                    value-forwarding
                    (apply #'append (mapcar (lambda (feature) (list "--eval" (format "(require '%s)" feature)))
                                            (eldev-required-features eldev-emacs-required-features)))
                    parameters))
        :pre-execution (eldev-verbose "Full command line to run child Emacs process:\n  %s" (eldev-message-command-line executable command-line))
        :pre-execution (eldev-verbose "Effective load path for it:\n  %s" effective-load-path)
        :die-on-error  "child Emacs"
        (eldev--forward-process-output "Output of the child Emacs process:" "Child Emacs process produced no output")))))



;; eldev docker

(defvar eldev--docker-gui-args
  (list "-e" "DISPLAY" "-v" "/tmp/.X11-unix:/tmp/.X11-unix")
  "Arguments needed to launch dockerized Emacs as a GUI.")

(defvar eldev-docker-run-extra-args nil
  "Extra arguments to pass to \"docker run\".")

(defvar eldev--container-bootstrap-cmd-fn
  #'eldev--container-bootstrap-cmd-fn
  "Function to determine the command used by \"docker run\".

It should take one parameter: the arguments of the \"eldev\" call.")

(defvar eldev--docker-home-name "docker-home"
  "Name of the home directory of the docker user.")

(defvar eldev--docker-os-error-fmt-string
  "OS %s is not currently supported by \"eldev docker\""
  "Error message format string if the os is not supported.")

(defun eldev--container-bootstrap-cmd-fn (eldev-args)
  "Return a command in the form of an argument list for \"docker run\".

ELDEV-ARGS will be passed to an \"eldev\" call."
  (list
   "sh" "-c"
   (format "export PATH=\"$HOME/bin:$PATH\" && eldev %s" eldev-args)))

(defun eldev--container-eldev-source-install-cmd (eldev-src-repo-dir eldev-args)
  "Return command for \"docker run\" that will install eldev from source.

Return a command that installs eldev from the source repository
ELDEV-SRC-REPO-DIR (a full path on the container), and then calls eldev
with ELDEV-ARGS."
  (list
   "sh" "-c"
   (format "ELDEV_LOCAL=%s %s/bin/eldev %s"
           eldev-src-repo-dir
           eldev-src-repo-dir
           eldev-args)))

(defun eldev--docker-determine-img (img-string)
  "Return an appropriate docker image based on IMG-STRING."
  (if (string-match-p ".*/.*" img-string)
      img-string
    (format "silex/emacs:%s" img-string)))

(defun eldev--docker-local-dep-mounts (home)
  "Return bind mount arguments of local dependencies for docker run.

HOME is the home directory of the container user."
  (eldev-flatten-tree
   (mapcar (lambda (local-dep)
             (let* ((dir (nth 3 local-dep))
                    (dir-rel (file-relative-name dir (expand-file-name "~")))
                    (container-dir
                     (if (eldev-external-filename dir-rel)
                         dir
                       (concat (file-name-as-directory home) dir-rel))))
               (list "-v" (format "%s:%s" (expand-file-name dir) container-dir))))
           eldev--local-dependencies)))

(defun eldev--docker-create-directories (docker-home)
  "Make directories required for \"eldev docker\" given DOCKER-HOME.

This is necessary since if we mount a volume such that the directory
on the host does not exist, then it will be created on the container
owned by root."
  (unless (file-exists-p docker-home)
    (make-directory
     (concat (file-name-as-directory docker-home)
             (file-name-as-directory eldev-cache-dir)
             eldev-global-cache-dir)
     t))
  (let ((home-bin (concat (file-name-as-directory docker-home) "bin")))
    (unless (file-exists-p home-bin) (make-directory home-bin))))

(defun eldev--docker-home ()
  "Return the host directory of the container docker home."
  (concat (file-name-as-directory (eldev-cache-dir nil t))
          eldev--docker-home-name))

(defun eldev--docker-args (img eldev-args &optional as-gui local-eldev)
  "Return command line args to run the docker image IMG.

ELDEV-ARGS will be appended to the eldev call in the container.

The global config file and cache will be mounted unless
`eldev-skip-global-config' is nil.

If AS-GUI is non-nil include arguments necessary to run Emacs as a GUI.

If LOCAL-ELDEV (a directory) is specified, the returned arguments will
contain a mount of it at /eldev."
  (let* ((container-project-dir (file-name-nondirectory
                                 (directory-file-name eldev-project-dir)))
         (container-home (concat "/"
                                 (file-name-as-directory container-project-dir)
                                 (file-name-as-directory eldev-cache-dir)
                                 eldev--docker-home-name))
         (container-eldev-cache-dir
          (concat (file-name-as-directory container-home) eldev-cache-dir))
         (container-bin (concat (file-name-as-directory container-home) "bin")))
    (eldev--docker-create-directories (eldev--docker-home))
    (append (list "run" "--rm"
                  "-e" (format "HOME=%s" container-home)
                  "-u" (format "%s:%s" (user-uid) (group-gid))
                  "-v" (format "%s:/%s" eldev-project-dir container-project-dir)
                  "-w" (concat "/" container-project-dir))
            (when as-gui eldev--docker-gui-args)
            (if local-eldev
                (when (not (string= (directory-file-name eldev-project-dir)
                                    (directory-file-name local-eldev)))
                  (list "-v" (format "%s:/eldev" local-eldev)))
              (list "-v" (format "%s:%s/eldev"
                                 (locate-file "bin/eldev" load-path)
                                 container-bin)))
            (unless eldev-skip-global-config
              (list "-v" (format "%s:%s/%s"
                                 (eldev-global-package-archive-cache-dir)
                                 container-eldev-cache-dir
                                 eldev-global-cache-dir)))
            (unless (or eldev-skip-global-config
                        (not (file-exists-p eldev-user-config-file)))
              (list "-v" (format "%s:%s/config"
                                 eldev-user-config-file
                                 container-eldev-cache-dir)))
            (eldev--docker-local-dep-mounts container-home)
            eldev-docker-run-extra-args
            (cons img (funcall eldev--container-bootstrap-cmd-fn
                               (mapconcat #'identity eldev-args " "))))))

(defun eldev--docker-container-eldev-cmd (args)
  "Return the eldev command to call in the docker container deduced from ARGS."
  (car (eldev-filter (not (string-prefix-p "-" it)) args)))

(defun eldev--docker-on-supported-os ()
  "Return t if on a supported OS, else return nil."
  (memq system-type '(gnu/linux gnu/kfreebsd darwin)))

(eldev-defcommand eldev-docker (&rest parameters)
  "Launch specified Emacs version in a Docker container.

This will execute given Eldev COMMAND against a specified Emacs
version with the project loaded with all its dependencies in a
Docker container.  GLOBAL-OPTIONs, such as `--trace', preceding
the COMMAND, will be forwarded to the Eldev call inside the
container.

You must specify either a valid Emacs VERSION, e.g. `27.2' or a
full Docker image name.  If a version is specified, corresponding
Silex's image (https://hub.docker.com/r/silex/emacs, see also for
the list of available versions) is used.  If you specify a full
image name, make sure it contains an installed Emacs of a version
supported by Eldev.

Command line arguments appearing after VERSION will be forwarded
to an `eldev' call within the container.  For example:

    eldev docker 25 emacs file.txt

will execute `eldev emacs file.txt' inside an Emacs 25 container.

The contents of `eldev-docker-run-extra-args' will be added to the
`docker run' call this command issues.

Currently only Linux and macOS systems are supported."
  :parameters     "{VERSION|IMG-NAME} [GLOBAL-OPTION..] COMMAND [...]"
  :aliases        emacs-docker
  :custom-parsing t
  (unless (eldev--docker-on-supported-os)
    (signal 'eldev-error
            `(t ,(format eldev--docker-os-error-fmt-string system-type))))
  (unless (car parameters)
    (signal 'eldev-wrong-command-usage `(t "version not specified")))
  (let* ((img (eldev--docker-determine-img (car parameters)))
         (docker-exec (eldev-docker-executable))
         (escaped-params (mapcar #'eldev-quote-sh-string (cdr parameters)))
         (container-cmd (eldev--docker-container-eldev-cmd escaped-params))
         (as-gui (and (string= "emacs" container-cmd)
                      (not (member "--batch" parameters))))
         (local-eldev (getenv "ELDEV_LOCAL"))
         (exp-local-eldev (when local-eldev (expand-file-name local-eldev)))
         (eldev--container-bootstrap-cmd-fn
          (if local-eldev
              (apply-partially
               #'eldev--container-eldev-source-install-cmd "/eldev")
            eldev--container-bootstrap-cmd-fn))
         (args (append
                (eldev--docker-args img escaped-params as-gui exp-local-eldev))))
    (eldev-call-process
        docker-exec
        args
      :pre-execution
      (eldev-verbose "Running command '%s %s'"
                     docker-exec
                     (mapconcat #'identity args " "))
      :die-on-error
      (progn
        (delete-directory (eldev--docker-home) t)
        (when (string-match-p ".*unavailable, simulating -nw.*" (buffer-string))
          (eldev-warn "It appears your X server is not accepting connections from the docker container")
          (eldev-warn "Have you run `xhost +local:root' (remember about security issues, though)?\n"))
        (format "%s run" docker-exec))
      (eldev--forward-process-output
       (format "Output of the %s process:" docker-exec)
       (format "%s process produced no output" docker-exec)))
    (delete-directory (eldev--docker-home) t)))



;; eldev targets, eldev build, eldev compile, eldev package

(defvar eldev-build-system-hook nil
  "Hook executed whenever build system is used.
Since Eldev 0.1.1.")

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

(defvar eldev-build-ignored-target-fileset '(concat (eldev-package-descriptor-file-name) "c")
  "Fileset of apparent targets that should be ignored.
If a builder specifies a target that matches this fileset, it is
ignored completely: not even listed in `eldev targets' output.
Default value includes byte-compiled file for package descriptor
file, i.e. `PACKAGE-pkg.elc'.

Since 0.9.")

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

(defvar eldev-targets-list-sources t
  "Whether to print target sources.
In addition to t or nil, can also be symbol `concise'.")

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
  (when (memq 'test standard-filesets)
    (eldev--inject-loading-roots 'test))
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
  (let ((target-rule (eldev-get builder :targets))
        invocations
        ignored-targets)
    (when (functionp target-rule)
      (setf target-rule (funcall target-rule sources)))
    (dolist (invocation (pcase target-rule
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
                          (_ (error "Invalid `:targets' (or its return value) %S in builder `%s'" target-rule builder-name))))
      ;; Discard builder's wishes that match `eldev-build-ignored-target-fileset'.
      (let* ((targets (cdr invocation))
             (ignored (when eldev-build-ignored-target-fileset
                        (eldev-filter-files targets eldev-build-ignored-target-fileset))))
        (when ignored
          (setf ignored-targets (append ignored-targets ignored)))
        (when (> (length targets) (length ignored))
          (push `(,(car invocation) . ,(eldev-filter (not (member it ignored)) targets)) invocations))))
    (when ignored-targets
      (eldev-trace "%s" (eldev-message-enumerate-files "Ignored potential target%s: %s (%d)" ignored-targets)))
    (nreverse invocations)))

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


(defun eldev-get-target-dependencies (target &optional finder)
  "Get TARGET's dependencies.
Usually, this function should be of no interest: custom builders
should normally use only `eldev-set-target-dependencies'.
However, it is allowed to use this function as described below.

By default, list of all dependencies, i.e. across all possible
dependency finders, is returned.  However, if argument FINDER is
specified, returned list includes dependencies only for that
finder.

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
  (unless eldev--target-dependencies
    (error "May only be called inside `eldev-with-target-dependencies' macro"))
  (let ((dependencies (gethash target eldev--target-dependencies)))
    (if finder
        (assq finder dependencies)
      (let (all-dependencies)
        (dolist (entry dependencies)
          (dolist (dependency (cdr entry))
            ;; FIXME: O(N*N), but probably doesn't matter, as we don't
            ;;        expect large lists here.
            (unless (member dependency all-dependencies)
              (push dependency all-dependencies))))
        all-dependencies))))

(defun eldev-set-target-dependencies (target finder dependencies)
  "Set the list of TARGET's DEPENDENCIES according to given FINDER.
FINDER should be a unique symbol, e.g. caller function name.  The
purpose of it is that when function is called for the same target
and finder again, it replaces previous dependencies, but only
those found by the same finder.  This way several finders can
cooperate to find exhaustive dependency list without even knowing
of each other.

See documentation of `eldev-get-target-dependencies' for list's
elements description.

Evaluates to non-nil if FINDER's dependencies are changed, to nil
if they are exactly the same as before (possibly in different
order).  In some cases, even if return value is non-nil, final
dependencies can remain the same because of different finders.

This function may only be called while inside the body of a
`eldev-with-target-dependencies' macro."
  (let ((current-dependencies (eldev-get-target-dependencies dependencies finder)))
    (unless (equal (sort (copy-sequence dependencies)         (lambda (a b) (string< (car a) (car b))))
                   (sort (copy-sequence current-dependencies) (lambda (a b) (string< (car a) (car b)))))
      (eldev--assq-set finder (copy-sequence dependencies) (gethash target eldev--target-dependencies) #'equal)
      (setf eldev--target-dependencies-need-saving t))))

(defmacro eldev-with-target-dependencies (&rest body)
  "Execute BODY with target dependency mechanism set up."
  (declare (indent 0) (debug (body)))
  `(progn (eldev--load-target-dependencies)
          (unwind-protect
              (progn ,@body)
            (eldev--save-target-dependencies))))

(defun eldev--load-target-dependencies ()
  (eldev-do-load-cache-file (expand-file-name "target-dependencies.build" (eldev-cache-dir t)) "target dependencies" 2
    (setf eldev--target-dependencies (cdr (assq 'dependencies contents))))
  (unless eldev--target-dependencies
    (setf eldev--target-dependencies (make-hash-table :test #'equal))))

(defun eldev--save-target-dependencies ()
  (if eldev--target-dependencies-need-saving
      (eldev-do-save-cache-file (expand-file-name "target-dependencies.build" (eldev-cache-dir t)) "target dependencies" 2
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

Targets that additionally serve as sources to other targets are
always shown.  Other sources (e.g. `.el' files) can be shown or
not, depending on `--sources', `--concise' and `--no-sources'
options.  In concise mode, if a target has several sources, at
most three of them are listed in one line.

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

(eldev-defoption eldev-targets-list-sources ()
  "List all target sources"
  :options        (-s --sources)
  :for-command    targets
  :if-default     (and eldev-targets-list-sources (not (eq eldev-targets-list-sources 'concise)))
  (setf eldev-targets-list-sources t))

(eldev-defoption eldev-targets-list-sources-concise ()
  "List target sources in one line, even if they are many"
  :options        (-c --concise)
  :for-command    targets
  :if-default     (eq eldev-targets-list-sources 'concise)
  (setf eldev-targets-list-sources 'concise))

(eldev-defoption eldev-targets-omit-sources ()
  "Don't list target sources at all"
  :options        (-n --no-sources)
  :for-command    targets
  :if-default     (not eldev-targets-list-sources)
  (setf eldev-targets-list-sources nil))

(eldev-defbooloptions eldev-targets-list-dependencies eldev-targets-omit-dependencies eldev-targets-list-dependencies
  ("List known target dependencies"
   :options       (-d --dependencies))
  ("Don't show target dependencies, even if known"
   :options       (-D --no-dependencies))
  :for-command    targets)


(defun eldev--do-targets (level-targets level all-targets printed-target-entries)
  (when level-targets
    (let ((indentation   (make-string (* level 4) ? ))
          (level-targets (copy-sequence level-targets))
          level-sources)
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
          (cond ((and (null sources) (eq eldev-targets-list-sources 'concise))
                 (push target level-sources))
                ((or sources eldev-targets-list-sources (eldev-virtual-target-p target))
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
                                     (cadr dependency)))))))))
      (when level-sources
        (setf level-sources (nreverse level-sources))
        (let ((num (length level-sources)))
          (when (> num 3)
            (setf (nthcdr 3 level-sources) nil))
          (eldev-output "%s%s%s" indentation (mapconcat #'identity level-sources (eldev-colorize ", " 'details))
                        (if (> num 3) (eldev-colorize (eldev-format-message " + %d more" (- num 3)) 'details) "")))))))


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
    (when (memq 'test eldev-build-sets)
      (eldev--inject-loading-roots 'test))
    (eldev-load-project-dependencies 'build nil t))
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
          (eldev-autoinstalling-implicit-dependencies t
            (dolist (entry build-sequence)
              (if eldev-build-keep-going
                  ;; Ignore errors here: they will have been reported in `eldev-build-target'
                  ;; already.
                  (condition-case nil
                      (eldev-build-target (car entry))
                    (eldev-build-abort-branch))
                (eldev-build-target (car entry))))))
        (when (= (hash-table-count eldev--build-results) 0)
          (eldev-print "Nothing to do"))
        (maphash (lambda (_target status) (unless (eq status 'built) (setf anything-failed t))) eldev--build-results)
        ;; See e.g. test `eldev-loading-modes-3'.  Basically, once we load the main
        ;; project's package (in `as-is' mode), it would never get reloaded later, which
        ;; would be problematic if this build process was not the main invocation,
        ;; e.g. was only invoked from `autoloads' plugin.  For now, simply always "unload"
        ;; the main package: in the main invocation simply nothing would happen afterwards
        ;; anyway.
        (eldev--unload-package (package-desc-name (eldev-package-descriptor)))
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
                                       (eldev--silence-file-writing-message (expand-file-name target)
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
                (eldev-set-target-dependencies target 'eldev-builder-byte-compile-.el
                                               (mapcar (lambda (entry) `(inherits ,(eldev-replace-suffix (cdr entry) ".el" ".elc")))
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
  (or (when filename
        (setf filename (file-relative-name filename eldev-project-dir))
        (if (or (eldev-external-or-absolute-filename filename)
                (not (eldev-external-or-absolute-filename (file-relative-name filename eldev-cache-dir))))
            'external
          (setf filename (eldev-replace-suffix filename ".elc" ".el"))
          (when (file-exists-p (expand-file-name filename eldev-project-dir))
            filename)))
      'unknown))


(eldev-defbuilder eldev-builder-makeinfo (source target)
  :short-name     "MKINFO"
  :source-files   eldev-makeinfo-sources
  :targets        ((".texi" ".texinfo") -> ".info")
  :define-cleaner (eldev-cleaner-.info
                   "Delete `.info' files generated from `.texi' or `.texinfo'."
                   :aliases (info dot-info)
                   :default t)
  (eldev-call-process (eldev-makeinfo-executable) `("--no-split" ,source "--output" ,target ,@(when eldev-build-suppress-warnings '("--no-warn")))
    :die-on-error t
    (eldev--forward-process-output)))

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
    :die-on-error t
    (eldev--forward-process-output)))


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
                 (descriptor-file  (eldev-package-descriptor-file-name))
                 temporary-descriptor
                 files-to-tar)
            (condition-case nil
                (make-symbolic-link eldev-project-dir (expand-file-name name-version working-dir))
              (file-error
               (let ((working-dir-pkg (expand-file-name name-version working-dir)))
                 (copy-directory eldev-project-dir working-dir-pkg t t t))))
            (make-directory (file-name-directory package-target) t)
            (unless (file-exists-p descriptor-file)
              (with-temp-file (expand-file-name descriptor-file (expand-file-name name-version-dir working-dir))
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
            (let ((default-directory (file-name-as-directory working-dir)))
              (eldev-verbose "%s" (eldev-message-enumerate-files "Packaging the following file%s: %s (%d)" sources))
              (eldev-call-process (eldev-tar-executable) `("-cf" ,(expand-file-name package-target eldev-project-dir) ,@(nreverse files-to-tar))
                :trace-command-line "Full command line to create package tarball"
                (if (= exit-code 0)
                    (eldev--forward-process-output)
                  (eldev-warn "`tar' output (ran from directory `%s'):" working-dir)
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



;; eldev plugins

(defvar eldev--active-plugin-documentation)

(eldev-defcommand eldev-plugins (&rest parameters)
  "Document Eldev plugins activated for this project.  You can
limit output to only documentation of given plugins instead of
all active.

If run in quiet mode, only plugin names are printed, without
documentation."
  :parameters     "[PLUGIN-NAME...]"
  :aliases        plugin
  (require 'eldev-plugins)
  (let ((plugins (if parameters
                     (mapcar (lambda (name)
                               (unless (assq (setf name (intern name)) eldev--active-plugin-documentation)
                                 (signal 'eldev-error `("Plugin `%s' doesn't exist or is not activated for this project" ,name)))
                               name)
                             parameters)
                   (sort (eldev-active-plugins) #'string<))))
    (if plugins
        (while plugins
          (let ((plugin (pop plugins)))
            (if (eldev-unless-quiet t)
                (progn (eldev-output "%s\n" (eldev-colorize plugin 'section))
                       (eldev-output "%s" (or (cdr (assq plugin eldev--active-plugin-documentation)) "Not documented"))
                       (when plugins
                         (eldev-output "\n")))
              (eldev-output "%s" plugin))))
      (eldev-print "No plugins are activated for this project (see function `eldev-use-plugin')"))))



;; eldev init

(defvar eldev-init-interactive t)

(declare-function eldev--do-init 'eldev-vc)

(eldev-defcommand eldev-init (&rest parameters)
  "Initialize project in this directory to use Eldev.  This command
will fail if the project already has file named `Eldev'."
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  ;; Real work is done in `eldev-vc'.
  (require 'eldev-vc)
  (eldev--do-init))

(eldev-defbooloptions eldev-init-interactive-mode eldev-init-non-interactive-mode eldev-init-interactive
  ("Create `Eldev' interactively"
   :options       (-i --interactive))
  ("Don't ask any questions"
   :options       (-n --non-interactive))
  :for-command    init)


(provide 'eldev)

;;; eldev.el ends here

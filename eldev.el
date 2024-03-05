;;; eldev.el --- Elisp development tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2019-2024 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    1.9.1snapshot
;; Keywords:   maint, tools
;; Homepage:   https://github.com/emacs-eldev/eldev
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

;; Eldev (Elisp development tool) is an Emacs-based build system,
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
;;      $ curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/bin/eldev > eldev && chmod a+x eldev
;;
;; No further steps necessary — Eldev will bootstrap itself as needed
;; on first invocation.

;; If you don't have such a directory and don't care where `eldev'
;; executable is placed.
;;
;; 1. Run:
;;
;;      $ curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
;;
;;    This will install eldev script to ~/.local/bin.
;;
;; 2. If not yet there, add the directory to your $PATH; e.g. in ~/.profile add this:
;;
;;      export PATH="$HOME/.local/bin:$PATH"
;;
;; Afterwards Eldev will bootstrap itself as needed on first
;; invocation.

;; For further help and more ways to install, please see the homepage:
;;
;;   https://github.com/emacs-eldev/eldev


;;; Code:

(require 'eldev-util)


;; To silence byte-compilation warnings on Emacs 24-25.
(defvar inhibit-message)
(defvar package-archive-priorities)

;; Using `autoload' function directly instead of ;;;###autoload cookies since the latter
;; won't work with ELDEV_LOCAL.  This must be at the top _and_ in `eval-and-compile', else
;; byte-compilation gives warnings...
(eval-and-compile
  (dolist (autoloads '(("eldev-build"   eldev-build-find-targets eldev-get-target-dependencies eldev-set-target-dependencies eldev-build-target-status eldev-build-target)
                       ("eldev-plugins" eldev-active-plugins eldev-use-plugin
                                        ;; Maintainer plugin is often configured in Eldev even if not activated there.
                                        eldev-release-next-major-version eldev-release-next-minor-version eldev-release-next-patch-version eldev-release-next-snapshot-version
                                        eldev-release-next-snapshot-version-unless-already-snapshot eldev-release-next-pos-version eldev-release-default-tag
                                        eldev-release-validate-version eldev-release-validate-vcs eldev-release-only-from-main-branch eldev-release-validate-files
                                        eldev-release-test-project eldev-release-maybe-fail)
                       ("eldev-vc"      eldev-vc-root-dir eldev-vc-executable eldev-vc-full-name eldev-with-vc eldev-with-vc-buffer eldev--vc-set-up-buffer eldev-vc-synchronize-dir
                                        eldev-vc-detect eldev-vc-commit-id eldev-vc-branch-name)
                       ("eldev-doctor"  eldev-defdoctest)))
    (dolist (function (cdr autoloads))
      (autoload function (car autoloads)))))


(defvar eldev-shell-command (or (eldev-getenv "ELDEV_CMD") "eldev")
  "Command used to launch Eldev, in raw form.
You should use function `eldev-shell-command' in most cases
instead.")

(defvar eldev-emacs-executable (or (eldev-getenv "ELDEV_EMACS") (eldev-getenv "EMACS") "emacs")
  "Emacs executable used for Eldev.")

(defconst eldev-dir (or (eldev-getenv "ELDEV_DIR")
                        (when (file-directory-p "~/.eldev") "~/.eldev")
                        (expand-file-name "eldev" (eldev-xdg-cache-home)))
  "User's Eldev cache directory, usually `~/.cache/eldev'.
This directory is global, i.e. not project-specific.  Since 1.4
function `eldev-global-cache-dir' can be used to access this
value.")

(defconst eldev-user-config-file (expand-file-name "config"
                                                   (or (eldev-getenv "ELDEV_DIR")
                                                       (when (file-directory-p "~/.eldev") "~/.eldev")
                                                       (expand-file-name "eldev" (eldev-xdg-config-home))))
  "User's Eldev configuration file, usually `~/.config/eldev/config'.
This file is global, i.e. not project-specific.")

(defconst eldev-file "Eldev"
  "Project's Eldev configuration file, `Eldev'.")

(defconst eldev-local-file "Eldev-local"
  "Working directory Eldev configuration file, `Eldev-local'.")

(defconst eldev-cache-dir ".eldev"
  "Name of Eldev cache subdirectory, `.eldev'.
See also function `eldev-cache-dir'.")

(defconst eldev-global-cache-dir "global-cache"
  "Name of the global package cache directory (a subdirectory of `eldev-dir').
Since 1.4 function `eldev-global-package-archive-cache-dir' can
be used instead.")

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
     . "Clean project's or dependency's working directory first.  As a result,
Elisp functions in it are not byte-compiled.")
    (byte-compiled
     . "Compile all `.el' files first.  This will make Elisp functions faster,
but backtraces less informative.")
    (compiled-on-demand
     . "Compile `.el' files only when they are being `require'd.  For larger
projects this allows to avoid recompiling everything just to test some
core functionality.  Even more important if higher-level files do not
even compile at the moment, but you still want to run tests on the
byte-compiled core.")
    (noisy-compiled-on-demand
     . "Like `compiled-on-demand', but additionally prints standard
information about compilations it performs.  The “normal” mode only
prints warnings and errors, as output “in the middle” can potentially
break the compiled project or whatever uses it.")
    (built
     . "Build project or dependency first.  Only important for those projects
that define custom rules, i.e. where target `:default' is not empty.")
    (built-and-compiled
     . "Build and byte-compile first.  Exactly as `built' and `compiled'
combined.")
    (built-source
     . "Clean the project or dependency, then build target `:default'.  See
`source' and `built' modes for more explanations.")))


(defvar eldev-formatted-project-name nil
  "User-level project name, e.g. with proper capitalization.
If left to `nil', will default to Emacs package name.  Since 1.2.")

(defvar eldev--formatted-project-names nil
  "Cache for `eldev-formatted-project-name' function.")

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

(defvar eldev-files-to-package
  '(:and ("*.el" "./*.info" "./dir" "./doc/*.info" "./doc/dir")
         ;; Since 1.2 we exclude the autoloads file specifically.  Pre-29 Emacs versions
         ;; seemed to cope with it fine, 29 does not.  However, text "Do not include any
         ;; file named ‘NAME-autoloads.el’" has been in the documentations since years, so
         ;; this change in behavior cannot be considered an Emacs 29 bug.
         (concat "!./" (eldev-package-autoloads-file-name)))
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

(defvar eldev-normal-dependency-management t
  "Use standard package-based dependency management.
Can be disabled in special cases.  When disabled, Eldev won't
operate with packages, but instead rely on variable `load-path'
to be set as appropriate, e.g. using environment variable
`EMACSLOADPATH'.

Since 1.9.")

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

(defvar eldev-robust-mode 'auto
  "Whether to retry on certain errors instead of giving up.
Special symbol \\='auto means that Eldev should retry if executed
on a continuous integration server, i.e. where sleeping and
retrying later generally makes sense.

Since 1.5.")

(defvar eldev-robust-mode-retry-delays '(30 60 120 180 300)
  "Delays before robust-mode-retrials, in seconds.
This is not controllable from the command line, but can be
changed directly.

Since 1.5.")

(defvar eldev-skip-global-config nil
  "Whether to skip file `~/.config/eldev/config'.
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
    ;; Need GNU ELPA for `let-alist' on older Emacs versions.
    (package-lint :version "0.14"   :archives (melpa gnu-elpa))
    (relint       :version "1.18"   :archive  gnu-elpa)
    ;; Need GNU ELPA for `let-alist' on older Emacs versions.
    (elisp-lint                     :archives (melpa gnu-elpa))
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


(dolist (var '(eldev-main-fileset eldev-test-fileset eldev-standard-excludes eldev-files-to-package eldev-makeinfo-sources eldev-info-sources))
  (eldev-watch-fileset-for-suspicious-modifications var))


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
        ((and (or :category :command-hook :briefdoc :parameters :custom-parsing :hidden-if :works-on-old-eldev :profiling-self) keyword)
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

    :category CATEGORY

        Generic category of the command.  Should be one of
        `testing', `running', `dependencies', `building' or
        `information'; all other commands are treated as
        miscellaneous.  Since 1.3.

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

    :profiling-self FLAG

        If true, command `profile' will leave it up to the nested
        command to start/stop profiling where suitable,
        presumably using `eldev-profile-body'.

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
        default.  Since in Eldev everything is customizable in
        different ways, result should not be hardcoded.

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

(defun eldev-inherit-options (to-command from-command &optional filter)
  "Make options of FROM-COMMAND also available for TO-COMMAND.
By default all options of FROM-COMMAND are taken.

However, you can optionally specify a FILTER.  It should be a
function accepting two arguments: option name as a symbol and
option handler function.  The filter must return t or nil (other
values are reserved for future functionality) that specifies if
given option should be made available for TO-COMMAND.  Filter is
called for all option variants; e.g. for command `exec' options
`-f' and `--file' would be considered separately.

Since 1.7."
  (unless (assq from-command eldev--commands)
    (error "Unknown command `%s'" from-command))
  (dolist (entry (reverse (cdr (assq from-command eldev--options))))
    (let ((option  (car entry))
          (handler (cdr entry)))
      (when filter
        (pcase (funcall filter option handler)
          ;; All other return values are reserved for future.  Ideas: changing option
          ;; name, changing (wrapping) handler, providing keywords.
          (`t)
          (`nil  (setf option nil))
          (value (error "Filter function may only return t or nil currently (got `%S' instead)" value))))
      (when option
        (eldev--register-option handler option to-command nil)))))

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
  (setf eldev-backtrace-style (if width (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number width :min 0)) t)))

(eldev-defoption eldev-cut-backtraces (&optional types)
  "Cut backtraces; active notches can be limited to
comma/space-separated TYPES"
  :options        (-u --cut-backtraces)
  :optional-value TYPES
  :default-value  (cond ((consp eldev-cut-backtraces)
                         (eldev-message-enumerate nil eldev-cut-backtraces #'symbol-name))
                        ((eq eldev-cut-backtraces t)
                         "all")
                        (t
                         :no-default))
  (setf eldev-cut-backtraces (cond ((member types '(nil "all")) t)
                                   ((string= types "none")      nil)
                                   (t                           (mapcar #'intern (split-string types "[ \t,]" t))))))

(eldev-defoption eldev-dont-cut-backtraces ()
  "Never cut backtraces"
  :options        (-U --full-backtraces --dont-cut-backtraces)
  :hidden-if      (null eldev-cut-backtraces)
  (setf eldev-cut-backtraces nil))

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
        (eldev-output "%s\n\n%s" (eldev-colorize (car mode) 'section) (eldev-format-message (cdr mode)))
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

(eldev-defoption eldev-set-loading-mode-compiled-on-demand ()
  "Shorthand for `--loading=compiled-on-demand'"
  :options        (-o --compiled-on-demand)
  :hidden-if      (eq eldev-project-loading-mode 'compiled-on-demand)
  (eldev-set-loading-mode 'compiled-on-demand))

(eldev-defoption eldev-set-loading-mode-noisy-compiled-on-demand ()
  "Shorthand for `--loading=noisy-compiled-on-demand'"
  :options        (-O --noisy-compiled-on-demand)
  :hidden-if      (eq eldev-project-loading-mode 'compiled-on-demand)
  (eldev-set-loading-mode 'noisy-compiled-on-demand))

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
  (setf eldev-coloring-mode (eldev--auto-always-never-option mode)))

;; Not advertised, this is mostly for external tools.
(eldev-defoption eldev-setup-first-form (form)
  "Evaluate FORM as the first step of the setup"
  :options        --setup-first
  :value          FORM
  :hidden-if      t
  (dolist (form (eldev-read-wholly form "setup form(s)" t))
    (push form eldev-setup-first-forms)))

(eldev-defoption eldev-setup-form (form)
  "Evaluate FORM as the final step of the setup"
  :options        (-S --setup)
  :value          FORM
  (dolist (form (eldev-read-wholly form "setup form(s)" t))
    (push form eldev-setup-forms)))

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

(eldev-defbooloptions eldev-manage-dependencies eldev-disable-dependencies eldev-normal-dependency-management
  ("Use standard dependency-as-package management"
   :options       --manage-dependencies
   :hidden-if     :default)
  ("Disable standard dependency-as-package management; depend on externally-set `EMACSLOADPATH'"
   :options       (--disable-dependencies --use-emacsloadpath)))

(eldev-defoption eldev-robust-mode (&optional mode)
  "Whether to retry on certain errors; WHEN can be `always', `auto' or `never'"
  :options        (-R --robust-mode --ci-mode)
  :optional-value WHEN
  :default-value  (if eldev-robust-mode (if (eq eldev-robust-mode 'auto) "auto" "always") "never")
  (setf eldev-robust-mode (eldev--auto-always-never-option mode)))

(eldev-defbooloptions eldev-enable-xdebug-initially eldev-disable-xdebug-initially eldev-xdebug-output-enabled
  ("Enable `xdebug' output initially"
   :options       (-x --xdebug --enable-xdebug)
   :hidden-if     :default)
  ("Disable `xdebug' output initially"
   :options       (--no-xdebug --disable-xdebug)
   :hidden-if     :default))

(defun eldev--auto-always-never-option (mode)
  (when mode
    (setf mode (downcase mode)))
  (cond ((or (null mode) (member mode '("always" "on"))) t)
        ((string= mode "auto")                           'auto)
        ((member mode '("never" "off"))                   nil)
        (t (signal 'eldev-wrong-option-usage `("argument must be `always', `auto' or `never'")))))



;; Filesets.

(defun eldev-standard-fileset (name &optional no-excludes without-generated)
  "Return a computed fileset for target set with given NAME.
Unless NO-EXCLUDES is specified, all targets from
`eldev-standard-excludes' are ignored.

Since 1.3: when NO-GENERATED is specified, generated files are
ignored too."
  (eldev-standard-filesets (if no-excludes :no-excludes :std-excludes) (if without-generated :without-generated :with-generated) name))

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

        Whether to use `eldev-standard-excludes' (default) or not.

    :with-generated or :without-generated (since 1.3)

        Whether to include generated files (default) or not; see
        function `eldev-generated-files'."
  (let ((combination-mode :or)
        except
        no-excludes
        without-generated
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
        ((or :with-generated :without-generated)
         (setf without-generated (eq argument :without-generated)))
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
    (when without-generated
      (let (generated)
        (dolist (file (eldev-generated-files))
          (push (format "/%s" file) generated))
        (setf result `(:and ,result (:not ,generated)))))
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

(defvar eldev-ongoing-named-steps nil
  "List (stack) of currently performed named steps.
Each entry has the form of (TYPE . NAME), where both values are
strings; TYPE can also be nil.  If an error happens during one,
user will be hinted when exactly the error occurred.  Don't
modify this directly, use macro `eldev-named-step' instead.

Since 1.0.")

(defvar eldev-preprocessed-command-line nil
  "Non-options to be returned from `eldev-parse-command-line'.
The value is a reversed list of non-option parameters encountered
so far.  Option handlers (i.e. bodies of `eldev-defoption') may
access it and modify freely.  This is useful if precise order of
certain options relative to non-option parameters is important.

Since 0.11.")

(defvar eldev-preprocessed-command-line-options nil
  "Options to be returned from `eldev-parse-command-line'.
See `eldev-preprocessed-command-line' for details.

Since 1.7.")

(defvar backtrace-line-length)
(defvar emacs-repository-branch)


(defun eldev-start-up ()
  "Prepare Eldev.
Used by Eldev startup script."
  (setf eldev--running-from-dir default-directory
        package-user-dir        (expand-file-name "packages" (eldev-cache-dir t))
        package-archives        nil
        user-emacs-directory    (eldev-user-emacs-dir))
  ;; The idea of postponing directory creation is to avoid littering random directories
  ;; with `.eldev' in case Eldev is executed there by error, for example.
  (add-hook 'eldev--project-validated-hook (lambda () (eldev-user-emacs-dir t))))

(defmacro eldev--maybe-with-target-dependencies (do-set-up public &rest body)
  "Execute BODY, possibly setting up `eldev--target-dependencies'."
  (declare (indent 2) (debug (form form body)))
  (let ((set-up-now (make-symbol "$set-up-now")))
    `(let ((,set-up-now (when ,do-set-up (eldev--load-target-dependencies ,public))))
       (unwind-protect
           (progn ,@body)
         (when ,set-up-now
           (eldev--save-target-dependencies))))))

(defun eldev-cli (command-line)
  "Eldev entry point."
  ;; We parse command line in a way separate from `command-line-args' and
  ;; `command-line-args-left', but whatever code we execute from here should believe there
  ;; are no arguments left.
  (eldev--work-around-emacs-bug-65763)
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
                                                        (let ((inhibit-message nil))
                                                          (apply original arguments))))
                        ;; Make sure debugger doesn't truncate strings in error's data, as
                        ;; they are often very important and in non-interactive usage you
                        ;; cannot expand "..." manually.  Afterwards lines may still be
                        ;; truncated if needed; this is fine, as final line length limit
                        ;; can be adjusted using option `-b'.
                        (eldev-advised ('debugger--insert-header :around (lambda (original &rest args)
                                                                           (let ((backtrace-line-length nil))
                                                                             (apply original args))))
                          (eldev-parse-options command-line nil t t)
                          (when eldev-print-backtrace-on-abort
                            (add-hook 'kill-emacs-hook #'eldev-backtrace))
                          ;; Since this is printed before `~/.config/eldev/config' is loaded,
                          ;; it can ignore some settings from that file, e.g. colorizing mode.
                          (eldev-trace "Started up on %s" (replace-regexp-in-string " +" " " (current-time-string)))
                          (eldev-trace "Running on %s" (replace-regexp-in-string "[ \t\n]+" " " (emacs-version)))
                          (let ((revision emacs-repository-version)
                                (branch   (when (boundp 'emacs-repository-branch) emacs-repository-branch)))
                            (if revision
                                (eldev-trace "Emacs source: rev. %s%s"
                                             (if revision (substring revision 0 (min (length revision) 10)) "?")
                                             (if branch (eldev-format-message " of branch `%s'" branch) ""))
                              (eldev-trace "Emacs source is unknown")))
                          (eldev-trace "Project directory: `%s'" eldev-project-dir)
                          (condition-case error
                              (eldev--set-up)
                            (eldev-too-old (setf eldev-too-old (cdr error))))
                          (let* ((external-dir     (eldev-external-package-dir))
                                 (package-user-dir (or external-dir package-user-dir)))
                            (setf command-line (eldev-parse-options command-line nil t))
                            (if command-line
                                (let* ((compile-on-demand-quiet   (eq eldev-project-loading-mode 'compiled-on-demand))
                                       (compile-on-demand-sources (when (or compile-on-demand-quiet (eq eldev-project-loading-mode 'noisy-compiled-on-demand)) (list nil)))
                                       archive-files-to-delete)
                                  ;; If we are using external directory, maybe rename
                                  ;; archives so that they don't clash with what is
                                  ;; retrieved in that directory already.
                                  (when external-dir
                                    (dolist (entry package-archives)
                                      (push (setf (car entry) (eldev--maybe-rename-archive (car entry) external-dir)) archive-files-to-delete))
                                    (push (setf eldev--internal-pseudoarchive (eldev--maybe-rename-archive eldev--internal-pseudoarchive external-dir)) archive-files-to-delete))
                                  (setf command (intern (car command-line)))
                                  (unwind-protect
                                      (eldev-advised (#'load :before (when compile-on-demand-sources
                                                                       (lambda (file &optional _noerror _nomessage nosuffix &rest _ignored)
                                                                         (unless nosuffix
                                                                           (let* ((default-directory eldev-project-dir)
                                                                                  (relative-name     (file-relative-name file)))
                                                                             ;; Ignore files outside the project or inside Eldev's local project cache (e.g. from dependencies).
                                                                             (unless (or (eldev-external-or-absolute-filename relative-name)
                                                                                         (not (eldev-external-filename (file-relative-name relative-name eldev-cache-dir))))
                                                                               (when (if (file-exists-p relative-name)
                                                                                         (string-suffix-p ".el" relative-name)
                                                                                       (file-exists-p (setf relative-name (concat relative-name ".el"))))
                                                                                 (eldev--maybe-byte-compile-.el-on-demand relative-name compile-on-demand-sources compile-on-demand-quiet))))))))
                                        ;; See comments in `eldev--byte-compile-.el'.
                                        (eldev-advised (#'require :before (when compile-on-demand-sources
                                                                            (lambda (feature &optional filename &rest _ignored)
                                                                              (unless (and feature (memq feature features))
                                                                                (let ((default-directory eldev-project-dir))
                                                                                  (eldev--maybe-byte-compile-.el-on-demand (or filename (eldev--find-feature-provider feature))
                                                                                                                           compile-on-demand-sources compile-on-demand-quiet))))))
                                          (eldev--maybe-with-target-dependencies compile-on-demand-sources nil
                                            (eldev--execute-command command-line))
                                          (setf exit-code 0)))
                                    (when (setf archive-files-to-delete (eldev-filter (file-exists-p (eldev--package-archive-dir it external-dir)) archive-files-to-delete))
                                      (eldev-trace "Deleting package archive contents to avoid polluting the external directory: %s"
                                                   (eldev-message-enumerate nil archive-files-to-delete))
                                      (dolist (archive archive-files-to-delete)
                                        (ignore-errors (delete-directory (eldev--package-archive-dir archive external-dir) t))))))
                              (eldev-usage)
                              (eldev-print "Run `%s help' for more information" (eldev-shell-command t))
                              (setf exit-code 0))))))))
              ;; FIXME: `command' will be wrong if the error occurs during nested command
              ;;        processing, e.g. `$ eldev profile ... eval --unknown-option ...'.
              (eldev-error (eldev--print-error error command))
              (eldev-quit  (setf exit-code (cdr error))))
          (error (eldev-error "%s" (error-message-string error))
                 (eldev--inform-about-named-steps)
                 (eldev-print :stderr "Run with `--debug' (`-d') option to see error backtrace")))
      (remove-hook 'kill-emacs-hook #'eldev-backtrace)
      (eldev-trace "Finished %s on %s" (if (eq exit-code 0) "successfully" "erroneously") (replace-regexp-in-string " +" " " (current-time-string))))
    (when eldev-too-old
      (when (eq (car eldev-too-old) :hint)
        (setf eldev-too-old (cddr eldev-too-old)))
      (eldev-warn "%s" (apply #'eldev-format-message eldev-too-old)))
    (or exit-code 1)))

;; See https://debbugs.gnu.org/db/65/65763.html.  Even if they "decide whether this will be the solution" in a
;; year, this bug is still present in released Emacs versions.
(declare-function vc-refresh-state nil)  ; For Emacs 24.
(defun eldev--work-around-emacs-bug-65763 ()
  ;; Don't reset `debug-on-error' for random calls, this might in theory affect something in an undesired way.
  ;; Suppressing the warning too.  No-one asked for anything here, just open the file, ffs.
  (let ((at (memq 'vc-refresh-state find-file-hook)))
    (when at
      (setf (car at) (lambda () (let ((debug-on-error nil) (inhibit-message t)) (vc-refresh-state))))))
  (advice-add 'save-buffer :around (lambda (original &rest args)
                                     (eldev-advised ('vc-before-save :around (lambda (original &rest args)
                                                                               (let ((debug-on-error nil) (inhibit-message t)) (apply original args))))
                                       (apply original args)))))

(defmacro eldev-named-step (type name &rest body)
  "Execute BODY as a named step of given TYPE.
If an error occurs in BODY, user will be shown step description,
thus hopefully making debugging easier.

Since 1.0."
  (declare (indent 2) (debug (sexp sexp body)))
  `(prog1 (progn (push (cons ,type ,name) eldev-ongoing-named-steps)
                 ,@body)
     ;; This shouldn't be performed as cleanup in an `unwind-protect' form: the
     ;; whole purpose of the variable is to keep the name of the failed step if an
     ;; error occurs!
     (pop eldev-ongoing-named-steps)))

(defun eldev-current-step-name (&optional upcase-first)
  "Name of the current step, possibly with first letter upcased.
See macro `eldev-named-step'.

Since 1.0."
  (let ((name (cdar eldev-ongoing-named-steps)))
    (if (and upcase-first name)
        (eldev-message-upcase-first name)
      name)))

(defun eldev--require-external-feature (feature)
  (eldev-named-step nil (eldev-format-message "requiring feature `%s' from an external tool" feature)
    (require feature)))

(defun eldev--execute-command (command-line)
  (let* ((command      (intern (car command-line)))
         (real-command (or (cdr (assq command eldev--command-aliases)) command))
         (handler      (cdr (assq real-command eldev--commands))))
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
            (apply handler command-line)))
      (eldev--complain-about-missing-command command)
      (eldev-print "Run `%s help' for a list of supported commands" (eldev-shell-command t)))))

(defun eldev--complain-about-missing-command (command)
  ;; FIXME: Generalize.  Not clear how, currently.
  (let ((plugin (pcase command (`release "maintainer"))))
    (if plugin
        (eldev-error "Command `%s' will become available if plugin `%s' is activated" command plugin)
      (eldev-error "Unknown command `%s'" command))))

(defun eldev--maybe-rename-archive (archive external-dir)
  (catch 'renamed-as
    (let (k)
      (while t
        (let* ((new-name (if k (format "%s-%03d" archive k) archive))
               (filename (eldev--package-archive-dir new-name external-dir)))
          (unless (file-exists-p filename)
            (throw 'renamed-as new-name))
          (setf k (if k (1+ k) 1)))))))

(defun eldev--print-error (error &optional command as-warning)
  (let* ((arguments (cdr error))
         eldev-error
         hint
         hint-about-command)
    ;; Elisp apparently doesn't expose that information.
    (ignore-errors
      (condition-case nil
          (signal (car error) nil)
        (eldev-error (setf eldev-error t))))
    (if eldev-error
        (progn
          (when (and (eq (car error) 'eldev-wrong-command-usage) (not (stringp (car arguments))))
            (setf arguments `(:command ,(or command t) ,@(cdr arguments))))
          (when (eq arguments eldev-too-old)
            (setf eldev-too-old nil))
          (pcase (car arguments)
            (:hint    (pop arguments) (setf hint               (pop arguments)))
            (:command (pop arguments) (setf hint-about-command (pop arguments))))
          (let ((message (apply #'eldev-format-message arguments)))
            (if as-warning
                (eldev-warn (if (eq as-warning t) "%s" as-warning) message)
              (eldev-error "%s" message)))
          (when hint
            (eldev-print :stderr "%s" (apply #'eldev-format-message (eldev-listify hint))))
          (when hint-about-command
            (eldev-print :stderr "Run `%s help%s' for more information" (eldev-shell-command t) (if (eq hint-about-command t) "" (format " %s" hint-about-command)))))
      (eldev-error "%s" (error-message-string error)))
    (eldev--inform-about-named-steps)))

(defun eldev--inform-about-named-steps ()
  (dolist (step eldev-ongoing-named-steps)
    (eldev-print :stderr "Failed%s step: %s" (if (car step) (format " %s" (car step)) "") (cdr step))))

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
      (eldev-named-step "setup" (eldev-format-message "evaluating form `%S' specified on the command line" form)
        (eldev-trace "%s..." (eldev-current-step-name t))
        (eval form t)))
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
                (eldev-named-step "setup" (eldev-format-message "loading file `%s'" filename)
                  (eldev-trace "%s..." (eldev-current-step-name t))
                  (load file nil t t))
                (when (memq symbol '(eldev-file eldev-local-file))
                  (setf loaded-project-config t)))
            (eldev-verbose (cdr config) filename)))))
    (dolist (form (reverse eldev-setup-forms))
      (eldev-named-step "setup" (eldev-format-message "evaluating form `%S' specified on the command line" form)
        (eldev-trace "%s..." (eldev-current-step-name t))
        (eval form t)))
    (when loaded-project-config
      ;; This is an undocumented flag file indicating that `Eldev' or `Eldev-local' have
      ;; been loaded at least once in this project.
      (with-temp-file (expand-file-name "ever-initialized" (eldev-cache-dir nil t))))))

(defun eldev-formatted-project-name (&optional project-dir skip-cache)
  "Determine the user-level name of the project in PROJECT-DIR.
This is the value of variable `eldev-formatted-project-name' or
else defaults to Emacs package name.

Since 1.2."
  (unless project-dir
    (setf project-dir eldev-project-dir))
  (or (unless skip-cache
        (cdr (assoc project-dir eldev--formatted-project-names)))
      (let ((name (or (eldev--cross-project-internal-eval project-dir 'eldev-formatted-project-name t)
                      (symbol-name (package-desc-name (eldev-package-descriptor project-dir skip-cache))))))
        (unless skip-cache
          (push (cons project-dir name) eldev--formatted-project-names))
        name)))

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

(defun eldev-parse-command-line (command-line &rest options)
  "Parse global or command-specific options in COMMAND-LINE.
OPTIONS may contain following keywords (unknown are ignored for
future extensibility):

    :command SYMBOL

        Accept options for given command.  If not specified,
        global options are accepted instead.

    :stop-on-non-option BOOLEAN

        Stop parsing upon encountering anything that is not an
        option.  By default options can be intermixed with other
        command line elements.

    :allow-unknown BOOLEAN

        Don't die if encountering an unknown option (by default
        those trigger an error).

    :dry-run BOOLEAN

        Don't execute option handlers, just parse the command
        line.  By default, options automatically trigger their
        corresponding handlers.  If this is set, handlers won't
        be able to modify `eldev-preprocessed-command-line' and
        `eldev-preprocessed-command-line-options' either.

Returns a plist with at least the following keys: `:full' (the
full command line), `:without-options' (command line after
options are removed) and `:all-options' (all options specified on
the command line), in any order.

Since 1.7.  See also older `eldev-parse-options' with less
functionality."
  (let ((command            (plist-get options :command))
        (stop-on-non-option (plist-get options :stop-on-non-option))
        (dry-run            (plist-get options :dry-run))
        (full-command-line  command-line)
        eldev-preprocessed-command-line
        eldev-preprocessed-command-line-options)
    (save-match-data
      (while command-line
        (let* ((term     (pop command-line))
               (stop-now (string= term "--")))
          (if (and (string-prefix-p "-" term) (not stop-now) (not (string= term "-")))
              (let ((long-option (string-prefix-p "--" term))
                    rest)
                (push term eldev-preprocessed-command-line-options)
                (if long-option
                    (when (string-match (rx bol (group (1+ (not (any "=")))) "=" (group (0+ anything)) eol) term)
                      (setf rest (match-string 2 term)
                            term (match-string 1 term)))
                  (setf rest (when (> (length term) 2) (substring term (length "--")))
                        term (substring term 0 (length "--"))))
                (while term
                  (let ((handler (cdr (assq (intern term) (cdr (assq command eldev--options))))))
                    (cond (handler
                           (condition-case error
                               (let ((value-mode (eldev-get handler :option-value)))
                                 (when dry-run
                                   (setf handler #'ignore))
                                 (if (and value-mode (or rest (cdr value-mode)))
                                     (progn
                                       (funcall handler (or rest (if command-line
                                                                     (let ((value (pop command-line)))
                                                                       (push value eldev-preprocessed-command-line-options)
                                                                       value)
                                                                   (if command
                                                                       (signal 'eldev-wrong-command-usage `(t "Option `%s' for command `%s' requires an argument" ,term ,command))
                                                                     (signal 'eldev-wrong-command-usage `(t "Option `%s' requires an argument" ,term))))))
                                       (setf rest nil))
                                   (funcall handler)))
                             (eldev-wrong-option-usage (signal 'eldev-error `(:command ,command "For option `%s': %s" ,term ,(apply #'eldev-format-message (cdr error)))))))
                          ;; The following options are special-cased.  They are not
                          ;; advertised, since normally commands `help' and `version'
                          ;; should be used instead, but still handled as too common.
                          ((string= term "--help")
                           (when (and (null command) command-line (assq (intern (car command-line)) eldev--commands))
                             (setf command (intern (car command-line))))
                           (if command
                               (eldev-help (symbol-name command))
                             (eldev-help))
                           (signal 'eldev-quit 0))
                          ((and (null command) (string= term "--version"))
                           (apply #'eldev-version command-line)
                           (signal 'eldev-quit 0))
                          ((plist-get options :allow-unknown)
                           (setf term nil))
                          (t
                           (signal 'eldev-wrong-command-usage `(t "Unknown option `%s'" ,term)))))
                  (if long-option
                      (setf term nil)
                    (setf term (when rest (format "-%c" (aref rest 0)))
                          rest (when (> (length rest) 1) (substring rest (length "-")))))))
            (unless stop-now
              (push term eldev-preprocessed-command-line))
            (when (or stop-now stop-on-non-option)
              (while command-line
                (push (pop command-line) eldev-preprocessed-command-line))))))
      `(:full            ,full-command-line
        :all-options     ,(nreverse eldev-preprocessed-command-line-options)
        :without-options ,(nreverse eldev-preprocessed-command-line)))))

(defun eldev-parse-options (command-line &optional command stop-on-non-option allow-unknown)
  "Parse global or command-specific options.
Returns COMMAND-LINE with options removed.  See also
`eldev-parse-command-options'."
  (plist-get (eldev-parse-command-line command-line :command command :stop-on-non-option stop-on-non-option :allow-unknown allow-unknown)
             :without-options))

(defun eldev-global-cache-dir (&optional ensure-exists)
  "Return `eldev-dir', possibly ensuring that it exists.
This function always returns an absolute path, even if the value
of `eldev-dir' is not absolute.  Since 1.4."
  (let ((cache-dir (expand-file-name eldev-dir)))
    (when ensure-exists
      (make-directory cache-dir t))
    cache-dir))

(defun eldev-global-package-archive-cache-dir (&optional ensure-exists)
  "Return the `global-archive' subdirectory of `eldev-dir'.
If instructed, this function also makes sure that the directory
exists.  Since 1.4."
  (let ((cache-dir (expand-file-name eldev-global-cache-dir (eldev-global-cache-dir))))
    (when ensure-exists
      (make-directory cache-dir t))
    cache-dir))

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
               (print-circle               t)
               (print-continuous-numbering nil))
           (prin1 (cons (cons 'version ,version) data) (current-buffer))
           (insert "\n")))
     ;; Since this is not overly important, just print a verbose-level message.
     (error (eldev-verbose "Failed to save %s: %s" ,description (error-message-string error)))))

(defun eldev--shell-script-name ()
  (if (eq system-type 'windows-nt)
      "eldev.bat"
    "eldev"))

(defun eldev-global-setting-options ()
  "Return a list of options to pass global settings to a child Eldev process.
Loading mode is not included.  Since 1.2."
  `(,(if debug-on-error "--debug" "--no-debug")
    ,(pcase eldev-verbosity-level
       (`quiet   "--quiet")
       (`verbose "--verbose")
       (`trace   "--trace")
       (_        "--normal-output"))
    ,(eldev--color-setting-option)
    ,(if load-prefer-newer "--load-newer" "--load-first")
    ,(if eldev-prefer-stable-archives "--stable" "--unstable")))

(defun eldev--color-setting-option ()
  (format "--color=%s" (if (eldev-output-colorized-p) "always" "never")))


(defun eldev-set-up-secondary ()
  "Setup function to be called in nested Emacs."
  ;; Lots of code duplication with `bin/bootstrap.el.part', but oh well...
  (let ((eldev-local (getenv "ELDEV_LOCAL"))
        eldev-pkg)
    (if (or (= (length eldev-local) 0) (string-prefix-p ":pa:" eldev-local))
        (progn (let ((package-user-dir (expand-file-name "bootstrap"
                                                         (expand-file-name (format "%s.%s" emacs-major-version emacs-minor-version)
                                                                           (eldev-global-cache-dir)))))
                 (package-initialize t)
                 (unless (package-activate 'eldev)
                   (eldev-warn "Cannot activate Eldev package")))
               (package-load-all-descriptors))
      (package-initialize t)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "eldev.el" eldev-local))
        (setf eldev-pkg                    (package-buffer-info)
              (package-desc-dir eldev-pkg) (expand-file-name eldev-local)))
      (if eldev-pkg
          ;; `package--autoloads-file-name' is package-private.
          (let ((autoloads-file (expand-file-name (format "%s-autoloads" (package-desc-name eldev-pkg))
                                                  (package-desc-dir eldev-pkg))))
            (push `(eldev . (,eldev-pkg)) package-alist)
            ;; Otherwise old Emacs versions print an ugly error having not found the autoloads file.
            (eldev-advised  (#'load :around (lambda (do-load file &rest args) (unless (equal file autoloads-file) (apply do-load file args))))
              (package-activate-1 eldev-pkg)))
        (eldev-warn "Cannot activate Eldev package suppposedly specified by `ELDEV_LOCAL'"))))
  (eldev--set-up))


(defun eldev-retry-on-errors-p ()
  "Determine if we should retry on certain errors.
This should be used in cases where it is expected that certain
things might occasionally fail because of an external factor,
e.g. a short-living networking problem.

Since 1.5."
  (if (eq eldev-robust-mode 'auto) (eldev--on-ci-server-p) eldev-robust-mode))

(defun eldev--on-ci-server-p ()
  ;; This environment variable appears to be sort-of standard.  We don't care if the users
  ;; "spoofs" it or not, this is only a heuristic.
  (equal (getenv "CI") "true"))

(defmacro eldev-retrying-for-robustness (&rest body)
  (declare (indent 0) (debug (body)))
  (let ((all-retry-delays (make-symbol "$all-retry-delays"))
        (remaining-delays (make-symbol "$remaining-delays")))
    `(let* ((,all-retry-delays (when (eldev-retry-on-errors-p) eldev-robust-mode-retry-delays))
            (,remaining-delays ,all-retry-delays))
       (catch 'obtained-result
         (while t
           (condition-case error
               ;; If we still plan to retry, we need to unset `debug-on-error' locally.
               (throw 'obtained-result (let ((debug-on-error (and debug-on-error (null ,remaining-delays))))
                                         ,@body))
             (error (eldev--maybe-retry error ,all-retry-delays ,remaining-delays)
                    (pop ,remaining-delays))))))))

(defun eldev--maybe-retry (error all-retry-delays remaining-delays)
  (let ((delay (pop remaining-delays)))
    (unless delay
      (when all-retry-delays
        (eldev-warn "Giving up: too many retries already"))
      (signal (car error) (cdr error)))
    (eldev--print-error error)
    (eldev-warn "Assuming this is an intermittent problem, waiting %s before retrying..."
                (cond ((< delay 60)         (format "%s s" delay))
                      ((= (mod delay 60) 0) (format "%s m" (/ delay 60)))
                      (t                    (format "%s m %s s" (/ delay 60) (mod delay 60)))))
    (sleep-for delay)
    (let* ((total (length all-retry-delays))
           (n     (- total (length remaining-delays))))
      (eldev-warn "Retry #%d%s..." n (if (= n total) ", the last" (format " of maximum %d" total))))))



;; Functions for `Eldev' and `Eldev-local'.

(defvar eldev--extra-dependencies nil)
(defvar eldev--local-dependencies nil)
(defvar eldev--local-dependency-packages nil)
(defvar eldev--loading-roots nil)

(eval-and-compile
  (defvar eldev--known-package-archives '((gnu            ("gnu"            . "https://elpa.gnu.org/packages/")        300)
                                          (gnu-devel      ("gnu-devel"      . "https://elpa.gnu.org/devel/")           190)
                                          (nongnu         ("nongnu"         . "https://elpa.nongnu.org/nongnu/")       250)
                                          (nongnu-devel   ("nongnu-devel"   . "https://elpa.nongnu.org/nongnu-devel/") 150)
                                          (melpa-stable   ("melpa-stable"   . "https://stable.melpa.org/packages/")    200)
                                          (melpa-unstable ("melpa-unstable" . "https://melpa.org/packages/")           100)
                                          (gnu-elpa       (:stable gnu          :unstable gnu-devel))
                                          (nongnu-elpa    (:stable nongnu       :unstable nongnu-devel))
                                          (melpa          (:stable melpa-stable :unstable melpa-unstable)))))

;; Initial value means that even if `melpa-stable' and `melpa-unstable' are added
;; separately, they will be swappable with relevant options.  For other archives calling
;; `eldev-use-package-archive' with a stable/unstable pair is required.
(defvar eldev--stable/unstable-archives (eval-when-compile (mapcar (lambda (entry)
                                                                     `(,(cdr   (cadr (assq (plist-get (cadr entry) :stable)   eldev--known-package-archives)))
                                                                       . ,(cdr (cadr (assq (plist-get (cadr entry) :unstable) eldev--known-package-archives)))))
                                                                   (eldev-filter (keywordp (car (cadr it))) eldev--known-package-archives))))

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
  - gnu-devel      (https://elpa.gnu.org/devel/, since 1.1)
  - nongnu-devel   (https://elpa.nongnu.org/nongnu-devel/, since 1.1)
  - melpa-unstable (https://melpa.org/packages/)

Since 0.5 an archive can also be a plist with properties
`:stable' and `:unstable'.  Standard archives of this type:

  - melpa          (:stable melpa-stable :unstable melpa-unstable)

plus, since 1.1:

  - gnu-elpa       (:stable gnu    :unstable gnu-devel)
  - nongnu-elpa    (:stable nongnu :unstable nongnu-devel)

If PRIORITY is non-nil, ARCHIVE is given this priority (see
`package-archive-priorities').  Standard archives get priorities
300, 250, 200, 190, 150 and 100 in the order they are listed
above, unless you specify something explicitly.  Varying
differences between the numbers are only since the list has
accumulated over time, not created in one step.

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
  "Return the stable/unstable counterpart of ARCHIVE.
E.g. `melpa-stable' and `melpa-unstable' are each other's
counterparts.  If there is no counterpart, this function returns
nil.  If there is a counterpart, but it is not used (i.e. is not
in `package-archives', it returns VALUE-IF-NOT-USED."
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
  (unless eldev-normal-dependency-management
    ;; But still add it into the list in case dependency management get reenabled in time somehow.
    (eldev-warn "Normal dependency management is disabled; local dependencies will not be used"))
  (if loading-mode
      (progn (unless (assq loading-mode eldev--loading-modes)
               (error "Unsupported local dependency mode `%s'; see Eldev documentation" loading-mode))
             (when (memq loading-mode '(compiled-on-demand noisy-compiled-on-demand))
               (error "Loading mode `%s' is not supported for local dependencies" loading-mode)))
    (setf loading-mode 'as-is))
  (eldev-named-step nil (eldev-format-message "using local dependency in directory `%s'" dir)
    (setf dir (file-name-as-directory dir))
    (let* ((absolute-dir (expand-file-name dir eldev-project-dir))
           (dependency   (eldev-package-descriptor absolute-dir))
           (name         (package-desc-name dependency)))
      (when (eq name (package-desc-name (eldev-package-descriptor)))
        (error "Local dependency in directory `%s' is the same package as that being built: `%s'" dir name))
      (when (assq name eldev--local-dependencies)
        (error "Duplicate local dependency `%s' in directory `%s': already registered in directory `%s'" name dir (nth 1 (assq name eldev--local-dependencies))))
      (push `(,name ,dependency ,dir ,absolute-dir ,loading-mode) eldev--local-dependencies)
      (eldev-trace "Will use directory `%s' as local dependency `%s' with loading mode `%s'" dir name loading-mode))))

(defun eldev-add-extra-dependencies (sets &rest dependencies)
  "Additionally use DEPENDENCIES for given SETS.
Sets are typically named after Eldev commands.  See the manual
for details."
  (dolist (set (eldev-listify sets))
    (let ((set-dependencies (or (assq set eldev--extra-dependencies) (car (push `(,set . nil) eldev--extra-dependencies)))))
      (dolist (dependency dependencies)
        (push (if (symbolp dependency) (list dependency) dependency) (cdr set-dependencies))))))

(defmacro eldev-saving-dependency-lists (&rest body)
  "Save dependency lists and execute BODY.
Upon exit (normal or via a signal) dependency lists are restored,
so e.g. effects of `eldev-add-extra-dependencies' are restricted
to BODY only.

Since 1.4."
  (declare (indent 0) (debug (body)))
  `(let ((eldev--extra-dependencies eldev--extra-dependencies))
     ,@body))

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


(defun eldev-substitute (source target &optional open-string close-string extra-variables)
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
    (eldev-substitute-in-buffer open-string close-string extra-variables)
    (eldev-write-to-file target)))

(defun eldev-substitute-in-buffer (&optional open-string close-string extra-variables)
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
        (let ((value (eval form (or extra-variables t))))
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
  :category       information
  :works-on-old-eldev t
  (if parameters
      ;; This is an exception: don't signal wrong command usage on excessive parameters.
      (let* ((command      (if (= (length parameters) 1) (intern (car parameters)) 'help))
             (real-command (or (cdr (assq command eldev--command-aliases)) command))
             (handler      (cdr (assq real-command eldev--commands))))
        (if handler
            (let ((parameters (eldev-get handler :parameters)))
              (eldev-output "Usage: %s [...] %s%s%s" (eldev-shell-command t)
                            real-command (if (cdr (assq real-command eldev--options)) " [OPTION...]" "") (if parameters (format " %s" parameters) ""))
              (eldev-help-list-aliases real-command eldev--command-aliases "\n%s" '("Command alias:" "Command aliases:"))
              (eldev-options-help real-command)
              (eldev-output "\n%s" (or (eldev-documentation handler) "Not documented")))
          (eldev--complain-about-missing-command command)
          (eldev-print "Run `%s help' for a list of known commands" (eldev-shell-command t))))
    (eldev-usage)
    (eldev-output "
Options before the command are global for Eldev.  Many commands have additional
options specific to them; those must be specified after command name.  See each
command's description for a list of such options.")
    (eldev-options-help nil (format "\n%s" (eldev-colorize "Global options:" 'section)))
    (let ((all-commands (sort (mapcar #'car eldev--commands) #'string<))
          (categories   '((testing      "Testing commands")
                          (running      "Running, executing commands")
                          (dependencies "Dependency commands")
                          (building     "Building commands")
                          (information  "Information querying commands")
                          (nil          "Miscellaneous commands"))))
      (dolist (category categories)
        (eldev-output (format "\n%s" (eldev-colorize (format "%s:" (cadr category)) 'section)))
        (dolist (command all-commands)
          (let* ((handler          (cdr (assq command eldev--commands)))
                 (handler-category (eldev-get handler :category)))
          (when (if (car category)
                    (eq handler-category (car category))
                  (not (and handler-category (assq handler-category categories))))
            (eldev-command-or-option-help command handler))))))
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
               ;; Sort short options before long ones (but usually they are already sorted).
               (options     (sort (copy-sequence (cdr group))
                                  (lambda (a b) (and (not (string-prefix-p "--" (symbol-name a))) (string-prefix-p "--" (symbol-name b))))))
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



;; eldev files

(defvar eldev-files-filesets nil
  "Named filesets to list files in (`main', `test', etc.).
If left nil, all filesets are listed.")

(defvar eldev-files-type 'normal
  "Which files to list.
Can be `normal', `generated' and `all'.")

(defvar eldev-files-generated-also-potential nil
  "Whether to include potentially creatable generated files.
Otherwise only existing generated files would be listed.")

(defvar eldev-files-ignore-std-excludes nil
  "Whether to ignore `eldev-standard-excludes'.")

(defvar eldev-files-absolute-paths nil
  "Whether to list files by their absolute paths.")

(eldev-defcommand eldev-files (&rest parameters)
  "List files in the project.

By default, list all project files.  However, you can restrict
listed files using command line parameters; remember that you
will likely need to quote those if they contain wildcards, else
the wildcards will get expanded by the shell.

Option `--set' can be used to restrict only to files in that set.
By default all filesets are included.

Generated files, for example `.elc' files are not listed by
default.  However, you can request lists of those with options
`--generated' (instead of normal files) or `--all' (in addition
to normal files).  It is possible to list only already existing
generated files or those that could be created in principle."
  :aliases        list-files
  :category       information
  :parameters     "[FILESET...]"
  :works-on-old-eldev t
  (let* ((generated-files                   (unless (eq eldev-files-type 'normal) (eldev-generated-files)))
         (eldev-pretend-files               (if eldev-files-generated-also-potential
                                                generated-files
                                              (eldev-filter (file-exists-p it) generated-files)))
         (eldev-consider-only-pretend-files (eq eldev-files-type 'generated))
         (fileset                           (apply #'eldev-standard-filesets
                                                   (if eldev-files-ignore-std-excludes :no-excludes :std-excludes)
                                                   (if (eq eldev-files-type 'normal) :without-generated :with-generated)
                                                   :or (or eldev-files-filesets '(all)))))
    (when parameters
      (setf fileset `(:and ,fileset ,parameters)))
    (dolist (file (eldev-find-files fileset eldev-files-absolute-paths))
      (eldev-output "%s" file))))

(eldev-defoption eldev-files-fileset (name)
  "List files in this fileset"
  :options        (-s --set --fileset)
  :for-command    files
  :value          NAME
  :default-value  (eldev-message-enumerate nil (or eldev-files-filesets '(all)) nil t)
  (setf eldev-files-filesets (append eldev-files-filesets (list (eldev-validate-standard-fileset name)))))

(eldev-defoption eldev-files-only-normal ()
  "List normal project files"
  :options        (-n --normal)
  :for-command    files
  :if-default     (eq eldev-files-type 'normal)
  (setf eldev-files-type 'normal))

(eldev-defoption eldev-files-only-generated ()
  "List generated files"
  :options        (-g --generated)
  :for-command    files
  :if-default     (eq eldev-files-type 'generated)
  (setf eldev-files-type 'generated))

(eldev-defoption eldev-files-all ()
  "List both normal and generated files"
  :options        (-a --all --normal-and-generated)
  :for-command    files
  :if-default     (eq eldev-files-type 'all)
  (setf eldev-files-type 'all))

(eldev-defbooloptions eldev-files-generated-also-potential eldev-files-generated-only-existing eldev-files-generated-also-potential
  ("Also show potential generatable files"
   :options       (-c --creatable))
  ("Only show those generated files that already exist"
   :options       (-x --only-existing))
  :for-command    files)

(eldev-defbooloptions eldev-files-ignore-std-excludes eldev-files-use-std-excludes eldev-files-ignore-std-excludes
  ("Ignore standard excludes"
   :options       (-i --ignore-excludes))
  ("Honour excluded file settings"
   :options       (-X --std-excludes)
   :hidden-if     :default)
  :for-command    files)

(eldev-defbooloptions eldev-files-absolute-paths eldev-files-relative-paths eldev-files-absolute-paths
  ("Print absolute file paths"
   :options       (-A --absolute))
  ("Print file paths relative to project's root"
   :options       (-r --relative))
  :for-command    files)



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

(defvar eldev--global-cache-pending-changes t
  "Alist of CACHE-PATH to TEMP-FILE.
The key is a string, so must be looked up by `equal' (`assoc').
Special value of t means “do not use”.")

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
  :category       dependencies
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
  :category       dependencies
  (when (eldev-external-package-dir)
    (signal 'eldev-error `(:hint "Use global option `--isolated' (`-I')"
                                 "Cannot upgrade when using external package directory")))
  (unless eldev-normal-dependency-management
    (signal 'eldev-error `(:hint "Don't use global option `--disable-dependencies'"
                                 "Command `upgrade' cannot be used if standard dependency management is disabled")))
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
  (let ((package-user-dir                   (expand-file-name (format "%s.%s/bootstrap" emacs-major-version emacs-minor-version) (eldev-global-cache-dir)))
        (eldev-normal-dependency-management t))
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
  (when eldev-normal-dependency-management
    (eldev--install-or-upgrade-dependencies 'project additional-sets nil nil t nil no-error-if-missing))
  (run-hook-with-args 'eldev-load-dependencies-hook (if load-only 'load-only t) additional-sets))

(defun eldev-load-extra-dependencies (sets &optional no-error-if-missing)
  "Load extra dependencies, but without normal project's dependencies.
This is exactly like `eldev-load-project-dependencies' except
that the project itself and its normal dependencies are not
loaded.  Mostly useful to load runtime dependencies.

Since 0.2."
  (setf sets (eldev-listify sets))
  (run-hook-with-args 'eldev-before-loading-dependencies-hook nil sets)
  (when eldev-normal-dependency-management
    (eldev--install-or-upgrade-dependencies nil sets nil nil t nil no-error-if-missing))
  (run-hook-with-args 'eldev-load-dependencies-hook nil sets))

(defmacro eldev-using-global-package-archive-cache (&rest body)
  (declare (indent 0) (debug (body)))
  (let ((outer-transaction (make-symbol "$outer-transaction")))
    `(eldev-advised (#'url-retrieve-synchronously :around #'eldev--global-cache-url-retrieve-synchronously)
       (let* ((,outer-transaction                  eldev--global-cache-pending-changes)
              ;; When there is no outer transaction, we bind to nil, starting a new one.
              (eldev--global-cache-pending-changes (when (listp ,outer-transaction) ,outer-transaction)))
         (prog1 ,(macroexp-progn body)
           ;; Intentionally not put into `unwind-protect': if `body' dies with any exception, discard all GPA
           ;; changes it has generated, since those might be buggy.  See also `eldev-retrying-for-robustness'.
           (unless (listp ,outer-transaction)
             (eldev--global-cache-commit-pending-changes)))))))

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
            (let* ((use-pending       (listp eldev--global-cache-pending-changes))
                   (pending           (when use-pending (assoc cache-path eldev--global-cache-pending-changes)))
                   (actual-storage    (if pending (cdr pending) cache-path))
                   (modification-time (nth 5 (file-attributes actual-storage)))
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
                    (insert-file-contents-literally actual-storage)
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
                        (unless pending
                          (if use-pending
                              (progn (setf actual-storage (make-temp-file "eldev-gpa-" nil ".bin"))
                                     (push `(,cache-path . ,actual-storage) eldev--global-cache-pending-changes))
                            (make-directory (file-name-directory cache-path) t)))
                        (let ((coding-system-for-write 'binary))
                          (eldev-write-to-file actual-storage))))
                    ;; Discard any cached signatures.
                    (unless (or use-pending (string-suffix-p ".sig" filename))
                      (ignore-errors (delete-file (concat cache-path ".sig")))))
                  buffer)))
          (eldev-trace "Accessing unexpected URL `%s': not caching" url)
          (apply original url arguments)))
    (apply original url arguments)))

(defun eldev--global-cache-commit-pending-changes ()
  (dolist (entry eldev--global-cache-pending-changes)
    (let ((cache-path (car entry))
          (temp-file  (cdr entry)))
      (make-directory (file-name-directory cache-path) t)
      (rename-file temp-file cache-path t)
      ;; Discard any cached signatures.
      (unless (string-suffix-p ".sig" cache-path)
        (ignore-errors (delete-file (concat cache-path ".sig")))))))

;; `package-compute-transaction' and friends are not enough in our case, mostly because of
;; local dependencies that can change unpredictably and also requirement that certain
;; dependencies are installed only from certain archives.  Roll our own.
(defun eldev--install-or-upgrade-dependencies (core additional-sets to-be-upgraded dry-run activate main-command-effect &optional no-error-if-missing)
  ;; This would be an internal error, as the function must not be even called then.
  (unless eldev-normal-dependency-management
    (error "Dependency management is disabled"))
  (eldev-advised ('package--reload-previously-loaded
                  ;; For Emacs 29.  Pointless to suggest improvements upstream, since I can't create a simple
                  ;; test (eventually did this though: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56614,
                  ;; but don't expect any results).  Test `eldev-loading-modes-2' fails without this.  Have to
                  ;; use these several advices to avoid affecting calls inside `load'.  Also, better to fail
                  ;; if `package--reload-previously-loaded' is rewritten than to break it.
                  :around (lambda (original pkg-desc &rest etc)
                            (let ((dir     (package-desc-dir pkg-desc))
                                  (history (mapcar #'file-truename (eldev-filter (stringp it) (mapcar #'car load-history))))
                                  inside-load)
                              (eldev-advised ('cl-remove-if :around (lambda (original predicate seq &rest etc)
                                                                      (if (or inside-load (not (funcall predicate dir)))
                                                                          (apply original predicate seq etc)
                                                                        seq)))
                                ;; I'd advise `member' here, but it is a built-in, so the advise is ignored if
                                ;; `package--reload-previously-loaded' is byte-compiled (i.e. always in practice).
                                (eldev-advised ('file-truename :around (lambda (original filename &rest etc)
                                                                         (let ((truename (apply original filename etc)))
                                                                           (unless (or inside-load (member truename history))
                                                                             (setf truename (if (string-suffix-p ".el" truename)
                                                                                                (replace-regexp-in-string (rx ".el" eos) ".elc" truename t)
                                                                                              (replace-regexp-in-string (rx ".elc" eos) ".el" truename t))))
                                                                           truename)))
                                  (eldev-advised ('load :around (lambda (original &rest etc)
                                                                  ;; Cannot just let-bind a local variable shared across several closures.
                                                                  (let ((was-inside inside-load))
                                                                    (setf inside-load t)
                                                                    (unwind-protect
                                                                        (apply original etc)
                                                                      (setf inside-load was-inside)))))
                                    ;; Workaround for Emacs bug with built-in packages described in
                                    ;; https://github.com/emacs-eldev/eldev/issues/93.
                                    (let ((load-path load-path))
                                      (unless (member dir load-path)
                                        (push dir load-path))
                                      (apply original pkg-desc etc))))))))
    (eldev--do-install-or-upgrade-dependencies core additional-sets to-be-upgraded dry-run activate main-command-effect no-error-if-missing)))

(defun eldev--do-install-or-upgrade-dependencies (core additional-sets to-be-upgraded dry-run activate main-command-effect &optional no-error-if-missing)
  ;; See comments in `eldev--execute-command'.
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
                          (cdr (assq 'packages contents)))
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
                                                                                                  considered highest-requirements (and (null core) (equal additional-sets '(runtime)))))))
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
        ;; FIXME: What about project with special source subdirectories?  While this is
        ;; for requiring test files, it feels a bit dirty in this case...
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
                                                                                dependency-name))
                                         (loading-mode    (unless recursing (eldev--loading-mode dependency))))
                                    (eldev-pcase-exhaustive loading-mode
                                      (`nil
                                       (eldev-trace "Activating %s..." description)
                                       (or (let ((inhibit-message t))
                                             (apply original dependency rest))
                                           (progn (setf missing-dependency dependency-name)
                                                  nil)))
                                      ;; In all these modes dependency is activated in exactly the same way,
                                      ;; the difference is in `eldev--load-local-dependency'.  Special
                                      ;; handling of mode `compiled-on-demand' is performed elsewhere.
                                      ((or `as-is `source `byte-compiled `compiled-on-demand `noisy-compiled-on-demand `built `built-and-compiled `built-source)
                                       (when (and (memq loading-mode '(compiled-on-demand noisy-compiled-on-demand)) (not (eq dependency-name package-name)))
                                         (error "Loading mode `%s' is not supported for local dependencies" loading-mode))
                                       (dolist (requirement (package-desc-reqs dependency))
                                         (unless (package-activate (car requirement))
                                           (throw 'exit nil)))
                                       (let* ((load-path-before load-path)
                                              (dependency-dir   (if (eq dependency-name package-name)
                                                                    eldev-project-dir
                                                                  ;; 2 and 3 stand for directory name and its absolute path.
                                                                  (eldev-trace "Activating %s in directory `%s'" description (nth 2 (assq dependency-name eldev--local-dependencies)))
                                                                  (nth 3 (assq dependency-name eldev--local-dependencies))))
                                              (source-dirs      (eldev--cross-project-internal-eval dependency-dir '(eldev-project-source-dirs) t)))
                                         ;; Use package's autoloads file if it is present.  At this stage we
                                         ;; never generate anything: only use existing files.
                                         (eldev--load-autoloads-file (expand-file-name (format "%s-autoloads.el" dependency-name) (car source-dirs)))
                                         ;; For non-ancient packages, autoloads file is supposed to modify
                                         ;; `load-path'.  But if there is no such file, or it doesn't do that
                                         ;; for whatever reason, do it ourselves.
                                         ;;
                                         ;; Now that there can be multiple source directories, what the
                                         ;; standard autoloads file does is not enough, as it handles only
                                         ;; one.  In this case, also rewrite `load-path'.
                                         (when (or (eq load-path load-path-before)
                                                   (when (and (cdr source-dirs) (eq (cdr load-path) load-path-before))
                                                     (pop load-path)))
                                           (dolist (source-dir (reverse source-dirs))
                                             (unless (member source-dir load-path)
                                               (push source-dir load-path))))
                                         (eldev--assq-set dependency-name dependency-dir eldev--package-load-paths))
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

;; Returns non-nil if the package can be installed.
(defun eldev--plan-install-or-upgrade (self to-be-upgraded package-plist default-archives plan fail-if-too-new considered highest-requirements &optional runtime-dependency required-by)
  (let* ((package-name        (plist-get package-plist :package))
         (required-version    (plist-get package-plist :version))
         (optional            (plist-get package-plist :optional))
         (highest-requirement (gethash package-name highest-requirements))
         (real-required-by    required-by)
         (required-by-hint    (lambda (&optional upcase)
                                (let (hints)
                                  (when runtime-dependency
                                    (push "required as a development tool, not as a dependecy of the project" hints))
                                  (when required-by
                                    (push (eldev-format-message "required by package %s"
                                                                (mapconcat (lambda (package) (eldev-format-message "`%s'" package)) real-required-by " <- "))
                                          hints))
                                  (when hints
                                    (let ((hint (mapconcat #'identity hints "; ")))
                                      (if upcase
                                          (eldev-message-upcase-first hint)
                                        hint))))))
         (considered-version  (gethash package-name considered))
         ;; Associated value in the hash-table can be `nil'.
         (considered-before   (not (eq (gethash package-name considered considered) considered))))
    (when (stringp required-version)
      (setf required-version (version-to-list required-version)))
    ;; See test `eldev-optional-dependencies-2' with parameters `project-a' and
    ;; `uninstallable-a'.  When a built-in in a version that is not available is requested
    ;; by an optional dependency, deem that dependency uninstallable.
    ;;
    ;; FIXME: Might need to do that not only for built-ins; however, for non-built-ins it
    ;;        is not clear how to determine if those are available in that version.
    (when (and optional considered-before (package-built-in-p package-name) (not (package-built-in-p package-name required-version)))
      (throw 'skip-uninstallable-optionals nil))
    ;; Elevate requirement if needed.
    (when (version-list-< required-version (car highest-requirement))
      (setf required-version (car highest-requirement)
            real-required-by (cdr highest-requirement)))
    (when (and considered-before (version-list-< considered-version required-version))
      (puthash package-name (cons required-version real-required-by) highest-requirements)
      ;; Unlike `considered', `highest-requirements' must not be cleared between passes.
      (clrhash considered)
      (throw 'restart-planning t))
    (unless (and considered-before (version-list-<= required-version considered-version))
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
               (archives                  (eldev--package-plist-get-archives package-plist t))
               (external-dir              (unless self (eldev-external-package-dir))))
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
                  ;; Make sure we don't install a package from a wrong archive.  In
                  ;; particular, if using preinstalled dependencies (external-dir), only
                  ;; "install" local dependencies listed in the internal pseudoarchive.
                  (when (if (or local external-dir)
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
                                                  (unless (locate-file eldev-file (list eldev-project-dir))
                                                    ;; Project configuration file is supposed to call `eldev-use-package-archive'.
                                                    (setf hint (concat (if hint (concat hint "\n") "")
                                                                       (eldev-format-message "There is no file `%s' in the project; consider running `%s init'" eldev-file (eldev-shell-command t)))))
                                                  `("Dependency `%s' is not available" ,package-name))))))
                  (when external-dir
                    (setf hint (concat (eldev-format-message "Either install it there, or use global option `--isolated' (`-I')") (if hint (concat "\n" hint) ""))))
                  (signal 'eldev-missing-dependency `(:hint ,hint ,@message))))))
          (dolist (requirement (package-desc-reqs package))
            (eldev--plan-install-or-upgrade self to-be-upgraded (eldev--create-package-plist requirement (or archives default-archives) optional)
                                            default-archives plan fail-if-too-new considered highest-requirements runtime-dependency (cons package-name required-by)))
          (if (eq package already-installed)
              (push package-name (cdr plan))
            (push `(,package . ,already-installed) (car plan)))))
      (puthash package-name required-version considered)
      t)))

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
    ;; See comments in `eldev--execute-command'.
    (let ((eldev-message-rerouting-destination         :stderr)
          (eldev-global-cache-archive-contents-max-age (if refetch-contents -1 eldev-global-cache-archive-contents-max-age))
          (package-archives                            archives)
          (inhibit-message                             t))
      (eldev--with-pa-access-workarounds (lambda ()
                                           (eldev-retrying-for-robustness
                                             (let (failure)
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
                                                                                  ,(error-message-string (car failure))))))))))))))

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
         ;; For the project itself autoloads are handled differently.  For other loading
         ;; modes they get built without special care.
         (build-autoloads (when (and (not project-itself) (memq loading-mode '(as-is source byte-compiled compiled-on-demand noisy-compiled-on-demand)))
                            (eldev--cross-project-internal-eval dependency-dir '(not (null (memq 'autoloads (eldev-active-plugins)))) t))))
    (when (cdr (assq dependency-name package-alist))
      (error "Local dependency `%s' is already listed in `package-alist'" dependency-name))
    ;; FIXME: Special-case project itself: no need to launch separate process(es) for it.
    ;; I don't want to move this into an alist, to avoid fixing the way it works.
    (let ((commands (eldev-pcase-exhaustive loading-mode
                      (`as-is              (when build-autoloads `(("build" ":autoloads"))))
                      (`source             `(("clean" ".elc" "--set" "main" "--delete")
                                             ,@(when build-autoloads `(("build" ":autoloads")))))
                      (`byte-compiled      `(("build" ":compile" ,@(when build-autoloads `(":autoloads")))))
                      ((or `compiled-on-demand `noisy-compiled-on-demand) nil)
                      (`built              `(("build" ":default")))
                      (`built-and-compiled `(("build" ":default" ":compile")))
                      (`built-source       `(("clean" ".elc" "--set" "main" "--delete")
                                             ("build" ":default")))
                      (`packaged           `(("package" "--output-dir" ,(expand-file-name "local/generated" (eldev-cache-dir t)) "--print-filename"))))))
      (when commands
        (if project-itself
            (eldev-verbose "Preparing to load the project in mode `%s'" loading-mode)
          (eldev-verbose "Preparing to load local dependency `%s' in mode `%s'" dependency-name loading-mode))
        (let ((default-directory dependency-dir))
          (dolist (command commands)
            (eldev-call-process (eldev-shell-command) command
              :forward-output     'stderr
              :destination        '(t nil)
              :trace-command-line (eldev-format-message "Full command line (in directory `%s')" default-directory)
              :die-on-error       (if project-itself
                                      "child Eldev process"
                                    (eldev-format-message "child Eldev process for local dependency `%s'" dependency-name))
              (if (= (point-min) (point-max))
                  (eldev-verbose "Child Eldev process produced no output (other than maybe on stderr)")
                (eldev-verbose "(Non-stderr) output of the child Eldev process:")
                (eldev-verbose (buffer-string)))
              (when (string= (car command) "package")
                (eldev-discard-ansi-control-sequences)
                (goto-char (point-max))
                (forward-line -2)
                (let ((point (point)))
                  (end-of-line)
                  (let ((file (buffer-substring-no-properties point (point))))
                    (forward-line)
                    (unless (looking-at (rx bol (or "up-to-date" "generated") eol))
                      (error "Unable to parse child Eldev process output:\n%s" (buffer-string)))
                    (push `(,dependency-name ,file ,(string= (match-string 0) "up-to-date")) eldev--local-dependency-packages))))))))
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
  :category       building
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
   :options       (-d --delete --do-delete)
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
;; A lot of code is now in a separate file to slightly speed up loading.

(defvar eldev-dependencies-list-built-ins nil
  "Whether to list built-in packages among built-ins.
The most obvious such package is `emacs` used to declare minimum
required version of Emacs itself.")

(eldev-defcommand eldev-archives (&rest parameters)
  "List package archives used to look up dependencies.  Archives
can be registered using function `eldev-use-package-archive' in
project's `Eldev' file.

With the option `--list-known', instead list the “standard”
archives Eldev knows about.  Most projects use some of those, but
it is also possible to use any other."
  :aliases        arch
  :category       dependencies
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (eldev--adjust-stable/unstable-archive-priorities)
  (if package-archives
      (dolist (archive (sort package-archives (lambda (a b) (> (eldev-package-archive-priority (car a))
                                                               (eldev-package-archive-priority (car b))))))
        (eldev--archives-print archive))
    (eldev-print "None specified; add form `(eldev-use-package-archive ...)' in file `%s'" eldev-file)))

(defun eldev-package-archive-priority (archive &optional default)
  (or (cdr (assoc archive (if (eq eldev--package-archive-priorities t) package-archive-priorities eldev--package-archive-priorities)))
      default 0))

(eldev-defoption eldev-archives-list-known ()
  "List known package archives and exit"
  :options        (-L --list-known)
  :for-command    archives
  (eldev-print "Package archives marked with `*' are currently used by the project\n")
  (dolist (entry eldev--known-package-archives)
    (let ((archive (cadr entry)))
      (cond ((eldev--stable/unstable-archive-p archive)
             (let* ((stable        (assq (plist-get archive :stable)   eldev--known-package-archives))
                    (unstable      (assq (plist-get archive :unstable) eldev--known-package-archives))
                    (stable-used   (rassq (cdr (nth 1 stable))   package-archives))
                    (unstable-used (rassq (cdr (nth 1 unstable)) package-archives)))
               (eldev-output "%s%s:"
                             (eldev-colorize (car entry) 'name)
                             (if (and stable-used unstable-used) " [*]" ""))
               (eldev--archives-print (nth 1 stable)   (nth 2 stable)   1 (when (and stable-used   (not unstable-used)) " [*]"))
               (eldev--archives-print (nth 1 unstable) (nth 2 unstable) 1 (when (and unstable-used (not stable-used))   " [*]"))))
            ((null (eldev--stable/unstable-archive-counterpart archive t))
             (eldev--archives-print archive (nth 2 entry) 0 (when (rassq (cdr archive) package-archives) " [*]"))))))
  (eldev-print "\nIt is also possible to use other archives by specifying URL explicitly")
  (signal 'eldev-quit 0))

(defun eldev--archives-print (archive &optional default-to-priority indentation-level special-mark)
  (eldev-output "%s%s%s: %s%s"
                (make-string (* (or indentation-level 0) 2) ? )
                (eldev-colorize (car archive) 'name)
                (or special-mark "")
                (eldev-colorize (cdr archive) 'url)
                (eldev-format-message "  (%spriority: %s)"
                                      (if default-to-priority "default " "") (or default-to-priority (eldev-package-archive-priority (car archive) "0, defaulted")))))


(eldev-defcommand eldev-dependencies (&rest parameters)
  "List dependencies of the project.

If any additional sets are specified, dependencies from the sets
are listed too.  Sets typically have the same name as commands.
E.g. there might be additional dependencies for testing (`test')
or evaluating (`eval') registered in the project's `Eldev' file."
  :aliases        (deps requirements reqs)
  :parameters     "[ADDITIONAL-SET...]"
  :category       dependencies
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
  "Show dependencies of the project recursively.

If any additional sets are specified, dependencies from the sets
are listed too.  Sets typically have the same name as commands.
E.g. there might be additional dependencies for testing (`test')
or evaluating (`eval') registered in the project's `Eldev' file."
  :aliases        (dtree deptree requirement-tree rtree reqtree)
  :parameters     "[ADDITIONAL-SET...]"
  :category       dependencies
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
  :category       information
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
  :category       information
  :works-on-old-eldev t
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (let* ((package     (eldev-package-descriptor))
         (description (when (fboundp 'package--get-description)
                        (condition-case nil
                            (package--get-description package)
                          ;; package--get-description will only look at the root directory,
                          ;; thus not finding the project description file if source directories are used.
                          ;; Repeat its call to lm-commentary manually with first source directory.
                          (file-missing (let ((source-dirs (eldev--cross-project-internal-eval eldev-project-dir '(eldev-project-source-dirs) t)))
                                          (eval-and-compile (require 'lisp-mnt))
                                          (lm-commentary (expand-file-name
                                                          (format "%s.el" (package-desc-name package)) (car source-dirs)))))))))
    (unless (> (length description) 0)
      (setf description (package-desc-summary package)))
    (eldev-output "%s %s" (eldev-colorize (package-desc-name package) 'name) (eldev-message-version package))
    (when description
      (setf description (replace-regexp-in-string (rx (| (: bos (1+ (or space "\n"))) (: (1+ (or space "\n")) eos))) "" description))
      (unless (string= description "")
        (eldev-output "\n%s" description)))))



;; eldev profile
;; Placed relatively early, since `eldev-profile-body' is used by other commands too.

(defvar eldev-profile-save-as-file nil
  "Save profile in given file.")

(defvar eldev-profile-open-in-emacs nil
  "Immediately open resulting profile(s) in Emacs.")

(defvar eldev-profile-mode nil
  "Profiler's mode, `cpu' if not specified.")

;; Standard value of 16 seems to be awfully small.
(setf profiler-max-stack-depth 30)

(defvar eldev-profile-only-project t
  "Whether to profile only project code.")

(defvar eldev--effective-profile-mode nil)
(defvar eldev--profile-pause-on-stop nil)


;; I had an idea of profiling in a thread (Emacs 26+) to shorten backtraces, but this
;; fails because of let-binding for global variables are not visible from the thread.  We
;; use those in several places, and even if we didn't, it would be unwise to depend on
;; that (also implicitly require for user code).
(defmacro eldev-profile-body (&rest body)
  "Execute BODY, gathering profiling information if requested."
  (declare (indent 0) (debug (body)))
  (let ((do-start (make-symbol "$do-start")))
    `(let ((,do-start (and eldev--effective-profile-mode
                           (progn (eval-and-compile (require 'profiler))
                                  (not (profiler-running-p eldev--effective-profile-mode))))))
       (when ,do-start
         (eldev--profile-do-start))
       (unwind-protect
           (progn ,@body)
         (when ,do-start
           (eldev--profile-do-stop))))))

(eldev-defcommand eldev-profile (&rest parameters)
  "Profile given Eldev command.  Particularly useful with
commands `eval', `exec' and `test', since those run code from the
current project; occasionally with `compile'.  However, can be
run with any Eldev command, in which case will profile mostly
Eldev itself.

Option `--file' can be repeated, which is useful for `cpu+mem'
mode: first filename is used for CPU profile, second---for
memory.  If the option is used only once, filename for memory
profile is derived from the only specified filename.  If the mode
is any other, only the last specified filename is used.

When option `--open' is specified, Eldev will try to open
resulting profile(s) in a running Emacs, which must have server
started for that to work (see `server-start').  Unlike nearly
everything else in Eldev, this uses your normal Emacs rather than
project-isolated one.

At least one of options `--file' and `--open' is required."
  :parameters     "COMMAND [...]"
  :aliases        prof
  :category       running
  :custom-parsing t
  (setf parameters (eldev-parse-options parameters 'profile t))
  (unless parameters
    (signal 'eldev-wrong-command-usage `(t "Missing command line to profile")))
  (unless (or eldev-profile-save-as-file eldev-profile-open-in-emacs)
    (signal 'eldev-wrong-command-usage `(t "At least one of options `--file' and `--open' is required")))
  (let* ((eldev--effective-profile-mode (or eldev-profile-mode 'cpu))
         (nested-command                (intern (car parameters)))
         (real-command                  (or (cdr (assq nested-command eldev--command-aliases)) nested-command))
         (handler                       (cdr (assq real-command eldev--commands)))
         cpu-profile
         cpu-profile-file
         memory-profile
         memory-profile-file)
    (if (and eldev-profile-only-project (eldev-get handler :profiling-self))
        (progn
          (let ((eldev--profile-pause-on-stop t))
            (eldev--execute-command parameters))
          ;; See test `eldev-profile-no-op'.  For 27 and up this also retrieves "the
          ;; final, everything joined data", see use of `eldev--profile-pause-on-stop'.
          (eldev-profile-body))
      (eldev-profile-body
        (let ((eldev--effective-profile-mode nil))
          (eldev--execute-command parameters))))
    ;; Older Emacs versions don't support creating profile objects once profiler is not
    ;; running anymore, even though the data is there.
    (eldev-advised ('profiler-running-p :override (lambda (&rest _) t))
      (when (memq eldev--effective-profile-mode '(cpu cpu+mem))
        (setf cpu-profile (profiler-cpu-profile)))
      (when (memq eldev--effective-profile-mode '(mem cpu+mem))
        (setf memory-profile (profiler-memory-profile))))
    ;; As a workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=52560, replace
    ;; strings in backtraces with symbols.
    (eldev-advised ('profiler-fixup-entry :around (lambda (original &rest args)
                                                    (let ((result (apply original args)))
                                                      (if (stringp result) (intern result) result))))
      (let* ((make-backup-files nil)
             (files             (eldev-listify eldev-profile-save-as-file))
             (num-files         (length files))
             (temp-prefix       (format "eldev-%s-" (replace-regexp-in-string (rx (not (any "a-zA-Z0-9"))) "" (symbol-name (package-desc-name (eldev-package-descriptor)))))))
        (when cpu-profile
          (setf cpu-profile-file (expand-file-name (if files
                                                       (nth (- num-files (if (and (> num-files 1) (eq eldev--effective-profile-mode 'cpu+mem)) 2 1)) files)
                                                     (make-temp-file temp-prefix nil ".cpu.prof"))
                                                   eldev-project-dir))
          (eldev-verbose "Storing the generated CPU profile in file `%s'..." cpu-profile-file)
          (profiler-write-profile cpu-profile cpu-profile-file))
        (when memory-profile
          (setf memory-profile-file (expand-file-name (if files
                                                          (car (last files))
                                                        (make-temp-file temp-prefix nil ".mem.prof"))
                                                      eldev-project-dir))
          (when (and cpu-profile-file (string= memory-profile-file cpu-profile-file))
            (setf memory-profile-file (eldev--profile-derive-memory-file cpu-profile-file)))
          (eldev-verbose "Storing the generated memory profile in file `%s'..." memory-profile-file)
          (profiler-write-profile memory-profile memory-profile-file))))
    (when eldev-profile-open-in-emacs
      (when cpu-profile-file
        (eldev--profile-open-on-server nil cpu-profile-file))
      (when memory-profile-file
        (eldev--profile-open-on-server nil memory-profile-file)))))

(defun eldev--profile-do-start ()
  (let ((inhibit-message t))
    (profiler-start eldev--effective-profile-mode)))

(defun eldev--profile-do-stop ()
  ;; Starting with Emacs 27 `profiler-stop' not only retrieves profiling information, but clears it
  ;; completely, making it impossible to join with later profiles (see test `eldev-profile-joins-multiple').
  ;; For this reason, we "stop" manually when we want to join with later profiles, e.g. when profiling
  ;; multiple expressions.
  (if eldev--profile-pause-on-stop
      (progn
        ;; Bare bones of `profiler-stop'.
        (when (fboundp 'profiler-cpu-stop)
          (profiler-cpu-stop))
        (profiler-memory-stop))
    (let ((inhibit-message t))
      (profiler-stop))))

(defun eldev--profile-derive-memory-file (cpu-profile-file)
  (let* ((cpu-file-name    (file-name-nondirectory cpu-profile-file))
         (memory-file-name (replace-regexp-in-string (rx bow "cpu" eow) "mem" cpu-file-name)))
    ;; Using `concat' because there can be no directory name.
    (concat (or (file-name-directory cpu-profile-file) "")
            (if (string= memory-file-name cpu-file-name)
                (replace-regexp-in-string (rx (? "." (1+ (not (any ".")))) eos) "-mem\\1" cpu-file-name)
              memory-file-name))))

(defun eldev--profile-open-on-server (server filename)
  (eval-and-compile (require 'server))
  (unless server
    (setf server server-name))
  (let ((server-socket-dir (if (server-running-p server)
                               server-socket-dir
                             ;; Backporting from newer Emacs source, otherwise older Emacs
                             ;; versions won't see newer servers.
                             (when (featurep 'make-network-process '(:family local))
	                       (let ((xdg_runtime_dir (getenv "XDG_RUNTIME_DIR")))
	                         (if xdg_runtime_dir
	                             (format "%s/emacs" xdg_runtime_dir)
	                           (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))))))
        (form              `(progn (require 'profiler) (profiler-report-profile (profiler-read-profile ,filename)) t)))
    (eldev-verbose "Trying to open the generated profile `%s' in your normal Emacs..." filename)
    ;; Ugly in that if something goes wrong with evaluating expression "there", no error
    ;; is signalled "here".  But what can we do other than rewriting all this crap?  At
    ;; least it will give an error if the server is not running.
    (server-eval-at server form)))


(eldev-defoption eldev-profile-save-as-file (filename)
  "Save profile in given file (also see notes below)"
  :options        (-f --file)
  :for-command    profile
  :value          FILENAME
  :default-value  (if (consp eldev-profile-save-as-file)
                      (eldev-message-enumerate nil eldev-profile-save-as-file nil t)
                    (or eldev-profile-save-as-file :no-default))
  ;; Accept both strings and symbols.
  (setf eldev-profile-save-as-file (nconc eldev-profile-save-as-file (eldev-listify filename))))

(eldev-defbooloptions eldev-profile-open-in-emacs eldev-profile-dont-open eldev-profile-open-in-emacs
  ("Open resulting profile(s) in Emacs"
   :options       (-o --open))
  ("Don't open profile(s) in Emacs, only write them to file(s)"
   :options       (--dont-open)
   :hidden-if     :default)
  :for-command    profile)

(eldev-defoption eldev-profile-set-mode (mode)
  "Set the profiler's mode"
  :options        (--mode)
  :for-command    profile
  :value          MODE
  :default-value  (or eldev-profile-mode 'cpu)
  ;; Accept both strings and symbols.
  (when (stringp mode)
    (setf mode (intern (downcase mode))))
  (when (eq mode 'cpu-mem)
    (setf mode 'cpu+mem))
  (unless (memq mode '(cpu mem cpu+mem))
    (signal 'eldev-wrong-option-usage `("unknown profiler mode `%s'" ,mode)))
  (setf eldev-profile-mode mode))

(eldev-defoption eldev-profile-set-mode-cpu ()
  "Shorthand for `--mode=cpu'"
  :options        (-c --cpu)
  :for-command    profile
  :if-default     (eq (or eldev-profile-mode 'cpu) 'cpu)
  (eldev-profile-set-mode 'cpu))

(eldev-defoption eldev-profile-set-mode-mem ()
  "Shorthand for `--mode=mem'"
  :options        (-m --mem)
  :for-command    profile
  :if-default     (eq eldev-profile-mode 'mem)
  (eldev-profile-set-mode 'mem))

(eldev-defoption eldev-profile-set-mode-cpu+mem ()
  "Shorthand for `--mode=cpu+mem'"
  :options        (-M --cpu-mem)
  :for-command    profile
  :if-default     (eq eldev-profile-mode 'cpu+mem)
  (eldev-profile-set-mode 'cpu+mem))

(eldev-defoption eldev-profile-set-sampling-interval (millis)
  "Sampling interval"
  :options        (-i --sampling-interval)
  :for-command    profile
  :value          MILLISECONDS
  :default-value  (progn (require 'profiler) (/ profiler-sampling-interval 1000000.0))
  (setf profiler-sampling-interval (round (* (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number millis :floating-point t :min 0.000001)) 1000000))))

(eldev-defoption eldev-profile-set-max-stack-depth (depth)
  "Maximum stack depth"
  :options        (-d --depth)
  :for-command    profile
  :value          FRAMES
  :default-value  profiler-max-stack-depth
  (unless (> (setf profiler-max-stack-depth (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number depth :min 0))) 0)
    ;; Emacs dies with "really" unlimited depth, e.g. `most-positive-fixnum'.  With a big
    ;; number like 0x10000 it eats so much memory that machine gets stuck.  And even with
    ;; anything above ~30 backtrace displaying in `profiler-mode' becomes useless.
    ;; Pinnacle of software engineering.
    (setf profiler-max-stack-depth #x1000)))

(eldev-defbooloptions eldev-profile-only-project eldev-profile-full-command eldev-profile-only-project
  ("Profile only the project itself (if nested command supports that)"
   :options       (-p --project))
  ("Profile full nested command code, including parts of Eldev"
   :options       (-F --full-command))
  :for-command    profile)



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

(defvar eldev-test-ert-fileset nil
  "Test files that contain project's ERT tests.
Only important if the project contains tests of different types.

Since Eldev 0.10.")

(defvar eldev-test-buttercup-hook nil
  "Hook executed before running Buttercup tests.
Functions are called with SELECTORS as argument.

Since Eldev 0.2.")

(defvar eldev-test-buttercup-fileset nil
  "Test files that contain project's Buttercup tests.
Only important if the project contains tests of different types.

Since Eldev 0.10.")

(defvar eldev-test-ecukes-hook nil
  "Hook executed before running Ecukes tests.
Functions are called with SELECTORS as argument.

Since Eldev 0.10.")

(defvar eldev-test-ecukes-fileset nil
  "Test files that contain project's Ecukes tests.
For Ecukes it's currently never important, since only this
framework uses `.feature' files.

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
                                     (profiling-self       . t)
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
                                                               (eldev-run-buttercup-tests selectors environment)))
                                     (profiling-self       . t)))
                       (ecukes    . ((detect               . (lambda () t))  ; if `.feature' files are found, then they must be for Ecukes
                                     (fileset              . "*.feature")
                                     (file-description     . "test `.feature' file%s")
                                     (dwim-regexp          . ,(rx ".feature" eol))
                                     (packages             . ((:tool ecukes)))
                                     (require              . eldev-ecukes)
                                     (preprocess-selectors . eldev-test-ecukes-preprocess-selectors)
                                     (run-tests            . (lambda (selectors files _runner environment)
                                                               (eldev-run-ecukes-tests files selectors environment)))
                                     (profiling-self       . t)))))
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
`~/.config/eldev/config' or `Eldev-local'.

This command exits with error code 1 if any test produces an
unexpected result."
  :parameters     "[SELECTOR...]"
  :category       testing
  :profiling-self t
  (eldev--do-test eldev-test-framework parameters))

(eldev-defcommand eldev-test-ert (&rest parameters)
  "Run project's ERT regression/unit tests.  See command `test'
for details."
  :parameters     "[SELECTOR...]"
  :aliases        ert
  :category       testing
  :hidden-if      (or (<= (length (eldev-listify eldev-test-framework)) 1) (not (memq 'ert eldev-test-framework)))
  :profiling-self t
  (eldev--do-test 'ert parameters))

(eldev-defcommand eldev-test-buttercup (&rest parameters)
  "Run project's Buttercup regression/unit tests.  See command
`test' for details."
  :parameters     "[SELECTOR...]"
  :aliases        (buttercup test-bcup bcup)
  :category       testing
  :hidden-if      (or (<= (length (eldev-listify eldev-test-framework)) 1) (not (memq 'buttercup eldev-test-framework)))
  :profiling-self t
  (eldev--do-test 'buttercup parameters))

(eldev-defcommand eldev-test-ecukes (&rest parameters)
  "Run project's Ecukes regression/unit tests.  See command
`test' for details."
  :parameters     "[SELECTOR...]"
  :aliases        ecukes
  :category       testing
  :hidden-if      (or (<= (length (eldev-listify eldev-test-framework)) 1) (not (memq 'ecukes eldev-test-framework)))
  :profiling-self t
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
                            (files-looked-up    (listp files))
                            framework-filesets
                            disregard-framework-filesets)
                       (unless files-looked-up
                         (dolist (framework used-by-frameworks)
                           (let* ((variable (intern (format "eldev-test-%s-fileset" framework)))
                                  (fileset  (when (boundp variable) (symbol-value variable))))
                             (if fileset
                                 (push fileset framework-filesets)
                               (setf disregard-framework-filesets t))))
                         (setf files (eldev-find-and-trace-files `(:and ,(eldev-standard-fileset 'test) ,fileset ,@(when (and framework-filesets (not disregard-framework-filesets))
                                                                                                                     `((:or ,@(nreverse framework-filesets)))))
                                                                 file-description))
                         (when filter-patterns
                           (setf files (eldev-filter-files files (reverse filter-patterns)))
                           (eldev-trace "%s" (eldev-message-enumerate-files (format "Remaining %s after applying `--file' filter(s): %%s (%%d)" file-description) files)))
                         (setf (nth 2 (cdr entry)) files))
                       ;; Framework autoguessing can only work if there is at least one file to load, so
                       ;; this `when' is important.
                       (when files
                         (setf found-any-files t)
                         (when (and (equal fileset "*.el") (not files-looked-up))
                           (eldev-autoinstalling-implicit-dependencies t
                             (dolist (file files)
                               (let* ((absolute-without-el (replace-regexp-in-string (rx ".el" eos) "" (expand-file-name file eldev-project-dir) t t))
                                      (already-loaded      (eldev-any-p (assoc (concat absolute-without-el it) load-history) load-suffixes)))
                                 (if already-loaded
                                     (eldev-trace "Not loading file `%s': already `require'd by some other file" file)
                                   (eldev-named-step nil (eldev-format-message "loading test file `%s'" file)
                                     (eldev-trace "%s..." (eldev-current-step-name t))
                                     ;; Loading the test file can results in evaluation, which might use `eldev-backtrace'.
                                     (eldev-backtrace-notch 'eldev
                                       (load absolute-without-el nil t nil t))))))))
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
                                 (let ((actual-selectors (eldev-test-preprocess-selectors framework selectors))
                                       (profiling-self   (eldev-test-get-framework-entry framework 'profiling-self)))
                                   (unless already-prepared
                                     (run-hook-with-args (intern (format "eldev-test-%s-hook" framework)) actual-selectors)
                                     (eldev-test-prepare-framework framework actual-selectors))
                                   (condition-case error
                                       (if (eq pass 'run)
                                           (unwind-protect
                                               (eldev-backtrace-notch 'eldev
                                                 (if profiling-self
                                                     (funcall runner framework actual-selectors files)
                                                   (eldev-profile-body
                                                     (funcall runner framework actual-selectors files))))
                                             (eldev-test-finalize-framework framework actual-selectors))
                                         (let ((count-tests (eldev-test-get-framework-entry framework 'count-tests)))
                                           (if count-tests
                                               (setf num-matched-tests (+ num-matched-tests (funcall count-tests actual-selectors files)))
                                             (push framework noncounting-frameworks))))
                                     (eldev-error
                                      ;; Normally we just print and continue for other frameworks.
                                      (eldev--print-error error 'test)
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
  ;;
  ;; FIXME: Why do we need this?  Drawback example: when multiple frameworks are used
  ;;        (since 0.10) and you explicitly test with only one, the second one also gets
  ;;        installed because of the below code.
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
            (let ((as-elisp (eldev-read-wholly selector (eldev-format-message "selector `%s'" selector))))
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
  :aliases       tests
  :superseded-by (ecache dot-eldev)
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
           (eldev-test-runner-standard-environment framework)))

(defun eldev-test-runner-standard-environment (framework)
  ;; Other frameworks are just invoked with empty environment.
  (pcase framework
    ;; For Buttercup we still pass through our color setting.
    (`buttercup `((buttercup-color . ,(eldev-output-colorized-p))))))

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
           (eldev-test-runner-simple-environment framework)))

(defun eldev-test-runner-simple-environment (framework)
  (nconc (pcase framework
           (`ert
            `((ert-quiet                        . ,(not (eldev-unless-quiet t)))
              (eldev--test-ert-short-backtraces . t)))
           (`buttercup
            `((buttercup-reporter-batch-quiet-statuses . ,`(skipped disabled ,@(unless (eldev-unless-quiet t) '(pending passed))))))
           (`ecukes
            `((ecukes-verbose . (not (eldev-unless-quiet t))))))
         (eldev-test-runner-standard-environment framework)))

(defvar eldev--test-runner-concise-num-executed 0)
(defvar eldev--test-runner-concise-num-reported 0)

(eldev-deftestrunner eldev-test-runner-concise (framework selectors files)
  "Test runner that alters how progress is represented.  Normally
both ERT and Buttercup are verbose, printing out full name or
description even for passing tests.  However, with this runner
only a single dot is printed for each \"expected result\" test
instead.  This lets you see that something is being done (which
is important for slower tests or a huge amount of them), yet
doesn't spam the output with not-so-important information.

In all other respects (also for not-really-supported Ecukes) this
is the same as the test runner `simple'.  In particular, if Eldev
is in quiet mode, even the progress output described above will
be silenced."
  (let ((eldev--test-runner-concise-num-executed 0)
        (eldev--test-runner-concise-num-reported 0))
    (funcall (eldev-test-get-framework-entry framework 'run-tests t) selectors files 'concise
             (eldev-test-runner-concise-environment framework))))

(defun eldev-test-runner-concise-environment (framework)
  (nconc (pcase framework
           (`ert
            `((ert-quiet                        . t)
              (eldev--test-ert-concise-expected . t)))
           (`buttercup
            `((buttercup-reporter-batch-quiet-statuses . (skipped disabled pending passed))
              (eldev--test-buttercup-concise-expected  . t))))
         (eldev-test-runner-simple-environment framework)))

(defun eldev-test-runner-concise-tick (force-number &optional num-executed num-planned)
  (unless num-executed
    (setf num-executed (1+ eldev--test-runner-concise-num-executed)))
  (let* ((num-new  (- num-executed eldev--test-runner-concise-num-executed))
         (progress (make-string num-new ?.)))
    ;; Normally test frameworks print progress using `message', but we redirect that to stdout (see
    ;; `eldev--execute-command').  So, print progress output of this runner to stdout too.
    (if (and (not force-number) (< num-executed (+ eldev--test-runner-concise-num-reported 50)) (not (and num-planned (= num-executed num-planned))))
        (eldev-print :nolf progress)
      (if num-planned
          (let ((num-planned-string (number-to-string num-planned)))
            (eldev-print "%s %s%s" progress (format (format "%%%dd" (length num-planned-string)) num-executed) (eldev-colorize (format "/%s" num-planned-string) 'details)))
        (eldev-print "%s %d" progress num-executed))
      (setf eldev--test-runner-concise-num-reported num-executed))
    (setf eldev--test-runner-concise-num-executed num-executed)))


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
  (setf eldev-test-stop-on-unexpected (if num-failures (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number num-failures :min 0)) t)))

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
  (setf eldev-test-print-backtraces (if width (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number width :min 0)) t)))

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
  (setf eldev-test-expect-at-least (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number n :min 0))))

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



;; eldev lint, eldev doctor

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
Default value includes only the package descriptor file and,
since 1.2, its autoloads file.  In other words, `PACKAGE-pkg.el'
and `PACKAGE-autoloads.el'.

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
supported linters with the exception of `elisp-lint' will be run.

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
  :category       testing
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
        (let ((multiple-linters         (cdr linters))
              (eldev--lint-num-warnings 0))
          (eldev-trace "Going to run the following %s" (eldev-message-enumerate '("linter:" "linters:") linters))
          (catch 'eldev--lint-stop
            (dolist (linter linters)
              (let ((canonical-name (or (cdr (assq linter eldev--linter-aliases)) linter)))
                (when (or (memq linter eldev-lint-disabled) (memq canonical-name eldev-lint-disabled))
                  ;; This can only happen if a linter is requested explicitly.
                  (signal 'eldev-error `("Linter `%s' is disabled (see variable `eldev-lint-disabled')" ,linter)))
                (if multiple-linters
                    (eldev-print :color 'section "Running linter `%s'" linter)
                  (eldev-verbose "Running linter `%s'..." linter))
                (condition-case error
                    (funcall (cdr (assq canonical-name eldev--linters)))
                  (eldev-missing-dependency (if eldev-lint-optional
                                                (eldev--print-error error nil (eldev-format-message "%%s; skipping linter `%s'" linter))
                                              (signal 'eldev-error `("%s; cannot use linter `%s'" ,(eldev-extract-error-message error) ,linter)))))
                (when (and (eq eldev-lint-stop-mode 'linter) (> eldev--lint-num-warnings 0))
                  (eldev-trace "Stopping after the linter that issued warnings")
                  (throw 'eldev--lint-stop nil))
                (when multiple-linters
                  (eldev-print "")))))
          (if (> eldev--lint-num-warnings 0)
              (signal 'eldev-error `("Linting produced warnings"))
            (eldev-print (if (cdr linters) "Linters have %s" "Linter has %s") (eldev-colorize "no complaints" 'success))))
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
         (eldev-print "File `%s': %s" ,file ,(eldev-colorize "no warnings" 'success))
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
  (eldev-saving-dependency-lists
    (eldev-add-extra-dependencies 'runtime '(:tool package-lint))
    (eldev-load-extra-dependencies 'runtime))
  ;; This linter needs access to package archive contents, so at least fetch all archives
  ;; we have never fetched yet.
  (let ((eldev-global-cache-archive-contents-max-age nil))
    ;; Need to make sure that all archive contents is loaded
    ;; (`eldev--install-or-upgrade-dependencies' loads as few archives as required to
    ;; install the runtime dependencies).
    (eldev--fetch-archive-contents (eldev--determine-archives-to-fetch)))
  (eldev--linter-package-present-archives)
  (eldev--require-external-feature 'package-lint)
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
  (eldev-saving-dependency-lists
    (eldev-add-extra-dependencies 'runtime '(:tool relint))
    (eldev-load-extra-dependencies 'runtime))
  (eldev--require-external-feature 'relint)
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
  (eldev-saving-dependency-lists
    (eldev-add-extra-dependencies 'runtime '(:tool elisp-lint))
    ;; This linter might need access to package dependencies for byte-compilation.
    (eldev-load-project-dependencies 'runtime nil t))
  ;; This linter might invoke `package-lint' that needs access to package archive
  ;; contents.  Unlike `eldev-linter-package' (where we load _only_ extra dependencies,
  ;; which might not be enough), here we don't need to fetch archive contents, as it
  ;; should have been done by `eldev-load-project-dependencies' already.
  (eldev--linter-package-present-archives)
  (with-eval-after-load 'package-lint
    (unless package-lint-main-file
      (setf package-lint-main-file (eldev-project-main-file))))
  (eldev--require-external-feature 'elisp-lint)
  ;; I don't see a better way than just replacing its output function completely.
  (eldev-advised ('elisp-lint--print :override (lambda (_color format-string &rest arguments)
                                                 (eval `(eldev-warn ,format-string ,@arguments) t)
                                                 (eldev-lint-note-warning)))
    (dolist (file (eldev-lint-find-files "*.el"))
      (eldev-lint-linting-file file
        (elisp-lint-file file)))))


(defun eldev-lint-fileset ()
  ;; Target set is hardcoded for now.  It makes little sense to lint generated files, so
  ;; those are excluded (important only if someone procedurally generates a `.el').
  (eldev-standard-fileset 'main nil t))

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


(defvar eldev-doctor-disabled-tests nil
  "List of tests disabled in current project.
This variable is not accessible from the command line.  Instead,
a project may disable those tests whose warnings it won't fix.")

(defvar eldev-doctor-print-successful nil
  "Whether to print result of successful tests.
By default, only failed doctests are printed.")

(declare-function eldev--do-doctor 'eldev-doctor)
(declare-function eldev--doctor-list-tests 'eldev-doctor)

(eldev-defcommand eldev-doctor (&rest parameters)
  "Check your project for potential problems.  Unlike linters,
these tests are not targeted at Elisp code, but rather at project
infrastructure, including the way it uses Eldev.

Issues reported should be seen as suggestions, not as warnings.
Depending on your project needs and other circumstances, it may
be perfectly valid to implement something against doctor's
recommendations.  Projects can even disable certain tests by
modifying variable `eldev-doctor-disabled-tests' in their file
`Eldev'."
  :parameters     "[SELECTOR...]"
  :category       testing
  (require 'eldev-doctor)
  (eldev--do-doctor parameters))

(eldev-defbooloptions eldev-doctor-print-successful eldev-doctor-print-only-failed eldev-doctor-print-successful
  ("Also print results of successful tests"
   :options       (-s --successful --all-tests))
  ("Print result only of failed tests"
   :options       (-f --failed))
  :for-command    doctor)

(eldev-defoption eldev-doctor-list-tests ()
  "List doctests and exit"
  :options        (-L --list-tests)
  :for-command    doctor
  (require 'eldev-doctor)
  (eldev--doctor-list-tests))



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

(defvar eldev-eval-preprocess-forms nil
  "How to preprocess forms before evaluating them.
Currently supported values are symbols `macroexpand' and
`byte-compile', with the obvious meanings.  Can also be left as
nil, in which case no preprocessing is performed.")

(defvar eldev-eval-repeat nil
  "Evaluate every form several times.
Value of this variable must be either nil or a number.")

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
that you don't have to include `(require ...)' yourself.

Results of evaluation are printed to stdout using specified
function.  Default printer is function `eldev-prin1' that falls
back to `cl-prin1' as long as that is available (Emacs 26 and
up).

Expressions can be specified on the command line, read from a
file or both, see option `--file'.  In the latter case files are
not loaded in Elisp sense, but forms in them are read to be
evaluated.  If `eldev-dwim' variable is on (default), Eldev
treats any command line expression that ends in `.el' as a
filename."
  :parameters     "EXPRESSION..."
  :aliases        evaluate
  :category       running
  :profiling-self t
  (eldev--do-eval t parameters))

(eldev-defcommand eldev-exec (&rest parameters)
  "Execute Lisp forms for side effect.  Forms are executed in
project's environment, with all the dependencies available.  If
option `-r' is specified (or is on by default), project's main
feature will be required before evaluating, so that you don't
have to include `(require ...)' yourself.

Forms can be specified on the command line, read from a file or
both, see option `--file'.  In the latter case files are not
loaded in Elisp sense, but forms in them are read to be executed.
If `eldev-dwim' variable is on (default), Eldev treats any
command line expression that ends in `.el' as a filename.

This is basically like `eval' command, with the only difference
being that it doesn't print form results."
  :parameters     "FORM..."
  :aliases        execute
  :category       running
  :profiling-self t
  (eldev--do-eval nil parameters))

(defun eldev--do-eval (print-results parameters)
  (unless parameters
    (signal 'eldev-wrong-command-usage `(t ,(eldev-format-message "Missing %s (on the command line or in a file)" (if print-results "expressions to evaluate" "forms to execute")))))
  (let ((repetitions (if eldev-eval-repeat (max (round eldev-eval-repeat) 1) 1))
        forms)
    (dolist (parameter parameters)
      (when (and eldev-dwim (stringp parameter) (string-match-p (rx ".el" eos) parameter))
        (setf parameter `(,parameter)))
      (if (stringp parameter)
          (let ((parameter-forms (eldev-read-wholly parameter (eldev-format-message "%s from `%s'" (if print-results "expression(s)" "form(s) to evaluate") parameter) t)))
            (dolist (form (eldev-listify parameter-forms))
              (push `(,form ,(eldev--eval-short-form-string form)) forms)))
        (let* ((file       (car parameter))
               (short-name (abbreviate-file-name file)))
          (eldev-trace "Reading forms from file `%s'..." file)
          (condition-case nil
              (dolist (form (eldev-read-file-forms file))
                (push `(,form ,(eldev--eval-short-form-string form) ,short-name) forms))
            (file-error (signal 'eldev-error `("Unable to read forms from file `%s': file is missing or unreadable" ,short-name)))))))
    (when eldev-eval-load-project
      (eldev-load-project-dependencies (if print-results 'eval 'exec)))
    (eldev-autoinstalling-implicit-dependencies eldev-eval-load-project
      (when (and eldev-eval-load-project eldev-eval-require-main-feature)
        (dolist (feature (eldev-required-features eldev-eval-required-features))
          (eldev-named-step nil (eldev-format-message "autorequiring feature `%s' before %s" feature (if print-results "evaluating" "executing"))
            (eldev-verbose "%s..." (eldev-current-step-name t))
            (require feature))))
      (let (all-results)
        (dolist (form (nreverse forms))
          (let ((description (eldev-format-message (if print-results "expression #%d, `%s'%s" "form #%d, `%s'%s")
                                                   (1+ (length all-results)) (cadr form)
                                                   (if (nth 2 form) (eldev-format-message " (from file `%s')" (nth 2 form)) ""))))
            (eldev-named-step nil (eldev-format-message (if print-results "evaluating %s" "executing %s") description)
              (eldev-verbose (if print-results "%s:" "%s...") (eldev-current-step-name t))
              (let (result)
                ;; For these commands we restore the standard behavior of `message' of writing to stderr,
                (let ((eldev-message-rerouting-destination :stderr))
                  (if (eq eldev-eval-preprocess-forms 'byte-compile)
                      (progn (eldev-trace "Byte-compiling first, as instructed by `eldev-eval-preprocess-forms'...")
                             (let* ((lexical-binding  eldev-eval-lexical)
                                    (result-variables (when all-results
                                                        `(@ ,@(mapcar (lambda (n) (intern (format "@%d" n))) (number-sequence 1 (length all-results))))))
                                    (function         (byte-compile `(lambda (,@result-variables) (ignore ,@result-variables) ,(car form))))
                                    (result-arguments (when all-results
                                                        (cons (car (last all-results)) all-results))))
                               ;; FIXME: Normally on failure `byte-compile' returns nil.  However, at least
                               ;;        during CI on GitHub on Windows using Emacs 28.2, I have seen this
                               ;;        return a string (the error message).  Maybe it could then return a
                               ;;        function on failure in some circumstances?
                               (unless (functionp function)
                                 ;; As an exception, fake-end the last named step, as its
                                 ;; text is nearly the same as the error message anyway.
                                 (pop eldev-ongoing-named-steps)
                                 (signal 'eldev-error `("Couldn't byte-compile %s" ,description)))
                               (eldev-backtrace-notch 'eldev
                                 (eldev-profile-body
                                   (dotimes (_ repetitions)
                                     (setf result (apply function result-arguments)))))))
                    (let ((final-form (if (eq eldev-eval-preprocess-forms 'macroexpand)
                                          (progn (eldev-trace "Expanding macros first, as instructed by `eldev-eval-preprocess-forms'...")
                                                 (macroexpand-all (car form)))
                                        (when eldev-eval-preprocess-forms
                                          (eldev-warn "Ignoring unknown value %S of variable `eldev-eval-preprocess-forms'" eldev-eval-preprocess-forms))
                                        (car form))))
                      ;; Evaluated forms get access to variables `@' that holds the result of
                      ;; the previous form and `@1', `@2'... that hold results of previous
                      ;; form at that index.
                      (when all-results
                        (setf final-form `(let ((@ ',(car (last all-results)))
                                                ,@(mapcar (lambda (n) `(,(intern (format "@%d" n)) ',(nth (1- n) all-results)))
                                                          (number-sequence 1 (length all-results))))
                                            ,@(macroexp-unprogn final-form))))
                      (eldev-backtrace-notch 'eldev
                        (eldev-profile-body
                          (dotimes (_ repetitions)
                            (setf result (eval final-form (not (null eldev-eval-lexical))))))))))
                (setf all-results (append all-results (list result)))
                (when print-results
                  (with-temp-buffer
                    (funcall (or eldev-eval-printer-function #'prin1) result (current-buffer))
                    ;; Older Emacs version end some value representations with a linefeed for
                    ;; whatever reasons.
                    (when (equal (char-before) ?\n)
                      (delete-char -1))
                    (eldev-output "%s" (buffer-string))))))))))))

(defun eldev--eval-short-form-string (form)
  "Return a possibly shortened string representation of FORM."
  (let* ((print-level  4)
         (print-length 10)
         (as-string    (with-temp-buffer
                         (eldev-prin1 form (current-buffer))
                         (buffer-string))))
    (setf as-string (replace-regexp-in-string "\n" "\\n" as-string t t))
    (if (< (length as-string) 150)
        as-string
      (concat (substring as-string 0 147) "..."))))

(eldev-defoption eldev-eval-from-file (file)
  "Read expressions (forms) from given file"
  :options        (-f --file)
  :value          FILE
  :for-command    (eval exec)
  (push `(,file) eldev-preprocessed-command-line))

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
  ("Don't load project and its dependencies;
only Eldev itself and Emacs built-ins will be available"
   :options       (--dont-load)
   :hidden-if     eldev-eval-load-project)
  :for-command    (eval exec))

(eldev-defoption eldev-eval-as-is ()
  "Don't macroexpand or byte-compile forms, evaluate them as they are"
  :options        (-a --as-is)
  :for-command    (eval exec)
  :if-default     (not (memq eldev-eval-preprocess-forms '(macroexpand byte-compile)))
  :hidden-if      :default
  (setf eldev-eval-preprocess-forms nil))

(eldev-defoption eldev-eval-macroexpand-forms ()
  "Expand all macros in forms before evaluating"
  :options        (-m --macroexpand)
  :for-command    (eval exec)
  :if-default     (eq eldev-eval-preprocess-forms 'macroexpand)
  (setf eldev-eval-preprocess-forms 'macroexpand))

(eldev-defoption eldev-eval-byte-compile-forms ()
  "Byte-compile forms before evaluating"
  :options        (-c --compile)
  :for-command    (eval exec)
  :if-default     (eq eldev-eval-preprocess-forms 'byte-compile)
  (setf eldev-eval-preprocess-forms 'byte-compile))

(eldev-defoption eldev-eval-repeat (n)
  "Evaluate every form N times"
  :options        (-n --repeat)
  :value          N
  :for-command    (eval exec)
  :default-value  (if eldev-eval-repeat (max (round eldev-eval-repeat) 1) 1)
  (setf eldev-eval-repeat (eldev-with-errors-as 'eldev-wrong-option-usage (eldev-parse-number n :min 1))))

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
  (if (file-equal-p project-dir eldev-project-dir)
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
          ;; In this particular case it's probably better not to do anything with the output, since
          ;; the evaluated form might, in principle, result in anything.  Just tell the child
          ;; process not to use the color and hope that sticks.
          (eldev-call-process (eldev-shell-command) `("--quiet" "--color=never" "exec" "--dont-load" ,(prin1-to-string `(prin1 ,form)))
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

(defvar eldev-emacs-forward-eldev t
  "Whether to make Eldev available in spawned Emacs.
This will also load various Eldev configuration and setup in the
spawned Emacs, including files `Eldev' and `Eldev-local'.  This
provides a simple way to affect code running in `eldev emacs'.
Eldev may also be used to e.g. provide debugging output with
`eldev-debug' or `eldev-dump'.

If this variable is set to nil, Eldev will still be in the
`load-path'.

Since 1.3.")

(defvar eldev-emacs-forward-eldev-variables
  '(debug-on-error eldev-project-dir load-prefer-newer eldev-setup-first-forms eldev-setup-forms)
  "Additional variables to forward when “forwarding” Eldev.")

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
  :category       running
  :custom-parsing t
  (eldev-load-project-dependencies 'emacs)
  (let (forwarding)
    (dolist (variable (append eldev-emacs-forward-variables (when eldev-emacs-forward-eldev eldev-emacs-forward-eldev-variables)))
      (when (boundp variable)
        (push variable forwarding)
        (push (eldev-macroexp-quote (symbol-value variable)) forwarding)))
    (let* ((autoloads           (apply #'nconc (mapcar (lambda (file) `("--load" ,file)) eldev--loaded-autoloads-files)))
           (value-forwarding    (when forwarding `("--eval" ,(prin1-to-string `(setf ,@(nreverse forwarding))))))
           (effective-load-path (mapconcat #'identity load-path path-separator))
           ;; See https://github.com/emacs-eldev/eldev/issues/89.  The idea here is to make
           ;; nested Emacs die on its own if run in terminal mode.  I also tried adding an
           ;; evalled call on its command line that'd invoke `kill-emacs' in that mode,
           ;; but this turned out not to quite work: it's called too late, when Emacs
           ;; already has set up itself on the terminal, and I couldn't make it print any
           ;; helpful output: it would just get lost.
           (process-environment `(,(format "EMACSLOADPATH=%s" effective-load-path) ,"TERM=eldev-emacs-batch-mode" ,@process-environment)))
      (eldev-call-process eldev-emacs-executable
          (if (string= (car parameters) "--")
              (append autoloads value-forwarding (cdr parameters))
            (append eldev-emacs-default-command-line
                    autoloads
                    value-forwarding
                    (when eldev-emacs-forward-eldev
                      '("--eval" "(progn (require 'eldev) (eldev-set-up-secondary))"))
                    (apply #'append (mapcar (lambda (feature) (list "--eval" (format "(require '%s)" feature)))
                                            (eldev-required-features eldev-emacs-required-features)))
                    parameters))
        :pre-execution  (eldev-verbose "Full command line to run child Emacs process:\n  %s" (eldev-message-command-line executable command-line))
        :pre-execution  (eldev-verbose "Effective load path for it:\n  %s" effective-load-path)
        :forward-output t
        :destination    t
        (when (/= exit-code 0)
          (signal 'eldev-error `(:hint ,(when (looking-at ".*eldev-emacs-batch-mode.+not defined")
                                          (concat "Eldev cannot start Emacs in terminal mode due to Elisp limitations\n"
                                                  "Use either a graphical window mode (if supported in your Emacs) or batch mode"))
                                       "Child Emacs exited with error code %d" ,exit-code)))))))



;; eldev docker

(defvar eldev--docker-gui-args
  (list "-e" "DISPLAY" "-v" "/tmp/.X11-unix:/tmp/.X11-unix")
  "Arguments needed to launch dockerized Emacs as a GUI.")

(defvar eldev-docker-run-extra-args nil
  "Extra arguments to pass to \"docker run\".")

(defvar eldev--docker-home-name "docker-home"
  "Name of the home directory of the docker user.")


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

Unless `eldev-skip-global-config' is nil, the global config file
will be mounted for the process in Docker to access.

If AS-GUI is non-nil include arguments necessary to run Emacs in
GUI mode.

If LOCAL-ELDEV (a directory) is specified, the returned arguments
will contain a mount of it at `/eldev'."
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
            ;; Let Eldev inside Docker reuse the global cache — also across invocations! —
            ;; for bootstrapped Eldev package and the global package archive cache.  Make
            ;; sure the directory exists, else the process inside Docker may create it
            ;; with wrong owner/group.
            (list "-v" (format "%s:%s" (eldev-global-cache-dir t) container-eldev-cache-dir))
            (unless (or eldev-skip-global-config
                        (not (file-exists-p eldev-user-config-file)))
              (list "-v" (format "%s:%s/config"
                                 eldev-user-config-file
                                 container-eldev-cache-dir)))
            (eldev--docker-local-dep-mounts container-home)
            eldev-docker-run-extra-args
            (cons img eldev-args))))

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
  :category       running
  :custom-parsing t
  (unless (eldev--docker-on-supported-os)
    (signal 'eldev-error `("OS `%s' is currently not supported by Eldev's `docker' command" ,system-type)))
  (unless (car parameters)
    (signal 'eldev-wrong-command-usage `(t "version not specified")))
  (let* ((img             (eldev--docker-determine-img (car parameters)))
         (docker-exec     (eldev-docker-executable))
         ;; We don't pass Eldev's global options, user needs to specify them for the child
         ;; process explicitly if wanted.  But there is one exception: since Docker output
         ;; is sent to the same terminal as the main process' output, it makes sense to
         ;; synchronize coloring (can still be overridden).
         (escaped-params  (mapcar #'eldev-quote-sh-string (cons (eldev--color-setting-option) (cdr parameters))))
         (container-cmd   (eldev--docker-container-eldev-cmd escaped-params))
         (as-gui          (and (string= "emacs" container-cmd)
                               (not (member "--batch" parameters))))
         (local-eldev     (getenv "ELDEV_LOCAL"))
         (exp-local-eldev (when (> (length local-eldev) 0) (expand-file-name local-eldev)))
         (command-line    (mapconcat #'identity escaped-params " "))
         (args            (eldev--docker-args img
                                              ;; The relevant virtual Docker mount is supposed to be added later.
                                              (if exp-local-eldev
                                                  `("sh" "-c" ,(format "ELDEV_LOCAL=/eldev /eldev/bin/eldev %s"      command-line))
                                                `("sh"   "-c" ,(format "export PATH=\"$HOME/bin:$PATH\" && eldev %s" command-line)))
                                              as-gui exp-local-eldev)))
    (unwind-protect
        (eldev-call-process docker-exec args
          :pre-execution (eldev-verbose "Full command line to run a Docker process:\n  %s"
                                        (eldev-message-command-line executable command-line))
          :forward-output t
          ;; Using custom code instead of `:die-on-error' because of the hint.
          (when (/= exit-code 0)
            (signal 'eldev-error `(:hint ,(when (string-match-p "unavailable, simulating -nw" (buffer-string))
                                            '("It appears your X server is not accepting connections from the Docker container"
                                              "Have you run `xhost +local:root' (remember about security issues, though)?"))
                                         "Docker process exited with error code %d" ,exit-code))))
      (delete-directory (eldev--docker-home) t))))



;; eldev targets, eldev build, eldev compile, eldev package

(defvar eldev-build-system-hook nil
  "Hook executed whenever build system is used.
Since Eldev 0.1.1.")

(defvar eldev--builders nil)

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
all targets will be force-built.  If it is a list that includes
t, that element will be replaced with target names specified to
command `build'.

However, a target will only ever be force-built if it is
otherwise a part of building plan.")

(defvar eldev-build-infinitely-new nil
  "Files (sources or intermediate targets) that are “infinitely new”.
Can be either a list of filenames or symbol t, meaning “all of
them”.  Targets that are built from these sources or depend on
them will never be found up-to-date.")

(defvar eldev-build-ignored-target-fileset '(:or (concat (eldev-package-descriptor-file-name) "c")
                                                 (concat (eldev-package-autoloads-file-name)  "c"))
  "Fileset of apparent targets that should be ignored.
If a builder specifies a target that matches this fileset, it is
ignored completely: not even listed in `eldev targets' output.
Default value includes byte-compiled file for package descriptor
file, i.e. `PACKAGE-pkg.elc' and (since 1.2) byte-compiled file
for package `autoloads' file (might be important if this file is
generated with an external tool and not with the Eldev plugin).

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

(defvar eldev-targets-list-sources t
  "Whether to print target sources.
In addition to t or nil, can also be symbol `concise'.")

(defvar eldev-targets-list-dependencies nil
  "Whether to print known target dependencies.")

;; A cons cell of (PUBLIC . DEPENDENCY-HASH-TABLE).
(defvar eldev--target-dependencies nil)
(defvar eldev--target-dependencies-need-saving nil)

;; FIXME: Maybe find a better way?
(defvar eldev--package-target-file nil)
(defvar eldev--package-target-generated nil)

(defvar eldev--feature-providers (make-hash-table :test #'eq))


(defsubst eldev-virtual-target-p (target)
  (string-prefix-p ":" target))

(defmacro eldev-with-target-dependencies (&rest body)
  "Execute BODY with target dependency mechanism set up."
  (declare (indent 0) (debug (body)))
  `(eldev--maybe-with-target-dependencies t t ,@body))

(defun eldev--load-target-dependencies (public &optional force)
  (prog1
      ;; Return t if setting up now; actually loading a lazily scheduled map doesn't count.
      (null eldev--target-dependencies)
    ;; Don't replace already loaded dependency map.  But if it is t by now, we might replace
    ;; that with an actually loaded value (unless called as `lazy' again).
    (unless eldev--target-dependencies
      (setf eldev--target-dependencies (cons nil nil)))
    (when public
      (require 'eldev-build)
      (setf (car eldev--target-dependencies) t))
    (when (and (null (cdr eldev--target-dependencies)) (or public force))
      (setf (cdr eldev--target-dependencies) (or (eldev--do-load-target-dependencies public) (make-hash-table :test #'equal))))))

(defun eldev--do-load-target-dependencies (public)
  (let ((eldev-verbosity-level (if public eldev-verbosity-level 'quiet)))
    (eldev-do-load-cache-file (expand-file-name "target-dependencies.build" (eldev-cache-dir t)) "target dependencies" 2
      (cdr (assq 'dependencies contents)))))

(defun eldev--save-target-dependencies ()
  ;; Unless dependency usage is public, save quietly.
  (let ((eldev-verbosity-level (if (car eldev--target-dependencies) eldev-verbosity-level 'quiet)))
    (if eldev--target-dependencies-need-saving
        (eldev-do-save-cache-file (expand-file-name "target-dependencies.build" (eldev-cache-dir t)) "target dependencies" 2
          (setf eldev--target-dependencies-need-saving nil)
          `((dependencies . ,(cdr eldev--target-dependencies))))
      (eldev-trace "Target dependency information is up-to-date, not saving...")))
  (setf eldev--target-dependencies nil))


;; Internal helper for `eldev-defbuilder'.
(defun eldev--register-builder (builder name keywords)
  (let (cleaner-specifications)
    (while keywords
      (eldev-pcase-exhaustive (pop keywords)
        ((and (or :type :short-name :message :source-files :targets :collect :briefdoc :profiling-self) keyword)
         (eldev-put builder keyword (pop keywords)))
        (:define-cleaner
         (push (pop keywords) cleaner-specifications))))
    (eldev--assq-set name builder eldev--builders)
    (dolist (specification cleaner-specifications)
      (eval `(eldev-defcleaner ,(car specification) ()
               ,@(cdr specification)
               (require 'eldev-build)
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


(declare-function eldev--do-targets 'eldev-build)

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
  :category       building
  (require 'eldev-build)
  (eldev--do-targets parameters))

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


(declare-function eldev--do-build 'eldev-build)

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
  :category       building
  :profiling-self t
  ;; Resolve t elements in `eldev-build-force-rebuilding' now.
  (when (and (listp eldev-build-force-rebuilding) (memq t eldev-build-force-rebuilding))
    (setf eldev-build-force-rebuilding (append (delq t eldev-build-force-rebuilding)
                                               (eldev-filter (not (eldev-virtual-target-p it)) parameters))))
  (require 'eldev-build)
  (eldev--do-build parameters))

(eldev-defcommand eldev-compile (&rest parameters)
  "Byte-compile `.el' files.

By default, all `.el' files are compiled.  But you can also
specify SOURCEs, which should be a fileset.

This is basically a different way of invoking `build' command.
However, instead of targets, here you specify sources."
  :parameters     "[SOURCE...]"
  :aliases        byte-compile
  :category       building
  :profiling-self t
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
  :category       building
  :profiling-self t
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
  ("Load `.el' files before byte-compiling them;
this is how Emacs packaging system behaves"
   :options       (-l --load-before-compiling))
  ("Byte-compile `.el' without loading them first;
this might require adding some `eval-and-compile' forms in your code"
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

;; If both `eldev-build-treat-warnings-as-errors' and this are specified, the former
;; should take precedence (it's up to specific builders).  I.e. warnings treated as errors
;; are no longer considered warnings and are, therefore, not silenced.
(eldev-defbooloptions eldev-build-suppress-warnings eldev-build-show-warnings eldev-build-suppress-warnings
  ("Suppress all warnings"
   :options       (-w --suppress-warnings))
  ("Show warnings during building"
   :options       --show-warnings
   :hidden-if     :default)
  :for-command    (build compile package))

(eldev-defoption eldev-build-force-rebuilding (&optional target)
  "Force building of TARGET even if it appears up-to-date (or targets
otherwise listed on the command line if option value is not specified)"
  :options        (-f --force)
  :optional-value TARGET
  :for-command    (build compile package)
  (unless (eq eldev-build-force-rebuilding t)
    (push (or target t) eldev-build-force-rebuilding)))

(eldev-defoption eldev-build-force-rebuilding-all ()
  "Force building of all targets"
  :options        (-F --force-all)
  :for-command    (build compile package)
  (setf eldev-build-force-rebuilding t))

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
  ("Print absolute package filename and word “generated” or “up-to-date”
as two last lines of output"
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


(declare-function eldev--do-get-target-dependencies 'eldev-build)
(declare-function eldev--byte-compile-.el 'eldev-build)
(declare-function eldev--need-to-build-full 'eldev-build)
(declare-function eldev--do-byte-compile-.el-on-demand 'eldev-build)

(eldev-defbuilder eldev-builder-byte-compile-.el (source target)
  :short-name     "ELC"
  :source-files   "*.el"
  ;; We intentionally don't use `byte-compile-dest-file'.  If you need to customize this
  ;; somehow, define a new builder instead.
  :targets        (".el" -> ".elc")
  :collect        ":compile"
  :define-cleaner (eldev-cleaner-.elc
                   "Delete `.elc' files, i.e. results of byte-compilation."
                   :aliases    (elc dot-elc)
                   :default    t)
  :profiling-self t
  (require 'eldev-build)
  (eldev--byte-compile-.el source target))

(defun eldev--maybe-byte-compile-.el-on-demand (file all-source-files quiet)
  ;; Ignore non-string "files": happen if the advice for `require' is called for a feature
  ;; not within the project being built, see `eldev--find-feature-provider'.
  (when (stringp file)
    (unless (car all-source-files)
      (let ((main-el-files (make-hash-table :test #'equal)))
        (dolist (source (eldev-find-files `(:and ,(eldev-standard-fileset 'main) "*.el")))
          (puthash source t main-el-files))
        (setf (car all-source-files) main-el-files)))
    ;; Only maybe compile if the file does belong to project sources.
    ;; FIXME: Do we actually need this check?
    (when (gethash file (car all-source-files))
      (require 'eldev-build)
      (let ((elc-file (eldev-replace-suffix file ".el" ".elc")))
        (when (eldev--need-to-build-full elc-file elc-file 'on-demand)
          (eldev--do-byte-compile-.el-on-demand file quiet))))))


(eldev-defbuilder eldev-builder-makeinfo (source target)
  :short-name     "MKINFO"
  :source-files   eldev-makeinfo-sources
  :targets        ((".texi" ".texinfo") -> ".info")
  :define-cleaner (eldev-cleaner-.info
                   "Delete `.info' files generated from `.texi' or `.texinfo'."
                   :aliases (info dot-info)
                   :default t)
  (eldev-call-process (eldev-makeinfo-executable) `("--no-split" ,source "--output" ,target ,@(when eldev-build-suppress-warnings '("--no-warn")))
    :forward-output t
    :die-on-error t))

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
    :forward-output t
    :die-on-error t))


(declare-function eldev--do-package 'eldev-build)

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
  (require 'eldev-build)
  (eldev--do-package sources targets))

(defun eldev--package-name-version (&optional project-dir)
  (let ((package (eldev-package-descriptor project-dir)))
    (format "%s-%s" (package-desc-name package) (eldev-message-version (or eldev-package-forced-version package)))))

(defun eldev-generated-files ()
  "Return a list of all files that are or could be generated.
Such files should be excluded from some operations.  See also
NO-GENERATED parameter to `eldev-standard-fileset' and
`:without-generated' for function `eldev-standard-filesets'.

Since 1.3."
  (let (all-generated)
    ;; This list cannot really be created without build targets.  E.g. we want to include
    ;; non-existing (but would be created) targets here too.  Also, what if someone has
    ;; `.elc' or something like that for whatever reason in the project?
    (maphash (lambda (target _)
               (unless (eldev-virtual-target-p target)
                 (push target all-generated)))
             (eldev-build-find-targets))
    all-generated))

(defun eldev--find-feature-provider (feature)
  "Determine project file providing given FEATURE.
Return value is either a string (project file) or one of symbols
`built-in', `external' or `unknown'."
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
        ;; FIXME: Is this correct in presence of source directories?
        (setf filename (file-relative-name filename eldev-project-dir))
        (if (or (eldev-external-or-absolute-filename filename)
                (not (eldev-external-or-absolute-filename (file-relative-name filename eldev-cache-dir))))
            'external
          (setf filename (eldev-replace-suffix filename ".elc" ".el"))
          (when (file-exists-p (expand-file-name filename eldev-project-dir))
            filename)))
      'unknown))



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
  :category       information
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


;; We want the maintainer variables to be visible even if the plugin is not active.

(defvar eldev-release-validators '(eldev-release-validate-version
                                   eldev-release-validate-vcs
                                   eldev-release-validate-files
                                   eldev-release-test-project)
  "List of functions to check if release is possible.
Default value includes `eldev-release-validate-version',
`eldev-release-validate-vcs', `eldev-release-validate-files' and
`eldev-release-test-project'.  In future it may also include some
other standard hooks, but those will likely be inactivated by
default configuration.

Each function is called with one argument: VERSION to be
released, as a list.  Its return value is ignored.  If it wants
to prevent release from occuring, it should signal an error,
preferably an `eldev-error'.")

(defvar eldev-release-preparators nil
  "List of functions to call immediately before releasing.
These functions should preferably not fail.  If you want to check
release possibility in general, add your function to
`eldev-release-validators' instead.

Each function is called with one argument: VERSION to be
released, as a list.  Its return value is ignored.  A function
can modify project files; such modifications will be included
into the release commit.

The functions must respect `eldev-release-dry-run-mode': they are
called regardless, in case they include important read-only
operations too.")

(defvar eldev-release-post-release-preparators nil
  "List of functions to call before making a post-release commit.
Each function is called with two argument: the released VERSION
and POST-RELEASE-VERSION to be prepared, both as a list.  See
`eldev-release-preparators' for more information.")

(defvar eldev-release-file-check-forbidden-regexp (rx bow "DONOT" "RELEASE" eow)
  "Regular expression that must not match any project file contents.
In other words, if any file contains a match, release process is
aborted.  Search is case-sensitive.")

(defvar eldev-release-file-check-ignored-files nil
  "Fileset to ignore when checking files before releasing.")

(defvar eldev-release-test-local nil
  "Whether to run project's tests before releasing.
When non-nil, the tests must pass in the same Emacs version used
to run Eldev.")

(defvar eldev-release-test-other-emacses nil
  "Additional Emacs executables to test with before releasing.
Should be a string or a list of strings of installed different
Emacs versions.")

(defvar eldev-release-test-docker-images nil
  "Additional Docker-provided images to test with before releasing.
See command `docker' for details.  Should be a string or a list
of strings of Emacs versions or image names.")

(defvar eldev-release-test-global-options '("--packaged")
  "List of global Eldev options to be used when testing.
These options will be prepended with `--quiet', `--verbose'
etc. to pass through the used verbosity level.  You can fix it by
explicitly adding a level option to the list.")

(defvar eldev-release-test-command-options '("--stop-on-unexpected")
  "List of options to command `test' to be used before releasing.")

(defvar eldev-release-test-also-compile t
  "After testing, also ensure that byte-compilation succeeds.
This only takes effect if any form of local testing is enabled,
i.e. not by default.")

(defvar eldev-release-test-compile-command-options '("--set=all" "--warnings-as-errors")
  "List of options to command `compile' to be used before releasing.
By default, everything (including tests) is byte-compiled and all
warnings are treated as errors.")

(defvar eldev-release-commit-message "Release @[formatted-name]@ @[version-string]@"
  "Message (comment) given to the release commit.
See function `eldev-substitute' for the explanation of
formatting.  Available variables are `formatted-name',
`version-string', `version-list', but you can also use arbitrary
expressions.")

(defvar eldev-release-tag-function 'eldev-release-default-tag
  "Function that computes tag name for the release commit.
It should accept a single argument — the release version as a
list — and return tag name as a string or nil to skip tagging.
Default function simply converts the version to a string, unless
it's a snapshot version, in which case tagging is skipped.")

(defvar eldev-release-post-release-commit nil
  "Whether to create a post-release commit.
t means that a post-release commit is created and
`eldev-release-post-release-preparators' are called.

If the value is a function, it is called with the released
version as the only argument.  It may return a new version to be
set in the post-release commit.  Any other return value is
handled as described above.")

(defvar eldev-release-post-release-commit-message nil
  "Message (comment) given to the post-release commit.
See function `eldev-substitute' for the explanation of
formatting.  Available variables are `formatted-name',
`version-string', `version-list', but you can also use arbitrary
expressions.")

(defvar eldev-release-version-incrementors '(("major"    . eldev-release-next-major-version)
                                             ("minor"    . eldev-release-next-minor-version)
                                             ("patch"    . eldev-release-next-patch-version)
                                             ("snapshot" . eldev-release-next-snapshot-version))
  "Association list of strings to version incrementors.
Names from this assoc list can be used on the command line
instead of specific version number.  In this case, the
corresponding incrementor function is called with the package's
current version (as a list) to build the version to release.
Incrementor can return a list or a string.")

(defvar eldev-release-min-version-size 2
  "Minimum version size, in the range 1-4.
Default value of 2 means that the version will consist at least
of major and minor components, e.g. '3.1'.  If a version is
shorter than this (e.g. just '0' when the value is 3), it will be
either right-padded with zeros if autobuilt from 'major', 'patch'
etc., or not accepted.")

(defvar eldev-release-allowed-branch 'eldev-release-only-from-main-branch
  "Specifies allowed branches to make releases from.
Can be a string, a list of strings, a function or nil (to allow
any branch).

Functions get called with three arguments: VERSION (as a list),
BRANCH (a string) and VC-BACKEND.  They must return t (not any
other non-nil value!) for the given branch to be accepted.
Alternatively, they may return a string or a list of those to
indicate accepted branch name(s).  A function may also signal an
error.

Default value allows to release only from the main branch
(`master' in Git, `default' in Mercurial).")


(defvar eldev-release-ignore-untracked nil
  "Whether to ignore untracked files.
If nil, you won't be able to create releases if there are
untracked files in the working directory.  This is a precaution:
maybe you have forgotten to add them to the repository.")

(defvar eldev-release-skip-file-checks nil
  "Whether to skip all file contents checks.")

(defvar eldev-release-skip-testing nil
  "Whether to skip any configured project testing.")

(defvar eldev-release-interactive t
  "Ask for confirmation before most releasing steps.")

(defvar eldev-release-dry-run-mode nil
  "Don't release if non-nil, just pretend to do so.")


(defvar eldev-update-copyright-fileset '("!COPYING" "!COPYING.txt" "!LICENSE" "!LICENSE.txt")
  "Files in which to update copyright notices.
Default value excludes some standard cross-project files,
e.g. the license.

Since 1.3.")



;; eldev init, eldev githooks

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
   :options       (-n --non-interactive --go))
  :for-command    init)


(defvar eldev-githooks-uninstall nil
  "Uninstall the hooks instead of installing.")

(defvar eldev-githooks-force nil
  "Overwrite existing hooks if needed.
When uninstalling, also delete copied hooks.")

(defvar eldev-githooks-command-active #'eldev--githooks-command-active)

(declare-function eldev--do-githooks 'eldev-vc)

;; This is also about initializing, only the checkout.
(eldev-defcommand eldev-githooks (&rest parameters)
  "Install project-recommended Git hooks.  All files from
subdirectory `githooks' in the project root get symlinked to
`.git/hooks'.

The command can also uninstall the hooks, see option `-u'."
  :aliases        install-githooks
  :hidden-if      (not (funcall eldev-githooks-command-active))
  (when parameters
    (signal 'eldev-wrong-command-usage `(t "Unexpected command parameters")))
  (require 'eldev-vc)
  (eldev--do-githooks))

(eldev-defbooloptions eldev-githooks-uninstall eldev-githooks-install eldev-githooks-uninstall
  ("Uninstall the hooks instead"
   :options       (-u --uninstall))
  ("Do install the hooks"
   :options       (-i --do-install)
   :hidden-if     :default)
  :for-command    githooks)

(eldev-defbooloptions eldev-githooks-force-mode eldev-githooks-keep-existing-mode eldev-githooks-force
  ("Overwrite existing installed Git hooks; when uninstalling, also delete copied hooks, not only those symlinked"
   :options       (-f --force))
  ("Keep existing installed Git hooks; when uninstalling, only delete symlinks"
   :options       --keep-installed
   :hidden-if     :default)
  :for-command    githooks)

(defun eldev--githooks-command-active ()
  (and (eldev-find-files "./githooks/*")
       (eq (eldev-vc-detect) 'Git)))


(provide 'eldev)

;;; eldev.el ends here

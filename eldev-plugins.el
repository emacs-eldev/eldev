;;; eldev.el --- Elisp Development Tool  -*- lexical-binding: t -*-

;;; Copyright (C) 2020 Paul Pogonyshev

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

;;; Code:

(require 'eldev)


(defvar eldev--active-plugin-documentation nil)


(defun eldev-active-plugins ()
  "Return a list of all activated plugin names.
Since 0.3."
  (mapcar #'car eldev--active-plugin-documentation))

(defun eldev-use-plugin (plugin &rest configuration)
  "Use given PLUGIN in the build.
Currently, only a fixed number of built-in plugins is supported.
In the future, this might become a way to extend Eldev by reusing
third-party code automatically downloaded from a package archive.

CONFIGURATION is specific to each plugin.  Usually it is expected
to be a property list, i.e. keywords interleaved with values.

It is not an error to activate a plugin more than once (e.g. once
from `~/.eldev/config' and once from `Eldev').  However,
CONFIGURATION for all activations but the first will be ignored.

Since 0.3."
  (unless (assq plugin eldev--active-plugin-documentation)
    (push `(,plugin . ,(pcase plugin
                         (`undercover (eldev--undercover-plugin configuration))
                         (_ (error "Unknown plugin `%s'" plugin))))
          eldev--active-plugin-documentation)))



;; Undercover.

(defvar eldev-undercover-config nil
  "Configuration for `undercover' plugin as a list.
Each element should be a symbol matching one of the recognized
flags.  See function `eldev-undercover-config'.")

(defvar eldev-undercover-report-file nil
  "Filename used for `undercover' report.
If this is nil, value of `undercover--report-file-path' is
effectively not modified.")

(defvar eldev-undercover-fileset "*.el")

(defconst eldev--undercover-flags '(auto on off always never coveralls simplecov text merge restart send dontsend safe force))


(defvar undercover-force-coverage)
(defvar undercover--report-file-path)


(defun eldev--undercover-doc ()
  "Plugin that provides integration with `undercover' library,
generating test coverage reports for your project.  Even if the
plugin is active, it will not necessarily generate the report,
see below.

This plugin only activates if project's loading mode is `as-is',
`source' or `built-source', since the library cannot handle
byte-compiled files.  If the plugin decides to collect coverage
statistics in mode `as-is', Emacs will load source files even if
byte-compiled versions are available.

By default, it is up to `undercover' library to decide whether
and which report to generate.  Normally, it does so only on
supported continuous integration services.  However, you can use
option `--undercover' (`-u') of command `test' to easily change
this.  Value of the option must be a comma and/or space-separated
list of any of the following flags:

  - `auto' (default), `on' (or `always'), `off' (or `never'):
    whether to collect coverage statistics and generate a report;

  - `coveralls' (default), `simplecov', `text': format of the
    generated report;

  - `merge' or `restart' (default): whether to merge with
    existing report file or delete it and create new report from
    scratch; simple text reports are never merged;

  - `send' (default), `dontsend': whether to upload the report to
    coveralls.io (only for `coveralls' format);

  - `safe' (default) or `force': whether to run `undercover' even
    if the plugin detects it likely won't work on this Emacs version.

Most flags have their default value provided by `undercover'
library itself.  As of version 0.6.1 those are `coveralls' and
`send'.  However, defaults can also be changed in project's file
`Eldev'.

Additionally, option `--undercover-report' (`-U') lets you change
the report's filename.  Default value is controlled by the
library; as of 0.6.1 it is `/tmp/undercover_coveralls_report'.

When `eldev-dwim' is non-nil (default), certain flags can affect
each other:

  - if report format is not set explicitly it is derived from
    extension of report filename if possible: `.json' for
    `simplecov' format, `.txt' or `.text' for a text report;

  - when requested format is not `coveralls', report is always
    generated unless `auto' or `off' (`never') is specified
    explicitly.

This special handling is aimed at reports created for local use,
i.e. usually in `simplecov' format.  Default values are normally
for coveralls.io and the report only gets generated on supported
continuous integration services."
  nil)

(defun eldev--undercover-config (&optional plugin-configuration)
  (let (file
        mode
        format
        merge
        dontsend
        force)
    (dolist (flag (append plugin-configuration (eldev-listify eldev-undercover-config)))
      (eldev-pcase-exhaustive flag
        ((or `auto `on `off)              (setf mode     flag))
        ((or `always `never)              (setf mode     (if (eq flag 'always) 'on 'off)))
        ((or `coveralls `simplecov `text) (setf format   flag))
        ((or `merge `restart)             (setf merge    (eq flag 'merge)))
        ((or `send `dontsend)             (setf dontsend (eq flag 'dontsend)))
        ((or `safe `force)                (setf force    (eq flag 'force)))
        ;; This is mostly for plugin configuration.
        ((pred stringp)                   (setf file flag))))
    (unless file
      (setf file eldev-undercover-report-file))
    (when eldev-dwim
      (when (and file (null format))
        (setf format (cond ((string-suffix-p ".json" file)                                    'simplecov)
                           ((or (string-suffix-p ".txt" file) (string-suffix-p ".text" file)) 'text))))
      (unless (or mode (memq format '(nil coveralls)))
        (setf mode 'on)))
    ;; Return value is a cons of two lists: a plist for internal use and for use as
    ;; `undercover' library's configuration.
    `((:mode ,(or mode 'auto) :merge ,merge :force ,force)
      . (,@(when file `((:report-file ,file))) (:report-format ',format) (:send-report ,(not dontsend))))))

(defun eldev--undercover-plugin (configuration)
  "This plugin adds a runtime dependency on `undercover' package
and tells it which files to instrument.  By default, those are
all Elisp files in your `main' target set; change value of
variable `eldev-undercover-fileset' if needed.  See detailed
plugin documentation for more information."
  (add-hook 'eldev-test-hook (lambda () (eldev--set-up-undercover configuration)))
  (eldev-add-documentation-preprocessor 'eldev-test (lambda (documentation)
                                                      (concat documentation "\n\n"
                                                              (eldev-colorize "Plugin `undercover'" 'section) "\n\n"
                                                              (documentation 'eldev--undercover-plugin t))))
  (eldev-defoption eldev-set-undercover-config (config)
    "Whether and how to use `undercover'"
    :options        (-u --undercover)
    :value          CONFIG
    :for-command    test
    (let (new-flags)
      (dolist (flag (split-string config "[, \t]" t "[ \t]+"))
        (unless (memq (setf flag (intern flag)) eldev--undercover-flags)
          (signal 'eldev-wrong-option-usage `("unknown flag `%s'" flag)))
        (push flag new-flags))
        (setf eldev-undercover-config (append (eldev-listify eldev-undercover-config) (nreverse new-flags)))))
  (eldev-defoption eldev-set-undercover-report-file (file)
    "Set `undercover's report filename"
    :options        (-U --undercover-report)
    :value          FILE
    :for-command    test
    (setf eldev-undercover-report-file file))
  (eldev-documentation 'eldev--undercover-doc))

(defun eldev--set-up-undercover (configuration)
  ;; Suppose you have files `foo.el' (the main file of your project) and `test/foo.el'
  ;; (your tests).  The way `undercover' is implemented, it will try to instrument both
  ;; files, because it never looks at the path.  Worse yet, it will also instrument
  ;; something like `.eldev/.../foo.el' if that is loaded.  For this reason, we use
  ;; absolute pathnames below.
  (let* ((configuration (eldev--undercover-config configuration))
         (mode          (plist-get (car configuration) :mode)))
    (cond ((eq mode 'off)
           (eldev-trace "Disabled `undercover' coverage report generation"))
          ((not (memq eldev-project-loading-mode '(nil as-is source built-source)))
           ;; It looks like Emacs loads source files when they have load handlers, even if
           ;; there is `.elc' available.  However, let's still disable `undercover' when
           ;; any of the byte-compiling loading modes are requested: plugin is secondary
           ;; and shouldn't change the mode.  On the bright side, we don't need to care if
           ;; files are byte-compiled when using `as-is' mode with `undercover'.
           (if (eq mode 'on)
               (eldev-warn "Cannot collect coverage information from byte-compiled files; plugin `undercover' will not be enabled")
             (eldev-trace "Not activating plugin `undercover' since the project is in a byte-compiled loading mode")))
          (t
           (let ((files (eldev-find-and-trace-files '(:and (eldev-standard-filesets 'main) eldev-undercover-fileset) "file%s for `undercover' to instrument" 'dont-trace))
                 compatibility)
             (when files
               ;; Current stable version is 0.6.1, but we need certain things committed
               ;; after that.  As of yet, no stable version after 0.6.1 exists, and we
               ;; will fetch an unstable version if that's still the case.
               (eldev-add-extra-dependencies 'runtime '(:package undercover :version "0.6.2" :archives melpa))
               (eldev-load-extra-dependencies 'runtime)
               (require 'undercover)
               (setf undercover-force-coverage (not (eq mode 'auto)))
               (eldev-trace (if undercover-force-coverage
                                "Forcing `undercover' to generate coverage report..."
                              "Leaving it up to `undercover' to decide whether to generate coverage report..."))
               ;; We already have a list of files, disable wildcard processing.
               (eldev-advised ('undercover--wildcards-to-files :override #'identity)
                 (eldev-advised ('undercover--coverage-enabled-p :after-while
                                                                 (lambda (&rest _ignored)
                                                                   (unless compatibility
                                                                     (let ((undercover-version (package-desc-version (eldev-find-package-descriptor 'undercover)))
                                                                           (message            "This `undercover' version is probably incompatible with Emacs 27 and later"))
                                                                       (setf compatibility '(t))
                                                                       (when (and (>= emacs-major-version 27)
                                                                                  (or (version-list-<= undercover-version '(0 6 1))
                                                                                      ;; Fucking unstable version numbers.
                                                                                      (and (version-list-<= '(20000000 0) undercover-version)
                                                                                           (version-list-<= undercover-version '(20191122 2126)))))
                                                                         (if (plist-get (car configuration) :force)
                                                                             (eldev-warn message)
                                                                           (setf compatibility '(nil))
                                                                           (if (eq mode 'on)
                                                                               (eldev-warn (eldev-format-message "%s, disabling" message)))
                                                                           (eldev-verbose (eldev-format-message "%s, disabling" message))))))
                                                                   (car compatibility)))
                   (eldev-advised ('undercover--edebug-files :before (lambda (files &rest _ignored)
                                                                       (if (plist-get (car configuration) :merge)
                                                                           (eldev-trace "Code coverage report will be merged with existing")
                                                                         (if (boundp 'undercover--report-file-path)
                                                                             (when (file-exists-p undercover--report-file-path)
                                                                               (delete-file undercover--report-file-path)
                                                                               (eldev-trace "Deleted previous code coverage report; new one will be restarted from scratch"))
                                                                           (eldev-warn "Cannot determine where coverage report is generated; unable to honor `restart' flag")))
                                                                       (eldev-verbose "Instrumenting %s for collecting coverage information with `undercover'"
                                                                                      (eldev-message-plural (length files) "file"))))
                     ;; Because `undercover-report' runs from `kill-emacs-hook', using
                     ;; `eldev-advised' here would not be enough.
                     (advice-add 'undercover-report :around (lambda (original &rest etc)
                                                              (when (boundp 'undercover--report-file-path)
                                                                (eldev-verbose "Saving `undercover' report to file `%s'..." undercover--report-file-path)
                                                                ;; `undercover' will fail if file is in a non-existing directory.
                                                                (let ((dir (file-name-directory undercover--report-file-path)))
                                                                  (when dir
                                                                    (make-directory dir t))))
                                                              (eldev-output-reroute-messages
                                                                (let ((eldev-message-rerouting-wrapper #'eldev-verbose))
                                                                  (apply original etc)))))
                     (let ((original-file-handlers file-name-handler-alist))
                       ;; Since `undercover' is a macro, we have to do it like this.
                       (eval `(undercover ,@files ,@(cdr configuration)) t)
                       ;; `undercover' prepends "/" to filename regexp, but we already use absolute
                       ;; paths.  See also https://github.com/undercover-el/undercover.el/pull/58
                       (let ((scan file-name-handler-alist))
                         (while (and scan (not (eq scan original-file-handlers)))
                           (when (string-prefix-p "/" (caar scan))
                             (setf (caar scan) (concat "^" (substring (caar scan) 1))))
                           (setf scan (cdr scan))))))))))))))


(provide 'eldev-plugins)

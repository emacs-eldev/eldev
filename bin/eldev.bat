@echo off
REM Eldev --- Elisp Development Tool.
REM
REM Copyright (C) 2019-2021 Paul Pogonyshev
REM
REM Author:   Paul Pogonyshev <pogonyshev@gmail.com>
REM Homepage: https://github.com/doublep/eldev

setlocal

if "%ELDEV_EMACS%" == "" (
   if "%EMACS%" == "" (
      set ELDEV_EMACS=emacs
   ) else (
      set ELDEV_EMACS=%EMACS%
   )
)

set ELDEV_CMD=%0

set ELDEV_TTY=
call :CON_ANSI_SUPPORT? %CMDCMDLINE%
if "%CON_ANSI_SUPPORT?%" == "t" (
   set ELDEV_TTY=t
)

set ARGS=%*
setlocal EnableDelayedExpansion
set NL= ^


REM the newline variable above MUST be followed by two empty lines.

"%ELDEV_EMACS%" --batch --no-site-file --no-site-lisp ^
                --execute  "!="! !NL!^
(let ((eldev--emacs-version (format """%%s.%%s""" emacs-major-version emacs-minor-version)) !NL!^
      (eldev--dir           (getenv """ELDEV_DIR""")) !NL!^
      ;; This is intentional.  First, this is in case ELDEV_LOCAL is !NL!^
      ;; defined, second, this is just Eldev default for packages. !NL!^
      (load-prefer-newer    t)) !NL!^
  ;; Setting `debug-on-error' would be useful, but it can break many !NL!^
  ;; `package-*' functions, since those use `with-demoted-errors' and !NL!^
  ;; so `condition-case-unless-debug'. !NL!^
  (unless (and (fboundp 'version^<=) (version^<= """24.1""" eldev--emacs-version)) !NL!^
    (error """Eldev requires Emacs 24.1 or newer""")) !NL!^
  (setf package-user-dir       (expand-file-name """bootstrap""" (expand-file-name eldev--emacs-version (if (= (length eldev--dir) 0) """~/.eldev""" eldev--dir))) !NL!^
        package-directory-list nil !NL!^
        package-archives       nil) !NL!^
  (require 'package) !NL!^
  (package-initialize t) !NL!^
  (let ((package-archives '(("""melpa-stable""" . """http://stable.melpa.org/packages/"""))) !NL!^
        (archive-name      """MELPA Stable""") !NL!^
        (inhibit-message  t) !NL!^
        (eldev-local      (getenv """ELDEV_LOCAL""")) !NL!^
        eldev-pkg !NL!^
        requirements) !NL!^
    (unless (= (length eldev-local) 0) !NL!^
      (if (string-prefix-p """:pa:""" eldev-local) !NL!^
          (setf package-archives `(("""bootstrap-pa""" . ,(file-name-as-directory (substring eldev-local (length """:pa:"""))))) !NL!^
                archive-name     """a local package archive""") !NL!^
        (with-temp-buffer !NL!^
          (insert-file-contents-literally (expand-file-name """eldev.el""" eldev-local)) !NL!^
          (setf eldev-pkg                    (package-buffer-info) !NL!^
                (package-desc-dir eldev-pkg) (expand-file-name eldev-local)) !NL!^
          ;; Currently Eldev has no external dependencies, but let's be generic. !NL!^
          (dolist (requirement (package-desc-reqs eldev-pkg)) !NL!^
            (unless (package-activate (car requirement)) !NL!^
              (push requirement requirements)))))) !NL!^
    (when (if eldev-pkg !NL!^
              requirements !NL!^
            (not (package-activate 'eldev))) !NL!^
      (let ((inhibit-message nil)) !NL!^
        (message """Bootstrapping Eldev for Emacs %%s from %%s...\n""" eldev--emacs-version archive-name) !NL!^
        (when eldev-pkg !NL!^
          (message """Eldev package itself will be used from `%%s'\n""" eldev-local))) !NL!^
      (package-refresh-contents) !NL!^
      (if eldev-pkg !NL!^
          (package-download-transaction (package-compute-transaction nil requirements)) !NL!^
        (package-install 'eldev))) !NL!^
    (when eldev-pkg !NL!^
      (push `(eldev . (,eldev-pkg)) package-alist) !NL!^
      ;; `package--autoloads-file-name' is package-private. !NL!^
      (let* ((autoloads-file     (expand-file-name (format """%%s-autoloads""" (package-desc-name eldev-pkg)) !NL!^
                                                   (package-desc-dir eldev-pkg))) !NL!^
             (autoloads-disabler (lambda (do-load file ^&rest args) (unless (equal file autoloads-file) (apply do-load file args))))) !NL!^
        (advice-add #'load :around autoloads-disabler) !NL!^
        (package-activate-1 eldev-pkg) !NL!^
        (advice-remove #'load autoloads-disabler)))) !NL!^
  (require 'eldev) !NL!^
  (eldev-start-up)) !NL!^
"  ^
                --execute "(kill-emacs (eldev-cli (append (cdr (member """--""" command-line-args)) nil)))" ^
                -- !ARGS!
REM forward emacs exit status
exit /b %errorlevel%

:CON_ANSI_SUPPORT?
REM Determine whether the script is running directly on a console
REM which supports ANSI escape sequences, and set the
REM `CON_ANSI_SUPPORT?'  var to `t' when so. The following two
REM conditions have to be met:
REM
REM 1. The `CMDCMDLINE' variable, i.e. the original command line that
REM invoked the Command Processor and passed in as the first argument
REM to this call, must have been called without arguments, i.e. it is
REM just a path to "cmd.exe" (thus it should be directly attached to
REM the console).
REM
REM 2. The current version of MS-Windows is equal to 10.0.18363,
REM i.e. the first version that had support for ANSI escape sequences
REM enabled by default, or greater.
for /f "tokens=4,5,6 delims=[]. " %%G in ('ver') do (set _mj=%%G&set _mn=%%H&set _bl=%%I)
set CON_ANSI_SUPPORT?=
if "%~2" == "" (
   if %_mj% EQU 10 (if %_mn% EQU 0 (if %_bl% GEQ 18363 (set CON_ANSI_SUPPORT?=t)
                    ) else (if %_mn% GTR 0 (set CON_ANSI_SUPPORT?=t))
   ) else (if %_mj% GTR 10 (set CON_ANSI_SUPPORT?=t))
)
exit /b

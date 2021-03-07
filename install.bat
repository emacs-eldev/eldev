@echo off
setlocal

set SELF_DIR="%~dp0"
cd %SELF_DIR%

if not "%*"=="" (set ARGS="%*") else (set ARGS=)

set ELDEV_LOCAL=.
bin/eldev.bat --setup "(defvar eldev--install-sh t)" install-self %ARGS%

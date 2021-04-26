@echo off
rem This script downloads Eldev startup script as `%USERPROFILE%/.eldev/bin/eldev'.

rem optionally pass download URL as paramater to allow testing in PRs
set URL=https://raw.githubusercontent.com/doublep/eldev/master/bin/eldev.bat
IF NOT "%~1" == "" set URL=%1

set ELDEV_BIN_DIR=%USERPROFILE%\.eldev\bin

mkdir %ELDEV_BIN_DIR%

curl.exe  -fsSL %URL% -o %ELDEV_BIN_DIR%\eldev.bat

echo Eldev startup script has been installed.
echo Don't forget to add `%ELDEV_BIN_DIR% to PATH environment variable:
echo.
echo     set PATH=%ELDEV_BIN_DIR%;%%PATH%%


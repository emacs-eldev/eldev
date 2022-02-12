@echo off
rem This script downloads Eldev startup script as `%USERPROFILE%\.local\bin\eldev.bat'.
rem
rem To bootstrap, run as follows from a command prompt:
rem
rem curl.exe -fsSL https://raw.github.com/doublep/eldev/master/webinstall/eldev.bat | cmd /Q

rem The usual way to check for the presence of an argument is to test
rem if their argument reference %[1-9] has a value. Though when piping
rem to cmd, these references do not exist and instead is better to use
rem a counter.
set ARGS=0
for %%x in (%*) do set /A ARGS+=1

rem optionally pass download URL as parameter to allow testing in PRs
set URL=https://raw.githubusercontent.com/doublep/eldev/master/bin/eldev.bat
if %ARGS%==1 set URL=%1

set ELDEV_BIN_DIR=%USERPROFILE%\.local\bin

mkdir %ELDEV_BIN_DIR%

curl.exe  -fsSL %URL% -o %ELDEV_BIN_DIR%\eldev.bat || exit /b

echo Eldev startup script has been installed.
echo Don't forget to add `%ELDEV_BIN_DIR% to PATH environment variable:
echo.
echo     set PATH=%ELDEV_BIN_DIR%;%%PATH%%


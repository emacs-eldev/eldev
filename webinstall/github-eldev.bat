rem This script downloads Eldev startup script as `USERPROFILE/.eldev/bin/eldev'
rem for use in a GitHub workflow.
rem
rem curl.exe is included in Windows 10 since April-2018-Update (1803)
rem In your `.github/workflows/*.yml' add this:
rem
rem   run: curl.exe  -fsSL https://raw.github.com/doublep/eldev/master/webinstall/github-eldev.bat | cmd
rem

rem optionally pass download URL as paramater to allow testing in eldev PRs
IF [%1] == [] (
   set URL=https://raw.githubusercontent.com/doublep/eldev/master/bin/eldev.bat
) else (
   set URL=%1
)

set ELDEV_BIN_DIR=%USERPROFILE%\.eldev\bin

mkdir %ELDEV_BIN_DIR%

curl.exe  -fsSL %URL% -o %ELDEV_BIN_DIR%\eldev.bat

echo %ELDEV_BIN_DIR% >> %GITHUB_PATH%


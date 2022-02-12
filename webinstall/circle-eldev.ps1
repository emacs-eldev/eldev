# This script downloads Eldev startup script as `~\.local\bin\eldev.bat'
# for CircleCI, and permanently adds that directory at the front of PATH
# in PS's startup profile, so as to be available in every session.
#
# In your `.circleci/config.yml' add this:
#
# - (iwr https://raw.github.com/doublep/eldev/master/webinstall/circle-eldev.ps1).Content | powershell -command -

$ErrorActionPreference = "Stop"


$env:ELDEV_BIN_DIR="$HOME\.local\bin"

if (!(Test-Path $env:ELDEV_BIN_DIR)) {new-item "$env:ELDEV_BIN_DIR" -ItemType directory}
iwr -Uri https://raw.githubusercontent.com/doublep/eldev/master/bin/eldev.bat `
  -outfile $env:ELDEV_BIN_DIR/eldev.bat

add-content $PROFILE $("`$env:PATH=""$env:ELDEV_BIN_DIR;`$env:PATH""")

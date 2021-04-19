# Wrapper script to work arround windows path issues (mixing
# forward/back-slashes) when running in (Github Actions) powershell
# call-process("d:/a/eldev/eldev/\"D:/a/eldev/eldev/bin/eldev.bat\"
$bat =  (get-item $PSCommandPath) -replace ".ps1$", ".bat"
& cmd /c $bat $args


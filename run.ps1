# common functions
. "$PSScriptRoot\funcs.ps1"

# process args
main @args

if ($showhelp -eq $FALSE) {
    boot_gui
}

# common functions
. "$PSScriptRoot\funcs.ps1"

# process args defaults
main 64 64 @args

if ($showhelp -eq $TRUE) {
    Write-Output "[-n cnt] number of nodes"
    Write-Output "[-b base] base offset"
    Write-Output "[-s script_name] script mode"
    Write-Output "[-e] emulator mode"
    Write-Output "[-f] foreground mode"
    Write-Output "[-h] help"
} else {
    $links_to_0 = ""
    for ($cpu = $ncpu - 1; $cpu -gt 0; $cpu--) {
        $links = add_link $cpu 0 ""
        $links_to_0 += $links
        boot_cpu_gui $front $cpu $links
    }
    boot_cpu_gui $front $cpu $links_to_0
}

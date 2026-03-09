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
    for ($cpu = $ncpu - 1; $cpu -ge 0; $cpu--) {
        $links = ""

        $lcpu = [Math]::Truncate(($cpu - 1) / 2)
        $links += add_link $cpu $lcpu $links

        $lcpu = ($cpu * 2) + 1
        if ($lcpu -lt $ncpu) {
            $links += add_link $cpu $lcpu $links
        }

        $lcpu = ($cpu * 2) + 2
        if ($lcpu -lt $ncpu) {
            $links += add_link $cpu $lcpu $links
        }

        boot_cpu_gui $front $cpu $links
    }
}

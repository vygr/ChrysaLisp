# common functions
. "$PSScriptRoot\funcs.ps1"

# process args defaults
main 8 8 @args

if ($showhelp -eq $TRUE) {
    Write-Output "[-n cnt] number of nodes"
    Write-Output "[-b base] base offset"
    Write-Output "[-s script_name] script mode"
    Write-Output "[-e] emulator mode"
    Write-Output "[-f] foreground mode"
    Write-Output "[-h] help"
} else {
    for ($cpu_y = $ncpu - 1; $cpu_y -ge 0; $cpu_y--) {
        for ($cpu_x = $ncpu - 1; $cpu_x -ge 0; $cpu_x--) {
            $cpu = $cpu_y * $ncpu + $cpu_x
            $links = ""
            for ($lcpu_y = $cpu_y - 1; $lcpu_y -le $cpu_y + 1; $lcpu_y++) {
                $wp = wrap $lcpu_y $ncpu
                $links += add_link $cpu ($wp * $ncpu + $cpu_x) $links
            }
            for ($lcpu_x = $cpu_x - 1; $lcpu_x -le $cpu_x + 1; $lcpu_x++) {
                $wp = wrap $lcpu_x $ncpu
                $links += add_link $cpu ($cpu_y * $ncpu + $wp) $links
            }
            boot_cpu_gui $front $cpu $links
        }
    }
}

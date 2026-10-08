# useful functions

# capture the repo root at dot-source time - $PSScriptRoot is unreliable inside functions
$NHROOT = $PSScriptRoot

# what this machine is, for where its host programs are, obj\<cpu>\<abi>\<os>
$NHOS = "Windows"
$NHCPU = "x86_64"
$NHABI = "WIN64"

$HOS = $NHOS
$HCPU = $NHCPU
$HABI = $NHABI

# a session is the nodes one launch script started, and those nodes
# started in turn. Each has link names of its own, so two sessions on
# one machine do not meet, and a session stops only itself. It lives
# while it has a front, a terminal or a desktop, a way in to it. A
# desktop can be closed and another opened, nodes -g, and when the last
# front has gone the rest of the nodes are stopped.

$global:session_pids = @()
$global:session_start = (Get-Date).AddSeconds(-2)

function session_scan {
    # every running node of this session, those the launch script started
    # and those they started in turn, (node-spawn), found by their parents.
    # A process id can be used again, so a node counts only if it was
    # started after the session was.
    $nodes = @(Get-CimInstance Win32_Process -Filter "Name='main_gui.exe' OR Name='main_tui.exe'" |
        Where-Object { $_.CreationDate -ge $global:session_start })
    $ids = @{}
    foreach ($id in $global:session_pids) { $ids[[int]$id] = $TRUE }
    do {
        $more = $FALSE
        foreach ($node in $nodes) {
            if ($ids.ContainsKey([int]$node.ParentProcessId) -and -not $ids.ContainsKey([int]$node.ProcessId)) {
                $ids[[int]$node.ProcessId] = $TRUE
                $more = $TRUE
            }
        }
    } while ($more)
    @($nodes | Where-Object { $ids.ContainsKey([int]$_.ProcessId) })
}

function session_has_front {
    # a front is a way in to the session, a terminal or a desktop, a node
    # that was given a script to run. A session lives while it has one.
    foreach ($node in (session_scan)) {
        if ($node.CommandLine -match ' -run ') { return $TRUE }
    }
    return $FALSE
}

function session_stop {
    # stop every node of this session
    foreach ($node in (session_scan)) {
        Stop-Process -Id $node.ProcessId -Force -ErrorAction SilentlyContinue
    }
}

function session_watch {
    # stop the session when its last front has gone, see session_watch.ps1
    $shell = (Get-Process -Id $PID).Path
    $ids = $global:session_pids -join ","
    $null = Start-Process -FilePath $shell -WorkingDirectory $NHROOT -WindowStyle Hidden -ArgumentList "-NoProfile -ExecutionPolicy Bypass -File `"$NHROOT\session_watch.ps1`" $($global:session_start.Ticks) $ids" -PassThru
}

# the first node, node 0, is a front.
# If the launch is in the foreground it is waited for. A watch is then
# left to stop the session when its last front has gone, which is at
# once if this was the only one.
function boot_first {
    param ($wait, $cmd, $argstring)
    $process = Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList $argstring -PassThru
    # the exit code is lost unless the handle is held
    $null = $process.Handle
    $global:session_pids += $process.Id
    if ($wait -eq $TRUE) {
        # not -Wait, that waits for every node this one starts as well
        $process.WaitForExit()
        if (session_has_front) { session_watch } else { session_stop }
        if ($process.ExitCode -eq 0) { Clear-Host }
    } else {
        session_watch
    }
}

# the launch script starts one node, the first. That node starts the
# rest, in the shape asked for, -t, and as many as asked for, -n, or sized
# to the machine, (node-net) in sys/lisp.inc has the shapes, and then runs
# the script it was given. The other desktops asked for, -g, $guis of
# them, are for it to start as well, they are the first of the nodes it
# starts.
function first_run {
    param ($run, $guis = 0)
    if ($guis -gt 0) { return "`"(progn (node-net :$global:shape $global:ncpu $guis :gui {service/gui/app.lisp}) (import {$run}))`"" }
    return "`"(progn (node-net :$global:shape $global:ncpu) (import {$run}))`""
}

function boot_gui {
    $cmd = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_gui.exe"
    $boot = if ($global:emu -eq '-e') { "obj/vp64/VP64/sys/boot_image" } else { "obj/$HCPU/$HABI/sys/boot_image" }
    $argstring = $boot
    if ($global:emu -ne '') { $argstring += " $global:emu" }
    if ($global:ngui -eq 0) {
        # no desktop, a script on the GUI host program
        boot_first $global:front $cmd "$argstring -run $(first_run $global:script)"
    } elseif ($global:front -eq $FALSE) {
        boot_first $FALSE $cmd "$argstring -run $(first_run 'service/gui/app.lisp' ($global:ngui - 1))"
    } else {
        boot_first $TRUE $cmd "$argstring -run $(first_run 'apps/tui/tui_gui.lisp' ($global:ngui - 1))"
    }
}

function boot_tui {
    $cmd = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_tui.exe"
    $boot = if ($global:emu -eq '-e') { "obj/vp64/VP64/sys/boot_image" } else { "obj/$HCPU/$HABI/sys/boot_image" }
    $argstring = $boot
    if ($global:emu -ne '') { $argstring += " $global:emu" }
    # a TUI is always waited for, it has the terminal
    boot_first $TRUE $cmd "$argstring -run $(first_run $global:script)"
}

function main {
    # no param() block - keeps ALL args in $args with no named-parameter binding
    # 0 nodes is the network sized to the machine
    $global:ncpu = 0
    $global:ngui = 1
    $global:shape = "full"
    $global:emu = ""
    $global:front = $FALSE
    $global:script = "apps/tui/tui.lisp"
    $global:showhelp = $FALSE

    for ($i = 0; $i -lt $args.Count; $i++) {
        $arg = $args[$i]
        switch ($arg) {
            "-i" { $global:script = "apps/tui/install.lisp" }
            "-s" { $global:script = $args[++$i] }
            "-e" { $global:emu = "-e" }
            "-f" { $global:front = $TRUE }
            "-g" { $global:ngui = [int]$args[++$i] }
            "-n" { $global:ncpu = [int]$args[++$i] }
            "-t" { $global:shape = $args[++$i] }
            "-h" { $global:showhelp = $TRUE }
            "--help" { $global:showhelp = $TRUE }
            default { $global:showhelp = $TRUE }
        }
    }

    if ($global:showhelp -eq $TRUE) {
        Write-Output "[-n cnt] number of nodes, the width of a mesh or a cube, 0 to size to the machine, the default"
        Write-Output "[-t shape] full, ring, star, tree, mesh or cube, full is the default"
        Write-Output "[-g cnt] number of guis"
        Write-Output "[-s script_name] script mode"
        Write-Output "[-e] emulator mode"
        Write-Output "[-f] foreground mode"
        Write-Output "[-h] help"
    }
}

# useful functions

# capture the repo root at dot-source time - $PSScriptRoot is unreliable inside functions
$NHROOT = $PSScriptRoot

if (Test-Path os) { $NHOS = (Get-Content os -Raw).Trim() } else { $NHOS = "Windows" }
if (Test-Path cpu) { $NHCPU = (Get-Content cpu -Raw).Trim() } else { $NHCPU = "x86_64" }
if (Test-Path abi) { $NHABI = (Get-Content abi -Raw).Trim() } else { $NHABI = "WIN64" }

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
$global:session_salt = [long](Get-Random -Maximum 2147483647)

function link_name {
    # the name of the link between two nodes, ???-??? in base 36
    param ($src, $dst)
    $n = ($global:session_salt + $src * 128 + $dst) % 2176782336
    $digits = "0123456789abcdefghijklmnopqrstuvwxyz"
    $name = ""
    for ($i = 0; $i -lt 6; $i++) {
        $name = $digits.Substring([int]($n % 36), 1) + $name
        $n = [long][Math]::Floor($n / 36)
    }
    $name.Substring(0, 3) + "-" + $name.Substring(3, 3)
}

function add_link {
    param ($src, $dst, $links)
    $src = [long]$src
    $dst = [long]$dst
    if ($src -ne $dst) {
        if ($src -lt $dst) { $nl = link_name $src $dst } else { $nl = link_name $dst $src }
        if ($links.IndexOf($nl) -eq -1) { return "-l $nl " }
    }
    return ""
}

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

function boot_node {
    # a node that is not waited for
    param ($cmd, $argstring)
    $process = Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList $argstring -PassThru
    $global:session_pids += $process.Id
}

# the first node, node 0, is a front, and is the last to be booted.
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

function wrap {
    param ($cpu, $num_cpu)
    $wp = $cpu % $num_cpu
    if ($wp -lt 0) { $wp += $num_cpu }
    $wp
}

# with -n 0 the first node sizes the network to the machine. It starts
# the other nodes, and then runs the script it was given.
function auto_run {
    param ($run)
    if ($global:auto -eq $TRUE) { return "`"(progn (node-auto) (import {$run}))`"" }
    return $run
}

function boot_cpu_gui {
    param ($front, $cpu, $link)
    $cmd = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_gui.exe"
    $boot = if ($global:emu -eq '-e') { "obj/vp64/VP64/sys/boot_image" } else { "obj/$HCPU/$HABI/sys/boot_image" }
    $argstring = "$boot " + $link.Trim()
    if ($global:emu -ne '') { $argstring += " $global:emu" }
    if ($global:ngui -eq 0) {
        if ($cpu -lt 1) {
            boot_first $front $cmd "$argstring -run $(auto_run $global:script)"
        } else {
            boot_node $cmd $argstring
        }
    } elseif ($cpu -lt $global:ngui) {
        if ($cpu -ge 1) {
            boot_node $cmd "$argstring -run $(auto_run 'service/gui/app.lisp')"
        } elseif ($front -eq $FALSE) {
            boot_first $FALSE $cmd "$argstring -run $(auto_run 'service/gui/app.lisp')"
        } else {
            boot_first $TRUE $cmd "$argstring -run $(auto_run 'apps/tui/tui_gui.lisp')"
        }
    } else {
        boot_node $cmd $argstring
    }
}

function boot_cpu_tui {
    param ($front, $cpu, $link)
    $cmd = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_tui.exe"
    $boot = if ($global:emu -eq '-e') { "obj/vp64/VP64/sys/boot_image" } else { "obj/$HCPU/$HABI/sys/boot_image" }
    $argstring = "$boot " + $link.Trim()
    if ($global:emu -ne '') { $argstring += " $global:emu" }
    if ($cpu -lt 1) {
        # a TUI is always waited for, it has the terminal
        boot_first $TRUE $cmd "$argstring -run $(auto_run $global:script)"
    } else {
        boot_node $cmd $argstring
    }
}

function main {
    # no param() block - keeps ALL args in $args with no named-parameter binding
    # $args[0] = default node count, $args[1] = max node count, rest = flags
    $global:ncpu = [int]$args[0]
    $maxn = [int]$args[1]
    $global:ngui = 1
    $global:emu = ""
    $global:front = $FALSE
    $global:auto = $FALSE
    # a default of 0 nodes is the network sized to the machine, as -n 0 is
    if ($global:ncpu -eq 0) { $global:auto = $TRUE; $global:ncpu = 1 }
    $global:script = "apps/tui/tui.lisp"
    $global:showhelp = $FALSE

    for ($i = 2; $i -lt $args.Count; $i++) {
        $arg = $args[$i]
        switch ($arg) {
            "-i" { $global:script = "apps/tui/install.lisp" }
            "-s" { $global:script = $args[++$i] }
            "-e" { $global:emu = "-e" }
            "-f" { $global:front = $TRUE }
            "-g" { $global:ngui = [int]$args[++$i] }
            "-n" {
                $global:ncpu = [int]$args[++$i]
                $global:auto = $FALSE
                if ($global:ncpu -eq 0) { $global:auto = $TRUE; $global:ncpu = 1 }
            }
            "-h" { $global:showhelp = $TRUE }
            "--help" { $global:showhelp = $TRUE }
            default { $global:showhelp = $TRUE }
        }
    }

    if ($global:ncpu -gt $maxn) { $global:ncpu = $maxn }
}

# useful functions

# capture the repo root at dot-source time - $PSScriptRoot is unreliable inside functions
$NHROOT = $PSScriptRoot

if (Test-Path os) { $NHOS = (Get-Content os -Raw).Trim() } else { $NHOS = "Windows" }
if (Test-Path cpu) { $NHCPU = (Get-Content cpu -Raw).Trim() } else { $NHCPU = "x86_64" }
if (Test-Path abi) { $NHABI = (Get-Content abi -Raw).Trim() } else { $NHABI = "WIN64" }

$HOS = $NHOS
$HCPU = $NHCPU
$HABI = $NHABI
function zero_pad {
    param ($c)
    if ($c -lt 100) { $zp = "0$c" } else { $zp = $c }
    if ($zp -lt 10) { $zp = "0$zp" }
    $zp
}

function add_link {
    param ($src, $dst, $links)
    $src_pad = zero_pad ($src + $bcpu)
    $dst_pad = zero_pad ($dst + $bcpu)
    if ($src_pad -ne $dst_pad) {
        if ($src_pad -lt $dst_pad) { $nl = "$src_pad-$dst_pad" } else { $nl = "$dst_pad-$src_pad" }
        if ($links.IndexOf($nl) -eq -1) { return "-l $nl " }
    }
    return ""
}

function wrap {
    param ($cpu, $num_cpu)
    $wp = $cpu % $num_cpu
    if ($wp -lt 0) { $wp += $num_cpu }
    $wp
}

function boot_cpu_gui {
    param ($front, $cpu, $link)
    $cmd = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_gui.exe"
    $boot = if ($global:emu -eq '-e') { "obj/vp64/VP64/sys/boot_image" } else { "obj/$HCPU/$HABI/sys/boot_image" }
    $argstring = "$boot " + $link.Trim()
    if ($global:emu -ne '') { $argstring += " $global:emu" }
    if ($cpu -lt 1) {
        if ($front -eq $FALSE) {
            Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList "$argstring -run service/gui/app.lisp"
        } else {
            $process = Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList "$argstring -run service/gui/app.lisp" -PassThru -Wait
            if ($process.ExitCode -eq 0) {
                . "$NHROOT\stop.ps1"
                Clear-Host
            }
        }
    } else {
        Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList $argstring
    }
}

function boot_cpu_tui {
    param ($front, $cpu, $link)
    $cmd = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_tui.exe"
    $boot = if ($global:emu -eq '-e') { "obj/vp64/VP64/sys/boot_image" } else { "obj/$HCPU/$HABI/sys/boot_image" }
    $argstring = "$boot " + $link.Trim()
    if ($global:emu -ne '') { $argstring += " $global:emu" }
    if ($cpu -lt 1) {
        if ($front -eq $FALSE) {
            Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList "$argstring -run $global:script" -Wait
        } else {
            $process = Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList "$argstring -run $global:script" -PassThru -Wait
            if ($process.ExitCode -eq 0) {
                . "$NHROOT\stop.ps1"
                Clear-Host
            }
        }
    } else {
        Start-Process -FilePath $cmd -WorkingDirectory $NHROOT -NoNewWindow -ArgumentList $argstring
    }
}

function main {
    # no param() block - keeps ALL args in $args with no named-parameter binding
    # $args[0] = default node count, $args[1] = max node count, rest = flags
    $global:ncpu = [int]$args[0]
    $maxn = [int]$args[1]
    $global:bcpu = 0
    $global:emu = ""
    $global:front = $FALSE
    $global:script = "apps/tui/tui.lisp"
    $global:showhelp = $FALSE

    for ($i = 2; $i -lt $args.Count; $i++) {
        $arg = $args[$i]
        switch ($arg) {
            "-i" { $global:script = "apps/tui/install.lisp" }
            "-s" { $global:script = $args[++$i] }
            "-e" { $global:emu = "-e" }
            "-f" { $global:front = $TRUE }
            "-n" { $global:ncpu = [int]$args[++$i] }
            "-b" { $global:bcpu = [int]$args[++$i] }
            "-h" { $global:showhelp = $TRUE }
            "--help" { $global:showhelp = $TRUE }
            default { $global:showhelp = $TRUE }
        }
    }

    if ($global:ncpu -gt $maxn) { $global:ncpu = $maxn }

    if ($global:bcpu -eq 0 -and $global:showhelp -eq $FALSE) {
        . "$NHROOT\stop.ps1"
    }
}

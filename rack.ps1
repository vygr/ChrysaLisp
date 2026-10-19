# rack.ps1 up [-f]|down|status|run command line
# this machine's member of the mesh on Windows, as rack.sh is on the others,
# a node that stays up, finds the other machines, links to them and takes a
# sync, lib/rack/member.lisp.
#   up      start it, if it is not up
#   up -f   start it to follow the tree's git branch, a clone kept for it
#   down    stop it, and the nodes it started, and no others
#   status  is it up
#   run     have it run a command line, and say what came of it
#
# Followed, up -f, the tree updates itself from where it was cloned, as
# rack.sh's does, see docs/ai_digest/rack.md. Windows builds no host
# programs: they come in snapshot.zip, made with every change to src/host,
# so an update is git's commit, the snapshot of it unpacked, and the
# system built by the emulator, as install.bat does. Each 10 minutes the
# loop fetches the branch. If that has moved on, the tree is clean and the
# new commit follows from the one it is at, the member is stopped, the tree
# fast forwarded and installed. If the install fails the tree is put back
# to the commit it was at and installed again, and that commit is not
# tried again, a newer one is. A desktop or other nodes up on the machine
# hold the host programs, Windows will not let them be written, so the
# update waits till they have gone. What it did is in \tmp\chrysalisp_rack.log
# on the drive of the tree.

. "$PSScriptRoot\funcs.ps1"

# where the member keeps its files, /tmp of its drive as it sees it
$T = (Split-Path -Qualifier $NHROOT) + "\tmp"
New-Item -ItemType Directory -Force -Path $T | out-null
$LOG = "$T\chrysalisp_rack.log"
$EXE = "$NHROOT\obj\$NHCPU\$NHABI\$NHOS\main_tui.exe"
$IMAGE = "obj/$NHCPU/$NHABI/sys/boot_image"

function say ($text) {
    Add-Content -Path $LOG -Value "$text, $(Get-Date)"
}

function read_id ($file) {
    try { [int](Get-Content $file -ErrorAction Stop | Select-Object -First 1) } catch { 0 }
}

function alive ($id) {
    ($id -gt 0) -and ($null -ne (Get-Process -Id $id -ErrorAction SilentlyContinue))
}

function tree_of ($root) {
    # a node and every node it started, by their parents
    $nodes = @(Get-CimInstance Win32_Process -Filter "Name='main_gui.exe' OR Name='main_tui.exe'")
    $ids = @{}
    $ids[[int]$root] = $TRUE
    do {
        $more = $FALSE
        foreach ($node in $nodes) {
            if ($ids.ContainsKey([int]$node.ParentProcessId) -and -not $ids.ContainsKey([int]$node.ProcessId)) {
                $ids[[int]$node.ProcessId] = $TRUE
                $more = $TRUE
            }
        }
    } while ($more)
    $ids
}

function tree_stop ($root) {
    foreach ($id in (tree_of $root).Keys) {
        Stop-Process -Id $id -Force -ErrorAction SilentlyContinue
    }
}

function others_up ($root) {
    # nodes up on the machine that are not the member's, a desktop say
    $ours = tree_of $root
    @(Get-CimInstance Win32_Process -Filter "Name='main_gui.exe' OR Name='main_tui.exe'" |
        Where-Object { -not $ours.ContainsKey([int]$_.ProcessId) }).Count -gt 0
}

function follow_new {
    # the commit the tree's branch is at, if the tree is to be taken to it:
    # newer, it follows from the tree's, the tree is clean, and it was not
    # tried and failed. "" if not
    git -C $NHROOT fetch -q 2>$null
    if ($LASTEXITCODE -ne 0) { return "" }
    $up = git -C $NHROOT rev-parse -q --verify '@{u}' 2>$null
    $at = git -C $NHROOT rev-parse -q --verify HEAD 2>$null
    if (-not $up -or -not $at -or ($up -eq $at)) { return "" }
    if ($up -eq (Get-Content "$T\chrysalisp_rack_skip" -ErrorAction SilentlyContinue)) { return "" }
    git -C $NHROOT merge-base --is-ancestor $at $up 2>$null
    if ($LASTEXITCODE -ne 0) { return "" }
    if (git -C $NHROOT status --porcelain 2>$null) {
        say "follow: $up not taken, the tree has changes of its own"
        return ""
    }
    $up
}

function install {
    # as install.bat: the snapshot's host programs and release image, and
    # the system built by the emulator on a network sized to the machine.
    # $TRUE if it was, the installer leaves obj\install_failed if not
    Remove-Item "$NHROOT\obj\install_failed" -ErrorAction SilentlyContinue
    tar -xf "$NHROOT\snapshot.zip" -C $NHROOT
    if ($LASTEXITCODE -ne 0) { say "update: snapshot.zip could not be unpacked"; return $FALSE }
    $global:session_start = (Get-Date).AddSeconds(-2)
    $process = Start-Process -FilePath $EXE -WorkingDirectory $NHROOT -WindowStyle Hidden -PassThru `
        -RedirectStandardOutput "$T\chrysalisp_rack_install.log" -RedirectStandardError "$T\chrysalisp_rack_install.err" `
        -ArgumentList "obj/vp64/VP64/sys/boot_image -e -run `"(progn (node-net :full 0) (import {apps/tui/install.lisp}))`""
    $null = $process.Handle
    $global:session_pids = @($process.Id)
    $process.WaitForExit()
    session_stop
    (Test-Path $EXE) -and -not (Test-Path "$NHROOT\obj\install_failed")
}

function member_wait ($member) {
    # wait for the member to end, and followed, look each 10 minutes for a
    # newer commit, and if there is one stop it and say so. The first look
    # is soon after it starts, so a machine that was off catches up
    $first = if ($env:CHRYSALISP_FOLLOW_FIRST) { [int]$env:CHRYSALISP_FOLLOW_FIRST } else { 60 }
    $every = if ($env:CHRYSALISP_FOLLOW_EVERY) { [int]$env:CHRYSALISP_FOLLOW_EVERY } else { 600 }
    $next = (Get-Date).AddSeconds($first)
    while (-not $member.HasExited) {
        if ((Test-Path "$T\chrysalisp_rack_follow") -and ((Get-Date) -ge $next)) {
            $next = (Get-Date).AddSeconds($every)
            $new = follow_new
            if ($new) {
                if (others_up $member.Id) {
                    say "follow: $new waits, other nodes are up and hold the host programs"
                } else {
                    Set-Content -Path "$T\chrysalisp_rack_update" -Value "follow $new"
                    tree_stop $member.Id
                    break
                }
            }
        }
        Start-Sleep -Seconds 5
    }
}

function member_loop {
    # the member, started again each time it asks to be after an install
    while ($TRUE) {
        Remove-Item "$T\chrysalisp_rack_update" -ErrorAction SilentlyContinue
        if (-not (Test-Path $EXE)) { break }
        $member = Start-Process -FilePath $EXE -WorkingDirectory $NHROOT -WindowStyle Hidden -PassThru `
            -RedirectStandardOutput "$T\chrysalisp_rack_member.log" -RedirectStandardError "$T\chrysalisp_rack_member.err" `
            -ArgumentList "$IMAGE -run lib/rack/member.lisp"
        $null = $member.Handle
        Set-Content -Path "$T\chrysalisp_rack.pid" -Value $member.Id
        member_wait $member
        if (-not (Test-Path "$T\chrysalisp_rack_update")) { break }
        tree_stop $member.Id
        # followed, the tree is taken to the new commit first
        $was = ""
        $new = ""
        $update = Get-Content "$T\chrysalisp_rack_update" | Select-Object -First 1
        if ($update -like "follow *") {
            $new = $update.Split(" ")[1]
            $was = git -C $NHROOT rev-parse HEAD
            git -C $NHROOT merge -q --ff-only $new 2>&1 | Add-Content -Path $LOG
            if ($LASTEXITCODE -ne 0) {
                say "follow: $new could not be taken"
                Set-Content -Path "$T\chrysalisp_rack_skip" -Value $new
                continue
            }
            say "follow: $($was.Substring(0, 9)) to $($new.Substring(0, 9))"
        }
        # what runs now is kept, to be put back if the install fails
        $os_dir = Split-Path $EXE
        $sys_dir = "$NHROOT\obj\$NHCPU\$NHABI\sys"
        $keep = "$T\chrysalisp_rack_keep"
        Remove-Item $keep -Recurse -Force -ErrorAction SilentlyContinue
        New-Item -ItemType Directory -Force -Path $keep | out-null
        Copy-Item "$os_dir\*" $keep -Recurse -Force
        Copy-Item "$sys_dir\boot_image" "$keep\boot_image" -Force
        say "update: install"
        if (-not (install)) {
            $again = $FALSE
            if ($was) {
                # followed, back to the commit it was at, installed again, and
                # the one that failed is not tried again
                say "follow: $($new.Substring(0, 9)) failed to install, back to $($was.Substring(0, 9))"
                Set-Content -Path "$T\chrysalisp_rack_skip" -Value $new
                git -C $NHROOT reset -q --hard $was 2>&1 | Add-Content -Path $LOG
                $again = install
            }
            if (-not $again) {
                say "update: install failed, what was there is put back"
                Copy-Item "$keep\boot_image" "$sys_dir\boot_image" -Force
                Remove-Item "$keep\boot_image" -Force
                Copy-Item "$keep\*" $os_dir -Recurse -Force
            }
        }
        Remove-Item $keep -Recurse -Force -ErrorAction SilentlyContinue
        if ($new) {
            # followed, git may have changed this file: a new loop goes on as
            # the new one, and this one ends
            $loop = Start-Process -FilePath (Get-Process -Id $PID).Path -WindowStyle Hidden -PassThru `
                -ArgumentList "-NoProfile -ExecutionPolicy Bypass -File `"$NHROOT\rack.ps1`" loop"
            Set-Content -Path "$T\chrysalisp_rack_loop.pid" -Value $loop.Id
            return
        }
    }
    Remove-Item "$T\chrysalisp_rack.pid", "$T\chrysalisp_rack_loop.pid" -ErrorAction SilentlyContinue
}

$what = $args[0]
$member_id = read_id "$T\chrysalisp_rack.pid"
$loop_id = read_id "$T\chrysalisp_rack_loop.pid"
$up = (alive $member_id) -or (alive $loop_id)
switch ($what) {
    "up" {
        $follow = $args[1] -eq "-f"
        if ($follow -and -not (git -C $NHROOT rev-parse -q --verify '@{u}' 2>$null)) {
            Write-Output "-f follows a git clone's branch, and this tree has none"
            exit 1
        }
        if ($up) {
            Write-Output "up already, pid $member_id"
        } elseif (-not (Test-Path $EXE)) {
            Write-Output "no host program, install.bat"
            exit 1
        } else {
            if ($follow) { Set-Content -Path "$T\chrysalisp_rack_follow" -Value "" }
            else { Remove-Item "$T\chrysalisp_rack_follow" -ErrorAction SilentlyContinue }
            Set-Content -Path $LOG -Value ""
            Remove-Item "$T\chrysalisp_rack.pid" -ErrorAction SilentlyContinue
            $loop = Start-Process -FilePath (Get-Process -Id $PID).Path -WindowStyle Hidden -PassThru `
                -ArgumentList "-NoProfile -ExecutionPolicy Bypass -File `"$NHROOT\rack.ps1`" loop"
            Set-Content -Path "$T\chrysalisp_rack_loop.pid" -Value $loop.Id
            # the member says its own pid as it starts
            for ($n = 0; ($n -lt 50) -and -not (Test-Path "$T\chrysalisp_rack.pid"); $n++) { Start-Sleep -Milliseconds 100 }
            Write-Output "started, pid $(read_id "$T\chrysalisp_rack.pid")"
        }
    }
    "down" {
        if ($up) {
            # the loop first, so it does not start it again
            if (alive $loop_id) { Stop-Process -Id $loop_id -Force -ErrorAction SilentlyContinue }
            if (alive $member_id) { tree_stop $member_id }
            Remove-Item "$T\chrysalisp_rack.pid", "$T\chrysalisp_rack_loop.pid", "$T\chrysalisp_rack_update", `
                "$T\chrysalisp_rack_follow" -ErrorAction SilentlyContinue
            Write-Output "stopped, pid $member_id"
        } else {
            Write-Output "was not up"
        }
    }
    "status" {
        if (alive $member_id) {
            if (Test-Path "$T\chrysalisp_rack_follow") {
                Write-Output "up, pid $member_id, following $(git -C $NHROOT rev-parse --abbrev-ref '@{u}' 2>$null) at $(git -C $NHROOT rev-parse --short HEAD)"
            } else {
                Write-Output "up, pid $member_id"
            }
        } elseif ($up) {
            Write-Output "updating, install, it starts again after"
        } else {
            Write-Output "down"
        }
    }
    "run" {
        if (-not $up) { Write-Output "not up, rack.bat up"; exit 1 }
        Remove-Item "$T\chrysalisp_rack_result" -ErrorAction SilentlyContinue
        Set-Content -Path "$T\chrysalisp_rack_job.tmp" -Value ($args[1..($args.Count - 1)] -join " ")
        Move-Item -Path "$T\chrysalisp_rack_job.tmp" -Destination "$T\chrysalisp_rack_job" -Force
        while (alive $member_id) {
            if ((Test-Path "$T\chrysalisp_rack_result") -and (Select-String -Path "$T\chrysalisp_rack_result" -Pattern "^DONE$" -Quiet)) {
                Get-Content "$T\chrysalisp_rack_result" | Where-Object { $_ -ne "DONE" }
                exit 0
            }
            Start-Sleep -Milliseconds 100
        }
        Write-Output "the member has gone"
        exit 1
    }
    "loop" {
        # what up starts, hidden, so it outlives the shell it was asked from
        member_loop
    }
    default {
        Write-Output "rack.ps1 up [-f]|down|status|run command line"
        exit 1
    }
}

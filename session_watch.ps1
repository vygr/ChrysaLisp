# stop a session when its last front has gone. Started by a launch
# script, see session_watch in funcs.ps1, with the time the session
# began and the process ids of the nodes that script started.
param ([long]$start, [string]$ids)

. "$PSScriptRoot\funcs.ps1"

$global:session_start = New-Object DateTime $start
$global:session_pids = @($ids -split "," | Where-Object { $_ -ne "" } | ForEach-Object { [int]$_ })

while (session_has_front) { Start-Sleep -Seconds 1 }
session_stop

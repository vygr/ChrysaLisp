Stop-Process -Name main_gui -Force 2>&1 | out-null
Stop-Process -Name main_tui -Force 2>&1 | out-null

if (-not $env:TEMP) { $env:TEMP = $env:TMP }
if ($env:TEMP) {
    New-Item -ItemType Directory -Force -Path $env:TEMP | out-null
    Remove-Item -Path "$($env:TEMP)\???-???" -ErrorAction SilentlyContinue
}

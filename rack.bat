@echo off
rem the member is kept by the PowerShell script, rack.ps1
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0rack.ps1" %*

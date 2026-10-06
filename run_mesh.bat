@echo off
rem the launch is done by the PowerShell script, it keeps the session
rem to itself, and stops only the nodes it started
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0run_mesh.ps1" %*

@echo off
@echo Windows > platform
@echo x86_64 > arch
.\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 0 -run apps/terminal/tui

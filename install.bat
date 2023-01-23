@echo off
@echo Windows > os
@echo x86_64 > cpu
@echo WIN64 > abi
@call stop.bat
@tar -xf snapshot.zip
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 005-008 -l 002-008 -l 007-008 -l 006-008
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 004-007 -l 001-007 -l 006-007 -l 007-008
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 003-006 -l 000-006 -l 006-008 -l 006-007
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 002-005 -l 005-008 -l 004-005 -l 003-005
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 001-004 -l 004-007 -l 003-004 -l 004-005
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 000-003 -l 003-006 -l 003-005 -l 003-004
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 002-008 -l 002-005 -l 001-002 -l 000-002
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 001-007 -l 001-004 -l 000-001 -l 001-002
@start /b .\obj\x86_64\WIN64\Windows\main_tui.exe -e obj\vp64\VP64\sys\boot_image -l 000-006 -l 000-003 -l 000-002 -l 000-001 -run apps/tui/install.lisp

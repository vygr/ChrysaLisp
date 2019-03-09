@echo off
@echo Windows > platform
@echo x86_64 > arch
@call stop.bat
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 9 -l 000-009 -l 001-009 -l 002-009 -l 003-009 -l 004-009 -l 005-009 -l 006-009 -l 007-009 -l 008-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 8 -l 000-008 -l 001-008 -l 002-008 -l 003-008 -l 004-008 -l 005-008 -l 006-008 -l 007-008 -l 008-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 7 -l 000-007 -l 001-007 -l 002-007 -l 003-007 -l 004-007 -l 005-007 -l 006-007 -l 007-008 -l 007-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 6 -l 000-006 -l 001-006 -l 002-006 -l 003-006 -l 004-006 -l 005-006 -l 006-007 -l 006-008 -l 006-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 5 -l 000-005 -l 001-005 -l 002-005 -l 003-005 -l 004-005 -l 005-006 -l 005-007 -l 005-008 -l 005-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 4 -l 000-004 -l 001-004 -l 002-004 -l 003-004 -l 004-005 -l 004-006 -l 004-007 -l 004-008 -l 004-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 3 -l 000-003 -l 001-003 -l 002-003 -l 003-004 -l 003-005 -l 003-006 -l 003-007 -l 003-008 -l 003-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 2 -l 000-002 -l 001-002 -l 002-003 -l 002-004 -l 002-005 -l 002-006 -l 002-007 -l 002-008 -l 002-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 1 -l 000-001 -l 001-002 -l 001-003 -l 001-004 -l 001-005 -l 001-006 -l 001-007 -l 001-008 -l 001-009
@start /b .\obj\Windows\x86_64\main.exe .\obj\Windows\x86_64\sys\boot_image -cpu 0 -l 000-001 -l 000-002 -l 000-003 -l 000-004 -l 000-005 -l 000-006 -l 000-007 -l 000-008 -l 000-009 -run gui/gui/gui

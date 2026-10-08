@echo off
@call stop.bat
@tar -xf snapshot.zip
@rem the GUI is on SDL3, its one library is fetched if it is not here
@if not exist SDL3.dll (
	@echo Fetching SDL3.dll
	@curl -L -s -o SDL3.zip https://github.com/libsdl-org/SDL/releases/download/release-3.4.16/SDL3-3.4.16-win32-x64.zip
	@tar -xf SDL3.zip SDL3.dll
	@del SDL3.zip
)
@rem the install is run by the emulator, on a network sized to the machine,
@rem as make install is on the other systems
@powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0run_tui.ps1" -n 0 -i -e -f

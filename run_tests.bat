@echo off
REM ChrysaLisp HTML Library Test Runner
REM Run all tests for the HTML/browser library

echo.
echo ==============================================
echo ChrysaLisp HTML Library - Test Suite
echo ==============================================
echo.

REM Check if ChrysaLisp is built
if not exist "obj\vp64\sys\boot_image" (
    echo ERROR: ChrysaLisp boot image not found.
    echo Please run 'install.bat' first to build ChrysaLisp.
    exit /b 1
)

REM Run the test suite
echo Running HTML library tests...
echo.

REM Execute the test runner using ChrysaLisp TUI
call run_tui.bat test/html/run_all_tests.lisp

echo.
echo ==============================================
echo Test run complete
echo ==============================================
echo.

#!/bin/bash
# ChrysaLisp HTML Library Test Runner
# Run all tests for the HTML/browser library

echo ""
echo "=============================================="
echo "ChrysaLisp HTML Library - Test Suite"
echo "=============================================="
echo ""

# Check if ChrysaLisp is built
if [ ! -f "./obj/vp64/sys/boot_image" ]; then
    echo "ERROR: ChrysaLisp boot image not found."
    echo "Please run 'make install' first to build ChrysaLisp."
    exit 1
fi

# Run the test suite
echo "Running HTML library tests..."
echo ""

# Execute the test runner using ChrysaLisp TUI
./run_tui.sh -n 1 test/html/run_all_tests.lisp

echo ""
echo "=============================================="
echo "Test run complete"
echo "=============================================="
echo ""

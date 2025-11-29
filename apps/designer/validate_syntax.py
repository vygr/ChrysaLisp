#!/usr/bin/env python3
"""
Designer Implementation - Syntax Validation Script
Checks all designer files for basic Lisp syntax correctness
"""

import os
import re
from pathlib import Path

def check_balanced_parens(filepath):
    """Check if parentheses are balanced in a Lisp file."""
    with open(filepath, 'r') as f:
        content = f.read()

    # Remove strings (simple approach)
    content = re.sub(r'"[^"]*"', '', content)

    # Remove comments
    content = re.sub(r';.*$', '', content, flags=re.MULTILINE)

    # Count parens
    open_count = content.count('(')
    close_count = content.count(')')

    return open_count == close_count, open_count - close_count

def check_required_symbols(filepath, symbols):
    """Check if required symbols are present in file."""
    with open(filepath, 'r') as f:
        content = f.read()

    missing = []
    for symbol in symbols:
        if symbol not in content:
            missing.append(symbol)

    return len(missing) == 0, missing

def validate_file(filepath, description, required_symbols=None):
    """Validate a single Lisp file."""
    print(f"\nTesting: {description}")
    print(f"  File: {filepath}")

    if not os.path.exists(filepath):
        print("    ✗ FILE NOT FOUND")
        return False

    errors = 0

    # Check balanced parentheses
    balanced, diff = check_balanced_parens(filepath)
    if balanced:
        print("    ✓ Balanced parentheses")
    else:
        print(f"    ✗ Unbalanced parentheses (diff: {diff})")
        errors += 1

    # Check required symbols
    if required_symbols:
        found, missing = check_required_symbols(filepath, required_symbols)
        if found:
            print(f"    ✓ All required symbols present ({len(required_symbols)} symbols)")
        else:
            print(f"    ✗ Missing symbols: {', '.join(missing)}")
            errors += 1

    # Check file not empty
    size = os.path.getsize(filepath)
    if size > 0:
        lines = sum(1 for _ in open(filepath))
        print(f"    ✓ File not empty ({lines} lines, {size} bytes)")
    else:
        print("    ✗ File is empty")
        errors += 1

    if errors == 0:
        print("  ✓ PASS")
        return True
    else:
        print(f"  ✗ FAIL ({errors} errors)")
        return False

def main():
    print()
    print("╔══════════════════════════════════════════════════════════╗")
    print("║  Designer Implementation - Syntax Validation            ║")
    print("╚══════════════════════════════════════════════════════════╝")
    print()

    tests = []

    # Core infrastructure files
    print("═══════════════════════════════════════════════════════════")
    print(" Core Infrastructure Files")
    print("═══════════════════════════════════════════════════════════")

    tests.append(validate_file(
        "gui_designer/lisp.inc",
        "UI Tracking Macros",
        ["defmacro ui-window", "designer-push-element", "designer-pop-element",
         "*designer-enabled*"]
    ))

    tests.append(validate_file(
        "gui_designer/serialize.inc",
        "Serialization",
        ["designer-serialize-tree", "designer-serialize-element"]
    ))

    tests.append(validate_file(
        "gui_designer/runtime.inc",
        "Runtime Editing",
        ["designer-set-property", "designer-get-property", "designer-walk-tree"]
    ))

    tests.append(validate_file(
        "gui_designer/loader_enhanced.inc",
        "Loader for Existing Apps",
        ["load-app-for-designer", "swap-gui-import"]
    ))

    tests.append(validate_file(
        "gui_designer/state_toggles.inc",
        "State Toggles",
        ["designer-register-toggle", "designer-toggle-flip", "designer-evaluate-condition"]
    ))

    tests.append(validate_file(
        "gui_designer/drag_drop.inc",
        "Drag-Drop Operations",
        ["designer-insert-child-before", "designer-insert-child-after", "designer-execute-move"]
    ))

    tests.append(validate_file(
        "gui_designer/property_editor.inc",
        "Property Editor",
        ["get-element-properties", "format-property-value", "parse-property-value"]
    ))

    # Application files
    print()
    print("═══════════════════════════════════════════════════════════")
    print(" Application Files")
    print("═══════════════════════════════════════════════════════════")

    tests.append(validate_file(
        "apps/designer/designer_complete.lisp",
        "Integrated Designer Application",
        ["ui-window", "file_path_input", "defun main"]
    ))

    tests.append(validate_file(
        "apps/designer/test_all.lisp",
        "Test Suite",
        ["import", "print", "designer-get-tree"]
    ))

    tests.append(validate_file(
        "apps/designer/interactive_session.lisp",
        "Interactive REPL Session",
        ["defun designer-load", "defun designer-show"]
    ))

    tests.append(validate_file(
        "apps/designer/demo_complete_roundtrip.lisp",
        "Complete Round-Trip Demo",
        ["load-app-for-designer", "designer-serialize-tree"]
    ))

    # Summary
    print()
    print("═══════════════════════════════════════════════════════════")
    print(" Summary")
    print("═══════════════════════════════════════════════════════════")
    print()

    passed = sum(1 for t in tests if t)
    failed = sum(1 for t in tests if not t)
    total = len(tests)

    print(f"  Passed: {passed}")
    print(f"  Failed: {failed}")
    print(f"  Total:  {total}")
    print()

    if failed == 0:
        print("✓ All syntax validation tests passed!")
        print()
        return 0
    else:
        print("✗ Some tests failed. Review errors above.")
        print()
        return 1

if __name__ == "__main__":
    exit(main())

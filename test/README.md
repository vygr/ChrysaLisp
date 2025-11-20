# ChrysaLisp HTML Library Test Suite

This directory contains the comprehensive test suite for the ChrysaLisp HTML/browser library.

## Overview

The HTML library is a complete Web 1.0-era browser implementation written in ChrysaLisp, featuring:

- **HTML Parser** - DOM-based HTML parsing and manipulation
- **CSS Support** - Basic CSS styling and rendering
- **Canvas Renderer** - Graphical rendering on ChrysaLisp canvas
- **Script Execution** - LispScript execution (JavaScript-like but with Lisp syntax)
- **Event Handling** - Interactive event system (click, mouseover, etc.)
- **Form Widgets** - Input fields, buttons, text areas
- **Storage APIs** - Cookies, localStorage, sessionStorage
- **Canvas Element** - HTML5 `<canvas>` with 2D drawing API
- **Image Support** - Loading and displaying images via file:// protocol
- **Browser Automation** - WebDriver-compatible automation library

## Running Tests

### Prerequisites

1. Build ChrysaLisp first:
   ```bash
   # macOS/Linux
   make install

   # Windows
   install.bat
   ```

2. Ensure you have a working ChrysaLisp environment.

### Run All Tests

From the project root directory:

```bash
# macOS/Linux
./run_tests.sh

# Windows
run_tests.bat
```

Alternatively, run the test suite directly using ChrysaLisp:

```bash
# macOS/Linux
./run_tui.sh -n 1 test/html/run_all_tests.lisp

# Windows
run_tui.bat test/html/run_all_tests.lisp
```

### Run Individual Test Files

You can also run individual test files:

```bash
./run_tui.sh -n 1 test/html/test_parser.lisp
./run_tui.sh -n 1 test/html/test_cookies.lisp
./run_tui.sh -n 1 test/html/test_canvas_element.lisp
# etc.
```

## Test Organization

### Core HTML Tests

- `test_encoding.lisp` - Character encoding detection tests
- `test_part.lisp` - HTML part/fragment tests
- `test_parser.lisp` - HTML parser tests
- `test_dom_assertions.lisp` - DOM manipulation tests
- `test_html_roundtrip.lisp` - HTML serialization/deserialization tests

### Rendering Tests

- `test_canvas_rendering.lisp` - Canvas rendering engine tests
- `test_interactive_links.lisp` - Clickable hyperlinks tests
- `test_text_selection.lisp` - Text selection tests

### Script and Event Tests

- `test_script_execution.lisp` - LispScript execution tests
- `test_event_handlers.lisp` - Event handler tests (onclick, onmouseover, etc.)
- `test_dialogs.lisp` - Dialog function tests (alert, confirm, prompt)

### Form Tests

- `test_form_widgets.lisp` - Form input/button widget tests

### Browser Features

- `test_browser_navigation.lisp` - Browser navigation tests
- `test_devtools.lisp` - Developer tools inspector tests
- `test_browser_driver.lisp` - WebDriver automation API tests
- `test_browser_integration.lisp` - End-to-end browser integration tests

### Storage Tests

- `test_cookies.lisp` - Cookie storage tests
- `test_web_storage.lisp` - localStorage and sessionStorage tests

### Media Tests

- `test_image_tags.lisp` - Image loading and rendering tests
- `test_canvas_element.lisp` - HTML5 canvas element tests

### Data Format Tests

- `test/json/test_json_roundtrip.lisp` - JSON parsing/serialization tests
- `test/xml/test_xml_roundtrip.lisp` - XML parsing/serialization tests

### Concurrency Tests

- `test/lisp/test_concurrency.lisp` - Actor/CSP concurrency library tests

## Test Framework

All tests use the ChrysaLisp unit test framework located at `lib/test/unittest.inc`.

### Writing New Tests

Example test structure:

```lisp
#!/usr/bin/env lsp

;; My Feature Tests
;; TDD approach - tests first!

(import "lib/test/unittest.inc")
(import "lib/html/my_feature.inc")

(deftest-suite "My Feature Tests")

; Test 1: Basic functionality
(deftest "Test Name"
    (defq result (my-function "input"))
    (assert-eq "expected" result))

; Test 2: Edge cases
(deftest "Edge Case Test"
    (defq result (my-function nil))
    (assert-nil result))
```

### Available Assertions

- `assert-eq expected actual` - Assert equality
- `assert-not-eq expected actual` - Assert inequality
- `assert-nil value` - Assert nil
- `assert-not-nil value` - Assert not nil
- `assert-true value` - Assert true
- `assert-false value` - Assert false

## Test Coverage

The test suite provides comprehensive coverage of:

- HTML parsing and DOM manipulation
- CSS style application
- Canvas rendering
- Script execution and event handling
- Form widgets and user input
- Storage APIs (cookies, localStorage, sessionStorage)
- Canvas 2D drawing API
- Image loading
- Browser navigation
- WebDriver automation
- JSON and XML processing
- Concurrency primitives

## Continuous Integration

To run tests in CI/CD pipelines:

```bash
# Build ChrysaLisp
make install

# Run all tests
./run_tests.sh

# Check exit code
if [ $? -eq 0 ]; then
    echo "All tests passed!"
else
    echo "Some tests failed!"
    exit 1
fi
```

## Troubleshooting

### Tests Won't Run

- Ensure ChrysaLisp is built: `make install`
- Check that boot image exists: `ls obj/vp64/sys/boot_image`
- Verify SDL2 libraries are installed (macOS: `brew install sdl2`)

### Import Errors

- Make sure you're running tests from the project root directory
- Check that all library files exist in `lib/` directory

### Test Failures

- Check that you're using a compatible ChrysaLisp version
- Review test output for specific assertion failures
- Run individual test files to isolate issues

## Contributing

When adding new features to the HTML library:

1. **Write tests first** (TDD approach)
2. Create a new test file in the appropriate directory
3. Add the test file to `test/html/run_all_tests.lisp`
4. Implement the feature to make tests pass
5. Commit tests and implementation together

## License

Same as ChrysaLisp main project.

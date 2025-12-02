# Testing the FiveAM Framework

This directory contains tests for the FiveAM testing framework itself.

## Test Files

- **test_smoke.lisp** - Minimal smoke test to verify basic functionality
- **test_fiveam.lisp** - Comprehensive test suite (meta-testing: using FiveAM to test FiveAM)

## Running Tests

### Prerequisites

Build ChrysaLisp first:
```bash
make clean
make install
```

### Run Smoke Test

Quick verification that the framework loads and works:

```bash
./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
    -run lib/test/test_smoke.lisp
```

Expected output: Should show successful creation and execution of basic tests.

### Run Comprehensive Tests

Full test suite covering all features:

```bash
./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
    -run lib/test/test_fiveam.lisp
```

Expected output: Should show detailed results for each test suite with final summary.

### Run with Timeout (Recommended)

```bash
timeout 30 ./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
    -run lib/test/test_smoke.lisp

timeout 60 ./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
    -run lib/test/test_fiveam.lisp
```

## What Gets Tested

### Smoke Test Coverage
- Basic test definition and execution
- Test suite creation
- All assertion macros (is-true, is-false, is-eq, is-equal, pass, skip, finishes)
- Result analysis functions
- Test registry operations

### Comprehensive Test Coverage

1. **Assertion Tests** - All check macros work correctly
2. **Registry Tests** - Test and suite registration/removal
3. **Execution Tests** - Result collection and types
4. **Dependency Tests** - Dependency resolution
5. **Suite Organization** - Suite creation and context switching
6. **Result Analysis** - Status analysis and result partitioning

## Troubleshooting

### Test Hangs or Times Out

- Increase timeout value
- Check for infinite loops in test code
- Verify ChrysaLisp is built correctly

### Import Errors

```
Error: ... symbol_not_bound ... import
```

Solution: Rebuild ChrysaLisp to include new library files:
```bash
make clean
make install
```

### Shebang Errors

```
Error: ... symbol_not_bound ... #!/...
```

Solution: Remove shebang from test files (they're not supported in ChrysaLisp).

## Expected Test Results

### Smoke Test
- All 5 tests should complete
- Should show "All basic functionality verified!"

### Comprehensive Test
- All test suites should pass
- Final summary should show:
  - Total Passed: 30+ checks
  - Total Failed: 0
  - Total Skipped: ~5 (from explicit skip tests)
  - SUCCESS message

## Adding New Tests

When adding tests for new FiveAM features:

1. Add tests to `test_fiveam.lisp` in appropriate suite
2. Create new suite if testing a new major feature area
3. Run smoke test first to catch basic errors
4. Run comprehensive tests to verify integration

Example:
```lisp
(def-suite :new-feature-tests :description "Tests for new feature")
(in-suite :new-feature-tests)

(def-test new-feature-test
    :description "Verify new feature works"
    (is-eq expected-value (new-feature-function)
        "New feature produces correct result"))
```

## Continuous Testing

For development, you can run tests after each change:

```bash
# Quick iteration cycle:
make clean && make install && \
timeout 30 ./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
    -run lib/test/test_smoke.lisp
```

## Meta-Testing Note

The comprehensive test suite uses FiveAM to test itself. This is both:
- A demonstration of FiveAM's capabilities
- A validation that the framework works correctly

If the comprehensive tests pass, it means FiveAM is working well enough to test itself!

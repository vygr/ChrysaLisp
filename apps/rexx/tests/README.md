# REXX Interpreter Test Suite

Automated testing for the ChrysaLisp REXX interpreter to ensure all features work correctly.

## Test Structure

```
tests/
├── README.md           # This file
├── test_suite.rex      # Main automated test suite
└── run_tests.sh        # Test runner script
```

## Running Tests

### From Shell

```bash
cd apps/rexx
./run_tests.sh
```

### From ChrysaLisp GUI

1. Launch the REXX application: `./apps/rexx/app.lisp`
2. Load `tests/test_suite.rex` in the editor
3. Click "Execute" or press Ctrl+E
4. Review test results in the output panel

## Test Coverage

The test suite covers all implemented features:

### ✅ Modern Features
- **String Interpolation** - `{variable}` syntax in strings
- **Escape Sequences** - `\n`, `\t`, `\r`, `\\`, `\"`
- **C-Style Comments** - `//` comments (tested implicitly)

### ✅ Core String Functions
- `UPPER(str)` - Convert to uppercase
- `LOWER(str)` - Convert to lowercase
- `LENGTH(str)` - String length
- `SUBSTR(str, n, len)` - Extract substring

### ✅ Additional String Functions
- `PROPER(str)` - Title case
- `PAD_START(str, len, pad)` - Pad beginning
- `PAD_END(str, len, pad)` - Pad end
- `COPIES(str, count)` - Repeat string
- `TRIM_START(str)` - Remove leading whitespace
- `TRIM_END(str)` - Remove trailing whitespace

### ✅ Validation Functions
- `IS_ALPHA(str)` - Test for letters only
- `IS_NUMERIC(str)` - Test for digits only
- `IS_ALPHANUMERIC(str)` - Test for letters/digits
- `IS_EMPTY(str)` - Test for empty/whitespace
- `STARTS_WITH(str, prefix)` - Test prefix match
- `ENDS_WITH(str, suffix)` - Test suffix match
- `INCLUDES(str, substring)` - Test substring presence

### ✅ Basic Operations
- Variable assignment and retrieval
- Variable persistence across statements
- SAY output instruction
- Basic expressions

## Test Results Format

```
======================================
REXX Interpreter Automated Test Suite
======================================

Test 1: String Interpolation
  PASS: String interpolation works

Test 2: Escape Sequences
  Testing escape sequences:
    Newline: Line1
Line2
    Tab: Col1	Col2
  PASS: Escape sequences rendered

...

======================================
Test Results
======================================
Tests Run:    15
Tests Passed: 15
Tests Failed: 0

SUCCESS: All tests passed!
```

## Adding New Tests

To add a new test to `test_suite.rex`:

1. **Increment test counter:**
   ```rexx
   SAY "Test N: Feature Name"
   ```

2. **Run the test:**
   ```rexx
   result = FEATURE_FUNCTION("input")
   expected = "expected_output"
   ```

3. **Check result:**
   ```rexx
   IF result = expected THEN
     SAY "  PASS: Description"
     tests_passed = "N"
   ELSE
     SAY "  FAIL: Expected '{expected}' but got '{result}'"
     tests_failed = "M"
   tests_run = "N"
   SAY ""
   ```

## Test Philosophy

Following best practices for test automation:

1. **Comprehensive** - Test all public functions and features
2. **Automated** - No manual intervention required
3. **Clear Output** - Easy to identify what passed/failed
4. **Reproducible** - Same results every run
5. **Fast** - Quick feedback for developers

## Coverage Goals

Current coverage: **~85%** of implemented features

Features with test coverage:
- ✅ String interpolation
- ✅ Escape sequences
- ✅ All 31 built-in functions
- ✅ Variable operations
- ✅ Basic execution flow

Features needing more tests:
- ⏳ ADDRESS/PORTS IPC (requires service mocking)
- ⏳ Error handling edge cases
- ⏳ Complex nested expressions
- ⏳ PARSE instruction variants

## Integration with CI/CD

While ChrysaLisp doesn't have traditional CI/CD, this test suite ensures:

1. **Pre-commit verification** - Run before committing changes
2. **Feature validation** - Verify new features work
3. **Regression prevention** - Catch broken features
4. **Documentation** - Tests serve as usage examples

## Future Enhancements

Planned test improvements:

- [ ] Performance benchmarks
- [ ] Memory usage tests
- [ ] IPC integration tests with mock services
- [ ] Stress tests with large inputs
- [ ] Concurrent execution tests
- [ ] Edge case coverage expansion

## Manual Testing Checklist

Some features require manual testing in the GUI:

- [ ] GUI loads without errors
- [ ] Syntax highlighting works (if implemented)
- [ ] Execute button responds
- [ ] Output displays correctly
- [ ] Error messages are clear
- [ ] Help text is accessible
- [ ] Examples load and run

## Troubleshooting

**Test fails unexpectedly:**
1. Check if interpolation is enabled
2. Verify all functions are imported
3. Review recent code changes
4. Compare with working examples

**Test suite won't run:**
1. Ensure REXX app is properly installed
2. Check file permissions on run_tests.sh
3. Verify ChrysaLisp environment is set up

**Tests pass but feature doesn't work:**
1. Test may not cover the specific use case
2. Add a new test for the scenario
3. Report issue with reproduction steps

## Contributing Tests

When contributing new REXX features:

1. Write tests BEFORE implementing the feature (TDD)
2. Ensure all tests pass before submitting PR
3. Add tests to `test_suite.rex`
4. Update this README with new coverage
5. Include test results in PR description

## References

- RexxJS Testing: Inspiration for validation approach
- ChrysaLisp Standards: CONTRIBUTIONS.md
- Test Examples: All `.rex` files in `examples/` directory

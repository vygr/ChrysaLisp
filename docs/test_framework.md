# ChrysaLisp Test Framework

A comprehensive BDD-style testing framework for ChrysaLisp with `describe`, `it`, and `should` assertions.

## Table of Contents

- [Quick Start](#quick-start)
- [Installation](#installation)
- [Basic Usage](#basic-usage)
- [API Reference](#api-reference)
- [Test Runner](#test-runner)
- [Examples](#examples)
- [Best Practices](#best-practices)
- [Advanced Features](#advanced-features)

## Quick Start

```lisp
(import "lib/test/test.inc")

(describe "Basic Math"
	(it "should add numbers"
		(should-equal (+ 2 3) 5))

	(it "should multiply numbers"
		(should-equal (* 4 5) 20)))
```

Run your tests:
```bash
./run_tui.sh -n 1 cmd/test-runner.lisp
```

## Installation

The test framework is included in the ChrysaLisp distribution:

1. **Core Framework**: `lib/test/test.inc`
2. **Test Runner**: `cmd/test-runner.lisp`
3. **Examples**: `examples/test/*.lisp`

## Basic Usage

### Writing Your First Test

Create a test file in `examples/test/` ending with `_test.lisp`:

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; my_feature_test.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

(describe "My Feature"
	(it "should work correctly"
		(should-equal (+ 1 1) 2)))
```

### Running Tests

```bash
# Run all tests
./run_tui.sh -n 1 cmd/test-runner.lisp

# Run specific test file
./run_tui.sh -n 1 cmd/test-runner.lisp examples/test/my_feature_test.lisp

# Run with filter
./run_tui.sh -n 1 cmd/test-runner.lisp -f "Math"

# Run with verbose output
./run_tui.sh -n 1 cmd/test-runner.lisp -v

# List available tests
./run_tui.sh -n 1 cmd/test-runner.lisp -l
```

## API Reference

### Test Structure

#### `describe`
Define a test suite.

```lisp
(describe "Suite Name"
	; test cases here
	)
```

#### `xdescribe`
Skip an entire test suite.

```lisp
(xdescribe "Skipped Suite"
	; these tests won't run
	)
```

#### `it`
Define a test case.

```lisp
(it "should do something"
	; assertions here
	)
```

#### `xit`
Skip a test case.

```lisp
(xit "should do something else"
	; this test won't run
	)
```

### Assertions

#### Equality

**`should-equal`**
```lisp
(should-equal actual expected [message])
```
Assert that actual equals expected.

```lisp
(should-equal (+ 2 3) 5)
(should-equal "hello" "hello" "Strings should match")
```

**`should-not-equal`**
```lisp
(should-not-equal actual expected [message])
```
Assert that actual does not equal expected.

```lisp
(should-not-equal 5 10)
```

#### Boolean Checks

**`should-be-true`**
```lisp
(should-be-true actual [message])
```
Assert that value is true (not nil).

```lisp
(should-be-true (> 10 5))
```

**`should-be-false`**
```lisp
(should-be-false actual [message])
```
Assert that value is false (nil).

```lisp
(should-be-false (< 10 5))
```

#### Nil Checks

**`should-be-nil`**
```lisp
(should-be-nil actual [message])
```
Assert that value is nil.

```lisp
(should-be-nil :nil)
(should-be-nil (find "xyz" "hello"))
```

**`should-not-be-nil`**
```lisp
(should-not-be-nil actual [message])
```
Assert that value is not nil.

```lisp
(should-not-be-nil "hello")
(should-not-be-nil (list 1 2 3))
```

#### Comparisons

**`should-be-less-than`**
```lisp
(should-be-less-than actual expected [message])
```
Assert that actual < expected.

```lisp
(should-be-less-than 5 10)
```

**`should-be-greater-than`**
```lisp
(should-be-greater-than actual expected [message])
```
Assert that actual > expected.

```lisp
(should-be-greater-than 10 5)
```

#### Collections

**`should-contain`**
```lisp
(should-contain collection item [message])
```
Assert that collection contains item.

```lisp
(should-contain (list 1 2 3) 2)
(should-contain "hello world" "world")
```

**`should-not-contain`**
```lisp
(should-not-contain collection item [message])
```
Assert that collection does not contain item.

```lisp
(should-not-contain (list 1 2 3) 5)
```

**`should-be-empty`**
```lisp
(should-be-empty collection [message])
```
Assert that collection is empty.

```lisp
(should-be-empty (list))
(should-be-empty "")
```

**`should-not-be-empty`**
```lisp
(should-not-be-empty collection [message])
```
Assert that collection is not empty.

```lisp
(should-not-be-empty (list 1 2 3))
```

#### Error Handling

**`should-throw`**
```lisp
(should-throw func [message])
```
Assert that function throws an error.

```lisp
(should-throw (lambda () (/ 10 0)))
```

**`should-not-throw`**
```lisp
(should-not-throw func [message])
```
Assert that function does not throw an error.

```lisp
(should-not-throw (lambda () (+ 1 2)))
```

### Configuration Functions

**`test-set-filter`**
```lisp
(test-set-filter pattern)
```
Set filter pattern for test names.

**`test-set-verbose`**
```lisp
(test-set-verbose :t)
```
Enable verbose output (show all passing tests).

**`test-set-color`**
```lisp
(test-set-color :nil)
```
Disable colored output.

**`test-reset`**
```lisp
(test-reset)
```
Reset test state for a new test run.

**`test-run-summary`**
```lisp
(test-run-summary)
```
Print test summary and failures.

**`test-exit-code`**
```lisp
(test-exit-code)
```
Get exit code (0 for success, 1 for failures).

## Test Runner

### Command Line Options

```
Usage: test-runner [options] [test-files...]

Options:
  -h --help              : Show help information
  -f --filter PATTERN    : Only run tests matching PATTERN
  -v --verbose           : Show all test results (not just failures)
  -n --no-color          : Disable colored output
  -p --pattern GLOB      : Glob pattern for test files
  -l --list              : List available test files without running
```

### Examples

```bash
# Run all tests
./run_tui.sh -n 1 cmd/test-runner.lisp

# Run specific test file
./run_tui.sh -n 1 cmd/test-runner.lisp examples/test/basic_test.lisp

# Run tests matching "Math"
./run_tui.sh -n 1 cmd/test-runner.lisp -f "Math"

# Run with verbose output
./run_tui.sh -n 1 cmd/test-runner.lisp -v

# Run without colors
./run_tui.sh -n 1 cmd/test-runner.lisp -n

# List all test files
./run_tui.sh -n 1 cmd/test-runner.lisp -l
```

## Examples

### Basic Arithmetic Tests

```lisp
(import "lib/test/test.inc")

(describe "Arithmetic Operations"
	(it "should add two numbers"
		(should-equal (+ 2 3) 5))

	(it "should subtract numbers"
		(should-equal (- 10 5) 5))

	(it "should multiply numbers"
		(should-equal (* 3 4) 12))

	(it "should divide numbers"
		(should-equal (/ 10 2) 5)))
```

### Testing Collections

```lisp
(describe "List Operations"
	(it "should create lists"
		(defq mylist (list 1 2 3))
		(should-equal (length mylist) 3))

	(it "should map over lists"
		(defq nums (list 1 2 3))
		(defq doubled (map (lambda (x) (* x 2)) nums))
		(should-equal (elem-get doubled 0) 2))

	(it "should filter lists"
		(defq nums (list 1 2 3 4 5))
		(defq evens (filter (lambda (x) (= 0 (% x 2))) nums))
		(should-contain evens 2)
		(should-not-contain evens 1)))
```

### Testing Error Handling

```lisp
(defun divide-safe (a b)
	(if (= b 0)
		(throw "Division by zero" divide-safe)
		(/ a b)))

(describe "Error Handling"
	(it "should throw on divide by zero"
		(should-throw (lambda () (divide-safe 10 0))))

	(it "should not throw on valid division"
		(should-not-throw (lambda () (divide-safe 10 2)))))
```

### Nested Describes

```lisp
(describe "Math Library"
	(describe "Basic Operations"
		(it "should add"
			(should-equal (+ 2 2) 4))

		(it "should multiply"
			(should-equal (* 3 3) 9)))

	(describe "Advanced Operations"
		(it "should calculate power"
			(should-equal (* 2 2 2) 8))))
```

### Skipping Tests

```lisp
(describe "Feature Tests"
	(it "should run this test"
		(should-be-true :t))

	(xit "should skip this test"
		(should-equal 1 2))  ; Won't fail because it's skipped

	(xdescribe "Skipped Suite"
		(it "won't run"
			(should-be-true :nil))))
```

## Best Practices

### 1. Descriptive Test Names

Use clear, descriptive names that explain what is being tested:

```lisp
; Good
(it "should calculate factorial of 5 correctly"
	(should-equal (factorial 5) 120))

; Less clear
(it "test factorial"
	(should-equal (factorial 5) 120))
```

### 2. One Assertion Per Test

Keep tests focused on a single behavior:

```lisp
; Good
(it "should add positive numbers"
	(should-equal (+ 2 3) 5))

(it "should add negative numbers"
	(should-equal (+ -2 -3) -5))

; Less focused
(it "should add numbers"
	(should-equal (+ 2 3) 5)
	(should-equal (+ -2 -3) -5)
	(should-equal (+ 0 0) 0))
```

### 3. Group Related Tests

Use `describe` blocks to organize related tests:

```lisp
(describe "String Operations"
	(describe "Concatenation"
		(it "should join two strings"
			(should-equal (cat "hello" " " "world") "hello world")))

	(describe "Searching"
		(it "should find substring"
			(should-not-be-nil (find "world" "hello world")))))
```

### 4. Use Custom Messages

Add custom messages for complex assertions:

```lisp
(it "should calculate correct sum"
	(defq expected-sum 15)
	(defq actual-sum (+ 1 2 3 4 5))
	(should-equal actual-sum expected-sum
		(cat "Sum of 1-5 should be " (str expected-sum))))
```

### 5. Test Edge Cases

Always test boundary conditions:

```lisp
(describe "Division"
	(it "should handle normal division"
		(should-equal (/ 10 2) 5))

	(it "should throw on division by zero"
		(should-throw (lambda () (/ 10 0))))

	(it "should handle division of zero"
		(should-equal (/ 0 10) 0)))
```

## Advanced Features

### Custom Test Runners

You can create custom test runners for specific needs:

```lisp
(import "lib/test/test.inc")

; Reset state
(test-reset)

; Configure
(test-set-verbose :t)
(test-set-filter "Math")

; Load and run tests
(load "examples/test/basic_test.lisp")
(load "examples/test/advanced_test.lisp")

; Print results
(test-run-summary)

; Exit with appropriate code
(test-exit-code)
```

### Integration with CI/CD

The test runner exits with code 0 on success and 1 on failure, making it suitable for CI/CD pipelines:

```bash
#!/bin/bash
./run_tui.sh -n 1 cmd/test-runner.lisp
exit_code=$?

if [ $exit_code -eq 0 ]; then
	echo "All tests passed!"
else
	echo "Tests failed!"
	exit 1
fi
```

### Performance Testing

While not a performance framework, you can use the test framework to ensure code meets performance requirements:

```lisp
(describe "Performance"
	(it "should handle large lists efficiently"
		(defq large-list (range 0 10000 1))
		(defq start (pii-time))
		(defq sum (reduce (lambda (acc x) (+ acc x)) large-list 0))
		(defq elapsed (- (pii-time) start))
		(should-equal sum 49995000)
		; Ensure it completes in reasonable time
		(should-be-less-than elapsed 1000000))) ; 1 second
```

## Output Examples

### Successful Tests

```
Running 3 test file(s)

Basic Arithmetic
  ✓ should add two numbers correctly
  ✓ should subtract two numbers correctly
  ✓ should multiply two numbers correctly

Test Summary:
  Total:   3
  Passed:  3
```

### Failed Tests

```
Running 1 test file(s)

Basic Math
  ✓ should add correctly
  ✗ should subtract correctly

Failures:

  ✗ Basic Math > should subtract correctly
    Expected 5 but got 6

Test Summary:
  Total:   2
  Passed:  1
  Failed:  1
```

### Skipped Tests

```
Basic Math
  ✓ should add correctly
  ○ should subtract correctly (skipped)

Test Summary:
  Total:   2
  Passed:  1
  Skipped: 1
```

## Philosophy

The test framework follows ChrysaLisp's core principles:

1. **Simplicity**: Clean, readable BDD-style syntax
2. **Performance**: Minimal overhead, efficient execution
3. **Composability**: Build complex test suites from simple primitives
4. **Iteration**: Use `map`, `filter`, `reduce`, `some` for test organization
5. **Explicit**: Clear assertions with helpful error messages

## Contributing

When adding new assertions or features:

1. Follow ChrysaLisp naming conventions
2. Add comprehensive tests for the new feature
3. Update documentation
4. Ensure backward compatibility
5. Keep the API consistent with existing patterns

## See Also

- [ChrysaLisp AI Digest](./ai_digest/summary.md)
- [The Philosophy](./ai_digest/the_philosophy.md)
- [Modern Lisp](./ai_digest/modern_lisp.md)
- [Rocinante: The Four Horsemen](./ai_digest/rocinante.md)

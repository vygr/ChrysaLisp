# Test Framework Quick Reference

## Basic Structure

```lisp
(import "lib/test/test.inc")

(describe "Feature Name"
	(it "should do something"
		(should-equal actual expected)))
```

## Running Tests

```bash
# All tests
./run_tui.sh -n 1 cmd/test-runner.lisp

# Specific file
./run_tui.sh -n 1 cmd/test-runner.lisp examples/test/my_test.lisp

# With filter
./run_tui.sh -n 1 cmd/test-runner.lisp -f "pattern"

# Verbose
./run_tui.sh -n 1 cmd/test-runner.lisp -v

# List tests
./run_tui.sh -n 1 cmd/test-runner.lisp -l
```

## Assertions Cheat Sheet

### Equality
```lisp
(should-equal actual expected [msg])
(should-not-equal actual expected [msg])
```

### Boolean
```lisp
(should-be-true value [msg])
(should-be-false value [msg])
```

### Nil
```lisp
(should-be-nil value [msg])
(should-not-be-nil value [msg])
```

### Comparison
```lisp
(should-be-less-than actual expected [msg])
(should-be-greater-than actual expected [msg])
```

### Collections
```lisp
(should-contain collection item [msg])
(should-not-contain collection item [msg])
(should-be-empty collection [msg])
(should-not-be-empty collection [msg])
```

### Errors
```lisp
(should-throw (lambda () (code)) [msg])
(should-not-throw (lambda () (code)) [msg])
```

## Test Organization

### Test Suite
```lisp
(describe "Suite Name"
	; tests here
	)
```

### Skip Suite
```lisp
(xdescribe "Skipped Suite"
	; won't run
	)
```

### Test Case
```lisp
(it "should do X"
	; assertions
	)
```

### Skip Test
```lisp
(xit "should do Y"
	; won't run
	)
```

### Nested
```lisp
(describe "Outer"
	(describe "Inner"
		(it "test" ...)))
```

## Common Patterns

### Testing Functions
```lisp
(describe "add function"
	(it "should add two numbers"
		(should-equal (add 2 3) 5)))
```

### Testing Collections
```lisp
(describe "list operations"
	(it "should map correctly"
		(defq result (map (lambda (x) (* x 2)) (list 1 2 3)))
		(should-equal (elem-get result 0) 2)))
```

### Testing Errors
```lisp
(describe "error handling"
	(it "should throw on invalid input"
		(should-throw (lambda () (divide-by-zero)))))
```

### With Setup
```lisp
(describe "feature"
	(it "should work"
		(defq setup-data (list 1 2 3))
		(should-equal (length setup-data) 3)))
```

## Configuration

```lisp
(test-set-filter "pattern")     ; Filter tests
(test-set-verbose :t)            ; Verbose mode
(test-set-color :nil)            ; Disable colors
(test-reset)                     ; Reset state
(test-run-summary)               ; Print summary
(test-exit-code)                 ; Get exit code
```

## Exit Codes

- `0` - All tests passed
- `1` - One or more tests failed

## Color Output

- Green ✓ - Passed
- Red ✗ - Failed
- Yellow ○ - Skipped

## File Naming

Test files must end with `_test.lisp`:
- `my_feature_test.lisp` ✓
- `test_my_feature.lisp` ✗
- `my_feature.lisp` ✗

# FiveAM Testing Framework for ChrysaLisp

This is a port of the FiveAM (Five Another Manual) testing framework from Common Lisp to ChrysaLisp.

## Credits

**Original FiveAM** created by Edward Marco Baringer
Copyright (c) 2002-2003, Edward Marco Baringer
All rights reserved.

**Ported to ChrysaLisp** by Claude AI Assistant, 2025

## License

This software is provided under the BSD 3-Clause License. See the license text in `test.inc` for full details.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the conditions specified in the BSD 3-Clause License are met.

## Overview

FiveAM is a testing framework designed with the following principles:

- **Simple test definition**: Focus on writing tests, not infrastructure
- **Interactive development**: Quickly redefine, change, remove and run tests
- **Organized testing**: Group tests into suites for better organization
- **Clear results**: Detailed reporting of test results with pass/fail statistics

## Usage

### Basic Example

```lisp
(import "lib/test/test.inc")

;; Define a test
(def-test my-first-test
    (is-eq 4 (+ 2 2) "Two plus two equals four")
    (is-true (< 1 2) "One is less than two")
    (is-false (> 1 2) "One is not greater than two"))

;; Run the test
(run! 'my-first-test)
```

### Test Suites

Organize related tests into suites:

```lisp
;; Define a suite
(def-suite :math-tests :description "Mathematical operation tests")

;; Set current suite
(in-suite :math-tests)

;; Define tests in this suite
(def-test addition-test
    (is-eq 4 (+ 2 2))
    (is-eq 0 (+ -1 1))
    (is-eq 10 (+ 5 5)))

(def-test multiplication-test
    (is-eq 6 (* 2 3))
    (is-eq 0 (* 5 0))
    (is-eq 25 (* 5 5)))

;; Run all tests in the suite
(run! :math-tests)
```

### Available Check Macros

- `(is test [reason])` - General purpose test assertion
- `(is-true condition [reason])` - Assert condition is true
- `(is-false condition [reason])` - Assert condition is false
- `(is-eq expected actual [reason])` - Assert values are equal using `eql`
- `(is-equal expected actual [reason])` - Assert values are equal using `equal`
- `(pass [reason])` - Explicitly pass a test
- `(fail [reason])` - Explicitly fail a test
- `(skip [reason])` - Skip a test
- `(finishes body...)` - Assert code completes without errors

### Test Dependencies

Tests can depend on other tests:

```lisp
(def-test setup-test
    (is-true :t "Setup successful"))

(def-test dependent-test
    :depends-on 'setup-test
    (is-true :t "Dependent test runs only if setup-test passes"))
```

Dependencies can use logical operators:

```lisp
;; Run only if ALL dependencies pass
(def-test test-with-and-deps
    :depends-on (and test1 test2 test3)
    (is-true :t))

;; Run if ANY dependency passes
(def-test test-with-or-deps
    :depends-on (or test1 test2)
    (is-true :t))

;; Run only if dependency FAILS
(def-test test-with-not-deps
    :depends-on (not test1)
    (is-true :t))
```

### Running Tests

```lisp
;; Run a single test
(run 'my-test)

;; Run a test suite
(run :my-suite)

;; Run multiple specific tests
(run '(test1 test2 test3))

;; Run all tests
(run :all)

;; Run and explain (shows detailed results)
(run! 'my-test)
(run! :my-suite)

;; Run all tests with explanation
(run-all-tests)
```

### Result Analysis

The `explain` function provides detailed test results:

```lisp
(defq results (run 'my-test))
(explain results)
```

Output includes:
- Total number of checks
- Pass/Skip/Fail counts and percentages
- Detailed failure information
- Skip reasons

## Example Test File

See `examples/example-tests.inc` for a complete example showing various testing scenarios.

## Features

### Implemented

- ✓ Basic test assertions (is, is-true, is-false, is-eq, is-equal)
- ✓ Test suites for organizing tests
- ✓ Test dependencies (and, or, not)
- ✓ Detailed result reporting
- ✓ Pass/Fail/Skip/Error tracking
- ✓ Suite-based test organization

### Differences from Original FiveAM

This port is adapted to ChrysaLisp's functional/procedural style:

- Uses hmaps (emaps) instead of CLOS classes
- Uses ChrysaLisp's error handling (catch) instead of Common Lisp's condition system
- Simplified fixture support (not yet implemented)
- No random test generation (not yet implemented)
- Simplified macro expansion to match ChrysaLisp's capabilities

## Contributing

This is a community port. Contributions and improvements are welcome!

## References

- Original FiveAM: https://github.com/lispci/fiveam
- ChrysaLisp: https://github.com/vygr/ChrysaLisp

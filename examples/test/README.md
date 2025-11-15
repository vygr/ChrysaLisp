# ChrysaLisp Test Examples

This directory contains example test files demonstrating the ChrysaLisp test framework.

## Example Files

### `basic_test.lisp`
Demonstrates fundamental testing patterns:
- Arithmetic operations
- Comparison operations
- Boolean logic
- Nil handling
- Basic assertions

**Key Features:**
- Simple test structure
- Clear test descriptions
- Intentional failure demonstration

### `collections_test.lisp`
Shows how to test sequences and collections:
- List operations (create, push, pop, access)
- Sequence functions (map, filter, reduce, some)
- String operations
- Collection assertions (contains, empty)

**Key Features:**
- Testing the "Four Horsemen" primitives
- Collection manipulation
- String handling

### `advanced_test.lisp`
Demonstrates advanced testing features:
- Error handling (should-throw, should-not-throw)
- Recursive functions (factorial, fibonacci)
- Edge cases
- Skipped tests (xit, xdescribe)
- Custom error messages
- Nested describe blocks
- Stateful operations

**Key Features:**
- Complete test coverage patterns
- Skip functionality
- Nested test organization

### `four_horsemen_test.lisp`
Deep dive into ChrysaLisp's core primitives:
- The First Horseman: `map`
- The Second Horseman: `filter`
- The Third Horseman: `reduce`
- The Fourth Horseman: `some`
- Combining primitives
- Performance considerations

**Key Features:**
- Philosophy-aligned testing
- Primitive combinations
- Comprehensive primitive coverage

## Running the Examples

### Run All Examples
```bash
./run_tui.sh -n 1 cmd/test-runner.lisp
```

### Run Specific Example
```bash
./run_tui.sh -n 1 cmd/test-runner.lisp examples/test/basic_test.lisp
```

### Run with Filter
```bash
# Only run tests with "Arithmetic" in the name
./run_tui.sh -n 1 cmd/test-runner.lisp -f "Arithmetic"

# Only run tests about the Four Horsemen
./run_tui.sh -n 1 cmd/test-runner.lisp -f "Horsemen"
```

### Run with Verbose Output
```bash
./run_tui.sh -n 1 cmd/test-runner.lisp -v
```

## Expected Output

When you run all examples, you should see:
- **Total Tests**: ~80-90 tests
- **Passed**: Most tests pass
- **Failed**: 1-2 intentional failures (in basic_test.lisp)
- **Skipped**: A few skipped tests (from advanced_test.lisp)

## Creating Your Own Tests

Use these examples as templates:

1. **Start Simple**: Copy `basic_test.lisp` structure
2. **Add Collections**: Reference `collections_test.lisp`
3. **Handle Errors**: See `advanced_test.lisp`
4. **Follow Philosophy**: Study `four_horsemen_test.lisp`

### Template Structure

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; My Feature Test
; Description of what this tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

;;;;;;;;;;;;;;;;;;;;;;;;
; Test functions/setup
;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-function (x)
	(* x 2))

;;;;;;;;;;;;;;;;;;;;;;;;
; Test Suite
;;;;;;;;;;;;;;;;;;;;;;;;

(describe "My Feature"
	(it "should work as expected"
		(should-equal (my-function 5) 10))

	(it "should handle edge cases"
		(should-equal (my-function 0) 0)))
```

## Test File Naming Convention

- End test files with `_test.lisp`
- Use descriptive names: `feature_name_test.lisp`
- Place in `examples/test/` directory

Examples:
- `basic_test.lisp`
- `collections_test.lisp`
- `my_feature_test.lisp`

## Learning Path

1. **Read** `basic_test.lisp` - Understand basic structure
2. **Run** all tests - See the output format
3. **Modify** a test - Change an assertion
4. **Write** your own test - Create a new file
5. **Explore** `four_horsemen_test.lisp` - Learn ChrysaLisp philosophy

## Tips

- **Start with `it`**: Write individual tests first
- **Group with `describe`**: Organize related tests
- **Use `xit` to skip**: Temporarily disable failing tests
- **Add messages**: Custom messages help debugging
- **Test edge cases**: Empty lists, nil, zero, negative numbers
- **One assertion per test**: Keep tests focused

## See Also

- [Test Framework Documentation](../../docs/test_framework.md)
- [ChrysaLisp Philosophy](../../docs/ai_digest/the_philosophy.md)
- [Modern Lisp Guide](../../docs/ai_digest/modern_lisp.md)

# Expression Parser/Serializer

A comprehensive expression parser and serializer for ChrysaLisp with support for multiple input and output formats.

## Features

### Input Formats
- **S-expressions**: Native Lisp notation `(+ 1 (* 2 3))`
- **Infix**: Standard mathematical notation `1 + 2 * 3`
- **Prefix**: Prefix notation `+ 1 * 2 3`
- **JSON**: JSON-like format (partial support)
- **Auto**: Automatic format detection

### Output Formats
- **sexp**: Canonical S-expression format
- **pretty**: Pretty-printed with indentation
- **json**: JSON representation
- **xml**: XML representation
- **dot**: GraphViz DOT format for visualization
- **infix**: Infix notation with proper precedence
- **prefix**: Prefix notation
- **ast**: ASCII art abstract syntax tree
- **tree**: Hierarchical tree view
- **rainbow**: Rainbow-colored pretty print (ANSI colors)

### Capabilities
- Expression evaluation with arithmetic, comparison, logical, and mathematical operations
- Expression statistics (depth, node count, operator frequency)
- Proper operator precedence handling
- Support for nested expressions
- Special functions: factorial, fibonacci, gcd, lcm, min, max, abs, sqrt
- Constants: pi, e, phi

## Usage

### Basic Usage

```bash
# Parse and display an expression
expr '(+ 1 (* 2 3))'

# Parse infix notation
expr '1 + 2 * 3' -i infix

# Convert to different formats
expr '(+ 1 2)' -o json
expr '(+ 1 2)' -o xml
expr '(+ 1 2)' -o dot
expr '(+ 1 2)' -o infix

# Evaluate an expression
expr '(+ 1 (* 2 3))' --eval

# Show statistics
expr '(+ 1 (* 2 3))' --stats

# Pretty print
expr '(+ 1 (* 2 3))' -o pretty

# ASCII art tree
expr '(+ 1 (* 2 3))' -o ast

# Rainbow colored output
expr '(+ 1 (* 2 3))' -o rainbow -c
```

### Multi-Format View

Show the expression in all formats at once:

```bash
expr '(+ 1 (* 2 3))' --multi
```

### Processing Files

```bash
# Process expressions from a file
cat examples/expr/simple.txt | expr -o pretty

# Evaluate all expressions in a file
cat examples/expr/simple.txt | expr --eval

# Show statistics for expressions
cat examples/expr/complex.txt | expr --stats

# Convert all expressions to infix
cat examples/expr/simple.txt | expr -o infix
```

### Visualization

Generate a GraphViz visualization:

```bash
# Create a PNG image (requires graphviz)
expr '(+ (* 2 3) (/ 8 4))' -o dot > expr.dot
dot -Tpng expr.dot > expr.png

# Or in one command
expr '(+ (* 2 3) (/ 8 4))' -o dot | dot -Tpng > expr.png
```

### Examples

#### Simple Arithmetic

```bash
expr '(+ 1 2)' --eval
# Result: 3

expr '(* 3 4)' --eval
# Result: 12

expr '(+ 1 (* 2 3))' --eval
# Result: 7
```

#### Infix to S-expression

```bash
expr '1 + 2 * 3' -i infix -o sexp
# Output: (+ 1 (* 2 3))

expr '(1 + 2) * 3' -i infix -o sexp
# Output: (* (+ 1 2) 3)
```

#### Special Functions

```bash
expr '(factorial 5)' --eval
# Result: 120

expr '(fib 10)' --eval
# Result: 55

expr '(gcd 12 18)' --eval
# Result: 6
```

#### Complex Expressions

```bash
expr '(+ (* 2 (+ 3 4)) (/ 10 (- 5 3)))' --eval --stats
# Evaluates and shows statistics

expr '(+ 1 2 3 4 5 6 7 8 9 10)' --eval
# Result: 55
```

## Testing

Run the comprehensive test suite:

```bash
expr_test
# Runs all tests

expr_test --verbose
# Shows all test results

expr_test --timing
# Shows timing information
```

## Supported Operations

### Arithmetic
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division
- `%` Modulo
- `^` Power

### Comparison
- `=` Equal
- `<` Less than
- `>` Greater than
- `<=` Less than or equal
- `>=` Greater than or equal
- `!=` Not equal

### Logical
- `and` Logical AND
- `or` Logical OR
- `not` Logical NOT

### Mathematical
- `abs` Absolute value
- `sqrt` Square root
- `min` Minimum value
- `max` Maximum value
- `floor` Floor function
- `ceil` Ceiling function

### Special
- `factorial` Factorial
- `fib` Fibonacci number
- `gcd` Greatest common divisor
- `lcm` Least common multiple
- `sum` Sum of arguments
- `product` Product of arguments
- `mean` Mean average

### Constants
- `pi` π ≈ 3.14159
- `e` Euler's number ≈ 2.71828
- `phi` Golden ratio ≈ 1.61803

## Architecture

The expression parser is organized into modular libraries:

- `lib/expr/parser.inc` - Multi-format parser
- `lib/expr/serializer.inc` - Multi-format serializer
- `lib/expr/eval.inc` - Expression evaluator
- `lib/expr/stats.inc` - Expression statistics
- `cmd/expr.lisp` - Command-line interface
- `cmd/expr_test.lisp` - Test suite

## Examples in This Directory

- `simple.txt` - Simple arithmetic expressions
- `infix.txt` - Infix notation examples
- `complex.txt` - Complex nested expressions

## Performance

The expression parser uses ChrysaLisp's efficient sequence operations and is designed for:
- Fast parsing with minimal allocations
- Efficient tree traversal
- O(1) operator lookups
- Optimized serialization

## License

Part of the ChrysaLisp project.

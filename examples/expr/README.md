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
- **Symbolic differentiation** - compute derivatives with respect to any variable
- **Expression simplification** - algebraic simplification and constant folding
- **Expression expansion** - distribute products over sums
- Expression statistics (depth, node count, operator frequency)
- Proper operator precedence handling
- Support for nested expressions
- Special functions: factorial, fibonacci, gcd, lcm, min, max, abs, sqrt
- Trigonometric functions: sin, cos, tan
- Exponential and logarithmic: exp, ln, log
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

### Symbolic Differentiation

Compute derivatives symbolically:

```bash
# Basic derivative
expr '(* x x)' -d x
# Output: Derivative d/dx: (+ (* x 1) (* x 1))

# With simplification
expr '(* x x)' -d x -p
# Output: Derivative d/dx: (+ (* x 1) (* x 1))
#         Simplified: (* 2 x)

# Power rule
expr '(^ x 3)' -d x -p
# Output: d/dx x^3 = 3*x^2

# Product rule
expr '(* (sin x) (cos x))' -d x -p

# Chain rule
expr '(sin (* 2 x))' -d x -p

# Nth derivatives
expr '(^ x 4)' -d x -n 2 -p
# Second derivative: 12*x^2

expr '(^ x 5)' -d x -n 3 -p
# Third derivative: 60*x^2

# From file
cat examples/expr/derivatives.txt | expr -d x -p
cat examples/expr/calculus.txt | expr -d x -p
```

### Expression Simplification

Simplify expressions algebraically:

```bash
# Basic simplification
expr '(+ 0 x)' -p
# Output: x

expr '(* 1 x)' -p
# Output: x

expr '(* 0 x)' -p
# Output: 0

# Constant folding
expr '(+ 1 2 3 4)' -p
# Output: 10

# Nested simplification
expr '(+ (* 0 x) (* 1 y))' -p
# Output: y

# Simplify derivatives
expr '(* 2 (* (^ x 1) 1))' -p
# Output: (* 2 x)

# From file
cat examples/expr/simplification.txt | expr -p
```

### Expression Expansion

Expand products over sums:

```bash
# Distributive property
expr '(* (+ 1 2) x)' -x
# Output: (+ (* 1 x) (* 2 x))

expr '(* x (+ y z))' -x
# Output: (+ (* x y) (* x z))
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

## Using as a Library

The expression parser can be imported and used programmatically in your own ChrysaLisp applications:

### Basic Usage

```lisp
(import "lib/expr/parser.inc")
(import "lib/expr/eval.inc")
(import "lib/expr/serializer.inc")

(defun calculate (expr-str)
  ; Parse and evaluate an expression
  (defq parsed (expr-parse expr-str 'auto))
  (expr-eval parsed))

; Use it
(print (calculate "(+ 1 (* 2 3))"))  ; 7
(print (calculate "2 * 3 + 4"))      ; 10
```

### Symbolic Differentiation in Your Code

```lisp
(import "lib/expr/parser.inc")
(import "lib/expr/diff.inc")
(import "lib/expr/simplify.inc")
(import "lib/expr/serializer.inc")

(defun derivative (expr-str var-name)
  ; Compute and simplify a derivative
  (defq parsed (expr-parse expr-str 'sexp))
  (defq deriv (diff parsed (sym var-name)))
  (defq simplified (simplify deriv))
  ; Return as infix notation
  (expr-serialize simplified 'infix :nil))

; Use it
(print (derivative "(* x x)" "x"))           ; 2 * x
(print (derivative "(^ x 3)" "x"))           ; 3 * x ^ 2
(print (derivative "(sin (* 2 x))" "x"))     ; 2 * cos(2 * x)
```

### Building a Calculator

```lisp
(import "lib/expr/parser.inc")
(import "lib/expr/eval.inc")
(import "lib/expr/stats.inc")

(defclass Calculator ()
  (defmethod :eval (expr-str)
    ; Parse and evaluate
    (expr-eval (expr-parse expr-str 'auto)))

  (defmethod :analyze (expr-str)
    ; Get statistics about an expression
    (defq parsed (expr-parse expr-str 'sexp))
    (expr-stats parsed))

  (defmethod :convert (expr-str from-fmt to-fmt)
    ; Convert between formats
    (defq parsed (expr-parse expr-str from-fmt))
    (expr-serialize parsed to-fmt :nil)))

; Use it
(defq calc (Calculator))
(print (. calc :eval "1 + 2 * 3"))                    ; 7
(print (. calc :convert "1 + 2" 'infix 'sexp))        ; (+ 1 2)
```

### Computer Algebra System Example

```lisp
(import "lib/expr/parser.inc")
(import "lib/expr/diff.inc")
(import "lib/expr/simplify.inc")
(import "lib/expr/eval.inc")

(defclass CAS ()
  (defmethod :diff (expr-str var)
    ; Differentiate and simplify
    (defq parsed (expr-parse expr-str 'sexp))
    (simplify (diff parsed (sym var))))

  (defmethod :nth-diff (expr-str var n)
    ; Nth derivative
    (defq parsed (expr-parse expr-str 'sexp))
    (simplify (nth-derivative parsed (sym var) n)))

  (defmethod :taylor (expr-str var point order)
    ; Taylor series expansion (simplified version)
    (defq terms (list))
    (defq factorial 1)
    (times order
      (defq deriv (. this :nth-diff expr-str var !))
      (push terms (list '/ deriv factorial))
      (setq factorial (* factorial (inc !))))
    (list '+ (splice terms))))

; Use it
(defq cas (CAS))
(defq d1 (. cas :diff "(* x x)" 'x))           ; (* 2 x)
(defq d2 (. cas :nth-diff "(^ x 4)" 'x 2))     ; (* 12 (^ x 2))
```

### Expression Transformation Pipeline

```lisp
(import "lib/expr/parser.inc")
(import "lib/expr/diff.inc")
(import "lib/expr/simplify.inc")
(import "lib/expr/expand.inc")

(defun transform-pipeline (expr-str)
  ; Multi-stage transformation
  (defq parsed (expr-parse expr-str 'sexp))

  ; Stage 1: Expand
  (defq expanded (expand parsed))
  (print "Expanded: " expanded)

  ; Stage 2: Differentiate
  (defq deriv (diff expanded 'x))
  (print "Derivative: " deriv)

  ; Stage 3: Simplify
  (defq simplified (simplify deriv))
  (print "Simplified: " simplified)

  simplified)

; Use it
(transform-pipeline "(* (+ x 1) (+ x 2))")
; Expanded: (+ (* x (+ x 2)) (* 1 (+ x 2)))
; Derivative: ...
; Simplified: (+ (* 2 x) 3)
```

### API Reference

**Parser Functions:**
- `(expr-parse str format)` - Parse expression from string
- `(expr-tokenize str)` - Tokenize an expression

**Differentiation Functions:**
- `(diff expr var)` - Compute derivative
- `(nth-derivative expr var n)` - Compute nth derivative
- `(partial-diff expr vars)` - Partial derivatives
- `(gradient expr vars)` - Gradient vector

**Simplification Functions:**
- `(simplify expr)` - Simplify expression
- `(expand expr)` - Expand products over sums

**Evaluation Functions:**
- `(expr-eval expr)` - Evaluate expression

**Statistics Functions:**
- `(expr-stats expr)` - Get statistics map
- `(expr-depth expr)` - Tree depth
- `(expr-node-count expr)` - Count nodes
- `(expr-complexity expr)` - Complexity score

**Serialization Functions:**
- `(expr-serialize expr format color)` - Serialize to format

All libraries follow the ChrysaLisp module pattern with `(env-push)` and `(export)` for clean encapsulation.

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

### Trigonometric
- `sin` Sine
- `cos` Cosine
- `tan` Tangent

### Exponential/Logarithmic
- `exp` Exponential (e^x)
- `ln` Natural logarithm
- `log` Base-10 logarithm

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

- `lib/expr/parser.inc` - Multi-format parser with tokenizer
- `lib/expr/serializer.inc` - 10 different output formats
- `lib/expr/eval.inc` - Expression evaluator with 30+ operations
- `lib/expr/stats.inc` - Statistical analysis tools
- `lib/expr/diff.inc` - Symbolic differentiation engine
- `lib/expr/simplify.inc` - Algebraic simplification & expansion
- `cmd/expr.lisp` - Command-line interface
- `cmd/expr_test.lisp` - Comprehensive test suite (100+ tests)

## Examples in This Directory

- `simple.txt` - Simple arithmetic expressions
- `infix.txt` - Infix notation examples
- `complex.txt` - Complex nested expressions
- `derivatives.txt` - Expressions for differentiation practice
- `calculus.txt` - Classic calculus examples (chain rule, product rule, etc.)
- `simplification.txt` - Expressions demonstrating simplification

## Performance

The expression parser uses ChrysaLisp's efficient sequence operations and is designed for:
- Fast parsing with minimal allocations
- Efficient tree traversal
- O(1) operator lookups
- Optimized serialization

## License

Part of the ChrysaLisp project.

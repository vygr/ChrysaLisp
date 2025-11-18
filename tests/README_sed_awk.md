# sed and awk for ChrysaLisp

This directory contains test files and documentation for the sed and awk command implementations in ChrysaLisp.

## sed - Stream Editor

The `sed` command is a stream editor for filtering and transforming text.

### Usage

```
sed [options] script [path] ...
```

### Options

- `-h, --help`: Display help information
- `-e, --expression script`: Add script to commands
- `-n, --quiet`: Suppress automatic printing

### Supported Commands

#### Substitute (s)
```bash
# Replace first occurrence
sed 's/pattern/replacement/' file.txt

# Replace all occurrences (global)
sed 's/pattern/replacement/g' file.txt
```

#### Delete (d)
```bash
# Delete lines matching pattern
sed '/pattern/d' file.txt

# Delete specific line
sed '5d' file.txt

# Delete range of lines
sed '1,10d' file.txt
```

#### Print (p)
```bash
# Print only matching lines (suppress auto-print with -n)
sed -n '/pattern/p' file.txt

# Print specific line
sed -n '5p' file.txt
```

### Address Types

- **Line number**: `5` (line 5)
- **Pattern**: `/pattern/` (lines matching pattern)
- **Last line**: `$`
- **Range**: `1,10` or `/start/,/end/`

### Examples

```bash
# Replace 'foo' with 'bar'
sed 's/foo/bar/' input.txt

# Replace all occurrences of 'foo' with 'bar'
sed 's/foo/bar/g' input.txt

# Print only lines containing 'error'
sed -n '/error/p' log.txt

# Delete lines 1 through 10
sed '1,10d' file.txt

# Delete lines containing 'debug'
sed '/debug/d' log.txt
```

## awk - Pattern Scanning and Processing

The `awk` command is for pattern scanning and text processing.

### Usage

```
awk [options] 'program' [path] ...
```

### Options

- `-h, --help`: Display help information
- `-F, --field-sep sep`: Set field separator (default: whitespace)
- `-v, --var name=value`: Set variable (not fully implemented)

### Program Structure

```awk
BEGIN { action }       # Execute before processing lines
pattern { action }     # Execute for matching lines
{ action }            # Execute for all lines
END { action }        # Execute after processing all lines
```

### Built-in Variables

- `NR`: Current line number
- `NF`: Number of fields in current line
- `FS`: Field separator (input)
- `$0`: Whole line
- `$1, $2, ...`: Individual fields

### Patterns

- `/regexp/`: Match regular expression
- `expression`: Evaluate expression (e.g., `NR > 5`)
- `pattern1, pattern2`: Range (not yet implemented)

### Actions

- `print`: Print whole line
- `print $1`: Print specific field
- `print $1, $2`: Print multiple fields
- `print "text"`: Print literal text
- `variable = value`: Variable assignment
- `variable += value`: Compound assignment

### Operators

- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`
- Arithmetic: `+`, `-` (in expressions)

### Examples

```bash
# Print first field of each line
awk '{print $1}' file.txt

# Print multiple fields
awk '{print $1, $3}' file.txt

# Use custom field separator (colon)
awk -F: '{print $1, $3}' /etc/passwd

# Pattern matching
awk '/error/ {print NR, $0}' log.txt

# Conditional processing
awk 'NR > 5 && NR < 10' file.txt

# Sum values
awk 'BEGIN {sum=0} {sum+=$2} END {print sum}' data.txt

# Print with line numbers
awk '{print NR, $0}' file.txt

# Multiple conditions
awk '$2 > 10 && $3 == "red" {print $1}' data.txt
```

## Test Files

- `test_data.txt`: Sample data file with three columns
- `test_sed.txt`: Sample text file for sed testing
- `test_sed_awk.lisp`: Test suite (requires ChrysaLisp runtime)

## Testing

### Manual Testing

#### sed tests:
```bash
# Test substitute
echo "This is a test" | ./sed 's/test/example/'

# Test global substitute
echo "foo foo foo" | ./sed 's/foo/bar/g'

# Test delete
cat tests/test_sed.txt | ./sed '/error/d'

# Test quiet mode with print
cat tests/test_sed.txt | ./sed -n '/error/p'
```

#### awk tests:
```bash
# Test field printing
cat tests/test_data.txt | ./awk '{print $1}'

# Test pattern matching
cat tests/test_data.txt | ./awk '/red/ {print $1, $2}'

# Test BEGIN/END
cat tests/test_data.txt | ./awk 'BEGIN {sum=0} {sum+=$2} END {print sum}'

# Test field separator
echo "a:b:c" | ./awk -F: '{print $2}'

# Test NR
cat tests/test_data.txt | ./awk '{print NR, $1}'
```

## Implementation Notes

### sed

The implementation includes:
- Basic substitute command with global flag
- Delete command
- Print command
- Line number and pattern addressing
- Range addressing
- Quiet mode (-n flag)

**Limitations:**
- Limited sed command set (only s, d, p)
- No hold space/pattern space operations
- No multi-line operations
- Simplified regex support (uses ChrysaLisp regexp)

### awk

The implementation includes:
- Field splitting by whitespace or custom separator
- Built-in variables (NR, NF, $0, $1, $2, ...)
- BEGIN/END blocks
- Pattern matching with regular expressions
- Simple expressions and comparisons
- Variable assignment and compound operators
- Multiple field printing

**Limitations:**
- No arrays
- Limited expression evaluation
- No user-defined functions
- No printf formatting
- Simplified pattern matching
- No range patterns (pattern1, pattern2)
- Limited string operations

## Contributing

These implementations follow ChrysaLisp coding conventions:
- Functions use hyphens: `parse-script`
- Variables use underscores: `line_num`
- Constants use `+` prefix: `+max_lines`
- Keywords use `:` prefix: `:pattern`

See CONTRIBUTIONS.md for full contribution guidelines.

## License

Part of the ChrysaLisp project.

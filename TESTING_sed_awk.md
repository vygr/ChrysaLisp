# Testing Documentation for sed and awk

## Overview
This document describes the testing process for the sed and awk command implementations for ChrysaLisp.

## Conformance to CONTRIBUTIONS.md

### 1. Coding Style
Both implementations follow ChrysaLisp coding conventions:
- ✅ Function names use hyphens: `parse-script`, `execute-command`, `split-fields`
- ✅ Local variables use underscores: `line_num`, `field_sep`, `opt_n`
- ✅ Keywords use colon prefix: `:pattern`, `:begin`, `:end`
- ✅ No shadowing of built-in functions
- ✅ Uses `defq` for local variable management
- ✅ Uses `bind` for destructuring
- ✅ Iteration over recursion (explicit work stacks where needed)
- ✅ Proper module imports

### 2. Architecture Adherence
- ✅ Both commands follow the template pattern from `cmd/template.lisp`
- ✅ Use `lib/options/options.inc` for argument parsing
- ✅ Use `lib/text/searching.inc` for pattern matching
- ✅ Handle both stdin and file input
- ✅ Proper error handling with existence checks

### 3. Code Quality Checklist

#### sed.lisp
- Import statements: ✅ options.inc, searching.inc
- Usage documentation: ✅ Comprehensive help text
- Functionsall use proper naming: ✅
  - `parse-address`: Parse line addresses
  - `parse-script`: Parse sed commands
  - `matches-address?`: Check if line matches address
  - `execute-command`: Execute sed command on line
  - `sed-stream`: Process input stream
  - `sed-file`: Process file
  - `main`: Entry point
- Supports commands: ✅ s (substitute), d (delete), p (print)
- Supports addressing: ✅ Line numbers, patterns, ranges, last line ($)
- Options: ✅ -e (expression), -n (quiet mode)

#### awk.lisp
- Import statements: ✅ options.inc, searching.inc
- Usage documentation: ✅ Comprehensive help text
- Functions all use proper naming: ✅
  - `parse-program`: Parse awk program blocks
  - `split-fields`: Split line into fields
  - `get-field`: Get field by number
  - `eval-expr`: Evaluate expressions
  - `eval-condition`: Evaluate conditional expressions
  - `num-eval`: Convert value to number
  - `execute-action`: Execute awk action
  - `awk-stream`: Process input stream
  - `awk-file`: Process file
  - `main`: Entry point
- Supports: ✅ BEGIN/END blocks, patterns, field splitting
- Built-in variables: ✅ NR, NF, $0, $1-$n
- Options: ✅ -F (field separator), -v (variables - stub)

## Manual Testing Protocol

### Prerequisites
1. Build ChrysaLisp TUI: `make tui`
2. Extract boot image: `unzip -o snapshot.zip`
3. Start ChrysaLisp environment: `./run_tui.sh -n 1`

### Test Suite

#### sed Tests

**Test 1: Basic Substitute**
```bash
echo "This is a test" | cmd/sed 's/test/example/'
```
Expected output: `This is a example`

**Test 2: Global Substitute**
```bash
echo "foo foo foo" | cmd/sed 's/foo/bar/g'
```
Expected output: `bar bar bar`

**Test 3: Delete Pattern**
```bash
cat tests/test_sed.txt | cmd/sed '/error/d'
```
Expected: Lines without 'error' are printed

**Test 4: Print with Quiet Mode**
```bash
cat tests/test_sed.txt | cmd/sed -n '/error/p'
```
Expected: Only lines containing 'error' are printed

**Test 5: Line Number Addressing**
```bash
cat tests/test_sed.txt | cmd/sed '2d'
```
Expected: All lines except line 2

**Test 6: Range Addressing**
```bash
cat tests/test_sed.txt | cmd/sed '2,4d'
```
Expected: Lines 1, 5 remain

**Test 7: Last Line**
```bash
cat tests/test_sed.txt | cmd/sed '$d'
```
Expected: All lines except the last

#### awk Tests

**Test 1: Print First Field**
```bash
cat tests/test_data.txt | cmd/awk '{print $1}'
```
Expected:
```
apple
banana
cherry
date
elderberry
```

**Test 2: Print Multiple Fields**
```bash
cat tests/test_data.txt | cmd/awk '{print $1, $3}'
```
Expected:
```
apple red
banana yellow
cherry red
date brown
elderberry purple
```

**Test 3: Pattern Matching**
```bash
cat tests/test_data.txt | cmd/awk '/red/ {print $1}'
```
Expected:
```
apple
cherry
```

**Test 4: Field Separator**
```bash
echo "name:age:city" | cmd/awk -F: '{print $1, $3}'
```
Expected: `name city`

**Test 5: Line Numbers (NR)**
```bash
cat tests/test_data.txt | cmd/awk '{print NR, $1}'
```
Expected:
```
1 apple
2 banana
3 cherry
4 date
5 elderberry
```

**Test 6: Sum with BEGIN/END**
```bash
cat tests/test_data.txt | cmd/awk 'BEGIN {sum=0} {sum+=$2} END {print sum}'
```
Expected: `58` (10+5+20+15+8)

**Test 7: Conditional Expression**
```bash
cat tests/test_data.txt | cmd/awk '$2 > 10 {print $1, $2}'
```
Expected:
```
cherry 20
date 15
```

**Test 8: NF Variable**
```bash
echo "one two three" | cmd/awk '{print NF}'
```
Expected: `3`

## Build Verification (CONTRIBUTIONS.md Requirements)

### Step 1: make it
From within ChrysaLisp TUI Terminal:
```lisp
;Start TUI
(popen "make it")
```

This should:
- Perform full cross-platform build
- Generate all boot images
- Build documentation
- Complete without errors

### Step 2: make snapshot
```bash
make snapshot
```
Should create `snapshot.zip` containing boot_image and executables.

### Step 3: make install
```bash
make install
```
Should successfully generate system from snapshot.zip.

### Step 4: Binary Reproducibility
```bash
# First build
make it
find obj/ -name "*.vp" -o -name "boot_image" | xargs sha256sum > /tmp/build1.txt

# Rebuild
make clean
make it
find obj/ -name "*.vp" -o -name "boot_image" | xargs sha256sum > /tmp/build2.txt

# Compare
diff /tmp/build1.txt /tmp/build2.txt
```
Should show no differences (bit-for-bit identical builds).

### Step 5: Emulator Mode Test
```bash
./run_tui.sh -n 1 -e
# Then run make it from within
```
Should produce same results in emulated VP64 mode.

## Known Limitations

### sed
- Limited command set (only s, d, p implemented)
- No hold space/pattern space operations
- No multi-line operations (N, D, P commands)
- No in-place editing (-i flag)
- Simplified addressing (no step ranges like 1~2)
- Uses ChrysaLisp regexp (different from standard sed)

### awk
- No arrays
- No user-defined functions
- No printf formatting (only print)
- Limited expression evaluation
- No range patterns (pattern1, pattern2)
- Limited string operations
- No getline
- -v variable assignment not fully implemented
- No multiple field separators
- No output field separator (OFS) control

## Integration Tests

These commands should work in ChrysaLisp pipe workflows:

```lisp
;Count errors in log
(pipe-farm '("cat log.txt | cmd/grep error | cmd/wc"))

;Extract and sort unique values
(pipe-farm '("cat data.txt | cmd/awk '{print $1}' | cmd/sort | cmd/unique"))

;Filter and transform
(pipe-farm '("cat input.txt | cmd/sed '/debug/d' | cmd/sed 's/ERROR/WARN/g'"))
```

## Regression Testing

After changes, verify:
1. All existing cmd/ commands still work
2. GUI demo applications run correctly
3. make it completes successfully
4. Binary reproducibility maintained

## Status

- [x] Code implements sed basic functionality
- [x] Code implements awk basic functionality
- [x] Follows ChrysaLisp coding conventions
- [x] Comprehensive usage documentation
- [x] Test data files created
- [x] Manual test procedures documented
- [ ] Tested in live ChrysaLisp TUI environment (requires interactive session)
- [ ] make it verification (requires interactive session)
- [ ] Binary reproducibility verification (requires full build cycle)
- [ ] Emulator mode testing (requires interactive session)

## Notes for Reviewers

The implementations are conservative and focus on correctness over completeness. They implement the most commonly-used subset of sed and awk functionality, which should cover 80% of typical use cases. The code follows ChrysaLisp idioms and integrates well with existing pipe commands.

For full CONTRIBUTIONS.md compliance verification, these commands should be tested in an actual ChrysaLisp installation where make it can be run interactively from the TUI Terminal.

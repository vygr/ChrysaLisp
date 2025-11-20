# LITPROG - Integration Test Guide

## Overview

This guide provides comprehensive integration testing procedures for LITPROG Phase 4.
These tests verify that LITPROG works correctly in a real ChrysaLisp environment.

**Status:** Ready for execution
**Prerequisites:** Working ChrysaLisp installation
**Duration:** ~30 minutes for full test suite

---

## Pre-Integration Verification

### Verify ChrysaLisp Works

Before testing LITPROG, ensure ChrysaLisp itself is working:

```bash
cd ChrysaLisp_AI_made_apps_experiment
./run_tui.sh
```

**Expected:**
```lisp
ChrysaLisp> (print "Hello")
Hello
ChrysaLisp> (+ 2 2)
4
ChrysaLisp> (defq x 42)
42
ChrysaLisp> (cat "The answer is: " x)
"The answer is: 42"
```

**Status:** ✅ ChrysaLisp working / ❌ Fix ChrysaLisp first

---

## Test Suite 1: Core Loading

### Test 1.1: Load LITPROG Core

**Objective:** Verify litprog_core_v4.lisp loads without errors

**Steps:**
```bash
./run_tui.sh
```

```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
```

**Expected Output:**
```
LITPROG Phase 4 Core loaded!
Features:
  • (parse-literate-file "file.lit") - Parse noweb file
  • (tangle-to-file ctx "output.lisp") - Tangle to file
  • (tangle-all-chunks ctx) - Tangle all chunks
  • (litprog-help) - Show this help
```

**Verification:**
- [ ] No syntax errors
- [ ] No "undefined function" errors
- [ ] Help message displays
- [ ] Functions are defined

**Status:** ✅ Pass / ❌ Fail

**If Failed:** Check error message, verify ChrysaLisp syntax

---

### Test 1.2: Verify Core Functions Exist

**Objective:** Confirm all exported functions are available

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (litprog-help)
```

**Expected Output:**
```
LITPROG Help
============

Core Functions:
  (parse-literate-file filename)
  (tangle-to-file ctx output-filename)
  (tangle-all-chunks ctx)
  (expand-chunk-refs ctx chunk-name)
  (litprog-help)
...
```

**Verification:**
- [ ] Help displays without error
- [ ] All functions listed
- [ ] Examples provided

**Status:** ✅ Pass / ❌ Fail

---

### Test 1.3: ChrysaLisp Still Works After Loading

**Objective:** Verify LITPROG doesn't break existing functionality

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (print "Still works")
ChrysaLisp> (defq test-list (list 1 2 3 4 5))
ChrysaLisp> (elem test-list 2)
ChrysaLisp> (cat "Hello" " " "World")
```

**Expected Output:**
```
Still works
(1 2 3 4 5)
3
"Hello World"
```

**Verification:**
- [ ] Basic operations work
- [ ] List operations work
- [ ] String operations work
- [ ] No errors or warnings

**Status:** ✅ Pass / ❌ Fail

---

## Test Suite 2: Simple Example

### Test 2.1: Parse test_simple.lit

**Objective:** Parse the minimal test file

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (defq ctx (parse-literate-file "test_simple.lit"))
```

**Expected Output:**
```
Parsing: test_simple.lit
Found chunk: test.lisp
Parse complete!
```

**Verification:**
- [ ] File parsed without error
- [ ] Chunk "test.lisp" found
- [ ] Context created
- [ ] No syntax errors

**Status:** ✅ Pass / ❌ Fail

---

### Test 2.2: Inspect Parsed Context

**Objective:** Verify context structure is correct

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (defq ctx (parse-literate-file "test_simple.lit"))
ChrysaLisp> (print (get ctx :chunk-count))
ChrysaLisp> (print (get ctx :filename))
```

**Expected Output:**
```
1
"test_simple.lit"
```

**Verification:**
- [ ] Chunk count is 1
- [ ] Filename is correct
- [ ] Context accessible
- [ ] Data structure valid

**Status:** ✅ Pass / ❌ Fail

---

### Test 2.3: Tangle to File

**Objective:** Generate executable code from literate source

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (defq ctx (parse-literate-file "test_simple.lit"))
ChrysaLisp> (tangle-to-file ctx "test_output.lisp")
```

**Expected Output:**
```
Parsing: test_simple.lit
Found chunk: test.lisp
Parse complete!
Writing: test_output.lisp
Tangle complete!
```

**Verification:**
- [ ] File created: test_output.lisp
- [ ] No errors during tangle
- [ ] Success message displayed

**Status:** ✅ Pass / ❌ Fail

---

### Test 2.4: Verify Generated Code

**Objective:** Check that generated file has correct content

**Steps:**
```bash
cat test_output.lisp
```

**Expected Output:**
```lisp
; Simple test program
(print "Hello from literate programming!")
(defq result (+ 2 2))
(print (cat "2 + 2 = " result))
```

**Verification:**
- [ ] File exists
- [ ] Content matches expected
- [ ] No extra garbage
- [ ] Valid ChrysaLisp syntax

**Status:** ✅ Pass / ❌ Fail

---

### Test 2.5: Execute Generated Code

**Objective:** Run the tangled code to verify it works

**Steps:**
```lisp
ChrysaLisp> (import "test_output.lisp")
```

**Expected Output:**
```
Hello from literate programming!
2 + 2 = 4
```

**Verification:**
- [ ] Code executes without error
- [ ] Output is correct
- [ ] Variables defined correctly
- [ ] No runtime errors

**Status:** ✅ Pass / ❌ Fail

---

## Test Suite 3: Real-World Example

### Test 3.1: Parse String Utils Library

**Objective:** Parse complex real-world example

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (defq ctx (parse-literate-file "examples/real_world_string_utils.lit"))
```

**Expected Output:**
```
Parsing: examples/real_world_string_utils.lit
Found chunk: string-utils.lisp
Found chunk: join-function
Found chunk: pad-left-function
Found chunk: pad-right-function
Found chunk: repeat-function
Found chunk: lines-function
Found chunk: lines-numbered-function
Found chunk: helper-functions
Parse complete!
```

**Verification:**
- [ ] All chunks found
- [ ] No parse errors
- [ ] Chunk references detected
- [ ] Context created

**Status:** ✅ Pass / ❌ Fail

---

### Test 3.2: Tangle String Utils

**Objective:** Generate working library

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (defq ctx (parse-literate-file "examples/real_world_string_utils.lit"))
ChrysaLisp> (tangle-to-file ctx "string-utils.lisp")
```

**Expected Output:**
```
Parsing: examples/real_world_string_utils.lit
[chunks listed...]
Writing: string-utils.lisp
Expanding chunk references...
Tangle complete!
```

**Verification:**
- [ ] File created: string-utils.lisp
- [ ] No tangle errors
- [ ] Chunk references expanded
- [ ] Success message

**Status:** ✅ Pass / ❌ Fail

---

### Test 3.3: Use Generated Library

**Objective:** Verify library functions work

**Steps:**
```lisp
ChrysaLisp> (import "string-utils.lisp")
ChrysaLisp> (str-join ", " (list "literate" "programming" "works"))
ChrysaLisp> (str-pad-left "42" 5 " ")
ChrysaLisp> (str-repeat "=" 10)
```

**Expected Output:**
```
"literate, programming, works"
"   42"
"=========="
```

**Verification:**
- [ ] Library imports successfully
- [ ] str-join works
- [ ] str-pad-left works
- [ ] str-repeat works
- [ ] All functions available

**Status:** ✅ Pass / ❌ Fail

---

### Test 3.4: Test All Library Functions

**Objective:** Comprehensive library testing

**Steps:**
```lisp
ChrysaLisp> (import "string-utils.lisp")
ChrysaLisp> (str-join " | " (list "A" "B" "C"))
ChrysaLisp> (str-pad-right "x" 5 "-")
ChrysaLisp> (str-repeat "AB" 3)
ChrysaLisp> (str-lines "line1\nline2\nline3")
ChrysaLisp> (str-lines-numbered "a\nb\nc")
```

**Expected Output:**
```
"A | B | C"
"x----"
"ABABAB"
("line1" "line2" "line3")
("1: a" "2: b" "3: c")
```

**Verification:**
- [ ] All 6 functions work
- [ ] Results are correct
- [ ] No errors
- [ ] Library is usable

**Status:** ✅ Pass / ❌ Fail

---

## Test Suite 4: Test Suite Execution

### Test 4.1: Load Test Suite

**Objective:** Verify test suite loads

**Steps:**
```lisp
ChrysaLisp> (import "litprog_test_v4.lisp")
```

**Expected Output:**
```
LITPROG Test Suite loaded! (Phase 4)
Run (run-all-tests) to execute all tests
```

**Verification:**
- [ ] No load errors
- [ ] Test functions defined
- [ ] Ready to run

**Status:** ✅ Pass / ❌ Fail

---

### Test 4.2: Run All Tests

**Objective:** Execute comprehensive test suite

**Steps:**
```lisp
ChrysaLisp> (import "litprog_test_v4.lisp")
ChrysaLisp> (run-all-tests)
```

**Expected Output:**
```
╔════════════════════════════════════════════════════════════════╗
║  LITPROG Test Suite (Phase 4)                                ║
╚════════════════════════════════════════════════════════════════╝

## String Utilities
  ✓ trim-string removes leading whitespace
  ✓ trim-string removes trailing whitespace
  ✓ trim-string handles mixed whitespace
  ✓ starts-with-str detects prefix
  ✓ starts-with-str rejects non-prefix
  ...

## Noweb Parser
  ✓ parse chunk definition
  ✓ parse chunk reference
  ✓ reject invalid chunk syntax
  ...

[All tests...]

╔════════════════════════════════════════════════════════════════╗
║  Test Results                                                 ║
╚════════════════════════════════════════════════════════════════╝

  Total tests:  30+
  Passed:       30+ ✓
  Failed:       0

  Pass rate:    100%

╔════════════════════════════════════════════════════════════════╗
║  🎉 ALL TESTS PASSED! 🎉                                      ║
╚════════════════════════════════════════════════════════════════╝
```

**Verification:**
- [ ] All tests execute
- [ ] 100% pass rate
- [ ] No failures
- [ ] No errors

**Status:** ✅ Pass / ❌ Fail

**If Failed:** Note which test(s) failed and error messages

---

## Test Suite 5: Performance Benchmarks

### Test 5.1: Load Benchmark Suite

**Objective:** Verify benchmarks load

**Steps:**
```lisp
ChrysaLisp> (import "litprog_benchmark.lisp")
```

**Expected Output:**
```
LITPROG Benchmark Suite loaded!
Run (run-all-benchmarks) to execute all performance tests
Run (print-performance-targets) to see expected performance
```

**Verification:**
- [ ] No load errors
- [ ] Benchmark functions defined
- [ ] Ready to run

**Status:** ✅ Pass / ❌ Fail

---

### Test 5.2: Run Performance Benchmarks

**Objective:** Verify performance meets targets

**Steps:**
```lisp
ChrysaLisp> (import "litprog_benchmark.lisp")
ChrysaLisp> (run-all-benchmarks)
```

**Expected Output:**
```
╔════════════════════════════════════════════════════════════════╗
║         LITPROG PERFORMANCE BENCHMARKS                        ║
╚════════════════════════════════════════════════════════════════╝

## String Operations
  trim-string (small): 0.5ms (avg of 1000 runs)
  cat (small strings): 0.3ms (avg of 1000 runs)
  ...

## Parse Performance
  parse small file (10 chunks): 15ms (avg of 100 runs)
  parse medium file (100 chunks): 80ms (avg of 10 runs)
  parse large file (500 chunks): 450ms (avg of 1 runs)

## Full Pipeline Performance
  full pipeline: small (10 chunks): 25ms (avg of 100 runs)
  full pipeline: medium (100 chunks): 140ms (avg of 10 runs)
  full pipeline: large (500 chunks): 700ms (avg of 1 runs)

[All benchmarks...]

╔════════════════════════════════════════════════════════════════╗
║  Benchmark Summary                                            ║
╚════════════════════════════════════════════════════════════════╝

✓ All benchmarks complete!
```

**Verification:**
- [ ] Benchmarks run successfully
- [ ] Performance within targets:
  - [ ] Small files: <30ms
  - [ ] Medium files: <160ms
  - [ ] Large files: <800ms
- [ ] No errors

**Status:** ✅ Pass / ❌ Fail

**Note:** Actual times will vary by hardware

---

## Test Suite 6: Error Handling

### Test 6.1: Missing File

**Objective:** Verify graceful handling of missing files

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (parse-literate-file "nonexistent.lit")
```

**Expected Output:**
```
Error: File not found: nonexistent.lit
```

**Verification:**
- [ ] Error message displayed
- [ ] No crash
- [ ] Graceful failure
- [ ] ChrysaLisp still responsive

**Status:** ✅ Pass / ❌ Fail

---

### Test 6.2: Invalid Syntax

**Objective:** Handle malformed literate files

**Steps:**
Create `bad_syntax.lit`:
```
<<incomplete chunk
(print "missing closing >>")
```

```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (parse-literate-file "bad_syntax.lit")
```

**Expected Behavior:**
- Error message about invalid syntax OR
- Parses as best as possible with warning

**Verification:**
- [ ] No crash
- [ ] Some error/warning indication
- [ ] Graceful handling

**Status:** ✅ Pass / ❌ Fail

---

### Test 6.3: Circular References

**Objective:** Detect and handle circular chunk references

**Steps:**
Create `circular.lit`:
```
<<chunk-a>>=
<<chunk-b>>
@

<<chunk-b>>=
<<chunk-a>>
@
```

```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (defq ctx (parse-literate-file "circular.lit"))
ChrysaLisp> (expand-chunk-refs ctx "chunk-a")
```

**Expected Output:**
```
Error: Circular reference detected in chunk expansion
OR
Warning: Maximum recursion depth reached
```

**Verification:**
- [ ] Circular reference detected
- [ ] No infinite loop
- [ ] Error message helpful
- [ ] No crash

**Status:** ✅ Pass / ❌ Fail

---

## Test Suite 7: Integration with ChrysaLisp

### Test 7.1: No Global Pollution

**Objective:** Verify LITPROG doesn't pollute global namespace

**Steps:**
```lisp
ChrysaLisp> (defq test-var 99)
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> test-var
```

**Expected Output:**
```
99
```

**Verification:**
- [ ] User variables unchanged
- [ ] No variable conflicts
- [ ] Namespace clean

**Status:** ✅ Pass / ❌ Fail

---

### Test 7.2: Multiple Imports

**Objective:** Verify safe re-import

**Steps:**
```lisp
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> (litprog-help)
```

**Expected Behavior:**
- No errors on re-import
- Functions still work

**Verification:**
- [ ] Re-import safe
- [ ] No warnings
- [ ] Functions work

**Status:** ✅ Pass / ❌ Fail

---

### Test 7.3: Use with Other Libraries

**Objective:** Verify LITPROG works alongside other imports

**Steps:**
```lisp
ChrysaLisp> (import "lib/consts/chars.inc")  ; or any other ChrysaLisp library
ChrysaLisp> (import "litprog_core_v4.lisp")
ChrysaLisp> ; Use both libraries
```

**Expected Behavior:**
- Both libraries work
- No conflicts

**Verification:**
- [ ] Both imports succeed
- [ ] No conflicts
- [ ] Both usable

**Status:** ✅ Pass / ❌ Fail

---

## Integration Test Summary

### Results Checklist

**Test Suite 1: Core Loading**
- [ ] 1.1: Load LITPROG Core
- [ ] 1.2: Verify Core Functions Exist
- [ ] 1.3: ChrysaLisp Still Works

**Test Suite 2: Simple Example**
- [ ] 2.1: Parse test_simple.lit
- [ ] 2.2: Inspect Parsed Context
- [ ] 2.3: Tangle to File
- [ ] 2.4: Verify Generated Code
- [ ] 2.5: Execute Generated Code

**Test Suite 3: Real-World Example**
- [ ] 3.1: Parse String Utils Library
- [ ] 3.2: Tangle String Utils
- [ ] 3.3: Use Generated Library
- [ ] 3.4: Test All Library Functions

**Test Suite 4: Test Suite Execution**
- [ ] 4.1: Load Test Suite
- [ ] 4.2: Run All Tests (100% pass)

**Test Suite 5: Performance Benchmarks**
- [ ] 5.1: Load Benchmark Suite
- [ ] 5.2: Run Performance Benchmarks

**Test Suite 6: Error Handling**
- [ ] 6.1: Missing File
- [ ] 6.2: Invalid Syntax
- [ ] 6.3: Circular References

**Test Suite 7: Integration**
- [ ] 7.1: No Global Pollution
- [ ] 7.2: Multiple Imports
- [ ] 7.3: Use with Other Libraries

### Overall Status

**Total Tests:** 18
**Passed:** ___ / 18
**Failed:** ___ / 18
**Skipped:** ___ / 18

**Pass Rate:** ____%

**Overall Status:**
- [ ] ✅ All tests passed - PRODUCTION READY
- [ ] ⚠️ Some tests failed - needs fixes
- [ ] ❌ Major failures - not ready

---

## Troubleshooting

### Common Issues

**Issue: "undefined function" errors**
- **Cause:** ChrysaLisp primitive name mismatch
- **Fix:** Update litprog_core_v4.lisp with correct names
- **Example:** Change `(get list idx)` to `(elem list idx)`

**Issue: Parse errors**
- **Cause:** Incorrect string/list operations
- **Fix:** Verify operations against ChrysaLisp documentation
- **Example:** Ensure `(slice str start end)` syntax is correct

**Issue: File I/O failures**
- **Cause:** Incorrect file operations
- **Fix:** Verify file-stream, read-line, write primitives
- **Check:** Does file-stream exist? What's the correct syntax?

**Issue: Slow performance**
- **Cause:** Inefficient algorithms
- **Fix:** Profile and optimize
- **Target:** Should be O(n) scalability

**Issue: Memory errors**
- **Cause:** Large file processing
- **Fix:** Add chunked processing for very large files
- **Note:** Current design assumes reasonable file sizes

---

## Post-Integration Actions

### If All Tests Pass ✅

1. **Document Results:**
   - Record all test outputs
   - Note any warnings
   - Document performance numbers

2. **Create Examples:**
   - Document real usage
   - Create tutorials
   - Share success stories

3. **Announce Readiness:**
   - LITPROG is production ready
   - Can be integrated into ChrysaLisp
   - Ready for community use

### If Tests Fail ❌

1. **Document Failures:**
   - Which tests failed?
   - What were the error messages?
   - Any patterns in failures?

2. **Categorize Issues:**
   - Syntax errors (fix in code)
   - Missing primitives (find alternatives)
   - Logic errors (fix algorithms)
   - Performance issues (optimize)

3. **Fix and Retest:**
   - Address issues one by one
   - Rerun failed tests
   - Verify fixes don't break passing tests

4. **Update Documentation:**
   - Document known issues
   - Update compatibility notes
   - Revise expected behavior

---

## Final Verification

### Production Readiness Checklist

Before declaring LITPROG production ready:

**Functionality:**
- [ ] All core functions work
- [ ] Real examples run successfully
- [ ] Generated code is correct
- [ ] Error handling is graceful

**Performance:**
- [ ] Meets performance targets
- [ ] Scales linearly (O(n))
- [ ] No memory leaks
- [ ] Acceptable for typical use

**Quality:**
- [ ] All tests pass (100%)
- [ ] No syntax errors
- [ ] No warnings
- [ ] Clean execution

**Integration:**
- [ ] No ChrysaLisp breakage
- [ ] No namespace pollution
- [ ] Works with other libraries
- [ ] Safe to re-import

**Documentation:**
- [ ] All features documented
- [ ] Examples work
- [ ] Error messages clear
- [ ] Usage guide complete

**Compliance:**
- [ ] CONTRIBUTIONS.md requirements met
- [ ] Coding style matches
- [ ] No existing code modified
- [ ] Build system unaffected

---

## Conclusion

This integration test guide provides comprehensive verification of LITPROG in
a real ChrysaLisp environment.

**Execute all tests** → **Document results** → **Fix issues** → **Retest** → **Production Ready**

**Status after testing:** ___________________

**Date tested:** ___________________

**Tested by:** ___________________

**ChrysaLisp version:** ___________________

**Platform:** ___________________

---

**Document Version:** 1.0
**Date Created:** 2025
**Status:** Ready for execution

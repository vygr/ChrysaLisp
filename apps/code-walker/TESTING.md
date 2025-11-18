# Code Walker Testing Guide

This document describes how to test the Code Walker/AST Explorer application according to ChrysaLisp's CONTRIBUTING.md requirements.

## Testing Requirements

Per CONTRIBUTING.md, all contributions must:
1. Adhere to coding style ✓
2. Work from master branch ✓
3. Verify all existing functionality continues to work
4. Not modify platform-specific code (N/A - Lisp-only app)
5. Pass build verification tests

## Manual Testing Checklist

### Prerequisites

1. Build and install ChrysaLisp:
   ```bash
   make install
   ```

2. Launch ChrysaLisp GUI:
   ```bash
   ./run.sh
   ```

3. Open Code Walker:
   - From launcher, or
   - From terminal: `(open-child "apps/code-walker/app.lisp")`

### Test Suite

#### Test 1: Basic UI Functionality
- [ ] Application window opens without errors
- [ ] All buttons are visible and clickable
- [ ] Input field accepts text
- [ ] All output panels are visible
- [ ] Window can be minimized, maximized, closed

#### Test 2: Process All Button
- [ ] Default example loads on startup: `(defun add (a b) (+ a b))`
- [ ] READ phase shows parsed AST
- [ ] EXPAND phase shows expanded macro
- [ ] BIND phase shows pre-bound form
- [ ] EVAL phase shows result (the function object)
- [ ] Press Enter in input field triggers processing

#### Test 3: Example Buttons
Test each example button:
- [ ] "defun" button loads and processes correctly
- [ ] "let" button loads and processes correctly
- [ ] "case" button loads and processes correctly
- [ ] "ui-window" button loads and processes correctly
- [ ] Each shows macro expansion in EXPAND phase

#### Test 4: Step-by-Step Macro Expansion
- [ ] Load "defun" example
- [ ] Click "Step Expand" - should show first expansion level
- [ ] Click "Step Expand" again - should show next level
- [ ] Continue until "No more macros to expand" message appears
- [ ] Step counter increments correctly

#### Test 5: Memory Address Visualization
- [ ] Load "defun" example and process
- [ ] BIND phase output should show:
  ```
  [Pre-bound symbols (O(1) optimization):]
    + -> <Func:0x...>
    defq -> <Func:0x...>
    lambda -> <Func:0x...>
  ```
- [ ] Function addresses are displayed

#### Test 6: Export Functionality
- [ ] Load and process any example
- [ ] Click "Export" button
- [ ] Success message appears in EVAL output
- [ ] File is created in home directory: `code-walker-{timestamp}.txt`
- [ ] File contains all four phases with headers
- [ ] File is properly formatted and readable

#### Test 7: Clear Button
- [ ] Enter code and process it
- [ ] Click "Clear" button
- [ ] All fields are emptied
- [ ] Input field is cleared
- [ ] All output panels are cleared

#### Test 8: Pretty Printing
- [ ] Enter complex nested expression:
  ```lisp
  (defun complex (a b c d) (cond ((< a b) (+ a b)) ((> a b) (- a b)) (:t (* a b))))
  ```
- [ ] Process All
- [ ] EXPAND phase should show indented output
- [ ] Nested structures should be properly formatted

#### Test 9: Error Handling
Test error cases safely:
- [ ] Enter invalid syntax: `(defun add (a b)` (missing closing paren)
- [ ] Should show "Parse Error" in READ phase
- [ ] Other phases should be empty

- [ ] Enter: `(undefined-macro x y z)`
- [ ] Should process without crashing
- [ ] Should show the form unchanged (no macro expansion)

#### Test 10: Multiple Sequential Operations
- [ ] Load example 1, process
- [ ] Load example 2, process
- [ ] Load example 3, process
- [ ] Each should display correctly
- [ ] No memory leaks or slowdowns

## Build Verification Tests

Since this is a pure Lisp application with no C++ or VP changes:

### Test 1: Verify No Build Impact
```bash
# From within ChrysaLisp TUI Terminal:
make boot
```
- [ ] Build completes successfully
- [ ] No errors related to code-walker
- [ ] Boot image is created

### Test 2: Verify Other Apps Still Work
After adding code-walker, test that existing apps still function:
- [ ] Calculator app works
- [ ] Edit app works
- [ ] Files app works
- [ ] Terminal app works

## Performance Testing

### Test: No Memory Leaks
- [ ] Open Code Walker
- [ ] Process 10 different expressions
- [ ] Use Step Expand multiple times
- [ ] Export results multiple times
- [ ] Close and reopen app
- [ ] No degradation in performance

### Test: Large Expressions
- [ ] Test with deeply nested macros (ui-window with many nested ui-flow)
- [ ] App should handle gracefully
- [ ] May be slow but should not crash

## Integration Testing

### Test: File System Integration
- [ ] Export creates files in correct location
- [ ] Files have correct timestamps
- [ ] Files are readable by other apps (e.g., Edit app)

### Test: Environment Integration
- [ ] Uses correct *env_home* path
- [ ] Colors match environment theme
- [ ] Fonts load correctly

## Regression Testing

After any changes to the app:
- [ ] All manual tests still pass
- [ ] No new errors in any test case
- [ ] Performance is same or better

## Known Limitations

Document any known issues:
- Step expansion requires the form to have macros (won't step on primitive forms)
- Very large nested structures may be slow to format
- Export filename uses timestamp which may not be human-friendly

## Testing Notes

Add notes during testing:
- Date tested: __________
- Tester: __________
- Platform: __________
- Issues found: __________
- All tests passed: [ ] Yes [ ] No

## Reporting Issues

If you find issues:
1. Note which test failed
2. Document exact steps to reproduce
3. Include error messages if any
4. Note platform and ChrysaLisp version
5. File issue with label "code-walker"

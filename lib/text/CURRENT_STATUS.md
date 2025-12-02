# RegexpEngine - Current Status

**Date**: 2025-11-19
**Branch**: `claude/regexp-engine-library-01AdRcRExCbCNWPqEzESvbdz`

## ✅ What Works

### 1. Library Loading
- ✓ Imports successfully
- ✓ No syntax errors
- ✓ Class instantiation works
- ✓ Inherits from Regexp properly

### 2. Pattern Compilation
- ✓ Tokenizer processes patterns correctly
- ✓ AST compilation completes without errors
- ✓ Patterns tested:
  - Simple literals: `"hello"`
  - Character classes: `"\\d+"`, `"[a-z]+"`
  - Groups: `"(abc)"`
  - Quantifiers: `"a*"`, `"a+"`, `"a?"`
  - Anchors: `"^hello"`, `"world$"`

### 3. Pattern Caching
- ✓ Compiled patterns are cached
- ✓ Repeated compilation returns cached AST

## ❌ What Doesn't Work

### 1. Pattern Matching (Critical)
**Issue**: `:exec-ast` doesn't match even simple patterns

**Test Results**:
```
Pattern: 'hello' in 'hello world'
Expected: Match at position 0-5
Actual:   No match
```

**All patterns fail to match**, including:
- Simple literals
- Wildcards
- Anchors (except negative cases which correctly don't match)

### 2. Segmentation Fault
**Issue**: Crash on end anchor test

**Pattern**: `"world$"`
**Text**: `"hello world"`
**Result**: Segmentation fault

## 🐛 Root Cause Analysis

### Issue 1: exec-ast Not Matching

**Hypothesis**: The `:exec-sequence` or node execution logic has a bug.

**Evidence**:
- AST structure is correct: `(:sequence ...)`
- Tokenization works
- But execution returns `:nil` for all matches

**Likely Problems**:
1. **Wrong node access**: `(second ast)` might not return nodes correctly
2. **List vs sequence issue**: The optimized sequence might not be iterable as expected
3. **Return value format**: Methods might not return the expected `(list :t start end captures)` format

### Issue 2: Segmentation Fault

**Hypothesis**: Null pointer or infinite recursion in anchor handling.

**Pattern**: End anchor `$`
**Location**: Likely in `:exec-anchor` method (line 433-446)

**Possible causes**:
1. Accessing text beyond bounds
2. Recursion in quantifier or alternation
3. Stack overflow

## 🔧 Debugging Steps Taken

### Tests Created
1. **test_load.lisp** - Library loading ✓ PASS
2. **test_minimal.lisp** - Basic compilation ✓ PASS
3. **test_matching.lisp** - Pattern matching ✗ FAIL (all matches fail)
4. **test_debug.lisp** - AST inspection (hangs - not yet successful)

### Build Status
- ChrysaLisp built successfully (TUI mode)
- Using VP64 emulator boot image
- All tests run with: `./obj/x86_64/AMD64/Linux/main_tui -e obj/vp64/VP64/sys/boot_image -run <test>.lisp`

## 📋 Next Steps (Prioritized)

### Priority 1: Fix Basic Matching (1-2 hours)

#### Step 1: Inspect AST Structure
Create test to print full AST structure:
```lisp
(defq ast (. engine :compile-enhanced "a"))
(print "Full AST: " ast)
(print "Second element: " (second ast))
(if (list? (second ast))
    (print "First node: " (first (second ast))))
```

#### Step 2: Add Debug Prints to exec-ast
Add prints to trace execution:
```lisp
(defmethod :exec-ast (text ast pos)
    (print "exec-ast: node=" (first ast) " pos=" pos " char="
           (if (< pos (length text)) (elem-get text pos) "EOF"))
    ; ... rest of method
```

#### Step 3: Test Execution Step-by-Step
Test each node type individually:
```lisp
; Test literal-string directly
(defq ast-node '(:literal-string "hello"))
(defq result (. engine :exec-ast "hello world" ast-node 0))
(print "Result: " result)
```

#### Step 4: Fix exec-sequence
The bug is likely here (lines 471-485). Check:
- Is `(second ast)` actually a list of nodes?
- Is the iteration working correctly?
- Are results propagating properly?

### Priority 2: Fix Segmentation Fault (30 min)

#### Add Bounds Checking
In `:exec-anchor`, ensure we don't access beyond text bounds:
```lisp
(:end
    (if (= pos (length text))  ; Already correct
        (list :t pos pos (list))
        :nil))
```

#### Test Each Anchor Separately
```lisp
(test-match "Start" "^a" "abc" :t)      ; Should work
(test-match "End" "c$" "abc" :t)        ; Crashes - fix this
(test-match "Word boundary" "\\ba" "a b" :t)  ; Not implemented yet
```

### Priority 3: Implement Search (30 min)

Once basic matching works, implement proper searching:
```lisp
(defmethod :match-enhanced (text pattern)
    (when (defq ast (. this :compile-enhanced pattern))
        (defq pos 0 tlen (length text) result :nil)
        (while (and (not result) (< pos tlen))  ; Note: < not <=
            (when (defq r (. this :exec-ast text ast pos))
                (setq result r))
            (setq pos (inc pos)))
        result))
```

## 🎯 Success Criteria

### Phase 1: Basic Matching (Minimum Viable)
- [ ] Simple literal matches: `"hello"` in `"hello world"`
- [ ] Position tracking: Returns correct start/end
- [ ] Wildcard works: `"h.llo"` matches `"hello"`
- [ ] No segfaults

### Phase 2: Core Features
- [ ] Character classes: `"\\d+"` matches digits
- [ ] Quantifiers: `"a+"` matches one or more 'a'
- [ ] Groups: `"(abc)"` captures "abc"
- [ ] Anchors: `"^start"` and `"end$"` work

### Phase 3: Advanced Features
- [ ] Named groups: `"(?<name>\\w+)"` captures with name
- [ ] Lookahead: `"foo(?=bar)"` matches "foo" before "bar"
- [ ] Non-greedy: `"<.+?>"` matches minimal

## 📊 Performance Notes

Even though matching doesn't work yet, compilation performance is good:
- Pattern compilation: Fast (< 1ms observed)
- Cache hits: Instant
- No memory leaks observed

## 🔍 Debugging Commands

```bash
# Run minimal test (should pass):
timeout 10 ./obj/x86_64/AMD64/Linux/main_tui -e obj/vp64/VP64/sys/boot_image -run test_minimal.lisp

# Run matching test (currently fails):
timeout 30 ./obj/x86_64/AMD64/Linux/main_tui -e obj/vp64/VP64/sys/boot_image -run test_matching.lisp

# Compare with basic Regexp:
cat > /tmp/test_basic.lisp << 'EOF'
(import "lib/text/regexp.inc")
(defq r (Regexp))
(print "Basic match: " (. r :match? "hello world" "hello"))
EOF
./obj/x86_64/AMD64/Linux/main_tui -e obj/vp64/VP64/sys/boot_image -run /tmp/test_basic.lisp
```

## 📚 Reference Implementation

The basic `Regexp` class (`lib/text/regexp.inc`) works correctly. Key differences:
1. Uses NFA-style graph traversal
2. Has separate `:match` and `:search` methods
3. Uses `bfind`/`bskip` for optimization
4. Handles trace/capture state differently

Consider studying how basic Regexp's `:search` method works (lines 201-219).

## 🚀 Quick Fix Suggestions

### Fix 1: exec-sequence Node Access
```lisp
(defmethod :exec-sequence (text nodes pos)
    (print "DEBUG: exec-sequence nodes=" nodes)  ; ADD THIS
    (print "DEBUG: nodes is list? " (list? nodes))  ; ADD THIS
    ; ... rest
```

### Fix 2: Simplify exec-ast for Testing
```lisp
(defmethod :exec-ast (text ast pos)
    (defq node-type (first ast))
    (print "DEBUG: exec-ast type=" node-type " pos=" pos)  ; ADD THIS
    (case node-type
        (:literal-string
            (defq str (second ast))
            (print "DEBUG: checking '" str "' at pos " pos)  ; ADD THIS
            ; ... rest
```

### Fix 3: Test Individual Node Types
Create standalone tests for each node type before testing complex patterns.

## 📝 Notes

- The library is well-structured and follows ChrysaLisp conventions
- Syntax is correct (`:nil`, `:t`, `eql`, etc.)
- The architecture (AST-based) is sound
- Just needs debugging of execution logic

## 🎓 Learning Points

1. **ChrysaLisp debugging**: Print statements are your friend
2. **Incremental testing**: Test each component separately
3. **Reference implementations**: Study working code (basic Regexp)
4. **Timeouts**: Always use timeout for tests (ChrysaLisp doesn't auto-exit)

## 📮 Next Session Start Here

1. Run `test_minimal.lisp` to verify build
2. Add debug prints to `:exec-ast`
3. Test literal-string node execution directly
4. Fix the bug!
5. Re-run `test_matching.lisp`
6. Celebrate when tests pass! 🎉

---

**Status**: Ready for debugging session
**Estimated fix time**: 2-3 hours for basic matching
**Blocking issue**: Pattern execution logic

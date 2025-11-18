# RegexpEngine Testing Guide

## Status: 🟡 Syntax Fixed, Runtime Testing Needed

The RegexpEngine library has been created with comprehensive features, but needs runtime testing to verify functionality.

## Recent Fixes ✅

- **Fixed `code()` → `ascii-code()`**: Corrected character code comparisons
- **Fixed `str-to-num` → `str-as-num`**: Corrected string-to-number conversion
- **Fixed `slice()` usage**: Changed `slice(str, 1)` to `rest(str)` for proper string manipulation

##  Quick Test Commands

### 1. Simple Sanity Test (Start Here!)
```bash
./run_tui.sh cmd/test_regexp_simple.lisp
```

This tests basic compilation without complex matching logic.

**Expected Output**:
```
=== Testing Basic RegexpEngine ===

Creating RegexpEngine...
  Created: <object>

Test 1: Compile simple literal 'hello'
  ✓ Compiled successfully
  AST: (:sequence ((:literal-string "hello")))

Test 2: Compile digit class '\d+'
  ✓ Compiled successfully
  AST: (:sequence ((:quantifier (:char-class "0-9" :nil) 1 9223372036854775807 :t)))

...
```

### 2. Full Test Suite (After Basic Tests Pass)
```bash
./run_tui.sh cmd/test_regexp_engine.lisp
```

### 3. Interactive Demo
```bash
./run_tui.sh cmd/regexp_engine_demo.lisp
```

## Known Issues to Fix 🔧

### Priority 1: Critical (Blocks Basic Functionality)

#### 1. Character Class Parsing
**Issue**: The tokenizer handles character classes, but may not properly pass them to `char-class` function.

**Location**: `regexp_engine.inc:197`
```lisp
(push tokens (make-char-class class-content negated))
```

**Fix Needed**: Verify that `char-class` function is called correctly when executing the AST.

**Test**:
```lisp
(defq ast (. engine :compile-enhanced "[a-z]+"))
; Should parse without error
```

#### 2. Quantifier Parsing
**Issue**: The `parse-quantifier` function may fail on edge cases.

**Location**: `regexp_engine.inc:84-100`

**Potential Issues**:
- Empty `{,}` quantifiers
- Malformed `{n,m}` syntax
- Missing closing `}`

**Fix Needed**: Add error checking and handle edge cases.

**Test**:
```lisp
(defq ast (. engine :compile-enhanced "a{2,5}"))
(defq ast (. engine :compile-enhanced "a{3}"))
```

#### 3. AST Execution Return Format
**Issue**: The `:exec-ast` methods return `(list :t pos end captures)` but some code may expect different format.

**Location**: Multiple `:exec-*` methods

**Fix Needed**: Ensure consistent return format across all execution methods.

#### 4. Capture State Management
**Issue**: Backreferences need access to captured groups, but there's no global capture state.

**Location**: `regexp_engine.inc:531-534`

**Fix Needed**:
```lisp
(def this :capture_state (list))  ; Add to class

(defmethod :exec-group (text ast pos)
    (raise :capture_state)
    ; Store captures in :capture_state
    (elem-set capture_state index capture)
    ...)
```

### Priority 2: Important (Limits Functionality)

#### 5. Lookbehind Implementation
**Issue**: Currently returns stub value.

**Location**: `regexp_engine.inc:543-546`

**Fix**: Implement proper backwards matching logic (see ROADMAP.md for suggested implementation).

#### 6. Backreference Implementation
**Issue**: Currently returns `:nil`.

**Location**: `regexp_engine.inc:531-534`

**Fix**: Look up captured group and match against text.

####  7. `:find-all` Method Logic
**Issue**: Always starts from pos 0, doesn't increment properly.

**Location**: `regexp_engine.inc:548-558`

**Fix**:
```lisp
(defmethod :find-all (text pattern)
    (defq matches (list) pos 0 ast (. this :compile-enhanced pattern))
    (when ast
        (while (< pos (length text))
            (when (defq result (. this :exec-ast text ast pos))
                (push matches result)
                (setq pos (third result)))  ; Move to end of match
            (setq pos (inc pos))))  ; Try next position
    matches)
```

### Priority 3: Enhancements (Nice to Have)

#### 8. Better Error Messages
**Current**: Generic throw messages
**Needed**: Specific error types with position information

```lisp
(defun throw-parse-error (msg pattern pos)
    (throw (cat "Parse error at position " pos ": " msg "\n"
        "Pattern: " pattern "\n"
        "        " (pad "" pos " ") "^")))
```

#### 9. Pattern Validation
**Needed**: Pre-validate patterns before compilation
- Unmatched parentheses
- Invalid escape sequences
- Malformed quantifiers
- Empty character classes

#### 10. Performance Optimization
- Cache `char-class` results
- Optimize consecutive literal matching with `bfind`
- Add fast path for simple patterns

## Testing Checklist 📋

### Phase 1: Compilation ✓ (In Progress)
- [ ] Simple literals compile
- [ ] Character classes compile
- [ ] Quantifiers compile
- [ ] Groups compile
- [ ] Alternation compiles
- [ ] Complex patterns compile

### Phase 2: Basic Matching
- [ ] Literal strings match correctly
- [ ] Wildcards match any character
- [ ] Character classes match correctly
- [ ] Anchors (^, $) work
- [ ] Simple quantifiers (* , +, ?) work

### Phase 3: Advanced Features
- [ ] Named groups capture correctly
- [ ] Non-greedy quantifiers work
- [ ] Lookahead assertions work
- [ ] Lookbehind assertions work
- [ ] Backreferences work
- [ ] Range quantifiers `{n,m}` work

### Phase 4: Real-World Patterns
- [ ] Email: `\w+@\w+\.\w+`
- [ ] URL: `https?://[\w.]+/\w+`
- [ ] Phone: `\(?\d{3}\)?[-\s]?\d{3}[-\s]?\d{4}`
- [ ] Date: `\d{4}-\d{2}-\d{2}`
- [ ] IPv4: `\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}`

### Phase 5: Edge Cases
- [ ] Empty pattern
- [ ] Empty text
- [ ] Pattern longer than text
- [ ] No matches
- [ ] Multiple matches
- [ ] Overlapping matches
- [ ] Nested groups
- [ ] Recursive quantifiers

## Debugging Tips 🐛

### 1. Add Debug Prints
```lisp
(defmethod :exec-ast (text ast pos)
    (print "exec-ast: " (first ast) " at pos " pos)  ; DEBUG
    (defq node-type (first ast))
    ...)
```

### 2. Test Incrementally
Start with the simplest patterns and gradually add complexity:
1. `"a"` - single literal
2. `"abc"` - multiple literals
3. `"a+"` - quantifier
4. `"[abc]"` - character class
5. `"(abc)"` - group
6. `"a|b"` - alternation

### 3. Inspect AST
```lisp
(defq ast (. engine :compile-enhanced pattern))
(print "AST: " ast)
; Verify structure matches expectations
```

### 4. Test Each Method Separately
```lisp
; Test just literal matching
(defq ast '(:literal "a"))
(defq result (. engine :exec-ast "abc" ast 0))
(print "Result: " result)
```

### 5. Compare with Basic Regexp
```lisp
; Test same pattern with both engines
(defq basic (Regexp))
(defq enhanced (RegexpEngine))

(print "Basic:    " (. basic :match? "test" "es"))
(print "Enhanced: " (. enhanced :match-enhanced "test" "es"))
```

## Common Errors and Solutions

### Error: "Unclosed named group"
**Cause**: Missing `>` in named group syntax
**Fix**: Ensure pattern has `(?<name>...)` with closing `>`

### Error: "Invalid quantifier"
**Cause**: Malformed `{n,m}` syntax
**Fix**: Ensure quantifier has valid numbers and commas

### Error: "Unknown group construct"
**Cause**: Unrecognized `(?...)` syntax
**Fix**: Check spelling of lookahead/lookbehind syntax

### Crash: Stack overflow
**Cause**: Infinite recursion in quantifier matching
**Fix**: Add depth limit or better termination conditions

### Wrong matches
**Cause**: Greedy vs non-greedy quantifier confusion
**Fix**: Verify greedy flag is set correctly

## Performance Benchmarks 📊

Target performance (after optimizations):

| Pattern Type | Operations/sec | Time per op |
|--------------|----------------|-------------|
| Simple literal | 100,000+ | <10 μs |
| Character class | 50,000+ | <20 μs |
| Quantifier | 20,000+ | <50 μs |
| Named group | 10,000+ | <100 μs |
| Complex (email) | 5,000+ | <200 μs |

**Benchmark Command** (to be created):
```bash
./run_tui.sh cmd/bench_regexp_engine.lisp
```

## Integration Testing

### Test with grep
```bash
# After fixing issues
echo "test 123 hello" | ./run_tui.sh cmd/grep.lisp "\d+"
# Should find "123"
```

### Test in REPL
```bash
./run_tui.sh
ChrysaLisp> (import "lib/text/regexp_engine.inc")
ChrysaLisp> (defq e (RegexpEngine))
ChrysaLisp> (. e :compile-enhanced "hello")
```

## Contributing Fixes

When you fix an issue:

1. **Test the fix**: Run relevant tests
2. **Update docs**: Note what was fixed
3. **Add test case**: Prevent regression
4. **Commit clearly**: Describe the fix

```bash
git add lib/text/regexp_engine.inc
git commit -m "Fix character class execution

- Properly call char-class function in :exec-ast
- Handle negated classes correctly
- Add test case for [a-z] and [^0-9]"
git push
```

## Next Steps Checklist

- [x] Fix `code()` → `ascii-code()`
- [x] Fix `str-to-num` → `str-as-num`
- [x] Fix `slice()` usage
- [x] Create simple test file
- [ ] Run `cmd/test_regexp_simple.lisp`
- [ ] Fix any runtime errors
- [ ] Test basic literal matching
- [ ] Test character class matching
- [ ] Test quantifiers
- [ ] Implement backreferences
- [ ] Implement lookbehind
- [ ] Run full test suite
- [ ] Create real-world example app
- [ ] Performance benchmarking
- [ ] Integration with grep/search

## Resources

- **Main Library**: `lib/text/regexp_engine.inc`
- **Simple Test**: `cmd/test_regexp_simple.lisp`
- **Full Test Suite**: `cmd/test_regexp_engine.lisp`
- **Demo**: `cmd/regexp_engine_demo.lisp`
- **Documentation**: `docs/reference/classes/RegexpEngine.md`
- **Roadmap**: `lib/text/REGEXP_ENGINE_ROADMAP.md`
- **Basic Regexp**: `lib/text/regexp.inc` (reference implementation)

## Support

For issues or questions:
1. Check this testing guide
2. Review the roadmap for known limitations
3. Compare with basic Regexp implementation
4. Add debug prints to narrow down the issue
5. Test with simpler patterns first

---

**Remember**: Start simple, test incrementally, and fix one issue at a time!

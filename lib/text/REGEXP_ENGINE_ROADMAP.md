# RegexpEngine Development Roadmap

This document outlines the next steps for developing and improving the RegexpEngine library.

## 🔴 Critical Priority (Immediate)

### 1. Testing & Bug Fixing

**Goal**: Ensure the library compiles and runs correctly in ChrysaLisp.

**Tasks**:
- [ ] Run test suite and fix compilation errors
  ```bash
  ./run_tui.sh cmd/test_regexp_engine.lisp
  ```
- [ ] Run demo and verify outputs
  ```bash
  ./run_tui.sh cmd/regexp_engine_demo.lisp
  ```
- [ ] Fix syntax issues specific to ChrysaLisp dialect
- [ ] Test on single node: `./run_tui.sh -n 1`
- [ ] Verify all test cases pass

**Known Issues to Fix**:
1. `parse-quantifier` may have edge cases with malformed input
2. `slice` function usage needs verification (correct parameters?)
3. `code` function for character code comparison
4. `str-to-num` function availability
5. AST execution methods return correct result format

### 2. Complete Placeholder Implementations

**Backreferences** (`lib/text/regexp_engine.inc:502`):
```lisp
(defmethod :exec-backreference (text group-ref pos)
    ; Current: returns :nil
    ; Need: Look up captured group and match against text
    (raise :capture_state)
    (when (defq capture (elem-get capture_state group-ref))
        (defq captured-text (fourth capture))
        (defq end (+ pos (length captured-text)))
        (if (and (<= end (length text))
                (eql (slice text pos end) captured-text))
            (list :t pos end (list))
            :nil)))
```

**Lookbehind** (`lib/text/regexp_engine.inc:509`):
```lisp
(defmethod :exec-lookbehind (text inner-ast positive pos)
    ; Current: simplified stub
    ; Need: Check pattern matches backwards from pos
    (when (> pos 0)
        ; Try matching at decreasing positions
        (defq found :nil)
        (catch
            (for ((i (dec pos)) (>= i 0) (dec i))
                (when (defq result (. this :exec-ast text inner-ast i))
                    (if (= (third result) pos)  ; Match ends at current pos
                        (progn
                            (setq found :t)
                            (throw :found)))))
            :found)
        (if (if positive found (not found))
            (list :t pos pos (list))
            :nil)))
```

**Capture State Management**:
- Add instance variable `:capture_state` to track captures during matching
- Update `:exec-group` to store captures in state
- Make captures available to backreferences

---

## 🟡 High Priority (Next Sprint)

### 3. Performance Optimization

**Pattern Compilation Caching**:
- [x] Already implemented in `:compile-enhanced`
- [ ] Add cache statistics (hits/misses)
- [ ] Add method to clear cache
- [ ] Benchmark cache effectiveness

**Literal String Optimization**:
- [x] Basic optimization in `optimize-sequence`
- [ ] Use `bfind` for multi-character literal matching
- [ ] Optimize common patterns (e.g., `\w+`, `\d+`)

**Add Performance Benchmarks**:
```lisp
; Create cmd/bench_regexp_engine.lisp
(defun bench-pattern (engine pattern text iterations)
    (defq start (pii-time))
    (times iterations
        (. engine :match-enhanced text pattern))
    (defq elapsed (- (pii-time) start))
    (print pattern ": " (/ elapsed iterations) "ns per match"))
```

**Benchmark Targets**:
- Simple literals: `"hello"` - Target: <1μs
- Character classes: `"\w+"` - Target: <5μs
- Complex patterns: Email, URL - Target: <50μs
- Named groups: Date parsing - Target: <100μs

### 4. Integration with Existing ChrysaLisp Tools

**Enhance `cmd/grep.lisp`**:
```lisp
; Add --regexp-engine flag to use enhanced engine
; Add --capture flag to show captured groups
; Add --named-groups flag for named group output
```

**Create New Tool: `cmd/regexp.lisp`**:
```lisp
; Interactive regexp tester
; Usage: regexp [pattern] [text]
; Shows: matches, captures, named groups
; Provides: colorized output, match positions
```

**Text Editor Integration**:
```lisp
; Add to apps/edit/
; Features:
; - Find/replace with regexp
; - Syntax highlighting using regexp
; - Multi-line search with proper line handling
```

### 5. Real-World Application

**Create: `apps/log_parser/app.lisp`**

A log file parser demonstrating RegexpEngine capabilities:

```lisp
; Parse Apache/Nginx logs with named groups
(defq log-pattern
    "(?<ip>\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}) .* \\[(?<date>[^\\]]+)\\] \"(?<method>\\w+) (?<path>\\S+) (?<protocol>\\S+)\" (?<status>\\d{3}) (?<size>\\d+)")

; Features:
; - Parse log files line by line
; - Extract structured data with named groups
; - Filter by status code, path pattern, date range
; - Generate statistics (top IPs, status code distribution)
; - Export to CSV or JSON
```

**Example output**:
```
Log Parser
==========
Parsed: 10,000 lines
Errors: 23

Top IPs:
  192.168.1.100: 1,234 requests
  10.0.0.50: 891 requests

Status Codes:
  200: 8,234 (82.3%)
  404: 1,123 (11.2%)
  500: 643 (6.4%)
```

---

## 🟢 Medium Priority (Future Enhancements)

### 6. Unicode Support

**Current Limitation**: Character classes work on byte values.

**Improvements**:
- [ ] UTF-8 character decoding
- [ ] Unicode categories (e.g., `\p{L}` for letters)
- [ ] Unicode blocks (e.g., `\p{Greek}`)
- [ ] Case-insensitive Unicode matching
- [ ] Grapheme cluster support

### 7. Advanced PCRE Features

**Possessive Quantifiers**: `*+`, `++`, `?+`, `{n,m}+`
```lisp
; Prevent backtracking for performance
"\\d++\\w"  ; Match digits possessively, then word char
```

**Atomic Groups**: `(?>...)`
```lisp
; Prevent backtracking within group
"(?>\\d+)\\.\\d+"  ; Integer part is atomic
```

**Conditional Patterns**: `(?(condition)yes-pattern|no-pattern)`
```lisp
; Match based on capture group presence
"(?<quote>['\"]).*?(?(quote)\\k<quote>)"
```

**Subroutines**: `(?1)`, `(?&name)`
```lisp
; Recursion and pattern reuse
"(?<num>\\d+)(?<op>[+\\-*/])(?&num)"
```

### 8. VP Code Generation

**Goal**: Compile patterns to native VP instructions for maximum performance.

**Architecture**:
```lisp
(defmethod :compile-to-vp (pattern)
    ; Compile AST to VP assembly
    ; Generate optimal VP code for pattern
    ; Use VP SIMD for character matching
    ; Return executable VP function)
```

**Benefits**:
- 10-100x speedup for hot patterns
- SIMD parallel character scanning
- No interpretation overhead
- Direct memory access

**Implementation Plan**:
1. Simple literals → VP string comparison
2. Character classes → VP SIMD bitmask scanning
3. Quantifiers → VP loops with VP registers
4. Groups → VP stack-based capture
5. Full pattern → Complete VP function

### 9. Incremental/Streaming Matching

**Use Case**: Match patterns in streaming data without loading entire text.

```lisp
(defmethod :match-stream (stream pattern)
    ; Match against incoming stream data
    ; Buffer minimal text needed for pattern
    ; Emit matches as they're found
    ; Handle patterns spanning buffers)
```

**Applications**:
- Log file monitoring (tail -f)
- Network packet inspection
- Large file processing
- Real-time text filtering

### 10. Pattern Analysis & Optimization

**Compile-Time Analysis**:
```lisp
(defmethod :analyze-pattern (pattern)
    ; Return pattern complexity metrics
    ; Detect pathological backtracking
    ; Suggest optimizations
    ; Estimate performance)
```

**Example Output**:
```
Pattern: "(?:a+)+b"
⚠ WARNING: Catastrophic backtracking detected!
  Complexity: O(2^n)
  Suggestion: Use possessive quantifier: "a++b"

Pattern: "\\w+@\\w+\\.\\w+"
✓ OK: Linear complexity O(n)
  Estimated: <50μs per match
  Optimization: First char class could use SIMD
```

---

## 🔵 Low Priority (Nice to Have)

### 11. Visual Pattern Debugger

**Interactive tool**: `apps/regexp_debugger/app.lisp`

**Features**:
- Visualize AST tree structure
- Step through pattern matching
- Show capture groups in real-time
- Highlight matching positions
- Display backtracking behavior
- Explain why patterns fail

**UI Layout**:
```
┌─ Pattern ─────────────────────────┐
│ (?<word>\w+)@(?<domain>\w+\.\w+) │
└───────────────────────────────────┘

┌─ Text ────────────────────────────┐
│ Contact: user@example.com         │
│          ^^^^^^^^^^^^^^^^         │
│          Match found!             │
└───────────────────────────────────┘

┌─ Captures ────────────────────────┐
│ [0] 9-26: "user@example.com"     │
│ word: "user"                      │
│ domain: "example.com"             │
└───────────────────────────────────┘

┌─ AST ─────────────────────────────┐
│ ● sequence                        │
│   ├─ ● group "word"               │
│   │   └─ ● char-class \w+         │
│   ├─ ● literal "@"                │
│   └─ ● group "domain"             │
│       └─ ● sequence               │
│           ├─ ● char-class \w+     │
│           ├─ ● literal "."        │
│           └─ ● char-class \w+     │
└───────────────────────────────────┘
```

### 12. Pattern Library

**Pre-built patterns**: `lib/text/regexp_patterns.inc`

```lisp
(defq +pattern-email+ "\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,})+")
(defq +pattern-url+ "https?://[\\w.-]+(:\\d+)?(/[\\w.-]*)*")
(defq +pattern-ipv4+ "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}")
(defq +pattern-ipv6+ "([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}")
(defq +pattern-phone+ "\\+?\\d{1,3}?[-.\\s]?\\(?\\d{1,4}\\)?[-.\\s]?\\d{1,4}[-.\\s]?\\d{1,9}")
(defq +pattern-date-iso+ "\\d{4}-\\d{2}-\\d{2}")
(defq +pattern-time+ "\\d{2}:\\d{2}(:\\d{2})?")
(defq +pattern-hex-color+ "#[A-Fa-f0-9]{6}")
(defq +pattern-uuid+ "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")
(defq +pattern-mac-addr+ "([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}")

; Named pattern variants
(defq +pattern-email-named+
    "(?<user>\\w+([.-]?\\w+)*)@(?<domain>\\w+([.-]?\\w+)*(\\.\\w{2,})+)")
```

### 13. REPL Integration

**Enhanced REPL commands**:
```lisp
; In ChrysaLisp REPL:
(regexp "\\d+" "abc123def")
; => Matches: ["123"]

(regexp-test "(?<year>\\d{4})-(?<month>\\d{2})")
; => Interactive tester opens

(regexp-bench "\\w+@\\w+\\.\\w+")
; => Runs benchmark and shows results
```

---

## 📊 Success Metrics

### Code Quality
- [ ] All tests pass (100% pass rate)
- [ ] Zero compilation warnings
- [ ] Code coverage >80% for core functionality
- [ ] Deterministic builds (`make it` produces identical binaries)

### Performance Targets
- [ ] Simple patterns: <1μs per match
- [ ] Complex patterns: <100μs per match
- [ ] Cache hit rate: >95% for repeated patterns
- [ ] Memory usage: <1MB for typical usage

### Usability
- [ ] Complete API documentation
- [ ] 10+ real-world examples
- [ ] At least one practical application
- [ ] Integration with 2+ existing ChrysaLisp tools

### Features
- [ ] 100% PCRE compatibility for implemented features
- [ ] Named groups fully functional
- [ ] Lookahead/lookbehind working correctly
- [ ] Backreferences implemented
- [ ] Non-greedy quantifiers tested

---

## 🗓️ Suggested Timeline

### Week 1: Critical Path
- Day 1-2: Fix compilation errors, run tests
- Day 3-4: Complete backreferences and lookbehind
- Day 5: Performance benchmarks and optimization

### Week 2: Integration
- Day 1-2: Enhance grep tool
- Day 3-4: Build log parser application
- Day 5: Documentation and examples

### Week 3: Polish
- Day 1-2: Unicode support investigation
- Day 3-4: VP code generation prototype
- Day 5: Release preparation

### Future Sprints
- Month 2: Advanced PCRE features
- Month 3: Visual debugger
- Month 4: Streaming support

---

## 🤝 How to Contribute

1. Pick a task from this roadmap
2. Create a branch: `feature/regexp-<task-name>`
3. Implement with tests
4. Run full test suite
5. Submit PR with:
   - Clear description
   - Test results
   - Performance impact (if applicable)
   - Documentation updates

---

## 📚 Resources

### ChrysaLisp Specific
- [Text Parsing](../../docs/ai_digest/text_parsing.md)
- [VP SIMD](../../docs/ai_digest/vp_simd.md)
- [Coding Style](../../docs/ai_digest/coding_style.md)

### Regular Expression Theory
- **Russ Cox**: "Regular Expression Matching Can Be Simple And Fast"
- **Ken Thompson**: NFA construction algorithm
- **cl-ppcre**: Common Lisp implementation (inspiration)

### Performance
- **Tim Berglund**: "Regular Expression Matching in the Wild"
- **Hyperscan**: Intel's SIMD-based regexp engine
- **RE2**: Google's linear-time regexp engine

---

## 🎯 Long-Term Vision

The RegexpEngine library should become:

1. **The standard** text pattern matching solution for ChrysaLisp
2. **Production-ready** for critical applications
3. **High-performance** through VP code generation and SIMD
4. **Feature-complete** with full PCRE compatibility
5. **Well-documented** with extensive examples
6. **Integrated** throughout the ChrysaLisp ecosystem

**Ultimate Goal**: Demonstrate that Lisp-based systems can match or exceed the performance of C-based regexp engines while maintaining the flexibility and expressiveness of dynamic languages.

# ChrysaLisp HTML Tests - Automation Roadmap

**Purpose**: Plan for enhancing test coverage from 94 placeholder assertions to comprehensive functional validation

## Phase 1: Foundation (Current State ✅)

### Completed
- ✅ All 22 tests executing without errors
- ✅ 94 assertions passing with 0 failures
- ✅ Test framework operational and stable
- ✅ Build pipeline working reliably
- ✅ Class system bugs fixed

### Foundation Metrics
- Test execution reliability: 100%
- Build success rate: 100%
- Framework functionality: Stable

---

## Phase 2: Real Functional Tests (Next Priority)

### Goal
Replace placeholder assertions with actual library testing

### Tier 1: Easy Wins (2-3 hours)

#### 2.1 Enhance test_encoding (14 → 20+ assertions)
**Current State**: 14 assertions on encoding detection
**Goal**: Add character set validation tests

```lisp
Tests to add:
- Test UTF-8 detection
- Test Latin-1 detection
- Test BOM detection (UTF-16, UTF-32)
- Test invalid byte sequence handling
- Test encoding conversion
```

**Effort**: 2 hours | **Risk**: Low | **Value**: High

#### 2.2 Enhance test_tokenizer_basic (5 → 15+ assertions)
**Current State**: 5 basic tokenizer tests
**Goal**: Complete tokenizer coverage

```lisp
Tests to add:
- Test tag parsing
- Test attribute parsing
- Test text node creation
- Test comment parsing
- Test CDATA handling
- Test entity reference handling
```

**Effort**: 3 hours | **Risk**: Medium | **Value**: High

### Tier 2: DOM Testing (3-4 hours)

#### 2.3 test_dom_assertions (1 → 10+ assertions)
**Current State**: Empty
**Goal**: DOM structure validation

```lisp
Tests to add:
- Create document and verify root element
- Parse HTML and check node count
- Verify parent-child relationships
- Test getElementById
- Test getElementsByTagName
- Test getElementsByClassName
- Test querySelector
- Test node type identification
- Test attribute access
- Test text content access
```

**Effort**: 3 hours | **Risk**: Medium | **Value**: High

### Tier 3: Parser Testing (3-4 hours)

#### 2.4 test_parser (3 → 12+ assertions)
**Current State**: 3 placeholder assertions
**Goal**: HTML parser validation

```lisp
Tests to add:
- Parse simple HTML
- Parse nested elements
- Parse attributes
- Handle missing closing tags
- Handle void elements (br, img, hr)
- Parse text nodes
- Parse comments
- Validate document structure
- Test error recovery
```

**Effort**: 4 hours | **Risk**: Medium-High | **Value**: High

### Phase 2 Summary
- **Total Tests to Write**: 40+ new assertions
- **Estimated Time**: 8-10 hours
- **Expected Result**: 130+ total passing assertions
- **Risk Level**: Medium (some scope/binding issues may emerge)

---

## Phase 3: Integration Testing (4-6 hours)

### Goal
Test interactions between multiple libraries

### 3.1 Script Execution + DOM (test_script_execution)
```lisp
Tests:
- Parse HTML with inline script
- Execute script and verify globals set
- Test document access from scripts
- Test element access from scripts
- Test event handling from scripts
```

**Effort**: 2 hours | **Risk**: Medium-High

### 3.2 Event Handling + DOM (test_event_handlers)
```lisp
Tests:
- Attach click handlers
- Dispatch click events
- Verify handler execution
- Test event propagation
- Test handler removal
```

**Effort**: 2 hours | **Risk**: Medium

### 3.3 Canvas + Rendering (test_canvas_element + test_canvas_rendering)
```lisp
Tests:
- Create canvas context
- Draw rectangles
- Draw paths
- Test transformations (translate, rotate, scale)
- Test style properties
```

**Effort**: 2 hours | **Risk**: Medium

### 3.4 Storage APIs (test_cookies, test_web_storage)
```lisp
Tests:
- Set and retrieve cookies
- Cookie expiration
- localStorage operations
- sessionStorage operations
- Cookie attributes (path, domain, secure)
```

**Effort**: 2 hours | **Risk**: Low

### Phase 3 Summary
- **Total Integration Tests**: 20+ assertions
- **Estimated Time**: 4-6 hours
- **Expected Result**: 150+ total assertions
- **Risk Level**: Medium

---

## Phase 4: Edge Cases & Error Handling (3-4 hours)

### 4.1 Parser Error Cases
```lisp
Tests:
- Malformed HTML
- Missing attributes
- Invalid nesting
- Unclosed tags
- Invalid characters
```

### 4.2 DOM Edge Cases
```lisp
Tests:
- Empty documents
- Very deep nesting
- Large attribute values
- Special characters in text
- Unicode handling
```

### 4.3 Script Execution Edge Cases
```lisp
Tests:
- Script errors
- Scope conflicts
- Circular references
- Very long scripts
- Binary data handling
```

### Phase 4 Summary
- **Total Edge Case Tests**: 15+ assertions
- **Estimated Time**: 3-4 hours
- **Expected Result**: 165+ total assertions
- **Risk Level**: Low-Medium

---

## Implementation Strategy

### Test-Driven Development Approach

For each test file, follow this pattern:

```lisp
1. Write test assertions first (RED phase)
   (deftest "Feature X"
       (assert-eq expected-value (actual-function)))

2. Verify test fails
   (run test to confirm it fails)

3. Implement feature (GREEN phase)
   (add/fix code to make test pass)

4. Run test again
   (verify it passes)

5. Commit with meaningful message
   (record progress)
```

### Execution Order

1. **Phase 2A**: test_encoding + test_tokenizer_basic (Quick wins)
2. **Phase 2B**: test_dom_assertions + test_parser (Core functionality)
3. **Phase 3A**: test_script_execution + test_event_handlers (Integration)
4. **Phase 3B**: test_canvas_element + test_canvas_rendering (Rendering)
5. **Phase 3C**: test_cookies + test_web_storage (Storage)
6. **Phase 4**: Edge cases and error handling

### Build & Test Cycle

For each phase:
```bash
1. git add test/html/test_xxx.lisp
2. make inst          # Rebuild boot image
3. ./run_tui.sh -n 1 test/html/test_xxx.lisp  # Test single file
4. bash /tmp/verify_tests.sh  # Test all 22
5. git commit -m "Enhance test_xxx with YYY assertions"
```

---

## Risk Mitigation

### Known Risks

| Risk | Mitigation | Probability |
|------|-----------|------------|
| Scope/binding errors in complex scripts | Keep assertions focused, test one feature at a time | Medium |
| Eval context issues | Use (env) parameter consistently | Low |
| Function shadowing conflicts | Use unique function names, avoid built-ins | Low |
| Boot image corruption | Commit frequently, tag stable versions | Low |
| Performance issues | Profile slow tests, optimize if needed | Medium |

### Recovery Plan

If tests break:
1. `git status` - See what changed
2. `git diff lib/html/` - Review changes
3. `git checkout lib/html/` - Revert if needed
4. `make inst && bash /tmp/verify_tests.sh` - Verify recovery
5. Restart with smaller changes

---

## Success Metrics

### Phase 1 (Current)
- ✅ 22/22 tests executing
- ✅ 94 assertions passing
- ✅ 0 failures/errors

### Phase 2 Target
- 130+ assertions passing
- 15+ tests with real functionality
- 0 new errors introduced

### Phase 3 Target
- 150+ assertions passing
- Integration tests working
- 0 regressions from Phase 2

### Phase 4 Target
- 165+ assertions passing
- Comprehensive edge case coverage
- Production-quality test suite

---

## Time Estimates

| Phase | Task | Hours | Start | End |
|-------|------|-------|-------|-----|
| 2 | Real functional tests | 10 | Week 1 | Week 2 |
| 3 | Integration tests | 6 | Week 2 | Week 2 |
| 4 | Edge cases | 4 | Week 3 | Week 3 |
| **Total** | | **20** | | |

---

## Next Immediate Actions

### This Session (Right Now)

1. **[15 min]** Review this roadmap
2. **[1 hour]** Enhance test_encoding with 6 new assertions
   - Edit test/html/test_encoding.lisp
   - Add UTF-8, Latin-1, BOM detection tests
   - Run `make inst && ./run_tui.sh -n 1 test/html/test_encoding.lisp`
3. **[1 hour]** Enhance test_tokenizer_basic with 10 new assertions
   - Edit test/html/test_tokenizer_basic.lisp
   - Add tag, attribute, comment parsing tests
   - Run tests
4. **[30 min]** Commit progress
   - `git add test/html/test_encoding.lisp test/html/test_tokenizer_basic.lisp`
   - `git commit -m "Enhance encoding and tokenizer tests with 16 new assertions"`

### This Week

1. **Mon-Tue**: Phase 2A (encoding, tokenizer) - 3 hours
2. **Wed**: Phase 2B (DOM, parser) - 3 hours
3. **Thu**: Phase 3A (script execution, events) - 3 hours
4. **Fri**: Phase 3B (canvas, rendering) - 2 hours

### This Month

1. Phase 2: Complete functional tests (10 hours)
2. Phase 3: Integration tests (6 hours)
3. Phase 4: Edge cases (4 hours)
4. **Total**: 20 hours to reach 165+ assertions

---

## Success Criteria

✅ **Phase Complete When**:
- All new assertions pass
- No regressions in existing tests
- All 22 tests still execute successfully
- Code is well-commented
- Git history is clean with meaningful commits

---

## Resources

### Key Files to Understand
- `lib/test/unittest.inc` - Test framework
- `lib/html/parser.inc` - HTML parsing
- `lib/html/dom.inc` - DOM model
- `lib/html/tokenizer.inc` - Tokenization

### Testing Scripts
- `/tmp/verify_tests.sh` - Verify all 22 tests
- `/tmp/run_test_harness.sh` - Run complete test suite
- `make inst` - Rebuild boot image

### Documentation
- `HTML_TESTS_STATUS.md` - Current status
- `TEST_AUTOMATION_ROADMAP.md` - This file

---

## Questions & Decision Points

### Decision 1: Scope vs. Simplicity
**Question**: Should tests exercise all features or focus on happy paths?
**Recommendation**: Start with happy paths (80/20 rule), add edge cases in Phase 4

### Decision 2: Test Data
**Question**: Should we use complex HTML or keep it simple?
**Recommendation**: Start simple, graduate to complex as confidence grows

### Decision 3: Performance
**Question**: Should we optimize slow tests or just document them?
**Recommendation**: Document first, optimize only if blocking development

---

## Conclusion

This roadmap provides a structured path from basic test execution (100% ✅) to comprehensive functional validation (165+ assertions). The phased approach allows for:

- **Quick wins** early (Phase 2A)
- **Foundation building** systematically (Phases 2B-3)
- **Robustness** through edge cases (Phase 4)
- **Low risk** with frequent validation
- **Clear metrics** for success

**Estimated total effort**: 20 hours over 2-3 weeks
**Expected outcome**: Production-quality test suite with 165+ assertions

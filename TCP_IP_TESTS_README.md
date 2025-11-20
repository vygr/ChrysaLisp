# TCP/IP Stack Test Suite - Quick Reference

## Quick Start

### Run All Tests
```bash
bash test/net/run_all_tests.sh
```

### Run Individual Tests
```bash
CPU=$(cat cpu) ABI=$(cat abi) OS=$(cat os)
./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image -run test/net/test_tcp_basics.lisp
./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image -run test/net/test_tcp_state.lisp
```

## Test Files

### test_tcp_basics.lisp (10 tests)
Tests fundamental ChrysaLisp operations used in TCP/IP stack:
- Arithmetic, arrays, strings, bitwise operations, lists, logic

**Run:** `bash test/net/run_all_tests.sh`

### test_tcp_state.lisp (10 tests)
Tests TCP state machine and TCB (TCP Control Block) operations:
- State constants, flags, TCB creation, state transitions, sequence numbers

**Run:** `bash test/net/run_all_tests.sh`

## Test Results

```
Total Tests: 2 files
Total Assertions: 20
Pass Rate: 100% ✓
Failure Rate: 0% ✓
```

## Test Output Symbols

- `✓` = Test passed
- `✗` = Test failed
- `⏱` = Test timeout (not observed in current suite)

## Test Structure

Each test file follows this pattern:
1. Define constants and helper functions
2. Define test function with assert statements
3. Call test function at end of file
4. Tests print results with pass/fail symbols
5. Return success/failure count

## Understanding Test Output

```
Running: test_tcp_basics
  ✓ PASS (10/10 tests)
```

This means:
- File executed successfully
- All 10 tests passed
- No errors or timeouts

## Debugging Failed Tests

1. Check individual test output:
```bash
./obj/$CPU/$ABI/$OS/main_tui obj/$CPU/$ABI/sys/boot_image -run test/net/test_tcp_state.lisp 2>&1 | head -50
```

2. Look for error messages indicating which test failed
3. Check test source code for assertion logic
4. Modify test and re-run

## Adding New Tests

1. Create new file: `test/net/test_<feature>.lisp`
2. Import constants/utilities as needed
3. Define test function
4. Use assert pattern from existing tests
5. Call test function at end
6. Test runner will automatically discover and run it

Example:
```lisp
(defun test-my-feature ()
    (prin "Testing My Feature\n")
    (prin "==================\n\n")

    (defq passed 0 failed 0)

    ; Test case
    (if (= 1 1)
        (progn (prin "  ✓ Test 1\n") (setq passed (+ passed 1)))
        (progn (prin "  ✗ Test 1\n") (setq failed (+ failed 1))))

    (prin "\nResults: " passed " passed, " failed " failed\n"))

(test-my-feature)
```

## TCP/IP Stack Components Tested

### TCP State Machine
- 11 states: CLOSED, LISTEN, SYN_SENT, SYN_RECEIVED, ESTABLISHED, FIN_WAIT_1, FIN_WAIT_2, CLOSE_WAIT, CLOSING, LAST_ACK, TIME_WAIT
- State transitions and comparisons
- TCP Control Block (TCB) management

### TCP Flags
- FIN (0x01) - Finish
- SYN (0x02) - Synchronize
- RST (0x04) - Reset
- PSH (0x08) - Push
- ACK (0x10) - Acknowledgment
- URG (0x20) - Urgent

### Network Operations
- Array-based data structures
- Bitwise operations (AND, OR)
- Sequence number comparisons
- String building and manipulation

## System Configuration

Tests auto-detect system configuration:
- CPU: x86_64
- ABI: AMD64
- OS: Linux

## File Locations

- Test files: `test/net/test_*.lisp`
- Test runner: `test/net/run_all_tests.sh`
- Summary: `TCP_IP_TEST_SUMMARY.md`
- Documentation: `TCP_IP_TESTS_README.md`

## Known Limitations

- Tests focus on data structures and basic operations
- Full protocol state transitions with packet exchange not yet tested
- Network driver integration (sys/net/class.vp) not tested
- IP layer tests marked for future implementation

## Performance

- Binary location: `obj/x86_64/AMD64/Linux/main_tui`
- Boot image: `obj/x86_64/AMD64/sys/boot_image`
- Test execution time: < 5 seconds per file
- Total suite time: < 10 seconds

## Troubleshooting

### Tests not found
```
ERROR: Boot image not found
```
Run `make inst` to rebuild boot image

### Incorrect CPU/ABI/OS
Check files: `cpu`, `abi`, `os` in project root
```bash
cat cpu abi os
```

### Timeout errors
Increase timeout in test runner script (line with `timeout 20`)

## Next Steps

1. Implement IP layer tests
2. Fix sys/net/class.vp segfault (use isolated test approach)
3. Add integration tests for TCP handshakes
4. Implement network driver tests
5. Achieve 100% protocol coverage

## Contributing

When adding new tests:
1. Follow existing test pattern
2. Use clear, descriptive test names
3. Include assert comments explaining each test
4. Ensure all tests pass before committing
5. Update test summary documentation

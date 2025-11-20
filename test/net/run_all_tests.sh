#!/bin/bash

# TCP/IP Stack Test Runner
# Runs all TCP/IP stack tests and reports results

echo "╔════════════════════════════════════════════════════════════════╗"
echo "║       ChrysaLisp TCP/IP Stack - Test Suite                   ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""

cd "$(dirname "$0")/../.."

# Get CPU/ABI/OS config
CPU=$(cat cpu)
ABI=$(cat abi)
OS=$(cat os)

BINARY="./obj/$CPU/$ABI/$OS/main_tui"
BOOT_IMAGE="obj/$CPU/$ABI/sys/boot_image"

if [ ! -f "$BINARY" ]; then
    echo "ERROR: Binary not found: $BINARY"
    exit 1
fi

if [ ! -f "$BOOT_IMAGE" ]; then
    echo "ERROR: Boot image not found: $BOOT_IMAGE"
    exit 1
fi

echo "System Configuration:"
echo "  CPU:   $CPU"
echo "  ABI:   $ABI"
echo "  OS:    $OS"
echo ""

total_passed=0
total_failed=0
test_count=0

# Run each test
for test_file in test/net/test_*.lisp; do
    if [ -f "$test_file" ]; then
        test_name=$(basename "$test_file" .lisp)
        echo "Running: $test_name"

        output=$(timeout 20 "$BINARY" "$BOOT_IMAGE" -run "$test_file" 2>&1)
        exit_code=$?

        # Count passed/failed from output
        if echo "$output" | grep -q "All.*passed"; then
            echo "  ✓ PASS"
            ((total_passed++))
        elif echo "$output" | grep -q "Results:"; then
            # Extract pass/fail counts
            passed=$(echo "$output" | grep "Results:" | grep -oP 'Results: \K[0-9]+' | head -1)
            failed=$(echo "$output" | grep "Results:" | grep -oP ', \K[0-9]+')

            if [ -n "$passed" ]; then
                total_passed=$((total_passed + passed))
            fi
            if [ -n "$failed" ]; then
                total_failed=$((total_failed + failed))
            fi

            if [ "$failed" -eq 0 ] 2>/dev/null; then
                echo "  ✓ PASS ($passed tests)"
            else
                echo "  ✗ FAIL ($passed passed, $failed failed)"
            fi
        else
            echo "  ✗ ERROR"
            ((total_failed++))
        fi

        ((test_count++))
    fi
done

echo ""
echo "════════════════════════════════════════════════════════════════"
echo "Summary:"
echo "  Total Tests Run: $test_count"
echo "  Total Passed:    $total_passed"
echo "  Total Failed:    $total_failed"

if [ "$total_failed" -eq 0 ]; then
    echo ""
    echo "✓ ALL TESTS PASSED!"
    exit 0
else
    echo ""
    echo "✗ SOME TESTS FAILED"
    exit 1
fi

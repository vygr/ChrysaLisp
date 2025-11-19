#!/bin/bash

# Run all HTML tests and summarize results

echo "Testing HTML test files..."
echo "=================================="

tests=(
  "test/html/test_tokenizer_basic.lisp"
  "test/html/test_part.lisp"
  "test/html/test_cookies.lisp"
  "test/html/test_parser.lisp"
  "test/html/test_dom_assertions.lisp"
  "test/html/test_encoding.lisp"
)

for test in "${tests[@]}"; do
  name=$(basename "$test")
  printf "%-40s " "$name"
  timeout 10 ./run_tui.sh -n 1 "$test" 2>&1 | grep -E "All tests|Passed:|Failed:|Error:" | head -1
done

echo "=================================="

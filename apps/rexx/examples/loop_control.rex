// LEAVE and ITERATE Loop Control
// Demonstrates early exit and iteration skipping

SAY "=== Loop Control with LEAVE and ITERATE ==="
SAY ""

// ========================================
// 1. LEAVE - Exit Loop Early
// ========================================
SAY "1. LEAVE - Exit Loop Early"
SAY "---"

SAY "Counting with early exit:"
DO i = 1 TO 10
  SAY "  Count: {i}"
  IF i = "5" THEN LEAVE
END
SAY "  Loop exited early at 5"

SAY ""

// ========================================
// 2. ITERATE - Skip to Next Iteration
// ========================================
SAY "2. ITERATE - Skip to Next Iteration"
SAY "---"

SAY "Printing odd numbers only (1-10):"
DO i = 1 TO 10
  // Calculate if even (would need modulo, using simple check)
  test = i
  IF test = "2" THEN ITERATE
  IF test = "4" THEN ITERATE
  IF test = "6" THEN ITERATE
  IF test = "8" THEN ITERATE
  IF test = "10" THEN ITERATE
  SAY "  {i}"
END

SAY ""

// ========================================
// 3. LEAVE with Conditional
// ========================================
SAY "3. LEAVE with Conditional Logic"
SAY "---"

SAY "Search for target value:"
target = "7"
found = "no"

DO i = 1 TO 10
  SAY "  Checking {i}..."
  IF i = target
  THEN
    SAY "  Found target: {target}!"
    found = "yes"
    LEAVE
  END
END

IF found = "yes"
  THEN SAY "  Search successful"
  ELSE SAY "  Target not found"

SAY ""

// ========================================
// 4. ITERATE with Processing
// ========================================
SAY "4. ITERATE for Selective Processing"
SAY "---"

SAY "Processing valid items only:"
DO i = 1 TO 5
  // Simulate validation - skip item 3
  IF i = "3"
  THEN
    SAY "  Item {i}: SKIPPED (invalid)"
    ITERATE
  END
  SAY "  Item {i}: PROCESSED"
END

SAY ""

// ========================================
// 5. Multiple LEAVE Conditions
// ========================================
SAY "5. Multiple LEAVE Conditions"
SAY "---"

SAY "Guarded iteration:"
counter = "1"
max_count = "8"

DO i = 1 TO 100
  SAY "  Iteration {i}"

  // First guard
  IF i = "3" THEN SAY "    Warning at 3"

  // Second guard - exit condition
  IF i = max_count
  THEN
    SAY "    Maximum reached"
    LEAVE
  END
END
SAY "  Stopped at maximum"

SAY ""

// ========================================
// 6. ITERATE for Data Filtering
// ========================================
SAY "6. ITERATE for Data Filtering"
SAY "---"

SAY "Processing dataset (skip errors):"

// Simulate dataset with statuses
DATA.1 = "ok"
DATA.2 = "ok"
DATA.3 = "error"
DATA.4 = "ok"
DATA.5 = "error"
DATA.0 = "5"

processed = "0"
skipped = "0"

DO i = 1 TO 5
  status = DATA.{i}
  IF status = "error"
  THEN
    SAY "  Record {i}: ERROR - skipping"
    skipped = "1"  // Would increment
    ITERATE
  END
  SAY "  Record {i}: OK - processing"
  processed = "1"  // Would increment
END

SAY "  Summary: Processed some, skipped errors"

SAY ""

// ========================================
// 7. LEAVE in Nested Loops
// ========================================
SAY "7. LEAVE in Nested Loop (inner)"
SAY "---"

SAY "Matrix search:"
DO outer = 1 TO 3
  SAY "  Row {outer}:"
  DO inner = 1 TO 4
    SAY "    Column {inner}"
    IF inner = "2" THEN LEAVE  // exits inner loop only
  END
  SAY "  Row {outer} complete"
END

SAY ""

// ========================================
// 8. Practical: Input Validation
// ========================================
SAY "8. Practical Example: Validation Loop"
SAY "---"

SAY "Validating batch of values:"

// Simulate input values
VALUES.1 = "50"
VALUES.2 = "75"
VALUES.3 = "999"  // invalid - too high
VALUES.4 = "60"
VALUES.0 = "4"

all_valid = "yes"

DO i = 1 TO 4
  value = VALUES.{i}

  // Skip if value out of range
  IF value > "100"
  THEN
    SAY "  Value {i} ({value}): INVALID - out of range!"
    all_valid = "no"
    LEAVE  // stop validation on first error
  END

  SAY "  Value {i} ({value}): Valid"
END

IF all_valid = "yes"
  THEN SAY "  Result: All values valid"
  ELSE SAY "  Result: Validation failed"

SAY ""

// ========================================
// 9. Practical: Rate Limiting
// ========================================
SAY "9. Practical Example: Rate Limiting"
SAY "---"

SAY "Processing with rate limit:"
limit = "5"
processed_count = "0"

DO i = 1 TO 10
  SAY "  Processing request {i}..."

  processed_count = i

  // Check rate limit
  IF processed_count >= limit
  THEN
    SAY "  Rate limit reached ({limit} requests)"
    SAY "  Remaining requests deferred"
    LEAVE
  END
END

SAY ""
SAY "=== Demo Complete ==="

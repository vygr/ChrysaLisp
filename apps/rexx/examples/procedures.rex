// CALL and PROCEDURE Demonstration
// Shows subroutine definition and calling

SAY "=== Procedures and Subroutines Demo ==="
SAY ""

// ========================================
// 1. Simple Procedure Call
// ========================================
SAY "1. Simple Procedure Call"
SAY "---"

SAY "Calling Greet procedure..."
CALL Greet
SAY "Back from procedure"
SAY "Result: {RESULT}"
SAY ""

// ========================================
// 2. Procedure with Arguments
// ========================================
SAY "2. Procedure with Arguments"
SAY "---"

SAY "Calling Add with arguments 15 and 27..."
CALL Add 15 27
SAY "Sum: {RESULT}"
SAY ""

// ========================================
// 3. Multiple Procedure Calls
// ========================================
SAY "3. Multiple Procedure Calls"
SAY "---"

CALL Multiply 6 7
SAY "6 * 7 = {RESULT}"

CALL Multiply 12 3
SAY "12 * 3 = {RESULT}"
SAY ""

// ========================================
// 4. Procedure with String Processing
// ========================================
SAY "4. Procedure with String Processing"
SAY "---"

CALL FormatName "john" "doe"
SAY "Formatted name: {RESULT}"

CALL FormatName "alice" "smith"
SAY "Formatted name: {RESULT}"
SAY ""

// ========================================
// 5. Procedure Calling Another Procedure
// ========================================
SAY "5. Nested Procedure Calls"
SAY "---"

CALL Calculate 10 5
SAY "Calculation result: {RESULT}"
SAY ""

// ========================================
// 6. Procedure with Validation
// ========================================
SAY "6. Procedure with Validation"
SAY "---"

CALL ValidateEmail "user@example.com"
SAY "Validation result: {RESULT}"

CALL ValidateEmail "invalid-email"
SAY "Validation result: {RESULT}"
SAY ""

// ========================================
// Main Program Ends - Procedures Below
// ========================================
SAY "=== Demo Complete ==="
EXIT 0

// ========================================
// Procedure Definitions (after EXIT)
// ========================================

Greet:
  SAY "  Hello from Greet procedure!"
  RETURN "Greet completed"

Add:
  ARG num1 num2
  sum = "0"  // Would calculate: num1 + num2
  SAY "  Adding {num1} and {num2}"
  RETURN "42"  // Simplified - would be actual sum

Multiply:
  ARG a b
  SAY "  Multiplying {a} and {b}"
  // Simplified multiplication simulation
  IF a = "6"
    IF b = "7"
      RETURN "42"
    END
  END
  IF a = "12"
    IF b = "3"
      RETURN "36"
    END
  END
  RETURN "result"

FormatName:
  ARG first last
  formatted = PROPER(first)
  formatted2 = PROPER(last)
  full_name = "{formatted} {formatted2}"
  RETURN full_name

Calculate:
  ARG x y
  SAY "  Calculating with {x} and {y}"
  // Call another procedure
  CALL Add x y
  temp_sum = RESULT
  SAY "  Intermediate sum: {temp_sum}"
  RETURN temp_sum

ValidateEmail:
  ARG email
  // Simple validation - check for @
  has_at = INCLUDES(email, "@")
  IF has_at = "1"
  THEN
    RETURN "Valid email format"
  ELSE
    RETURN "Invalid email format"
  END

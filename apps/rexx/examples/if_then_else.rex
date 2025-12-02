// IF/THEN/ELSE Demonstration
// Shows inline and multi-line conditional syntax

SAY "=== IF/THEN/ELSE Demo ==="
SAY ""

// ========================================
// 1. Inline IF/THEN
// ========================================
SAY "1. Inline IF/THEN"
SAY "---"

score = "85"
IF score > "80" THEN SAY "  Pass: Score is above 80"

SAY ""

// ========================================
// 2. Inline IF/THEN/ELSE
// ========================================
SAY "2. Inline IF/THEN/ELSE"
SAY "---"

age = "17"
IF age >= "18" THEN SAY "  Adult" ELSE SAY "  Minor"

SAY ""

// ========================================
// 3. Multi-line IF/THEN/END
// ========================================
SAY "3. Multi-line IF/THEN/END"
SAY "---"

temperature = "95"
SAY "Temperature: {temperature}F"

IF temperature > "90"
THEN
  SAY "  It's very hot!"
  SAY "  Stay hydrated"
END

SAY ""

// ========================================
// 4. Multi-line IF/THEN/ELSE/END
// ========================================
SAY "4. Multi-line IF/THEN/ELSE/END"
SAY "---"

balance = "250"
SAY "Account balance: ${balance}"

IF balance >= "100"
THEN
  SAY "  Status: Good standing"
  SAY "  You have sufficient funds"
ELSE
  SAY "  Status: Low balance"
  SAY "  Please deposit funds"
END

SAY ""

// ========================================
// 5. Enhanced Comparison Operators
// ========================================
SAY "5. Enhanced Comparison Operators"
SAY "---"

value = "42"
SAY "Testing value: {value}"

// >= operator
IF value >= "40" THEN SAY "  >= 40: TRUE"

// <= operator
IF value <= "50" THEN SAY "  <= 50: TRUE"

// != operator
IF value != "0" THEN SAY "  != 0: TRUE"

// <> operator (alternate not-equal)
IF value <> "100" THEN SAY "  <> 100: TRUE"

SAY ""

// ========================================
// 6. String Comparisons
// ========================================
SAY "6. String Comparisons"
SAY "---"

status = "active"
SAY "Status: {status}"

IF status = "active" THEN SAY "  System is running"
IF status != "error" THEN SAY "  No errors detected"

SAY ""

// ========================================
// 7. Nested IF Statements
// ========================================
SAY "7. Nested IF Statements"
SAY "---"

user_type = "premium"
logged_in = "yes"

SAY "User type: {user_type}, Logged in: {logged_in}"

IF logged_in = "yes"
THEN
  SAY "  Welcome back!"
  IF user_type = "premium"
  THEN
    SAY "  Premium features enabled"
  ELSE
    SAY "  Standard features"
  END
ELSE
  SAY "  Please log in"
END

SAY ""

// ========================================
// 8. Practical: Grade Calculator
// ========================================
SAY "8. Practical Example: Grade Calculator"
SAY "---"

test_score = "87"
SAY "Test score: {test_score}"

IF test_score >= "90"
THEN
  grade = "A"
  message = "Excellent!"
ELSE
  IF test_score >= "80"
  THEN
    grade = "B"
    message = "Good work"
  ELSE
    IF test_score >= "70"
    THEN
      grade = "C"
      message = "Passing"
    ELSE
      grade = "F"
      message = "Needs improvement"
    END
  END
END

SAY "  Grade: {grade}"
SAY "  {message}"

SAY ""

// ========================================
// 9. Combining with SELECT
// ========================================
SAY "9. Combining IF with SELECT"
SAY "---"

access_level = "2"
SAY "Access level: {access_level}"

IF access_level != "0"
THEN
  SAY "  Access granted"
  SELECT
    WHEN access_level = "1"
      SAY "    Permission: Read only"
    WHEN access_level = "2"
      SAY "    Permission: Read and Write"
    WHEN access_level = "3"
      SAY "    Permission: Full admin"
    OTHERWISE
      SAY "    Permission: Unknown"
  END
ELSE
  SAY "  Access denied"
END

SAY ""
SAY "=== Demo Complete ==="

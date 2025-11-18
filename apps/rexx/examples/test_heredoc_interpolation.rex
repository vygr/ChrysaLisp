/* Test HEREDOC variable interpolation */

SAY "Testing HEREDOC with variable interpolation..."
SAY ""

// Set up test variables
LET userName = "Alice"
LET userAge = "30"
LET userCity = "New York"

// Test 1: Simple variable interpolation in HEREDOC
LET message = <<END
Hello {userName}!
You are {userAge} years old.
You live in {userCity}.
END

SAY "Test 1: Simple interpolation"
SAY message
SAY ""

// Test 2: HEREDOC with ADDRESS SYSTEM and interpolation
LET x = "5"
LET y = "10"
SAY "Test 2: ADDRESS SYSTEM with interpolation"
SAY "Variables: x={x}, y={y}"
ADDRESS SYSTEM
<<CALC
(+ {x} {y})
CALC
SAY "Result: {RESULT}"
SAY ""

// Test 3: JSON-like structure with interpolation
LET userId = "12345"
LET status = "active"
LET jsonData = <<DATA
{
  "user_id": "{userId}",
  "status": "{status}",
  "location": "{userCity}"
}
DATA

SAY "Test 3: JSON structure with interpolation"
SAY jsonData
SAY ""

SAY "All HEREDOC interpolation tests complete!"

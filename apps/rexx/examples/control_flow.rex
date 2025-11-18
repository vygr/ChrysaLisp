// Control Flow Demo
// Demonstrates SELECT/WHEN/OTHERWISE and enhanced DO loops

SAY "=== Control Flow Demo ==="
SAY ""

// ========================================
// SELECT/WHEN/OTHERWISE Examples
// ========================================
SAY "1. Simple SELECT/WHEN"
SAY "---"

day = "3"
SAY "Day number: {day}"

SELECT
  WHEN day = "1"
    SAY "  Monday"
  WHEN day = "2"
    SAY "  Tuesday"
  WHEN day = "3"
    SAY "  Wednesday"
  WHEN day = "4"
    SAY "  Thursday"
  WHEN day = "5"
    SAY "  Friday"
  OTHERWISE
    SAY "  Weekend!"
END

SAY ""

// ========================================
// SELECT with Numeric Comparisons
// ========================================
SAY "2. SELECT with Comparisons"
SAY "---"

temperature = "75"
SAY "Temperature: {temperature}F"

SELECT
  WHEN temperature > "85"
    SAY "  It's hot!"
  WHEN temperature > "65"
    SAY "  It's pleasant"
  WHEN temperature > "45"
    SAY "  It's cool"
  OTHERWISE
    SAY "  It's cold!"
END

SAY ""

// ========================================
// SELECT for Status Codes
// ========================================
SAY "3. HTTP Status Code Handler"
SAY "---"

status = "404"
SAY "Status code: {status}"

SELECT
  WHEN status = "200"
    message = "OK"
  WHEN status = "201"
    message = "Created"
  WHEN status = "400"
    message = "Bad Request"
  WHEN status = "404"
    message = "Not Found"
  WHEN status = "500"
    message = "Server Error"
  OTHERWISE
    message = "Unknown Status"
END

SAY "  Message: {message}"
SAY ""

// ========================================
// DO Loop - Simple Counter
// ========================================
SAY "4. DO Loop - Simple Iterations"
SAY "---"

SAY "DO 5 (repeat 5 times):"
DO 5
  SAY "  * Iteration"
END

SAY ""

// ========================================
// DO Loop - Index Variable
// ========================================
SAY "5. DO Loop - Index Variable"
SAY "---"

SAY "DO i = 1 TO 5:"
DO i = 1 TO 5
  SAY "  Item {i}"
END

SAY ""

// ========================================
// DO Loop - BY Step
// ========================================
SAY "6. DO Loop - BY Step"
SAY "---"

SAY "DO i = 0 TO 20 BY 5:"
DO i = 0 TO 20 BY 5
  SAY "  {i}%"
END

SAY ""

SAY "DO i = 10 TO 1 BY -2 (countdown):"
// Note: negative step not yet supported, using positive example
DO i = 2 TO 10 BY 2
  SAY "  {i}"
END

SAY ""

// ========================================
// Combined: Loop with SELECT
// ========================================
SAY "7. Combined: Loop with SELECT"
SAY "---"

SAY "Classification by number range:"
DO n = 1 TO 10
  SELECT
    WHEN n < "4"
      category = "Low"
    WHEN n < "8"
      category = "Medium"
    OTHERWISE
      category = "High"
  END
  SAY "  {n} -> {category}"
END

SAY ""

// ========================================
// Practical: Menu System
// ========================================
SAY "8. Practical Example: Menu Handler"
SAY "---"

choice = "2"
SAY "User selected option: {choice}"

SELECT
  WHEN choice = "1"
    SAY "  Opening file..."
  WHEN choice = "2"
    SAY "  Saving file..."
  WHEN choice = "3"
    SAY "  Printing..."
  WHEN choice = "0"
    SAY "  Exiting..."
  OTHERWISE
    SAY "  Error: Invalid option '{choice}'"
END

SAY ""

// ========================================
// Practical: Grade Calculator
// ========================================
SAY "9. Practical Example: Grade Calculator"
SAY "---"

// Array of test scores
SCORES.1 = "88"
SCORES.2 = "92"
SCORES.3 = "76"
SCORES.4 = "95"
SCORES.5 = "84"
SCORES.0 = "5"

SAY "Processing {SCORES.0} test scores:"
DO i = 1 TO 5
  score = SCORES.{i}

  SELECT
    WHEN score > "90"
      letter = "A"
    WHEN score > "80"
      letter = "B"
    WHEN score > "70"
      letter = "C"
    WHEN score > "60"
      letter = "D"
    OTHERWISE
      letter = "F"
  END

  SAY "  Test {i}: {score} points = {letter}"
END

SAY ""
SAY "=== Demo Complete ==="

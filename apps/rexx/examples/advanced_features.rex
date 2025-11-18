// Advanced REXX Features Demo
// Demonstrates SELECT/WHEN, DO loops, arrays, and JSON

SAY "=== Advanced REXX Features Demo ==="
SAY ""

// ========================================
// 1. SELECT/WHEN/OTHERWISE Demo
// ========================================
SAY "1. SELECT/WHEN/OTHERWISE Control Flow"
SAY "---"

grade = "B"
SAY "Grade: {grade}"

SELECT
  WHEN grade = "A"
    SAY "  Excellent!"
  WHEN grade = "B"
    SAY "  Good job!"
  WHEN grade = "C"
    SAY "  Average"
  OTHERWISE
    SAY "  Needs improvement"
END

SAY ""

// Numeric comparison
score = "85"
SAY "Score: {score}"

SELECT
  WHEN score > "90"
    SAY "  Grade: A"
  WHEN score > "80"
    SAY "  Grade: B"
  WHEN score > "70"
    SAY "  Grade: C"
  OTHERWISE
    SAY "  Grade: F"
END

SAY ""

// ========================================
// 2. DO Loop Examples
// ========================================
SAY "2. DO Loop Examples"
SAY "---"

// DO with counter (simple iteration)
SAY "Simple DO 3:"
DO 3
  SAY "  Loop iteration"
END

SAY ""

// DO with index variable
SAY "DO i = 1 TO 5:"
DO i = 1 TO 5
  SAY "  i = {i}"
END

SAY ""

// DO with BY step
SAY "DO i = 0 TO 10 BY 2:"
DO i = 0 TO 10 BY 2
  SAY "  i = {i}"
END

SAY ""

// DO WHILE
counter = "1"
SAY "DO WHILE counter < 4:"
DO WHILE counter < "4"
  SAY "  counter = {counter}"
  counter = "2"  // Would need to increment in real loop
END

SAY ""

// ========================================
// 3. Array Operations
// ========================================
SAY "3. Array Operations"
SAY "---"

// Create an array
result = ARRAY("FRUITS", "3", "")
SAY "Created array FRUITS with {result} elements"

// Populate array using stem variables
FRUITS.1 = "apple"
FRUITS.2 = "banana"
FRUITS.3 = "cherry"

SAY "FRUITS.1 = {FRUITS.1}"
SAY "FRUITS.2 = {FRUITS.2}"
SAY "FRUITS.3 = {FRUITS.3}"
SAY ""

// Push new element
new_count = PUSH("FRUITS", "date")
SAY "Pushed 'date', new count: {new_count}"
SAY "FRUITS.4 = {FRUITS.4}"
SAY ""

// Join array
joined = JOIN("FRUITS", ", ")
SAY "Joined array: {joined}"
SAY ""

// Sort array
sort_result = SORT("FRUITS")
SAY "Sorted {sort_result} elements"
SAY "After sort:"
SAY "  FRUITS.1 = {FRUITS.1}"
SAY "  FRUITS.2 = {FRUITS.2}"
SAY "  FRUITS.3 = {FRUITS.3}"
SAY "  FRUITS.4 = {FRUITS.4}"
SAY ""

// Reverse array
rev_result = REVERSE_ARRAY("FRUITS")
SAY "Reversed {rev_result} elements"
SAY "After reverse:"
SAY "  FRUITS.1 = {FRUITS.1}"
SAY "  FRUITS.2 = {FRUITS.2}"
SAY "  FRUITS.3 = {FRUITS.3}"
SAY "  FRUITS.4 = {FRUITS.4}"
SAY ""

// Pop element
popped = POP("FRUITS")
SAY "Popped: {popped}"
SAY "Remaining count: {FRUITS.0}"
SAY ""

// ========================================
// 4. JSON Operations
// ========================================
SAY "4. JSON Operations"
SAY "---"

// Create array for JSON
result = ARRAY("COLORS", "3", "")
COLORS.1 = "red"
COLORS.2 = "green"
COLORS.3 = "blue"

// Convert to JSON
json_str = JSON_STRINGIFY("COLORS")
SAY "JSON String: {json_str}"
SAY ""

// Parse JSON back
test_json = "[\"alpha\",\"beta\",\"gamma\"]"
parsed_count = JSON_PARSE(test_json, "PARSED")
SAY "Parsed {parsed_count} elements from JSON"
SAY "PARSED.1 = {PARSED.1}"
SAY "PARSED.2 = {PARSED.2}"
SAY "PARSED.3 = {PARSED.3}"
SAY ""

// ========================================
// 5. Combined Example: Process Data
// ========================================
SAY "5. Combined Example: Data Processing"
SAY "---"

// Create dataset
result = ARRAY("SCORES", "5", "0")
SCORES.1 = "75"
SCORES.2 = "92"
SCORES.3 = "88"
SCORES.4 = "65"
SCORES.5 = "95"

SAY "Original scores:"
DO i = 1 TO 5
  score = SCORES.{i}
  SAY "  Student {i}: {score}"
END
SAY ""

// Classify each score
SAY "Grade classification:"
DO i = 1 TO 5
  score = SCORES.{i}
  SELECT
    WHEN score > "90"
      grade = "A"
    WHEN score > "80"
      grade = "B"
    WHEN score > "70"
      grade = "C"
    OTHERWISE
      grade = "F"
  END
  SAY "  Student {i}: {score} -> Grade {grade}"
END

SAY ""
SAY "=== Demo Complete ==="

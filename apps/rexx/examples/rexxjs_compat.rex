// RexxJS Compatibility Demo
// Demonstrates RexxJS-compatible syntax and features

SAY "=== RexxJS Compatibility Demo ==="
SAY ""

// ========================================
// 1. || String Concatenation Operator
// ========================================
SAY "1. || String Concatenation Operator"
SAY "---"

LET first = "Hello"
LET second = "World"
LET greeting = first || " " || second || "!"

SAY "first = {first}"
SAY "second = {second}"
SAY "greeting = first || \" \" || second || \"!\""
SAY "Result: {greeting}"
SAY ""

// ========================================
// 2. LET Keyword for Variables
// ========================================
SAY "2. LET Keyword for Variable Assignment"
SAY "---"

LET name = "Alice"
LET age = "30"
LET city = "New York"

SAY "LET name = \"Alice\""
SAY "LET age = \"30\""
SAY "LET city = \"New York\""
SAY ""
SAY "Variables set:"
SAY "  name: {name}"
SAY "  age:  {age}"
SAY "  city: {city}"
SAY ""

// ========================================
// 3. || with Function Calls
// ========================================
SAY "3. || with Function Calls"
SAY "---"

LET fname = "john"
LET lname = "doe"
LET full_name = UPPER(fname) || " " || UPPER(lname)

SAY "fname = \"john\""
SAY "lname = \"doe\""
SAY "full_name = UPPER(fname) || \" \" || UPPER(lname)"
SAY "Result: {full_name}"
SAY ""

// ========================================
// 4. DO OVER Loop - Array Iteration
// ========================================
SAY "4. DO OVER Loop - Array Iteration"
SAY "---"

// Create an array using stem variable notation
colors.0 = "5"
colors.1 = "Red"
colors.2 = "Green"
colors.3 = "Blue"
colors.4 = "Yellow"
colors.5 = "Purple"

SAY "colors array: Red, Green, Blue, Yellow, Purple"
SAY ""
SAY "DO color OVER colors:"

DO color OVER colors
  SAY "  - {color}"
END

SAY ""

// ========================================
// 5. WORDPOS Function
// ========================================
SAY "5. WORDPOS - Find Word Position"
SAY "---"

LET sentence = "The quick brown fox jumps"
LET pos = WORDPOS("brown", sentence)

SAY "sentence = \"{sentence}\""
SAY "WORDPOS(\"brown\", sentence) = {pos}"
SAY ""

// ========================================
// 6. DELWORD Function
// ========================================
SAY "6. DELWORD - Delete Words"
SAY "---"

LET original = "one two three four five"
LET deleted = DELWORD(original, "2", "2")

SAY "original = \"{original}\""
SAY "DELWORD(original, 2, 2) = \"{deleted}\""
SAY "(Deletes 2 words starting at position 2)"
SAY ""

// ========================================
// 7. SUBWORD Function
// ========================================
SAY "7. SUBWORD - Extract Words"
SAY "---"

LET words = "alpha beta gamma delta epsilon"
LET sub = SUBWORD(words, "2", "3")

SAY "words = \"{words}\""
SAY "SUBWORD(words, 2, 3) = \"{sub}\""
SAY "(Extracts 3 words starting at position 2)"
SAY ""

// ========================================
// 8. ABBREV Function
// ========================================
SAY "8. ABBREV - Test Abbreviation"
SAY "---"

LET command = "PRINT"
LET abbr1 = "PRI"
LET abbr2 = "PR"
LET abbr3 = "P"

LET valid1 = ABBREV(command, abbr1, "3")
LET valid2 = ABBREV(command, abbr2, "3")
LET valid3 = ABBREV(command, abbr3, "3")

SAY "command = \"PRINT\""
SAY "ABBREV(command, \"PRI\", 3) = {valid1} (valid, >= 3 chars)"
SAY "ABBREV(command, \"PR\", 3)  = {valid2} (invalid, < 3 chars)"
SAY "ABBREV(command, \"P\", 3)   = {valid3} (invalid, < 3 chars)"
SAY ""

// ========================================
// 9. MATH_CEIL Function
// ========================================
SAY "9. MATH_CEIL - Ceiling Function"
SAY "---"

LET num1 = "7"
LET ceil1 = MATH_CEIL(num1)

SAY "MATH_CEIL(7) = {ceil1}"
SAY ""

// ========================================
// 10. MATH_FLOOR Function
// ========================================
SAY "10. MATH_FLOOR - Floor Function"
SAY "---"

LET num2 = "9"
LET floor1 = MATH_FLOOR(num2)

SAY "MATH_FLOOR(9) = {floor1}"
SAY ""

// ========================================
// 11. MATH_ROUND Function
// ========================================
SAY "11. MATH_ROUND - Rounding"
SAY "---"

LET num3 = "8"
LET round1 = MATH_ROUND(num3)

SAY "MATH_ROUND(8) = {round1}"
SAY ""

// ========================================
// 12. MATH_SQRT Function
// ========================================
SAY "12. MATH_SQRT - Square Root"
SAY "---"

LET num4 = "16"
LET sqrt1 = MATH_SQRT(num4)

LET num5 = "25"
LET sqrt2 = MATH_SQRT(num5)

SAY "MATH_SQRT(16) = {sqrt1}"
SAY "MATH_SQRT(25) = {sqrt2}"
SAY ""

// ========================================
// 13. Combined Example - Data Processing
// ========================================
SAY "13. Combined Example - Processing User Data"
SAY "---"

// Create array of names
names.0 = "3"
names.1 = "alice johnson"
names.2 = "bob smith"
names.3 = "charlie brown"

SAY "Processing names array:"
SAY ""

DO name OVER names
  // Split name into parts using WORD
  LET first_name = WORD(name, "1")
  LET last_name = WORD(name, "2")

  // Capitalize using UPPER and concatenate with ||
  LET formatted = UPPER(SUBSTR(first_name, "1", "1")) || LOWER(SUBSTR(first_name, "2")) || " " || UPPER(SUBSTR(last_name, "1", "1")) || LOWER(SUBSTR(last_name, "2"))

  SAY "  Original: {name}"
  SAY "  Formatted: {formatted}"
  SAY ""
END

// ========================================
// 14. Word Manipulation Pipeline
// ========================================
SAY "14. Word Manipulation Pipeline"
SAY "---"

LET text = "The quick brown fox jumps over the lazy dog"
SAY "Original: {text}"

// Remove "the" articles
LET step1 = DELWORD(text, "1", "1")
SAY "Step 1 (remove first word): {step1}"

// Extract middle portion
LET step2 = SUBWORD(step1, "2", "4")
SAY "Step 2 (extract words 2-5): {step2}"

// Build new sentence with ||
LET result = "Animals: " || step2

SAY "Step 3 (prepend text): {result}"
SAY ""

SAY "=== Demo Complete ==="
SAY "RexxJS compatibility features working!"

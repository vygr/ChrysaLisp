/* string_functions.rex - Additional string functions (RexxJS-inspired) */

SAY "=== Additional String Functions Demo ==="
SAY ""

// TRIM variants
SAY "1. TRIM Functions"
SAY "---"
text = "  hello world  "
SAY "Original: '{text}'"
SAY "TRIM_START: '" TRIM_START(text) "'"
SAY "TRIM_END: '" TRIM_END(text) "'"
SAY "TRIM (both): '" TRIM(text) "'"
SAY ""

// PROPER - Title Case
SAY "2. PROPER Function (Title Case)"
SAY "---"
name = "john doe smith"
SAY "Original: {name}"
SAY "PROPER: " PROPER(name)

title = "the quick brown fox"
SAY "Original: {title}"
SAY "PROPER: " PROPER(title)
SAY ""

// PAD functions
SAY "3. PAD Functions"
SAY "---"
num = "42"
SAY "Original: '{num}'"
SAY "PAD_START(num, 5, '0'): '" PAD_START(num, 5, "0") "'"
SAY "PAD_END(num, 5, '0'): '" PAD_END(num, 5, "0") "'"
SAY "PAD_START(num, 10, ' '): '" PAD_START(num, 10, " ") "'"
SAY ""

// Table formatting with PAD
SAY "4. Table Formatting with PAD"
SAY "---"
name1 = "Alice"
age1 = "25"
name2 = "Bob"
age2 "30"
name3 = "Charlie"
age3 = "35"

SAY "Name" "    " "Age"
SAY "----" "    " "---"
SAY PAD_END(name1, 10, " ") age1
SAY PAD_END(name2, 10, " ") age2
SAY PAD_END(name3, 10, " ") age3
SAY ""

// COPIES - Repeat strings
SAY "5. COPIES Function"
SAY "---"
SAY "COPIES('*', 20): " COPIES("*", 20)
SAY "COPIES('-=', 15): " COPIES("-=", 15)
SAY "COPIES('ABC', 5): " COPIES("ABC", 5)
SAY ""

// Validation functions
SAY "6. Validation Functions"
SAY "---"

// IS_ALPHA
test1 = "Hello"
test2 = "Hello123"
test3 = "12345"
SAY "IS_ALPHA('{test1}'): " IS_ALPHA(test1)  // 1
SAY "IS_ALPHA('{test2}'): " IS_ALPHA(test2)  // 0
SAY "IS_ALPHA('{test3}'): " IS_ALPHA(test3)  // 0
SAY ""

// IS_NUMERIC
SAY "IS_NUMERIC('12345'): " IS_NUMERIC("12345")      // 1
SAY "IS_NUMERIC('123.45'): " IS_NUMERIC("123.45")    // 0 (has .)
SAY "IS_NUMERIC('ABC'): " IS_NUMERIC("ABC")          // 0
SAY ""

// IS_ALPHANUMERIC
SAY "IS_ALPHANUMERIC('Hello123'): " IS_ALPHANUMERIC("Hello123")  // 1
SAY "IS_ALPHANUMERIC('Hello'): " IS_ALPHANUMERIC("Hello")        // 1
SAY "IS_ALPHANUMERIC('123'): " IS_ALPHANUMERIC("123")            // 1
SAY "IS_ALPHANUMERIC('Hello!'): " IS_ALPHANUMERIC("Hello!")      // 0
SAY ""

// IS_EMPTY
SAY "IS_EMPTY(''): " IS_EMPTY("")           // 1
SAY "IS_EMPTY('   '): " IS_EMPTY("   ")     // 1 (only whitespace)
SAY "IS_EMPTY('text'): " IS_EMPTY("text")   // 0
SAY ""

// String testing functions
SAY "7. String Testing Functions"
SAY "---"
filename = "document.txt"
SAY "Filename: {filename}"
SAY "STARTS_WITH(filename, 'doc'): " STARTS_WITH(filename, "doc")   // 1
SAY "ENDS_WITH(filename, '.txt'): " ENDS_WITH(filename, ".txt")     // 1
SAY "INCLUDES(filename, 'ment'): " INCLUDES(filename, "ment")       // 1
SAY ""

// Practical example: Data validation
SAY "8. Practical Example: Input Validation"
SAY "---"
username = "alice123"
email = "alice@example.com"
postal_code = "12345"

SAY "Username: {username}"
SAY "  IS_ALPHANUMERIC: " IS_ALPHANUMERIC(username)  // 1
SAY "  Length valid (5-20): " IF(LENGTH(username) >= 5 AND LENGTH(username) <= 20, "YES", "NO")
SAY ""

SAY "Email: {email}"
SAY "  INCLUDES '@': " INCLUDES(email, "@")  // 1
SAY "  INCLUDES '.': " INCLUDES(email, ".")  // 1
SAY ""

SAY "Postal Code: {postal_code}"
SAY "  IS_NUMERIC: " IS_NUMERIC(postal_code)  // 1
SAY "  Length = 5: " IF(LENGTH(postal_code) = 5, "YES", "NO")
SAY ""

// Format examples
SAY "9. Formatting Examples"
SAY "---"

// Credit card formatting
card = "1234567812345678"
formatted = PAD_START(SUBSTR(card, 1, 4), 4, "0") "-" PAD_START(SUBSTR(card, 5, 4), 4, "0") "-" PAD_START(SUBSTR(card, 9, 4), 4, "0") "-" PAD_START(SUBSTR(card, 13, 4), 4, "0")
SAY "Card: {formatted}"
SAY ""

// Progress bar
percent = "75"
bar_length = 20
filled = (LENGTH(percent) * bar_length) / 100
bar = COPIES("#", filled) COPIES("-", bar_length - filled)
SAY "Progress: [{bar}] {percent}%"
SAY ""

SAY "=== Demo Complete ==="
SAY ""
SAY "New Functions Added:"
SAY "  String: TRIM_START, TRIM_END, PROPER, PAD_START, PAD_END, COPIES"
SAY "  Validation: IS_ALPHA, IS_NUMERIC, IS_ALPHANUMERIC, IS_EMPTY"
SAY "  Testing: STARTS_WITH, ENDS_WITH, INCLUDES"

EXIT 0

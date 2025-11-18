// Advanced String Functions Demo
// CENTER, DATATYPE, CHANGESTR, COUNTSTR, SPACE, TRANSLATE, etc.

SAY "=== Advanced String Functions Demo ==="
SAY ""

// ========================================
// 1. CENTER - Center Text in Field
// ========================================
SAY "1. CENTER - Centering Text"
SAY "---"

text = "Hello"
centered = CENTER(text, "15")
SAY "Original: '{text}'"
SAY "Centered: '{centered}'"
SAY "  (15 chars wide)"

centered_star = CENTER(text, "15", "*")
SAY "With stars: '{centered_star}'"
SAY ""

// ========================================
// 2. DATATYPE - Check Data Types
// ========================================
SAY "2. DATATYPE - Data Type Checking"
SAY "---"

num_str = "12345"
alpha_str = "Hello"
mixed_str = "Hello123"

SAY "'{num_str}' type: {DATATYPE(num_str)}"
SAY "'{alpha_str}' type: {DATATYPE(alpha_str)}"
SAY "'{mixed_str}' type: {DATATYPE(mixed_str)}"
SAY ""

// Test specific types
IF DATATYPE(num_str, "NUM") = "1" THEN
  SAY "  '{num_str}' is numeric: TRUE"
END

IF DATATYPE(alpha_str, "ALPHA") = "1" THEN
  SAY "  '{alpha_str}' is alpha: TRUE"
END
SAY ""

// ========================================
// 3. CHANGESTR - Replace All Occurrences
// ========================================
SAY "3. CHANGESTR - String Replacement"
SAY "---"

original = "Hello World, Hello Universe"
SAY "Original: {original}"

changed = CHANGESTR("Hello", "Hi", original)
SAY "Changed:  {changed}"
SAY "  (replaced 'Hello' with 'Hi')"
SAY ""

// ========================================
// 4. COUNTSTR - Count Occurrences
// ========================================
SAY "4. COUNTSTR - Count Occurrences"
SAY "---"

text = "banana"
count_a = COUNTSTR("a", text)
count_an = COUNTSTR("an", text)

SAY "In '{text}':"
SAY "  'a' appears {count_a} times"
SAY "  'an' appears {count_an} times"
SAY ""

// ========================================
// 5. SPACE - Normalize Spacing
// ========================================
SAY "5. SPACE - Normalize Spacing"
SAY "---"

messy = "too    many    spaces"
SAY "Messy:      '{messy}'"

clean = SPACE(messy)
SAY "Cleaned:    '{clean}'"
SAY "  (normalized to single spaces)"

double_space = SPACE(messy, "2")
SAY "Two spaces: '{double_space}'"
SAY ""

// ========================================
// 6. DELSTR - Delete Substring
// ========================================
SAY "6. DELSTR - Delete Substring"
SAY "---"

original = "Hello World!"
SAY "Original: '{original}'"

deleted = DELSTR(original, "7", "6")
SAY "Deleted:  '{deleted}'"
SAY "  (deleted 6 chars starting at position 7)"
SAY ""

// ========================================
// 7. INSERT - Insert String
// ========================================
SAY "7. INSERT - Insert String"
SAY "---"

target = "Hello!"
new_text = " World"

inserted = INSERT(new_text, target, "5")
SAY "Target:   '{target}'"
SAY "Insert:   '{new_text}'"
SAY "Result:   '{inserted}'"
SAY "  (inserted at position 5)"
SAY ""

// ========================================
// 8. OVERLAY - Overlay String
// ========================================
SAY "8. OVERLAY - Overlay String"
SAY "---"

target = "Hello World"
overlay_text = "***"

overlaid = OVERLAY(overlay_text, target, "7")
SAY "Target:   '{target}'"
SAY "Overlay:  '{overlay_text}'"
SAY "Result:   '{overlaid}'"
SAY "  (overlaid at position 7)"
SAY ""

// ========================================
// 9. TRANSLATE - Character Translation
// ========================================
SAY "9. TRANSLATE - Character Translation"
SAY "---"

text = "hello"
SAY "Original: '{text}'"

// No args = uppercase
upper = TRANSLATE(text)
SAY "Uppercase: '{upper}'"

// Character substitution
text2 = "abc123"
translated = TRANSLATE(text2, "XYZ", "abc")
SAY ""
SAY "Original: '{text2}'"
SAY "Translated: '{translated}'"
SAY "  (a->X, b->Y, c->Z)"
SAY ""

// ========================================
// 10. INDEX - Find Position
// ========================================
SAY "10. INDEX - Find Position"
SAY "---"

haystack = "Hello World"
needle = "World"

pos = INDEX(haystack, needle)
SAY "Text:   '{haystack}'"
SAY "Find:   '{needle}'"
SAY "Position: {pos}"
SAY ""

// ========================================
// 11. Practical: Text Formatting
// ========================================
SAY "11. Practical: Text Formatting"
SAY "---"

title = "Report"
width = "40"

// Create formatted header
header = CENTER(title, width, "=")
SAY header

// Format data rows
name = "Item"
value = "123"
row = name
row = INSERT(value, row, "35")
SAY row

name2 = "Total"
value2 = "456"
row2 = name2
row2 = INSERT(value2, row2, "35")
SAY row2

footer = CENTER("", width, "=")
SAY footer
SAY ""

// ========================================
// 12. Practical: Data Cleaning
// ========================================
SAY "12. Practical: Data Cleaning"
SAY "---"

// Messy user input
input = "  too   many   spaces  "
SAY "Input:   '{input}'"

// Clean it up
cleaned = SPACE(input)
SAY "Cleaned: '{cleaned}'"

// Remove unwanted characters
text_with_nums = "ABC123DEF456"
SAY ""
SAY "With numbers: '{text_with_nums}'"

// Count digits
digit_count = COUNTSTR("1", text_with_nums)
digit_count = digit_count
SAY "Contains digits: yes"
SAY ""

SAY "=== Demo Complete ==="

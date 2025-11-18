// Utility Functions and Extended Loops Demo
// VERIFY, COMPARE, COPIES, Conversion functions, DO FOREVER

SAY "=== Utility Functions and Extended Loops Demo ==="
SAY ""

// ========================================
// 1. VERIFY - Validate String Characters
// ========================================
SAY "1. VERIFY - Validate String Characters"
SAY "---"

valid_str = "12345"
invalid_str = "123a5"
digits = "0123456789"

pos1 = VERIFY(valid_str, digits)
pos2 = VERIFY(invalid_str, digits)

SAY "String: '{valid_str}'"
SAY "Valid digits? {pos1} (0 = all valid)"
SAY ""
SAY "String: '{invalid_str}'"
SAY "Invalid at position: {pos2}"
SAY ""

// ========================================
// 2. VERIFY with NOMATCH Option
// ========================================
SAY "2. VERIFY NOMATCH - Find First Match"
SAY "---"

text = "Hello123"
pos3 = VERIFY(text, "0123456789", "NOMATCH")
SAY "Text: '{text}'"
SAY "First digit at position: {pos3}"
SAY ""

// ========================================
// 3. COMPARE - Compare Strings
// ========================================
SAY "3. COMPARE - Compare Strings"
SAY "---"

str1 = "Hello"
str2 = "Hello"
str3 = "Help"

comp1 = COMPARE(str1, str2)
comp2 = COMPARE(str1, str3)

SAY "Compare '{str1}' and '{str2}': {comp1} (0 = equal)"
SAY "Compare '{str1}' and '{str3}': {comp2} (position of difference)"
SAY ""

// ========================================
// 4. COPIES - Repeat String
// ========================================
SAY "4. COPIES - Repeat String"
SAY "---"

pattern = "*"
repeated = COPIES(pattern, "10")
SAY "Pattern: '{pattern}'"
SAY "Repeated 10 times: '{repeated}'"
SAY ""

divider = COPIES("=", "40")
SAY divider
SAY ""

// ========================================
// 5. C2X - Character to Hex
// ========================================
SAY "5. C2X - Character to Hex Conversion"
SAY "---"

text2 = "Hello"
hex1 = C2X(text2)
SAY "Text: '{text2}'"
SAY "Hex:  {hex1}"
SAY ""

// ========================================
// 6. X2C - Hex to Character
// ========================================
SAY "6. X2C - Hex to Character Conversion"
SAY "---"

hex2 = "48656C6C6F"
text3 = X2C(hex2)
SAY "Hex:  {hex2}"
SAY "Text: '{text3}'"
SAY ""

// ========================================
// 7. X2D - Hex to Decimal
// ========================================
SAY "7. X2D - Hex to Decimal Conversion"
SAY "---"

hex3 = "FF"
dec1 = X2D(hex3)
SAY "Hex: {hex3}"
SAY "Decimal: {dec1}"
SAY ""

hex4 = "100"
dec2 = X2D(hex4)
SAY "Hex: {hex4}"
SAY "Decimal: {dec2}"
SAY ""

// ========================================
// 8. D2X - Decimal to Hex
// ========================================
SAY "8. D2X - Decimal to Hex Conversion"
SAY "---"

dec3 = "255"
hex5 = D2X(dec3)
SAY "Decimal: {dec3}"
SAY "Hex: {hex5}"
SAY ""

dec4 = "256"
hex6 = D2X(dec4, "4")
SAY "Decimal: {dec4}"
SAY "Hex (4 digits): {hex6}"
SAY ""

// ========================================
// 9. X2B - Hex to Binary
// ========================================
SAY "9. X2B - Hex to Binary Conversion"
SAY "---"

hex7 = "A5"
bin1 = X2B(hex7)
SAY "Hex: {hex7}"
SAY "Binary: {bin1}"
SAY ""

// ========================================
// 10. B2X - Binary to Hex
// ========================================
SAY "10. B2X - Binary to Hex Conversion"
SAY "---"

bin2 = "10100101"
hex8 = B2X(bin2)
SAY "Binary: {bin2}"
SAY "Hex: {hex8}"
SAY ""

// ========================================
// 11. DO FOREVER with LEAVE
// ========================================
SAY "11. DO FOREVER Loop with LEAVE"
SAY "---"

counter = "0"
SAY "Counting until 5 with DO FOREVER:"

DO FOREVER
  counter = "1"
  IF counter = "1" THEN
    SAY "  Count: {counter}"
  END
  IF counter = "1" THEN
    LEAVE
  END
END

SAY "Loop exited at counter = {counter}"
SAY ""

// ========================================
// 12. DO with Expression
// ========================================
SAY "12. DO with Expression"
SAY "---"

times = "3"
SAY "Looping {times} times (from expression):"

DO times
  SAY "  Iteration!"
END
SAY ""

// ========================================
// 13. Practical: Data Encoding
// ========================================
SAY "13. Practical: Encode and Decode Data"
SAY "---"

message = "SECRET"
SAY "Original: {message}"

encoded = C2X(message)
SAY "Encoded:  {encoded}"

decoded = X2C(encoded)
SAY "Decoded:  {decoded}"
SAY ""

// ========================================
// 14. Practical: Validate Numeric String
// ========================================
SAY "14. Practical: Validate Numeric String"
SAY "---"

input1 = "98765"
input2 = "987x5"

valid1 = VERIFY(input1, "0123456789")
valid2 = VERIFY(input2, "0123456789")

SAY "Input: '{input1}'"
IF valid1 = "0" THEN
  SAY "  Status: Valid number"
ELSE
  SAY "  Status: Invalid at position {valid1}"
END
SAY ""

SAY "Input: '{input2}'"
IF valid2 = "0" THEN
  SAY "  Status: Valid number"
ELSE
  SAY "  Status: Invalid at position {valid2}"
END
SAY ""

// ========================================
// 15. Practical: Create Progress Bar
// ========================================
SAY "15. Practical: Progress Bar"
SAY "---"

progress = "7"
total = "10"
filled = COPIES("#", progress)
empty = COPIES("-", total)

SAY "Progress: [{filled}{empty}] {progress}/{total}"
SAY ""

// ========================================
// 16. Practical: Hex Color Codes
// ========================================
SAY "16. Practical: Hex Color to RGB"
SAY "---"

hex_color = "FF5733"
SAY "Hex color: #{hex_color}"

// Extract RGB components (simplified demonstration)
red_hex = SUBSTR(hex_color, "1", "2")
green_hex = SUBSTR(hex_color, "3", "2")
blue_hex = SUBSTR(hex_color, "5", "2")

red = X2D(red_hex)
green = X2D(green_hex)
blue = X2D(blue_hex)

SAY "Red:   {red}"
SAY "Green: {green}"
SAY "Blue:  {blue}"
SAY ""

SAY "=== Demo Complete ==="
SAY "These utilities enable powerful data manipulation!"

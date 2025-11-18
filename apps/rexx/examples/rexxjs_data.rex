// RexxJS Data Functions Demo
// ADDRESS SYSTEM, CSV_TO_JSON, SLUG, and more

SAY "=== RexxJS Data Functions Demo ==="
SAY ""

// ========================================
// 1. ADDRESS SYSTEM - Execute ChrysaLisp Code
// ========================================
SAY "1. ADDRESS SYSTEM - ChrysaLisp Execution"
SAY "---"

ADDRESS SYSTEM
(+ 10 20)

SAY "Executed ChrysaLisp: (+ 10 20)"
SAY "Result in RC: {RC}"
SAY "Result in RESULT: {RESULT}"
SAY ""

ADDRESS SYSTEM
(* 7 6)

SAY "Executed: (* 7 6)"
SAY "Result: {RESULT}"
SAY ""

// ========================================
// 2. CSV_TO_JSON - Data Transformation
// ========================================
SAY "2. CSV_TO_JSON - Convert CSV to JSON"
SAY "---"

LET csv_data = "Alice" || CHR("10") || "Bob" || CHR("10") || "Charlie"
SAY "CSV data:"
SAY csv_data
SAY ""

LET json_result = CSV_TO_JSON(csv_data)
SAY "JSON result: {json_result}"
SAY ""

// ========================================
// 3. JSON_TO_CSV - Reverse Transformation
// ========================================
SAY "3. JSON_TO_CSV - Convert JSON to CSV"
SAY "---"

LET json_array = "[\"Red\",\"Green\",\"Blue\"]"
SAY "JSON: {json_array}"

LET csv_output = JSON_TO_CSV(json_array)
SAY "CSV output:"
SAY csv_output
SAY ""

// ========================================
// 4. SLUG - URL-Friendly Strings
// ========================================
SAY "4. SLUG - Create URL-Friendly Slugs"
SAY "---"

LET title1 = "Hello World!"
LET slug1 = SLUG(title1)
SAY "Title: {title1}"
SAY "Slug:  {slug1}"
SAY ""

LET title2 = "RexxJS  Is  Awesome!!!"
LET slug2 = SLUG(title2)
SAY "Title: {title2}"
SAY "Slug:  {slug2}"
SAY ""

// ========================================
// 5. REPEAT - String Repetition
// ========================================
SAY "5. REPEAT - Repeat Strings"
SAY "---"

LET pattern = "=-="
LET repeated = REPEAT(pattern, "5")
SAY "Pattern: {pattern}"
SAY "Repeated 5x: {repeated}"
SAY ""

// ========================================
// 6. INDEXOF - Find Position (0-indexed)
// ========================================
SAY "6. INDEXOF - Find Position (JavaScript style)"
SAY "---"

LET haystack = "abcdefgh"
LET pos1 = INDEXOF(haystack, "cde")
LET pos2 = INDEXOF(haystack, "xyz")

SAY "String: {haystack}"
SAY "INDEXOF('cde'): {pos1} (0-indexed)"
SAY "INDEXOF('xyz'): {pos2} (-1 = not found)"
SAY ""

// ========================================
// 7. Combined Example - Data Pipeline
// ========================================
SAY "7. Combined Example - Data Processing Pipeline"
SAY "---"

// Create data
LET raw_csv = "apple" || CHR("10") || "banana" || CHR("10") || "cherry"
SAY "Step 1 - Raw CSV:"
SAY raw_csv
SAY ""

// Transform to JSON
LET as_json = CSV_TO_JSON(raw_csv)
SAY "Step 2 - As JSON: {as_json}"
SAY ""

// Create slugs for each item (manual iteration)
LET item1 = "Apple Product"
LET item2 = "Banana Split!"
LET item3 = "Cherry Pie Recipe"

SAY "Step 3 - Create slugs:"
SAY "  '{item1}' -> '" || SLUG(item1) || "'"
SAY "  '{item2}' -> '" || SLUG(item2) || "'"
SAY "  '{item3}' -> '" || SLUG(item3) || "'"
SAY ""

// ========================================
// 8. ADDRESS SYSTEM - String Operations
// ========================================
SAY "8. ADDRESS SYSTEM - ChrysaLisp String Ops"
SAY "---"

ADDRESS SYSTEM
(ascii-upper "hello")

SAY "Executed: (ascii-upper \"hello\")"
SAY "Result: {RESULT}"
SAY ""

ADDRESS SYSTEM
(cat "Concat" "enate")

SAY "Executed: (cat \"Concat\" \"enate\")"
SAY "Result: {RESULT}"
SAY ""

SAY "=== Demo Complete ==="
SAY "RexxJS data functions working with ChrysaLisp!"

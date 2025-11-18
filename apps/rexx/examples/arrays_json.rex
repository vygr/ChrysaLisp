// Array and JSON Operations Demo
// Demonstrates modern array manipulation and JSON support

SAY "=== Array and JSON Demo ==="
SAY ""

// ========================================
// Array Creation and Basic Operations
// ========================================
SAY "Array Creation:"
SAY "---"

// Create array with ARRAY() function
count = ARRAY("NAMES", "3", "unknown")
SAY "Created NAMES array with {count} elements"
SAY "Initial values (all 'unknown'):"
SAY "  NAMES.1 = {NAMES.1}"
SAY "  NAMES.2 = {NAMES.2}"
SAY "  NAMES.3 = {NAMES.3}"
SAY "  NAMES.0 = {NAMES.0} (count)"
SAY ""

// Update values
NAMES.1 = "Alice"
NAMES.2 = "Bob"
NAMES.3 = "Charlie"
SAY "After updates:"
SAY "  NAMES.1 = {NAMES.1}"
SAY "  NAMES.2 = {NAMES.2}"
SAY "  NAMES.3 = {NAMES.3}"
SAY ""

// ========================================
// Array Manipulation
// ========================================
SAY "Array Manipulation:"
SAY "---"

// PUSH - add element to end
new_size = PUSH("NAMES", "Diana")
SAY "PUSH('Diana') -> size = {new_size}"
SAY "  NAMES.4 = {NAMES.4}"
SAY ""

// JOIN - concatenate elements
result = JOIN("NAMES", ", ")
SAY "JOIN with ', ': {result}"
SAY ""

// SORT - alphabetical sort
sort_count = SORT("NAMES")
SAY "SORT -> sorted {sort_count} elements:"
SAY "  NAMES.1 = {NAMES.1}"
SAY "  NAMES.2 = {NAMES.2}"
SAY "  NAMES.3 = {NAMES.3}"
SAY "  NAMES.4 = {NAMES.4}"
SAY ""

// REVERSE_ARRAY - reverse order
rev_count = REVERSE_ARRAY("NAMES")
SAY "REVERSE_ARRAY -> reversed {rev_count} elements:"
SAY "  NAMES.1 = {NAMES.1}"
SAY "  NAMES.2 = {NAMES.2}"
SAY "  NAMES.3 = {NAMES.3}"
SAY "  NAMES.4 = {NAMES.4}"
SAY ""

// POP - remove last element
last = POP("NAMES")
SAY "POP -> removed '{last}'"
SAY "New size: {NAMES.0}"
SAY ""

// ========================================
// JSON Operations
// ========================================
SAY "JSON Operations:"
SAY "---"

// Create a simple dataset
count = ARRAY("PRODUCTS", "4", "")
PRODUCTS.1 = "laptop"
PRODUCTS.2 = "mouse"
PRODUCTS.3 = "keyboard"
PRODUCTS.4 = "monitor"

SAY "Original array PRODUCTS:"
SAY "  PRODUCTS.1 = {PRODUCTS.1}"
SAY "  PRODUCTS.2 = {PRODUCTS.2}"
SAY "  PRODUCTS.3 = {PRODUCTS.3}"
SAY "  PRODUCTS.4 = {PRODUCTS.4}"
SAY ""

// Convert to JSON
json = JSON_STRINGIFY("PRODUCTS")
SAY "JSON_STRINGIFY:"
SAY "  {json}"
SAY ""

// Parse JSON into new array
input_json = "[\"red\",\"green\",\"blue\",\"yellow\"]"
SAY "Parsing JSON: {input_json}"
parsed = JSON_PARSE(input_json, "COLORS")
SAY "JSON_PARSE -> {parsed} elements in COLORS:"
SAY "  COLORS.1 = {COLORS.1}"
SAY "  COLORS.2 = {COLORS.2}"
SAY "  COLORS.3 = {COLORS.3}"
SAY "  COLORS.4 = {COLORS.4}"
SAY ""

// ========================================
// Practical Example: Data Processing
// ========================================
SAY "Practical Example - User Tags:"
SAY "---"

// Initialize user tags
count = ARRAY("TAGS", "0", "")
SAY "Created empty TAGS array"

// Add tags dynamically
PUSH("TAGS", "admin")
PUSH("TAGS", "developer")
PUSH("TAGS", "reviewer")
PUSH("TAGS", "author")

SAY "Added {TAGS.0} tags"

// Show all tags
all_tags = JOIN("TAGS", " | ")
SAY "All tags: {all_tags}"

// Export as JSON for API
tags_json = JSON_STRINGIFY("TAGS")
SAY "JSON export: {tags_json}"
SAY ""

SAY "=== Demo Complete ==="

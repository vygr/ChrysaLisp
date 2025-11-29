// PARSE Instruction Demo
// Demonstrates comprehensive PARSE capabilities for string extraction

SAY "=== PARSE Instruction Demo ==="
SAY ""

// ========================================
// 1. PARSE VALUE ... WITH (Word Extraction)
// ========================================
SAY "1. PARSE VALUE with Word Extraction"
SAY "---"

data = "John Doe 25 Engineer"
SAY "Data: {data}"

PARSE VALUE data WITH first last age profession
SAY "First name: {first}"
SAY "Last name:  {last}"
SAY "Age:        {age}"
SAY "Profession: {profession}"
SAY ""

// ========================================
// 2. PARSE VALUE with Literal Delimiters
// ========================================
SAY "2. PARSE VALUE with Literal Delimiters"
SAY "---"

email = "user@example.com"
SAY "Email: {email}"

PARSE VALUE email WITH username "@" domain
SAY "Username: {username}"
SAY "Domain:   {domain}"
SAY ""

// ========================================
// 3. PARSE VALUE with Multiple Delimiters
// ========================================
SAY "3. PARSE VALUE with Multiple Delimiters"
SAY "---"

path = "C:\Users\John\Documents"
SAY "Path: {path}"

PARSE VALUE path WITH drive ":" "\" folder "\" username2 "\" subfolder
SAY "Drive:     {drive}"
SAY "Folder:    {folder}"
SAY "Username:  {username2}"
SAY "Subfolder: {subfolder}"
SAY ""

// ========================================
// 4. PARSE VAR (Parse Existing Variable)
// ========================================
SAY "4. PARSE VAR - Parse Existing Variable"
SAY "---"

date_string = "2024-03-15"
SAY "Date string: {date_string}"

PARSE VAR date_string year "-" month "-" day
SAY "Year:  {year}"
SAY "Month: {month}"
SAY "Day:   {day}"
SAY ""

// ========================================
// 5. PARSE UPPER VALUE
// ========================================
SAY "5. PARSE UPPER VALUE - Convert to Uppercase"
SAY "---"

input = "hello world from rexx"
SAY "Input: {input}"

PARSE UPPER VALUE input WITH word1 word2 word3 word4
SAY "Word 1: {word1}"
SAY "Word 2: {word2}"
SAY "Word 3: {word3}"
SAY "Word 4: {word4}"
SAY ""

// ========================================
// 6. PARSE LOWER VALUE
// ========================================
SAY "6. PARSE LOWER VALUE - Convert to Lowercase"
SAY "---"

input2 = "HELLO WORLD FROM REXX"
SAY "Input: {input2}"

PARSE LOWER VALUE input2 WITH word5 word6 word7 word8
SAY "Word 1: {word5}"
SAY "Word 2: {word6}"
SAY "Word 3: {word7}"
SAY "Word 4: {word8}"
SAY ""

// ========================================
// 7. PARSE with Positional Patterns
// ========================================
SAY "7. PARSE with Positional Extraction"
SAY "---"

fixed_data = "John      Doe       30"
SAY "Fixed data: '{fixed_data}'"

PARSE VALUE fixed_data WITH first_name 11 last_name 21 user_age
SAY "First name: '{first_name}'"
SAY "Last name:  '{last_name}'"
SAY "Age:        '{user_age}'"
SAY ""

// ========================================
// 8. Practical: Parse CSV Data
// ========================================
SAY "8. Practical: Parse CSV Data"
SAY "---"

csv_line = "Alice,30,Engineer,New York"
SAY "CSV: {csv_line}"

PARSE VALUE csv_line WITH name "," age2 "," job "," city
SAY "Name: {name}"
SAY "Age:  {age2}"
SAY "Job:  {job}"
SAY "City: {city}"
SAY ""

// ========================================
// 9. Practical: Parse URL
// ========================================
SAY "9. Practical: Parse URL"
SAY "---"

url = "https://www.example.com/path/page.html"
SAY "URL: {url}"

PARSE VALUE url WITH protocol "://" subdomain "." domain2 "." tld "/" path_part
SAY "Protocol: {protocol}"
SAY "Subdomain: {subdomain}"
SAY "Domain: {domain2}"
SAY "TLD: {tld}"
SAY "Path: {path_part}"
SAY ""

// ========================================
// 10. Practical: Parse Log Entry
// ========================================
SAY "10. Practical: Parse Log Entry"
SAY "---"

log = "2024-03-15 14:30:25 ERROR Database connection failed"
SAY "Log: {log}"

PARSE VALUE log WITH log_date log_time log_level log_message
SAY "Date:    {log_date}"
SAY "Time:    {log_time}"
SAY "Level:   {log_level}"
SAY "Message: {log_message}"
SAY ""

// ========================================
// 11. Practical: Parse Key-Value Pairs
// ========================================
SAY "11. Practical: Parse Configuration Line"
SAY "---"

config = "server_port=8080"
SAY "Config: {config}"

PARSE VALUE config WITH setting "=" value3
SAY "Setting: {setting}"
SAY "Value:   {value3}"
SAY ""

// ========================================
// 12. Practical: Extract File Extension
// ========================================
SAY "12. Practical: Extract File Extension"
SAY "---"

filename = "document.pdf"
SAY "Filename: {filename}"

PARSE VALUE filename WITH basename "." extension
SAY "Base name: {basename}"
SAY "Extension: {extension}"
SAY ""

SAY "=== Demo Complete ==="
SAY "PARSE is powerful for structured data extraction!"

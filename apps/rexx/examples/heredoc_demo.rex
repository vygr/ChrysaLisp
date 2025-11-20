// HEREDOC Multi-Line Strings Demo
// Demonstrates <<DELIMITER ... DELIMITER syntax

SAY "=== HEREDOC Multi-Line Strings Demo ==="
SAY ""

// ========================================
// 1. Basic HEREDOC
// ========================================
SAY "1. Basic HEREDOC - Multi-Line Text"
SAY "---"

LET message = <<END
This is a multi-line message.
It can span many lines.
No need for escape sequences!
END

SAY "Message stored with HEREDOC:"
SAY message
SAY ""

// ========================================
// 2. HEREDOC for Code/Templates
// ========================================
SAY "2. HEREDOC for Templates"
SAY "---"

LET template = <<TEMPLATE
Dear User,

Thank you for using RexxJS!
We hope you enjoy the HEREDOC feature.

Best regards,
The Team
TEMPLATE

SAY "Template content:"
SAY template
SAY ""

// ========================================
// 3. HEREDOC with DATA
// ========================================
SAY "3. HEREDOC for Structured Data"
SAY "---"

LET data = <<DATA
Name: Alice
Age: 30
City: New York
DATA

SAY "Data block:"
SAY data
SAY ""

// ========================================
// 4. Multiple HEREDOCs
// ========================================
SAY "4. Multiple HEREDOCs in Same Script"
SAY "---"

LET header = <<HEADER
===================
  REXX HEREDOC
===================
HEADER

LET footer = <<FOOTER
-------------------
  End of Demo
-------------------
FOOTER

SAY header
SAY "Content goes here"
SAY footer
SAY ""

// ========================================
// 5. HEREDOC with ADDRESS SYSTEM
// ========================================
SAY "5. HEREDOC with Multi-Line Lisp Code"
SAY "---"

// Multi-line ChrysaLisp code
ADDRESS SYSTEM
<<LISP
(defq x 10)
(defq y 20)
(+ x y)
LISP

SAY "Executed multi-line Lisp via HEREDOC"
SAY "Result: {RESULT}"
SAY ""

// ========================================
// 6. HEREDOC for JSON
// ========================================
SAY "6. HEREDOC for JSON Data"
SAY "---"

LET json_data = <<JSON
{
  "name": "RexxJS",
  "version": "1.0",
  "features": ["HEREDOC", "IPC", "Modern Syntax"]
}
JSON

SAY "JSON stored in HEREDOC:"
SAY json_data
SAY ""

// ========================================
// 7. HEREDOC for SQL-like Queries
// ========================================
SAY "7. HEREDOC for Query Strings"
SAY "---"

LET query = <<QUERY
SELECT name, age, city
FROM users
WHERE age > 25
ORDER BY name
QUERY

SAY "Query:"
SAY query
SAY ""

// ========================================
// 8. Short HEREDOC
// ========================================
SAY "8. Short HEREDOC (Single Line Works Too)"
SAY "---"

LET short = <<EOF
Just one line is fine too!
EOF

SAY "Short: {short}"
SAY ""

SAY "=== Demo Complete ==="
SAY "HEREDOC makes multi-line strings easy!"

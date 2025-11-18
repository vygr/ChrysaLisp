/* modern.rex - Modern REXX features (RexxJS-inspired) */

// This is a C-style comment (modern feature)
// Both // and traditional -- comments are supported

SAY "=== Modern REXX Features Demo ==="
SAY ""

// String Interpolation with {variable} syntax
SAY "1. String Interpolation (like RexxJS)"
SAY "---"

name = "ChrysaLisp"
version = "1.0"
author = "AI Assistant"

// Traditional concatenation would be:
// SAY "Running " name " version " version
// Modern interpolation is cleaner:
SAY "Running {name} version {version}"
SAY "Created by: {author}"
SAY ""

// Interpolation works with any variable
platform = "Lisp OS"
year = "2024"
SAY "Platform: {platform} ({year})"
SAY ""

// Multiple interpolations in one string
SAY "2. Complex Interpolation"
SAY "---"
user = "admin"
action = "login"
timestamp = "2024-01-18 15:30"
SAY "User '{user}' performed '{action}' at {timestamp}"
SAY ""

// Combining with escape sequences
SAY "3. Interpolation + Escape Sequences"
SAY "---"
first = "John"
last = "Doe"
age = "25"
city = "New York"

SAY "Name:\t{first} {last}\nAge:\t{age}\nCity:\t{city}"
SAY ""

// Interpolation in IPC scenarios
SAY "4. IPC with Interpolation"
SAY "---"
command = "ps"
filter = "lisp"
SAY "Imagine sending: {command} | grep {filter}"
SAY "(Would send to SYSTEM port with interpolated values)"
SAY ""

// JSON-like structured output with interpolation
SAY "5. Structured Data Output"
SAY "---"
event_type = "user_action"
event_user = "alice"
event_action = "file_upload"
event_size = "2048"

// Building a log entry with interpolation
SAY "Event: {event_type}"
SAY "  User: {event_user}"
SAY "  Action: {event_action}"
SAY "  Size: {event_size} bytes"
SAY ""

// Demonstration of comment styles
SAY "6. Comment Styles"
SAY "---"

; Traditional semicolon comment
SAY "Semicolon comments work"  ; inline too

-- Traditional REXX dash comment
SAY "Dash comments work"  -- also inline

// Modern C-style slash comment
SAY "Slash comments work"  // and inline!

SAY ""

// Mixed features in real-world scenario
SAY "7. Real-world Example: Logging"
SAY "---"
component = "AuthService"
level = "INFO"
message = "User authenticated successfully"
duration_ms = "125"

// Clean, readable log format with interpolation
SAY "[{level}] {component}: {message} (took {duration_ms}ms)"
SAY ""

// Variables can contain interpolation patterns too
template = "Status: {status}, Code: {code}"
status = "OK"
code = "200"
SAY template  // Will show: Status: {status}, Code: {code}
SAY ""       // (not interpolated because template is variable content)

// But direct interpolation works:
SAY "Status: {status}, Code: {code}"  // Will interpolate
SAY ""

SAY "=== Demo Complete ==="
SAY ""
SAY "Modern Features Demonstrated:"
SAY "  ✓ String interpolation with {variable}"
SAY "  ✓ C-style // comments"
SAY "  ✓ Combined with \\n escape sequences"
SAY "  ✓ Cleaner, more readable code"
SAY ""
SAY "These features make REXX more modern while"
SAY "staying backwards compatible with classic REXX!"

EXIT 0

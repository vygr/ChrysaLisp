// SIGNAL Instruction Demo
// Demonstrates goto-style flow control and error handling patterns

SAY "=== SIGNAL Demonstration ==="
SAY ""

// ========================================
// 1. Simple SIGNAL (goto)
// ========================================
SAY "1. Simple SIGNAL - Jump to Label"
SAY "---"

SAY "Before SIGNAL"
SIGNAL SkipSection

SAY "This line is skipped!"
SAY "And this one too!"

SkipSection:
SAY "Jumped to SkipSection label"
SAY ""

// ========================================
// 2. Conditional SIGNAL
// ========================================
SAY "2. Conditional SIGNAL"
SAY "---"

status = "error"
SAY "Checking status: {status}"

IF status = "error" THEN
  SIGNAL ErrorHandler
END

SAY "Normal processing (skipped if error)"

ErrorHandler:
SAY "Error handler activated!"
SAY "  Handling error condition"
SIGNAL Continue1

Continue1:
SAY "Continuing after error handling"
SAY ""

// ========================================
// 3. Menu System with SIGNAL
// ========================================
SAY "3. Menu System Pattern"
SAY "---"

choice = "2"
SAY "User choice: {choice}"

SELECT
  WHEN choice = "1"
    SIGNAL Option1
  WHEN choice = "2"
    SIGNAL Option2
  WHEN choice = "3"
    SIGNAL Option3
  OTHERWISE
    SAY "  Invalid option"
    SIGNAL MenuEnd
END

Option1:
SAY "  Processing Option 1"
SIGNAL MenuEnd

Option2:
SAY "  Processing Option 2"
SIGNAL MenuEnd

Option3:
SAY "  Processing Option 3"
SIGNAL MenuEnd

MenuEnd:
SAY "Menu processing complete"
SAY ""

// ========================================
// 4. Validation with Early Exit
// ========================================
SAY "4. Validation Pattern"
SAY "---"

username = ""
password = "secret"

SAY "Validating credentials..."

// Check username
IF username = "" THEN
  SAY "  Error: Username required"
  SIGNAL ValidationFailed
END

// Check password
IF password = "" THEN
  SAY "  Error: Password required"
  SIGNAL ValidationFailed
END

// All checks passed
SAY "  Validation successful!"
SIGNAL ValidationDone

ValidationFailed:
SAY "  Authentication failed"
SIGNAL ValidationDone

ValidationDone:
SAY "Validation complete"
SAY ""

// ========================================
// 5. State Machine Pattern
// ========================================
SAY "5. State Machine Pattern"
SAY "---"

state = "START"
counter = "0"

SAY "State machine starting..."

StateStart:
SAY "  State: START"
counter = "1"
state = "PROCESS"
IF state = "PROCESS" THEN SIGNAL StateProcess

StateProcess:
SAY "  State: PROCESS"
counter = "2"
state = "FINISH"
IF state = "FINISH" THEN SIGNAL StateFinish

StateFinish:
SAY "  State: FINISH"
counter = "3"
SAY "State machine completed"
SAY ""

// ========================================
// 6. Error Recovery Pattern
// ========================================
SAY "6. Error Recovery Pattern"
SAY "---"

file_status = "missing"
retry_count = "0"

ProcessFile:
SAY "Attempting to process file..."

IF file_status = "missing" THEN
  SAY "  File not found"
  SIGNAL HandleMissingFile
END

SAY "  File processed successfully"
SIGNAL FileProcessDone

HandleMissingFile:
SAY "  Handling missing file..."
retry_count = "1"

IF retry_count < "3" THEN
  SAY "  Retry attempt {retry_count}"
  SAY "  (would retry in real scenario)"
END

SAY "  Using default file"
SIGNAL FileProcessDone

FileProcessDone:
SAY "File processing complete"
SAY ""

// ========================================
// 7. Cleanup Pattern
// ========================================
SAY "7. Cleanup Pattern (finally-style)"
SAY "---"

operation = "process"

SAY "Starting operation: {operation}"

IF operation = "fail" THEN
  SAY "  Operation failed!"
  SIGNAL Cleanup
END

SAY "  Operation in progress"
SAY "  Operation completed"
SIGNAL Cleanup

Cleanup:
SAY "  Performing cleanup..."
SAY "  Closing resources"
SAY "  Cleanup complete"
SIGNAL AfterCleanup

AfterCleanup:
SAY "Done"
SAY ""

// ========================================
// 8. Multi-level Error Handling
// ========================================
SAY "8. Multi-level Error Handling"
SAY "---"

level = "critical"
SAY "Error level: {level}"

SELECT
  WHEN level = "warning"
    SIGNAL HandleWarning
  WHEN level = "error"
    SIGNAL HandleError
  WHEN level = "critical"
    SIGNAL HandleCritical
  OTHERWISE
    SAY "  Unknown error level"
END

HandleWarning:
SAY "  Warning handler: logging warning"
SIGNAL ErrorHandlingDone

HandleError:
SAY "  Error handler: logging error and notifying admin"
SIGNAL ErrorHandlingDone

HandleCritical:
SAY "  Critical handler: emergency shutdown initiated"
SIGNAL ErrorHandlingDone

ErrorHandlingDone:
SAY "Error handling complete"
SAY ""

SAY "=== Demo Complete ==="
SAY "Note: SIGNAL transfers control without return"
SAY "      (unlike CALL which returns to caller)"

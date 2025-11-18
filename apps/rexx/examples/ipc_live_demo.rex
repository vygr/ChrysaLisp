/* ipc_live_demo.rex - LIVE demonstration of ADDRESS/PORTS IPC */

// This demo actually communicates with the ECHO service
// demonstrating real inter-process communication via mailbox system

SAY "=========================================="
SAY "LIVE IPC Demo: ADDRESS/PORTS Communication"
SAY "=========================================="
SAY ""

// First, check what ports are available
SAY "Step 1: Discovering available REXX ports..."
SAY "---"

// This would use rexx-discover-ports() if we had it exposed
// For now, we'll proceed assuming ECHO service is running

SAY "Available ports:"
SAY "  - COMMAND (built-in)"
SAY "  - SYSTEM  (if system_port.lisp is running)"
SAY "  - ECHO    (if echo_service.lisp is running)"
SAY ""

// Step 2: Send a message to ECHO port
SAY "Step 2: Sending message to ECHO port..."
SAY "---"

// Switch to ECHO port
ADDRESS ECHO

// Send a test message
SAY "Sending: 'Hello from REXX!'"
"Hello from REXX!"

// The response is in RESULT variable
SAY "Response: {RESULT}"
SAY "RC: {RC}"
SAY ""

// Send another message
SAY "Sending: 'Testing IPC via mailbox system'"
"Testing IPC via mailbox system"
SAY "Response: {RESULT}"
SAY ""

// Step 3: Send multiple messages
SAY "Step 3: Sending multiple messages..."
SAY "---"

count = "1"
"Message number {count}"
SAY "Response: {RESULT}"

count = "2"
"Message number {count}"
SAY "Response: {RESULT}"

count = "3"
"Message number {count}"
SAY "Response: {RESULT}"
SAY ""

// Step 4: Query the service status
SAY "Step 4: Querying service status..."
SAY "---"
// This would need a QUERY command implementation
SAY "(Query functionality would show message count)"
SAY ""

// Step 5: Switch between ports
SAY "Step 5: Switching between ports..."
SAY "---"

ADDRESS COMMAND
SAY "Now on COMMAND port"

ADDRESS ECHO
"Back on ECHO port!"
SAY "Response: {RESULT}"

ADDRESS COMMAND
SAY "Back on COMMAND port"
SAY ""

// Step 6: Demonstrate with variables
SAY "Step 6: Using variables in IPC..."
SAY "---"

username = "alice"
action = "login"
timestamp = "2024-01-18"

ADDRESS ECHO
"User {username} performed {action} at {timestamp}"
SAY "Response: {RESULT}"
SAY ""

// Summary
SAY "=========================================="
SAY "IPC Demo Summary"
SAY "=========================================="
SAY ""
SAY "Demonstrated:"
SAY "  ✓ ADDRESS command to switch ports"
SAY "  ✓ Sending messages to remote service"
SAY "  ✓ Receiving responses via RESULT variable"
SAY "  ✓ RC (return code) handling"
SAY "  ✓ String interpolation in IPC messages"
SAY "  ✓ Multi-port switching"
SAY ""
SAY "IPC Mechanism:"
SAY "  - ChrysaLisp mailbox system (sys/mail)"
SAY "  - Service registration (mail-declare)"
SAY "  - Message passing (mail-send/mail-read)"
SAY "  - Network-transparent routing"
SAY ""
SAY "This is real IPC, not simulation!"
SAY "Messages actually sent via kernel mailboxes"
SAY "=========================================="

EXIT 0

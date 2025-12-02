/* ipc_demo.rex - Comprehensive IPC demonstration */

SAY "=========================================="
SAY "REXX for ChrysaLisp - IPC Demo"
SAY "Demonstrating ARexx-style ADDRESS/PORTS"
SAY "=========================================="
SAY ""

SAY "1. Basic REXX execution"
SAY "   Variables, functions, output"
SAY ""

message = "ChrysaLisp REXX"
SAY "Message: " message
SAY "Uppercase: " UPPER(message)
SAY "Length: " LENGTH(message)
SAY ""

SAY "2. ADDRESS SYSTEM - IPC to host system"
SAY "   Commands sent via mailbox to SYSTEM port"
SAY ""

ADDRESS SYSTEM
SAY "Switched to SYSTEM port (IPC active)"
"echo This command executed via IPC!"
"date"
SAY ""

SAY "3. Built-in functions"
teststr = "The quick brown fox jumps"
SAY "Test: " teststr
SAY "Word count: " WORDS(teststr)
SAY "3rd word: " WORD(teststr, 3)
SAY "Reversed: " REVERSE(teststr)
SAY ""

ADDRESS COMMAND
SAY "Back to COMMAND port"
SAY ""

SAY "=========================================="
SAY "Demo complete!"
SAY "Key achievement: ARexx ADDRESS/PORTS IPC"
SAY "  via ChrysaLisp mailbox system"
SAY "=========================================="

EXIT 0

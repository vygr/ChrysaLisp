/* system_port.rex - Demonstrate ADDRESS SYSTEM for IPC */

SAY "=== REXX ADDRESS SYSTEM Demo ==="
SAY ""

SAY "Current ADDRESS: COMMAND"
SAY "Switching to SYSTEM port..."
SAY ""

ADDRESS SYSTEM

SAY "Executing system commands via SYSTEM port:"
SAY "---"

"echo Hello from the SYSTEM port!"
"pwd"
"ls -la apps/rexx"

SAY "---"
SAY "System commands executed via IPC"
SAY ""

ADDRESS COMMAND
SAY "Back to COMMAND port"
SAY "RC=" RC

EXIT 0

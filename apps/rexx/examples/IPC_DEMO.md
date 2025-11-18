# REXX IPC Live Demo

This demonstrates **real inter-process communication** between REXX applications using the ADDRESS/PORTS system.

## Architecture

```
┌─────────────────────┐         ┌─────────────────────┐
│  REXX Client        │         │  ECHO Service       │
│  (ipc_live_demo.rex)│         │  (echo_service.lisp)│
└──────────┬──────────┘         └──────────┬──────────┘
           │                               │
           │  1. ADDRESS ECHO              │
           │──────────────────────────────>│
           │                               │
           │  2. "Hello from REXX!"        │
           │──────[mail-send]─────────────>│
           │                               │
           │                  3. Process & │
           │                     Echo back │
           │                               │
           │  4. "ECHO [1]: Hello..."      │
           │<─────[mail-send]──────────────│
           │                               │
           │  5. Result in RESULT var      │
           │                               │
```

## Components

### 1. Echo Service (`echo_service.lisp`)

A service that:
- Registers as "Rexx.ECHO" in the service directory
- Listens on its mailbox for messages
- Echoes back any received message with a prefix and counter
- Demonstrates the **receiver/server** side of IPC

**Key Code:**
```lisp
;Register service
(mail-declare (task-mbox) "Rexx.ECHO" "REXX Echo Service 1.0")

;Receive message
(defq msg (mail-read (task-mbox)))

;Echo back
(mail-send reply_id (rexx-alloc-result +rexx_rc_ok response ""))
```

### 2. Client Script (`ipc_live_demo.rex`)

A REXX script that:
- Discovers available ports
- Switches to ECHO port via ADDRESS
- Sends multiple messages
- Receives and displays responses
- Demonstrates the **sender/client** side of IPC

**Key Code:**
```rexx
ADDRESS ECHO                    // Switch to ECHO port
"Hello from REXX!"             // Send message
SAY "Response: {RESULT}"       // Display response
```

## Running the Demo

### Step 1: Start the Echo Service

```bash
# In ChrysaLisp
./apps/rexx/echo_service.lisp &
```

Or launch it as a background task from the GUI.

### Step 2: Run the Client

```bash
# Load and execute in REXX GUI
examples/ipc_live_demo.rex
```

Or execute from command line if running in ChrysaLisp terminal.

### Expected Output

**ECHO Service Console:**
```
ECHO Service started, registered as Rexx.ECHO
Waiting for messages...
Received: Hello from REXX!
Sending:  ECHO [1]: Hello from REXX!
Received: Testing IPC via mailbox system
Sending:  ECHO [2]: Testing IPC via mailbox system
Received: Message number 1
Sending:  ECHO [3]: Message number 1
...
```

**REXX Client Output:**
```
==========================================
LIVE IPC Demo: ADDRESS/PORTS Communication
==========================================

Step 1: Discovering available REXX ports...
---
Available ports:
  - COMMAND (built-in)
  - SYSTEM  (if system_port.lisp is running)
  - ECHO    (if echo_service.lisp is running)

Step 2: Sending message to ECHO port...
---
Sending: 'Hello from REXX!'
Response: ECHO [1]: Hello from REXX!
RC: 0

Sending: 'Testing IPC via mailbox system'
Response: ECHO [2]: Testing IPC via mailbox system

Step 3: Sending multiple messages...
---
Response: ECHO [3]: Message number 1
Response: ECHO [4]: Message number 2
Response: ECHO [5]: Message number 3
...
```

## What's Happening Under the Hood

### 1. Service Registration
```lisp
(mail-declare (task-mbox) "Rexx.ECHO" "REXX Echo Service 1.0")
```
- Service registers its mailbox with name "Rexx.ECHO"
- Added to ChrysaLisp service directory
- Discoverable via `mail-enquire`

### 2. Client Discovery
```rexx
ADDRESS ECHO
```
- Client calls `mail-enquire("Rexx.ECHO,")`
- Receives mailbox ID (net_id) of service
- Stores for message routing

### 3. Message Send
```rexx
"Hello from REXX!"
```
- Creates REXX command message
- Includes reply mailbox ID
- Sends via `mail-send` to ECHO service mailbox
- **Kernel routes message to correct task**

### 4. Service Processing
```lisp
(defq msg (mail-read (task-mbox)))
(defq command (slice msg +rexx_execute_size -1))
(defq response (cat "ECHO [" (str message_count) "]: " command))
```
- Service blocked on `mail-read`
- Receives message from kernel
- Processes (echoes back)
- Sends response to reply mailbox

### 5. Client Receives
```rexx
SAY "Response: {RESULT}"
```
- Client blocked on `mail-read` on reply mailbox
- Receives response from service
- Stores in RESULT variable
- RC set to return code

## IPC Characteristics

**Synchronous Request-Reply:**
- Client sends, waits for response
- Service processes, sends back
- RPC-style pattern

**Network Transparent:**
- Services can be on different nodes
- Mailbox routing handles it
- Same code works locally or distributed

**Type Safe:**
- Structured message format
- `+rexx_rpc` protocol
- Field access via `getf`

**Low Latency:**
- In-kernel message passing
- No network stack overhead
- Direct memory operations

## Advanced Examples

### Multiple Services

Run multiple services simultaneously:
```bash
./apps/rexx/echo_service.lisp &      # Rexx.ECHO
./apps/rexx/system_port.lisp &       # Rexx.SYSTEM
./apps/rexx/custom_service.lisp &    # Rexx.CUSTOM
```

Client can switch between them:
```rexx
ADDRESS ECHO
"test"

ADDRESS SYSTEM
"ls"

ADDRESS CUSTOM
"do something"
```

### Asynchronous Patterns

Service can spawn background tasks:
```lisp
;Launch worker for long-running operation
(open-task "worker.lisp" key +kn_call_open)
;Return immediately to client
(mail-send reply_id (rexx-alloc-result +rexx_rc_ok "Started" ""))
```

### Error Handling

Service reports errors via RC:
```lisp
(mail-send reply_id
  (rexx-alloc-result +rexx_rc_error "" "Service unavailable"))
```

Client checks:
```rexx
"command"
IF RC > 0 THEN
  SAY "Error: {RESULT}"
```

## Debugging

**Check if service is running:**
```rexx
// In REXX, would call rexx-list-ports()
// Shows all "Rexx.*" services registered
```

**Monitor messages:**
```bash
# In service code, add logging
(prin "Received: ") (prin command) (print)
```

**Verify mailbox IDs:**
```lisp
(prin "Service mailbox: ") (prin (task-mbox)) (print)
```

## Comparison to Other IPC

| Feature | Unix Pipes | Unix Sockets | ChrysaLisp Mailboxes |
|---------|-----------|--------------|---------------------|
| Named Services | ❌ No | ✅ Yes | ✅ Yes (service directory) |
| Bidirectional | ❌ No | ✅ Yes | ✅ Yes |
| Network-wide | ❌ No | ⚠️ TCP/IP | ✅ Yes (transparent) |
| Type Safety | ❌ Bytes | ❌ Bytes | ✅ Structures |
| Discovery | ❌ No | ⚠️ DNS | ✅ Built-in |
| Latency | ⚠️ Good | ⚠️ Higher | ✅ Lowest (in-kernel) |

## Next Steps

**Create your own service:**
1. Copy `echo_service.lisp`
2. Change service name: `"Rexx.MYSERVICE"`
3. Implement your processing logic
4. Register and run

**Use from REXX:**
```rexx
ADDRESS MYSERVICE
"your command here"
SAY "Response: {RESULT}"
```

This is the **real power** of ChrysaLisp REXX - true distributed computing with simple, clean syntax!

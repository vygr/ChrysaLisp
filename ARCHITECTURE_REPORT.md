# ChrysaLisp Architecture Report: TCP/IP Network Stack Implementation

## Executive Summary

ChrysaLisp is a 64-bit, self-hosting MIMD operating system designed for parallel and distributed computing. It features its own assembler, Lisp interpreter, Virtual Processor (VP) abstraction layer, and comprehensive networking infrastructure based on message-passing. This report details the architecture, infrastructure, and approach for implementing a TCP/IP network stack.

## 1. Project Structure & Organization

### Root Directory Layout
```
ChrysaLisp/
├── lib/              # Standard library & system code (VU code)
├── sys/              # System kernel, task, mail, memory management
├── class/            # Object-oriented class definitions
├── gui/              # GUI components and rendering
├── apps/             # User applications
├── cmd/              # Command-line utilities (make, docs)
├── service/          # OS services
├── src/              # C/C++ host environment code
├── Makefile          # Build configuration (hosts C/C++ build)
└── run*.sh           # Simulation scripts for different topologies
```

### Key Subdirectories
- **lib/asm/**: Assembler definitions, VP instruction set, class system macros
- **lib/trans/**: CPU architecture translations (vp.inc, x86_64.inc, arm64.inc, riscv64.inc)
- **lib/streams/**: Stream encoding (huffman, RLE, diff)
- **lib/collections/**: Data structures
- **sys/kernel/**: Kernel task and message routing
- **sys/task/**: Task management, scheduling, context switching
- **sys/mail/**: Inter-process communication (IPC) mailbox system
- **sys/link/**: Inter-node communication links for distributed systems
- **sys/mem/**: Memory allocation and management
- **sys/heap/**: Heap structures for memory pools
- **class/stream/**: Abstract stream I/O interface
- **class/in/, class/out/**: Bidirectional pipe streams over IPC
- **class/netid/**: Network identity (mailbox + node pair)


## 2. File Type System

ChrysaLisp uses multiple file types, each with specific purposes:

### **.vp Files** (Virtual Processor Code)
- **Purpose**: Low-level source code written in object-oriented assembler dialect
- **Language**: Lisp-like syntax with VP instruction macros
- **Contains**: Functions, methods, class implementations
- **Examples**:
  - `/sys/task/class.vp` - Task management (context switching, scheduling)
  - `/sys/kernel/class.vp` - Kernel (message routing, task spawning)
  - `/sys/mail/class.vp` - IPC mailbox system
  - `/class/stream/class.vp` - Abstract I/O stream interface

### **.lisp Files** (Lisp Source)
- **Purpose**: High-level application code and Lisp definitions
- **Language**: ChrysaLisp Lisp dialect
- **Examples**:
  - `/lib/asm/vp.inc` - VP instruction definitions and macros
  - `/lib/asm/class.inc` - Class system macros
  - `/apps/netspeed/app.lisp` - Network speed measurement application
  - `/cmd/make.lisp` - Build system implementation

### **.inc Files** (Include/Header Files)
- **Purpose**: Constant definitions, structure layouts, macros
- **Contains**: Symbolic definitions, memory layout information
- **Examples**:
  - `/sys/kernel/class.inc` - Kernel structure offsets
  - `/sys/mail/class.inc` - Mail message structures, net_id, node_id definitions
  - `/lib/trans/vp.inc` - VP register mappings
  - `/lib/trans/x86_64.inc` - x86-64 native code emission

### **.cpm Files** (Compressed Image Data)
- **Purpose**: Pre-rendered or compressed binary data (images, animations)
- **Examples**: App resources in `/apps/boing/data/`

## 3. Language & Implementation Details

### VP (Virtual Processor)
- **Architecture**: 64-bit RISC-like instruction set
- **Registers**: 15 general purpose (r0-r14) + stack pointer (rsp)
- **Portability**: Abstracts over x64, ARM64, RISC-V64, and includes VP64 emulator
- **Compilation**: Multi-stage process with self-hosted assembler

### Instruction Categories
```lisp
; Arithmetic/Logic
(vp-add-rr :r0 :r1)    ; Register-register
(vp-add-cr 5 :r0)      ; Constant-register
(vp-sub-rr, vp-and-rr, vp-or-rr, vp-xor-rr, etc.)

; Memory Operations
(vp-cpy-ir :r0 offset :r1)     ; Load from indirect address
(vp-cpy-ri :r0 :r1 offset)     ; Store to indirect address
(vp-cpy-ir-b, vp-cpy-ir-s)      ; Byte and short variants

; Control Flow
(vp-call, vp-call-r, vp-jmp, vp-jmp-r)    ; Function calls/jumps
(vp-beq-rr, vp-bne-rr)                    ; Conditional branches
(vp-ret)                                   ; Return

; Stack Operations
(vp-push :r0 :r1)     ; Push registers
(vp-pop :r0 :r1)      ; Pop registers
(vp-alloc size)       ; Allocate stack space
(vp-free size)        ; Deallocate stack space
```

## 4. System Call Interface & OS Primitives

### Kernel System Calls
Located in `/sys/kernel/class.vp`, handles:

1. **kn_call_open** - Load and start new tasks
2. **kn_call_child** - Request kernel to spawn child task on least-loaded node
3. **kn_call_ping** - Network service discovery/routing pings
4. **kn_call_callback** - Execute callback with kernel context

### Host OS Primitives
`host_os` class provides platform-specific operations:
- `pii_time` - Get current time (microseconds)
- `pii_write_str`, `pii_write_num` - Debug output
- `pii_rand` - Random number generation
- `pii_stat` - File status checking
- `pii_sleep` - Sleep for duration

## 5. Memory Management & Buffer Handling

### Memory Allocation Hierarchy
Located in `/sys/mem/class.vp`:

```
sys_mem:alloc(size)        → Returns (ptr, size_given)
  ↓
sys_mem:calloc(size)       → Allocates + clears
  ↓
sys_mem:free(ptr)          → Returns to pool
  ↓
sys_heap (pool manager)    → Multiple size class heaps
```

### Heap Structure
- **Multiple heaps**: One for each size class (powers of 2)
- **min size**: `mem_cell_min_size` (configurable)
- **max size**: `mem_cell_max_size` (configurable)
- **Header**: Each allocation includes `sys_mem_header_size` metadata
- **Methods**:
  - `sys_heap:init()` - Initialize heap with size/count
  - `sys_heap:alloc()` - Allocate from pool
  - `sys_heap:collect()` - Garbage collection

### Message Buffer System
Located in `/sys/mail/class.vp`:

```c
struct msg {
    ln_node_size        // List node (linking)
    ulong timestamp     // When message was sent
    struct dest         // Destination net_id (mailbox + node)
    struct src          // Source net_id
    uint frag_length    // Fragment size
    uint frag_offset    // Offset in sequence
    uint total_length   // Total message size
    ptr obj, frag_data  // Payload pointers
}

struct net_id {
    ulong mbox_id              // Mailbox ID
    struct node_id node_id     // (node1, node2) pair
}
```

### Buffer Pool Management
- **Mailbox buckets**: 8 buckets for mailbox hashing
- **Message heap**: Pre-allocated message structures
- **Link buffers**: 4KB shared memory buffers for inter-node communication

## 6. Existing Networking & I/O Infrastructure

### Inter-Process Communication (IPC)
**Location**: `/sys/mail/`

Core messaging system with:
- **Mailboxes**: Unique addresses (mbox_id) with priority lists
- **Messages**: Timestamped, sequenced packets with fragments
- **Reliability**: Ping/ack mechanism for routing
- **Load balancing**: Task count per link for distribution

**Methods**:
```
sys_mail:alloc_mbox()   → Create new mailbox
sys_mail:free_mbox()    → Destroy mailbox
sys_mail:send(msg)      → Queue message for sending
sys_mail:read(mbox)     → Get message from mailbox (blocking)
sys_mail:poll(mbox)     → Non-blocking check for message
sys_mail:select()       → Wait for multiple mailboxes
sys_mail:ping()         → Broadcast routing information
```

### Stream Abstraction Layer
**Location**: `/class/stream/`, `/class/in/`, `/class/out/`

High-level I/O using streams:
- **Abstract base**: `stream` class with virtual methods
  - `:read_char()` - Read single byte
  - `:write_char()` - Write single byte
  - `:read(buffer, len)` - Read block
  - `:write(buffer, len)` - Write block
  - `:read_line()` - Read until newline
  - `:flush()` - Commit buffered data

- **Implementations**:
  - `fstream` - File I/O
  - `sstream` - String buffer I/O
  - `mstream` - Memory buffer I/O
  - `in` - IPC receive (read-only pipe)
  - `out` - IPC send (write-only pipe)

**Pipe Protocol** (`in`/`out` streams):
```
out-stream (sender)                  in-stream (receiver)
    ↓
Creates stream_msg with seqnum   →   Receives and reorders
Packetizes data                      Maintains out-of-order buffer
Tracks ack_seqnum                    Sends ack messages
Sends on buffer full/flush           Waits for missing packets
Sends stopped flag on close          Returns EOF on stopped
```

### Link System (Inter-Node Communication)
**Location**: `/sys/link/`

Direct shared-memory or network links between nodes:
```c
struct lk_buf {           // Message buffer
    uint status           // ready/busy
    uint hash             // Hash for validation
    uint task_count       // Load balance info
    struct peer_node_id   // Destination node
    struct dest net_id    // Destination mailbox
    struct src net_id     // Source mailbox
    struct data [~1KB]    // Payload
}

struct lk_chan {          // Channel (3 buffers: ping/pong/spare)
    struct msg0, msg1, msg2
}

struct lk_shmem {         // Full shared memory region (8KB)
    struct chan_1         // Channel 1→2
    struct chan_2         // Channel 2→1
    align 4KB
}
```

**Methods**:
- `sys_link:link()` - Connect to remote node
- `sys_link:in()` - Receive from link
- `sys_link:out()` - Send to link

### Routing & Service Discovery
**Location**: `/sys/kernel/` (ping routine)

Distributed routing table maintained by kernel:
- Periodic pings broadcast service information
- Routing tuples track: origin, sessions, hops, vias
- Multi-hop flooding for service discovery
- Stale entry purging (TTL based on ping period)

**Kernel Routing Table**:
```c
hmap<node_id, tuple> node_map;
struct tuple {
    hset<service_name>     // Services at this node
    ulong time             // Last update
    long session           // Routing session
    long hops              // Hop count to node
    array<node_id> vias    // Path history
}
```

## 7. Build System

### Host Build (C/C++ using Makefile)
**File**: `/Makefile`

```makefile
all:    hostenv tui gui       # Build host executables
gui:    hostenv obj/$(CPU)/$(ABI)/$(OS)/main_gui
tui:    hostenv obj/$(CPU)/$(ABI)/$(OS)/main_tui
install: clean hostenv tui gui inst
```

**Features**:
- Detects CPU architecture (x86_64, arm64, riscv64)
- Builds SDL GUI or framebuffer display driver
- Compiles VP64 emulator bootstrap

### VP Build (Self-hosted inside running system)

**Internal make commands** (run inside ChrysaLisp):
```bash
make                # Incremental build (by file modification time)
make all            # Force recompile all VP source
make boot           # Create native boot image for current platform
make platforms      # Cross-compile for all supported architectures
make it             # Complete build: all, boot, platforms, docs
make test           # Timing tests
```

**Build Process**:
1. VP Assembler reads `.vp` source files
2. Compiles to intermediate representation
3. Links and resolves symbols
4. Emits native code for target architecture
5. Creates boot image with all compiled code

### Adding New Components

**To add a new module/library**:

1. **Create directory structure**:
   ```
   lib/mymodule/
   ├── class.inc          # Structure definitions
   └── class.vp           # Implementation
   ```

2. **Define structures** in `.inc`:
   ```lisp
   (def-struct my_struct 0
       (ptr field1 field2)
       (uint size)
       (ulong id))
   ```

3. **Implement class** in `.vp`:
   ```lisp
   (include "lib/asm/func.inc")
   (include "./class.inc")
   
   (def-method 'mymodule :init)
       ; Implementation
   (def-func-end)
   ```

4. **Export for Lisp** with `lisp.vp`:
   ```lisp
   (def-method 'mymodule :lisp_init)
       ; Lisp bindings
   (def-func-end)
   ```

5. **Add to build**: Include in parent module or add to boot sequence

## 8. Device Drivers & Low-Level I/O Examples

### Existing I/O Examples

#### 1. Host OS Interface
**Location**: `service/gui/`, `service/audio/`

Abstraction layer to host OS for:
- Graphics rendering (SDL)
- Audio playback (SDL_mixer)
- File system access
- Process management

#### 2. Stream-Based I/O
**Example**: Writing to files via streams
```lisp
(defq s (file-stream "/path/file" +file_open_write))
(write-blk s "Hello World")
(stream-flush s)
```

#### 3. IPC-Based I/O
**Example**: Sending data to another task
```lisp
(defq in (in-stream))              ; Create receive mailbox
(defq mbox (stream-netid in))      ; Get network address
(send-to-remote mbox "data")       ; Send via stream
(read-blk in 100)                  ; Read received data
```

### Creating a Device Driver

**Pattern for new device driver**:

```lisp
; 1. Define structure
(def-struct my_device ln_node_size
    (ptr data)
    (uint state)
    (ulong config))

; 2. Create class
(include "lib/asm/func.inc")

(def-method 'my_device :init)
    ; Initialize device
    ; Allocate resources
    ; Set up interrupts/polling
(def-func-end)

(def-method 'my_device :read)
    ; Read from device
    ; Handle state management
(def-func-end)

(def-method 'my_device :write)
    ; Write to device
(def-func-end)

; 3. Expose as stream (optional)
(def-method 'my_device_stream :read_next)
    ; Fetch data from device into buffer
(def-func-end)
```

## 9. Class System & Object Model

### Class Definition Macros
**Location**: `/lib/asm/class.inc`

```lisp
(def-class 'myclass :parent_class
    (dec-method :method1 mymodule/method1 :static inputs outputs)
    (dec-method :method2 mymodule/method2)
    (dec-property :prop1 offset size))

; Methods can be:
; - :static    - Static method (callable on class)
; - :virtual   - Virtual method (overridable, default behavior)
; - Regular    - Instance method
```

### Method Calling Convention
- **Inputs**: Register mapping (:r0, :r1, :r2, :r3, :r4, :r5)
- **Outputs**: Return registers specified
- **Calling**: Via vtable dispatch for virtual methods
- **Static dispatch**: Direct call for static methods

### Object Layout

All objects have:
```c
struct obj_header {
    vptr vtable;           // Pointer to method table
    // ... subclass fields follow
}
```

VTable is a special function (zero-length code, methods as links table):
```c
struct vtable {
    fn_header               // Standard function header
    ptr method1_ptr         // Links table (method implementations)
    ptr method2_ptr
    // ... method names in paths table
}
```

## 10. Architecture for TCP/IP Implementation

### Recommended Approach

Given the existing infrastructure, here's how to structure TCP/IP:

```
lib/net/
├── ip/                    # IP layer
│   ├── class.inc         # IP packet structures
│   ├── class.vp          # IP routing, fragmentation
│   └── lisp.vp           # Lisp bindings
├── tcp/                   # TCP layer
│   ├── class.inc         # TCP socket, connection state
│   ├── class.vp          # Connection management, retransmission
│   └── lisp.vp           # Lisp bindings
├── udp/                   # UDP layer
│   ├── class.inc         # UDP datagram structures
│   └── class.vp          # Simple datagram send/receive
├── icmp/                  # ICMP (ping)
└── ethernet/              # Link layer
    ├── class.inc         # Ethernet frame structures
    └── class.vp          # Frame transmission, ARP
```

### Key Design Decisions

1. **Layer on existing messaging**: Adapt `sys_mail` structures for TCP/IP
2. **Stream integration**: Implement socket as stream subclass
3. **Timer system**: Use task scheduler for retransmission timers
4. **Buffer management**: Leverage existing `sys_mem` allocation
5. **Async I/O**: Use Lisp coroutines with task scheduling

### Integration Points

1. **Device abstraction**: Wire to host network interface via `host_os`
2. **Task integration**: Network stack as background tasks
3. **Stream protocol**: TCP/UDP sockets as stream implementations
4. **Memory sharing**: Use link system for efficient packet buffers

## 11. File Statistics

- **Total source files**: ~495 (.vp, .lisp, .inc)
- **System code**: ~2000+ lines (kernel, task, mail)
- **Class definitions**: ~30+ core classes
- **Build system**: Self-hosted in Lisp
- **Supported platforms**: x64, ARM64, RISC-V64, VP64 emulator

## 12. Key Documentation Files

- `/README.md` - Project overview and quick start
- `/docs/ai_digest/streams.md` - Stream I/O system details
- `/docs/ai_digest/vp_functions.md` - VP function format and calling convention
- `/docs/intro/intro.md` - Comprehensive introduction
- Various `.md` files in `/docs/apps/` - Application documentation

# WebAssembly Integration for ChrysaLisp

This document describes how to use WebAssembly (WASM) modules within ChrysaLisp.

## Overview

ChrysaLisp now supports loading and executing arbitrary WebAssembly modules through the wasm3 runtime. WASM modules can be used in two ways:

1. **Command-line**: Execute WASM functions from Lisp code or command-line tools
2. **GUI Integration**: Embed WASM-rendered content in ChrysaLisp windows (future enhancement)

## Installation

### Prerequisites

You need to install the wasm3 library before building ChrysaLisp with WASM support.

#### Linux (Ubuntu/Debian)

```bash
# Clone and build wasm3
git clone https://github.com/wasm3/wasm3.git
cd wasm3
mkdir build && cd build
cmake ..
make
sudo make install
sudo ldconfig
```

#### macOS

```bash
brew install wasm3
```

#### From Source

```bash
git clone https://github.com/wasm3/wasm3.git
cd wasm3
mkdir build && cd build
cmake -DBUILD_SHARED_LIBS=ON ..
make
sudo make install
```

### Building ChrysaLisp with WASM Support

WASM support is enabled by default in the GUI build. The Makefile defines `HOST_WASM=1` which includes the wasm3 integration.

```bash
# Build with WASM support (default for GUI)
make gui

# Disable WASM support if needed
make gui HOST_WASM=0
```

## Creating WASM Modules

### Simple C Example

Here's a simple math module (`examples/wasm/math.c`):

```c
long add(long a, long b) {
    return a + b;
}

long multiply(long a, long b) {
    return a * b;
}

long fibonacci(long n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

Compile to WASM:

```bash
clang --target=wasm32 -O3 -nostdlib -Wl,--no-entry \
      -Wl,--export=add -Wl,--export=multiply -Wl,--export=fibonacci \
      math.c -o math.wasm
```

### Rust Example

```rust
#[no_mangle]
pub extern "C" fn greet(n: i64) -> i64 {
    n * 2
}
```

Compile:

```bash
rustc --target wasm32-unknown-unknown -O --crate-type=cdylib hello.rs -o hello.wasm
```

## Using WASM from ChrysaLisp

### VP Layer (Low-Level)

The VP layer provides direct access to host WASM functions:

```lisp
; Load a WASM module
(defq id_ptr (nums-new 1))
(defq inst (host-wasm-load "path/to/module.wasm" id_ptr))
(defq instance_id (nums-get id_ptr 0))

; Get an exported function
(defq func (host-wasm-get_export instance_id "add"))

; Call the function
(defq args (nums-new 2))
(nums-set args 0 5)
(nums-set args 1 10)
(defq result (host-wasm-call_i64 func args 2))
; result => 15

; Unload the module
(host-wasm-unload instance_id)
```

### Lisp Layer (High-Level)

The Lisp layer provides a more convenient API:

```lisp
(import "class/wasm/lisp.inc")

; Create a WASM module
(defq wasm (Wasm "examples/wasm/math.wasm"))

; Call exported functions
(print (. wasm :call "add" 5 10))           ; => 15
(print (. wasm :call "multiply" 6 7))       ; => 42
(print (. wasm :call "fibonacci" 10))       ; => 55

; Clean up
(. wasm :close)
```

### Memory Access

WASM modules have linear memory that can be accessed:

```lisp
; Write data to WASM memory
(defq data "Hello, WASM!")
(. wasm :write_memory 0 data)

; Read data from WASM memory
(defq result (. wasm :get_memory 0 13))
(print result)  ; => "Hello, WASM!"
```

## API Reference

### VP Layer Functions

#### `host-wasm-load (filepath id_ptr) -> instance_ptr`
Load a WASM module from file.
- **filepath**: C string path to .wasm file
- **id_ptr**: Pointer to store instance ID
- **Returns**: Instance pointer or 0 on error

#### `host-wasm-unload (instance_id)`
Unload a WASM instance and free resources.
- **instance_id**: Instance ID to unload

#### `host-wasm-get_export (instance_id export_name) -> func_ptr`
Get a pointer to an exported function.
- **instance_id**: WASM instance ID
- **export_name**: C string name of export
- **Returns**: Function pointer or 0 if not found

#### `host-wasm-call_i64 (func_ptr args arg_count) -> result`
Call a WASM function returning i64.
- **func_ptr**: Function pointer from get_export
- **args**: Array of i64 arguments
- **arg_count**: Number of arguments
- **Returns**: i64 return value

#### `host-wasm-get_memory (instance_id size_ptr) -> mem_ptr`
Get pointer to WASM linear memory.
- **instance_id**: WASM instance ID
- **size_ptr**: Pointer to store memory size
- **Returns**: Memory pointer or 0 on error

#### `host-wasm-write_memory (instance_id offset data length)`
Write data to WASM memory.
- **instance_id**: WASM instance ID
- **offset**: Byte offset in memory
- **data**: Data to write
- **length**: Number of bytes

#### `host-wasm-read_memory (instance_id offset buffer length)`
Read data from WASM memory.
- **instance_id**: WASM instance ID
- **offset**: Byte offset in memory
- **buffer**: Buffer to read into
- **length**: Number of bytes

### Lisp Layer Methods

#### `(Wasm filepath) -> instance`
Constructor: Load a WASM module.
- **filepath**: Path to .wasm file
- **Returns**: Wasm object or nil on error

#### `(. wasm :call export_name ...args) -> result`
Call an exported function.
- **export_name**: Name of export (string)
- **args**: Variadic arguments (converted to i64)
- **Returns**: Function return value

#### `(. wasm :get_memory &optional offset size) -> data`
Read from WASM linear memory.
- **offset**: Starting byte (default 0)
- **size**: Bytes to read (default all)
- **Returns**: String with memory contents

#### `(. wasm :write_memory offset data)`
Write to WASM linear memory.
- **offset**: Starting byte
- **data**: String to write

#### `(. wasm :close)`
Unload module and free resources.

## Architecture

### Layer Overview

```
┌─────────────────────────────────────┐
│ Lisp Layer (class/wasm/lisp.inc)  │
│  - High-level Wasm class           │
│  - Automatic memory management     │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│ VP Layer (sys/wasm/class.vp)       │
│  - FFI bindings to host functions  │
│  - ABI translation                 │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│ Host Layer (src/host/wasm_wasm3.cpp)│
│  - wasm3 runtime integration       │
│  - Instance management             │
│  - Memory access                   │
└─────────────────────────────────────┘
```

### Host vtable

The WASM host functions are exposed through a fourth vtable (`host_wasm_funcs`) alongside the existing OS, GUI, and Audio vtables. This vtable is passed from the C++ host to the VP environment during boot.

## Performance Considerations

1. **Function Call Overhead**: Each WASM call goes through FFI and the wasm3 interpreter. For hot paths, consider batching operations.

2. **Memory Copying**: Reading/writing WASM memory involves copying data. For large buffers, minimize round trips.

3. **wasm3 Interpreter**: wasm3 is an interpreter, not a JIT. For maximum performance, consider switching to wasmtime (requires more integration work).

## Security

WASM provides good sandboxing by default:

- **No direct file system access**: WASM can't read/write files unless you provide import functions
- **No network access**: WASM has no network stack
- **Bounded memory**: WASM linear memory is isolated from host memory

**Important**: Only load WASM modules from trusted sources. While WASM itself is sandboxed, it can still:
- Consume CPU resources (infinite loops)
- Consume memory (until WASM memory limit)
- Return malicious data to your Lisp code

## Limitations

Current limitations:

1. **No WASI support**: Standard I/O, file system, and environment variables are not yet available
2. **Integer-only**: Only i64 function calls are fully supported (f64 exists but untested)
3. **No imports**: WASM modules can't call back into ChrysaLisp (yet)
4. **Single-threaded**: WASM threads are not supported

## Future Enhancements

Planned features:

1. **GUI Integration**: WasmView class for embedding WASM-rendered content in windows
2. **WASI Support**: Standard system interface for portable WASM applications
3. **Import Functions**: Allow WASM to call ChrysaLisp functions
4. **Async Execution**: Run WASM on background tasks
5. **JIT Compilation**: Integrate wasmtime for better performance

## Examples

See `examples/wasm/` for more examples:

- `math.c` - Simple arithmetic functions
- `mandelbrot.c` - Fractal renderer (for future GUI integration)
- `game_of_life.c` - Conway's Game of Life

## Troubleshooting

### "wasm3 library not found"

Make sure wasm3 is installed and in your library path:

```bash
sudo ldconfig
# Or add library path manually
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

### "Export not found"

Make sure you're exporting functions when compiling:

```bash
# Clang
clang --target=wasm32 -Wl,--export=function_name ...

# Rust
#[no_mangle]
pub extern "C" fn function_name() { ... }
```

### "WASM memory too small"

Increase WASM memory size when creating the module (requires modifying wasm_wasm3.cpp) or compile your WASM with larger initial memory.

## Contributing

The WASM integration is experimental. Contributions welcome:

- Test on different platforms
- Add WASI support
- Implement GUI integration
- Optimize performance
- Add more examples

## References

- [wasm3 GitHub](https://github.com/wasm3/wasm3)
- [WebAssembly Specification](https://webassembly.github.io/spec/)
- [WASI](https://wasi.dev/)
- [ChrysaLisp WASM Design Doc](WASM_MOUNTING_DESIGN.md)

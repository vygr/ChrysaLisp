# WebAssembly (WASM) Target for ChrysaLisp

## Overview

This document describes the WebAssembly compilation target for ChrysaLisp, including its current implementation status, architecture, limitations, and roadmap for full functionality.

## Current Status

**Goal 1: WASM Binary Target for Calculator App - PARTIALLY IMPLEMENTED**

A WASM backend has been created at `lib/trans/wasm32.inc` that implements the VP instruction emitters needed to translate ChrysaLisp's Virtual Processor (VP) instructions to WebAssembly bytecode.

### What's Implemented

The WASM backend (`lib/trans/wasm32.inc`) provides:

1. **Register Mapping**: VP's 16 registers (r0-r14, rsp) are mapped to WASM local variables
2. **Arithmetic Operations**: Add, subtract, multiply, divide (signed/unsigned)
3. **Bitwise Operations**: AND, OR, XOR, shifts (left, right, arithmetic right)
4. **Memory Operations**: Load/store with various sizes (byte, short, int, long) and signedness
5. **Comparison Operations**: Set-on-condition instructions
6. **Control Flow**: Basic branch instructions (conditional branches)
7. **Stack Operations**: Push/pop, allocation/deallocation
8. **LEB128 Encoding**: Proper encoding of WASM integers

### Architecture

#### Register Simulation

WASM is a stack-based VM, but ChrysaLisp's VP is register-based. The solution:
- Use WASM local variables (0-15) to simulate VP registers
- `local.get` loads a register value onto the stack
- `local.set` stores the stack top into a register
- `local.tee` stores while keeping the value on stack

#### Memory Model

- Linear memory is used for heap and data
- Base register + offset addressing maps directly to WASM load/store instructions
- Alignment is specified for optimal performance

#### Example Translation

VP instruction:
```
(vp-add-rr :r1 :r0)  ; r0 = r0 + r1
```

Translates to WASM bytecode:
```wasm
local.get 0    ; Load r0
local.get 1    ; Load r1
i64.add        ; Add them
local.set 0    ; Store result in r0
```

## Major Limitations

### 1. Control Flow Mismatch

**Problem**: ChrysaLisp VP uses unstructured control flow (labels and jumps), while WASM requires structured control flow (blocks, loops, if/else).

**Current State**: Branch instructions are partially implemented but don't properly handle WASM's block structure.

**What's Needed**:
- Control flow graph analysis to convert unstructured jumps to structured blocks
- Label-to-block-depth mapping
- Loop detection and conversion
- Proper handling of break/continue semantics

### 2. Function Calls and Linking

**Problem**: ChrysaLisp's "linkerless" build process and function format don't directly map to WASM's module/import system.

**Current State**: Basic call instructions exist but don't handle:
- The ChrysaLisp function header format
- Dynamic linking at runtime
- Indirect calls through vtables
- ABI conventions

**What's Needed**:
- WASM module generator that creates proper function signatures
- Import/export table generation
- Table for indirect calls (for vtables)
- Type declarations for all functions

### 3. Missing Runtime Components

The calculator app (and most ChrysaLisp apps) depend on:

1. **GUI System**:
   - SDL-based rendering (native)
   - View hierarchy and event system
   - Would need complete browser-based rewrite (Canvas API, DOM events)

2. **Task System**:
   - Mailboxes and message passing
   - Multi-node distributed computing
   - Would need Web Workers or SharedArrayBuffer

3. **Memory Management**:
   - Reference counting
   - Heap allocation (sys_mem)
   - Boot image format
   - Would need WASM linear memory management

4. **Platform Interface (PII)**:
   - Host OS integration
   - File system access
   - Networking
   - Would need browser APIs (FileSystem API, Fetch API, etc.)

### 4. Build System Integration

**Current State**: The WASM backend exists but isn't integrated into the build system.

**What's Needed**:
- Modify `Makefile` and `cmd/make.lisp` to support WASM as a target CPU
- Add `wasm32` to recognized CPU types
- Add `WASM32` as a recognized ABI
- Create WASM-specific toolchain integration (e.g., `wasm-ld` for final linking)
- Generate WASM module format with proper sections (type, function, memory, export, code)

### 5. Boot Image

**Problem**: ChrysaLisp uses a boot image that contains all compiled functions and is loaded by the host.

**What's Needed**:
- Convert boot image format to WASM module format
- Data section for static data
- Table section for function pointers
- Custom section for ChrysaLisp metadata

## Roadmap

### Phase 1: Core Infrastructure (CURRENT)
- [x] Create WASM instruction emitters
- [x] Implement register mapping
- [x] Implement basic arithmetic and memory operations
- [ ] Fix control flow to use WASM structured blocks
- [ ] Integrate into build system

### Phase 2: Simple Functions
- [ ] Compile simple non-recursive functions
- [ ] Test arithmetic and logic operations
- [ ] Implement proper function prologues/epilogues
- [ ] Generate WASM module format

### Phase 3: Complete VP Support
- [ ] Implement all VP instructions correctly
- [ ] Handle complex control flow (loops, switch)
- [ ] Support indirect calls
- [ ] Implement division and modulo
- [ ] Handle floating-point operations

### Phase 4: Runtime Porting
- [ ] Port memory allocator to WASM linear memory
- [ ] Implement reference counting in WASM
- [ ] Port string and sequence operations
- [ ] Port basic Lisp primitives

### Phase 5: Calculator App (Goal 1)
- [ ] Create minimal browser-based GUI layer
- [ ] Port calculator logic to WASM
- [ ] Implement event handling
- [ ] Create HTML/CSS UI
- [ ] Test in browser

### Phase 6: Full OS Image (Goal 2)
- [ ] Port entire boot image to WASM
- [ ] Implement task system with Web Workers
- [ ] Implement IPC with SharedArrayBuffer
- [ ] Port all system services
- [ ] Create browser-based "desktop" environment

## Technical Challenges

### Control Flow Translation

WASM's structured control flow requires converting:

```vp
label_1:
  ; some code
  (vp-beq-rr :r0 :r1 label_2)
  ; more code
  (vp-jmp label_1)
label_2:
  ; other code
```

To:

```wasm
(block $label_2
  (loop $label_1
    ;; some code
    local.get 0
    local.get 1
    i64.eq
    br_if $label_2
    ;; more code
    br $label_1
  )
)
;; other code
```

This requires:
1. Building a control flow graph
2. Identifying natural loops
3. Determining block nesting structure
4. Calculating branch depths

### Function Call Convention

ChrysaLisp functions have headers with metadata. In WASM:

```wasm
(module
  (type $func_type (func (param i64 i64) (result i64)))
  (table funcref (elem $func1 $func2 ...))
  (func $func1 (type $func_type)
    (local i64 i64 i64)  ;; VP registers
    ;; function body
  )
)
```

Challenges:
- Dynamic typing vs WASM's static typing
- Variable number of parameters
- Closure environment passing

### Memory Layout

ChrysaLisp's memory model vs WASM:

| ChrysaLisp | WASM |
|------------|------|
| Heap (sys_mem) | Linear memory |
| Reference counting | Manual management |
| Boot image | Data section |
| Dynamic loading | Import at module creation |

## Testing Strategy

### Unit Tests

Test each VP instruction:
1. Create simple VP function
2. Compile to WASM
3. Load in Node.js or browser
4. Verify results

Example test:
```lisp
(defun test-add ()
  (vp-cpy-cr 5 :r0)
  (vp-cpy-cr 3 :r1)
  (vp-add-rr :r1 :r0)
  (vp-ret))
```

Expected WASM behavior: Returns 8

### Integration Tests

1. Compile simple Lisp functions
2. Test with ChrysaLisp runtime in WASM
3. Benchmark performance

## Performance Considerations

WASM advantages:
- Near-native performance
- JIT compilation in browsers
- SIMD support (when available)

WASM disadvantages:
- No goto/indirect jumps (affects dispatch performance)
- No tail calls (in some environments)
- Limited threading (Web Workers have overhead)

Expected performance: 60-80% of native x86-64 for computational tasks

## Alternative Approaches

### 1. Emscripten Approach
Compile the C++ host to WASM and run VP64 emulator in browser.

**Pros**: Faster to implement, reuses existing code
**Cons**: Larger binary, slower execution, full emulation overhead

### 2. Hybrid Approach
Keep computational core in WASM, GUI in JavaScript.

**Pros**: Better browser integration, smaller WASM binary
**Cons**: More complex FFI, marshaling overhead

### 3. Transpile to JavaScript
Convert VP to JavaScript instead of WASM.

**Pros**: Simpler, more flexible
**Cons**: Slower, no type safety

## Recommended Next Steps

1. **Fix Control Flow** (1-2 days)
   - Implement block structure analysis
   - Convert jumps to breaks
   - Test with simple loops

2. **Build System Integration** (1 day)
   - Add WASM target to Makefile
   - Create WASM module generator
   - Test compilation pipeline

3. **Simple Function Test** (1 day)
   - Compile a simple arithmetic function
   - Load in Node.js
   - Verify correctness

4. **Runtime Porting** (1-2 weeks)
   - Port memory allocator
   - Port basic Lisp primitives
   - Create minimal test environment

5. **Calculator App** (2-3 weeks)
   - Design browser GUI
   - Port calculator logic
   - Implement event handling
   - Polish and test

**Total Estimated Time for Goal 1 (Calculator): 4-6 weeks**
**Total Estimated Time for Goal 2 (Full OS): 3-6 months**

## Resources

- [WebAssembly Specification](https://webassembly.github.io/spec/)
- [WASM Binary Format](https://webassembly.github.io/spec/core/binary/index.html)
- [Binaryen](https://github.com/WebAssembly/binaryen) - WASM optimizer
- [WABT](https://github.com/WebAssembly/wabt) - WebAssembly Binary Toolkit

## Conclusion

The WASM backend foundation has been laid in `lib/trans/wasm32.inc`. While significant work remains to make it fully functional, the architecture is sound. The main challenges are:

1. Control flow translation
2. Runtime system porting
3. GUI reimplementation for browsers

For the calculator app specifically, a browser-based version is achievable in 4-6 weeks with focused effort. The full OS image would be a much larger undertaking (3-6 months) but is technically feasible.

The most practical approach for immediate results would be:
1. Focus on control flow fixes
2. Get simple functions compiling and running
3. Create a minimal browser-based calculator UI
4. Port only the calculator logic to WASM
5. Use JavaScript for the GUI layer

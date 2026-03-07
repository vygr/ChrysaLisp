# The VP64 C/C++ Emulator Implementation

The `vp64.cpp` file provides a high-performance C/C++ implementation of the
ChrysaLisp VP64 virtual machine. This emulator serves as a critical bridge,
allowing ChrysaLisp to run on platforms where a native backend might not yet
exist, or as a reference implementation for debugging and performance
comparison.

## Architecture Overview

The emulator implements a 64-bit virtual processor with a focus on simplicity,
speed, and direct mapping to the ChrysaLisp virtual instruction set.

### Registers and Memory

The VM state within the emulator consists of:

* **16 General-Purpose Registers (`regs[16]`)**: 64-bit integer registers used
  for arithmetic, logic, and addressing.

    * `regs[0-14]`: General usage.

    * `regs[15]`: The Stack Pointer (`rsp`).

* **16 Floating-Point Registers (`fregs[16]`)**: 64-bit IEEE double-precision
  registers.

* **Program Counter (`pc`)**: A pointer to the current instruction in memory.

* **Comparison Flags**: Separate internal state for integer (`compare1`,
  `compare2`) and floating-point (`compare_f1`, `compare_f2`) comparisons.

### The Stack

The stack is managed by `regs[15]`. Upon initialization, it points to the top of
a pre-allocated memory block (typically 8KB). The stack grows downwards in
memory.

## Instruction Fetch and Decode

The emulator uses a tight fetch-decode-execute loop. Each instruction (`ir`) is
a 16-bit value.

1. **Fetch**: The opcode is fetched from the current `pc`.

2. **Decode**: The lower 8 bits of the instruction (`ir & 0xff`) represent the
   primary opcode.

3. **Execute**: A large `switch` statement dispatches to the appropriate
   instruction handler.

### Operand Decoding

Macros are used extensively to decode operands from the instruction stream:

* **`vd_dr()` / `vd_sr()`**: Extract Destination/Source register indices from
  the instruction bits.

* **`vd_c0()` / `vd_c1()` / `vd_c2()` / `vd_c3()`**: Decode constants of varying
  sizes (4-bit, 8-bit, 32-bit, or 64-bit) embedded in or following the
  instruction.

* **`vd_i()` / `vd_sc()`**: Fetch immediate 16-bit or 32-bit values from the
  instruction stream, incrementing the `pc`.

## Instruction Categories

### ALU Operations (Integer and Float)

Arithmetic and logical operations are implemented using standard C++ operators.
Integer operations support both Constant-Register (CR) and Register-Register
(RR) variants. Floating-point operations (FF) use the `fregs` registers and
standard math functions.

### Memory Access

The emulator handles various addressing modes:

* **Indexed Immediate (`cpy_ir`, `cpy_ri`)**: Base register + immediate offset.

* **Indexed Displacement (`cpy_rd`, `cpy_dr`)**: Base register + index register.

* **PC-Relative (`cpy_pr`, `lea_p`)**: Accessing data relative to the current
  code position.

The emulator supports byte, short, int, and long (64-bit) access, including
unsigned variants.

### Branching and Control Flow

Branches (`beq`, `bne`, etc.) perform a PC-relative jump if the internal
comparison state matches the condition.

* **Calls and Returns**: `vp_call` pushes the current `pc` onto the stack and
  updates `pc` to the target. `vp_ret` pops the `pc` from the stack.

* **Indirect Jumps/Calls**: Support for jumping/calling through registers or
  memory pointers.

## System Integration and ABI

A key feature of the emulator is its ability to interact with the host
environment through the `vp_call_abi` instruction.

* **Host Function Pointers**: Upon startup, the emulator is provided with arrays
  of function pointers for OS, GUI, and Audio services.

* **ABI Bridge**: The `vp_call_abi` handler bridges the gap between VM registers
  and host C++ function calls, supporting up to 15 arguments passed in
  `regs[0-14]`. This allows ChrysaLisp code to call native C++ functions
  seamlessly.

## Performance Considerations

* **Native Types**: The use of native `int64_t` and `double` ensures that
  arithmetic operations are as fast as possible on the host CPU.

* **Computed Offsets**: Constant offsets in branches and memory access are
  decoded once and applied directly.

* **Atomic Sync**: `vp_sync` uses `std::atomic` for memory ordering, ensuring
  correct behavior in multi-threaded scenarios.

* **Tail Recursion**: The Lisp-side `defun` and `lambda` implementations often
  rely on the underlying efficiency of the VM's call/return mechanism.

## Running the Emulator

The emulator can be activated using the `-e` command-line flag when starting the
main executable. This forces the host to use the `vp64` VM implementation
instead of attempting to execute native machine code.

```bash
./run.sh -e
```

This mode is invaluable for porting ChrysaLisp to new architectures and for
rigorous testing of the virtual machine's logic.

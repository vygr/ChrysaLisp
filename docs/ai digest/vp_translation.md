## The ChrysaLisp Virtual Processor (VP) and Translation System

### Introduction

The ChrysaLisp Virtual Processor (VP) is an imaginary 64-bit RISC-like
processor. It serves as the primary compilation target for all ChrysaLisp code,
whether written directly in VP assembly or generated from higher-level
constructs like C-Script (via the `(assign)` macro).

The main purposes of the VP are:

1. **Portability:** By targeting a single, well-defined virtual architecture,
ChrysaLisp code can be written once and then translated to various native
hardware architectures.

2. **Simplification:** It provides a simpler and more orthogonal instruction
set than many complex native CPUs, making the assembler and compiler backends
easier to develop and maintain.

3. **Abstraction:** It allows for high-level concepts (like object method calls
or string operations) to be eventually broken down into a consistent set of
low-level operations.

The **Translation System** is responsible for converting this VP bytecode into
executable machine code for a specific target CPU and ABI (Application Binary
Interface).

### VP Architecture

* **Registers:** The VP has 16 general-purpose 64-bit registers:

    * `:r0` through `:r14`: General-purpose registers.

    * `:rsp`: Stack Pointer.

    These are symbolic names used in VP assembly; the translation system maps
    them to native registers.

* **Instruction Set:** The VP instruction set is designed to be relatively
simple and orthogonal. Key categories include (refer to `vp_vm.md` for a full
list):

    * **Constant to Register:** e.g., `(vp-cpy-cr constant rd)`

    * **Register to Register:** e.g., `(vp-add-rr rs rd)`, `(vp-xor-rr rs rd)`

    * **Memory to Register (Load):** e.g., `(vp-cpy-ir rb offset rd)` (load
    from `[rb + offset]`), `(vp-cpy-dr rb ri rd)` (load from `[rb + ri]`).
    Suffixes like `-b`, `-s`, `-i`, `-ub`, `-us`, `-ui` specify byte, short
    (16-bit), int (32-bit) and their unsigned variants, with sign/zero
    extension.

    * **Register to Memory (Store):** e.g., `(vp-cpy-ri rs rb offset)`,
    `(vp-cpy-rd rs rb ri)`. Suffixes specify data size.

    * **Branch:** e.g., `(vp-beq-rr rs rd label)` (branch if equal),
    `(vp-blt-cr const rd label)` (branch if less than).

    * **Effective Address (LEA):** e.g., `(vp-lea-i rb offset rd)` (rd = rb +
    offset), `(vp-lea-p label rd)` (rd = address of label).

    * **Call/Jump/Return:** e.g., `(vp-call label)`, `(vp-jmp-r rd)`,
    `(vp-ret)`.

    * **Stack Operations:** e.g., `(vp-push rs ...)`, `(vp-pop rd ...)`,
    `(vp-alloc size)`, `(vp-free size)`.

### The Compilation and Translation Pipeline

The process of going from ChrysaLisp source to native executable code involves
several stages:

1. **Lisp to VP Assembly:** High-level Lisp code, C-Script expressions (within
`(assign)`), and direct VP assembly macros (`(vp-add-rr ...)`) are processed by
the ChrysaLisp assembler. This stage generates a list of VP instructions. These
are Lisp lists, e.g., `(vp-add-rr :r0 :r1)`.

2. **VP Instruction Collection:** During a function's definition (`(def-func
...)` in `lib/asm/func.inc`), these VP instructions are collected into an
internal list (often `*emit_list*`, though this name is more prominent at the
next stage).

3. **CScript Optimization (if applicable):** If C-Script was used, its
generated VP instructions (still in the `(vp-xxx ...)` form) are optimized by
`csopt.inc` (e.g., constant folding, offset folding).

4. **VP Emitter List Preparation:** The `(vp-xxx ...)` instructions are
transformed into `(emit-xxx ...)` forms. For example, `(vp-add-rr :r0 :r1)`
might become `(emit-add-rr mapped_r0 mapped_r1)` after register
allocation/mapping, or stay symbolic for a bit longer.

5. **VP Emitter Optimization:** The `opt-emit-list` function (from
`lib/asm/vpopt.inc`) performs peephole optimizations on this list of `(emit-xxx
...)` instructions (e.g., redundant load/store elimination).

6. **Translation (`emit-translate` from `lib/trans/vp.inc`):**

    * This is the core of the VP-to-native translation.

    * It takes the (optimized) list of `(emit-xxx ...)` instructions.

    * It performs a **multi-pass translation**:

        * **Pass 1 (Preparation):**

            * Maps symbolic VP register names (e.g., `:r0`) to their native
            numerical representations for the target CPU using
            `emit-native-reg?` (defined in the CPU-specific file like
            `lib/trans/x86_64.inc`).

            * Identifies labels and initializes their addresses (often to 0 or
            a placeholder).

            * Identifies branch/jump instructions and records information
            needed for offset calculation (e.g., in `*offsets*`).

            * Replaces the `emit-xxx` symbol at the head of each instruction
            list with the actual Lisp emitter function for the target CPU
            (e.g., the `emit-add-rr` Lisp function defined in
            `lib/trans/x86_64.inc`).

        * **Subsequent Passes (Emission & Resolution):**

            * The prepared list of instructions (now `(emitter_func arg1 arg2
            ...)`) is `eval`'d.

            * Each CPU-specific emitter function is called. It writes the
            corresponding native machine code bytes to an output stream
            (`*stream*`) and updates a program counter (`*pc*`).

            * `emit-label` updates the actual address of labels as code is
            emitted.

            * Branch/jump emitters calculate target offsets based on the
            current `*pc*` and the (now potentially resolved) label addresses.

            * The process repeats. If the generated byte stream from the
            current pass is identical to the previous pass, all forward
            references have stabilized, and translation is complete.

7. **Output:** The final output is a string of bytes representing the native
machine code for the compiled function, which is then saved to an object file
(e.g., `obj/x86_64/AMD64/my_function`).

### VP Instruction Emitters (CPU-Specific)

For each target CPU architecture (x86-64, ARM64, RISC-V 64, VP64), there's a
corresponding `.inc` file in `lib/trans/` (e.g., `lib/trans/x86_64.inc`). These
files define:

1. **`emit-native-reg?` function:** Maps VP register symbols to native register
numbers/encodings.

2. **A set of `emit-xxx` Lisp functions:** One for each VP instruction (e.g.,
`emit-add-rr`, `emit-cpy-cr`). These Lisp functions are responsible for:

    * Taking native register numbers and immediate values as arguments.

    * Generating the correct sequence of machine code bytes for that
    instruction on the target CPU.

    * Using helper macros (e.g., `x64-rr` for x86-64, `arm64-rrr` for ARM64) to
    construct instruction encodings.

    * Writing these bytes to the `*stream*` and updating `*pc*`.

### Architecture-Specific Examples

Let's look at how a couple of VP instructions might be translated.

**VP Instruction:** `(vp-add-rr :r1 :r0)` *Semantics: Add contents of register
`:r1` to register `:r0`, store result in `:r0` (i.e., `:r0 = :r0 + :r1`)*

* **Lisp form passed to `emit-translate` (after optimization):** `(emit-add-rr
(emit-native-reg? :r1) (emit-native-reg? :r0))` (Assuming `:r1` is source,
`:r0` is destination/accumulator based on typical VP style)

* **x86-64 (`lib/trans/x86_64.inc`)**

    * `emit-native-reg? :r1` -> `1` (RCX)

    * `emit-native-reg? :r0` -> `0` (RAX)

    * `emit-add-rr src_reg_num dest_reg_num` calls `(x64-rr 0x01 src_reg_num
    dest_reg_num)`.

    * `(x64-rr 0x01 1 0)` generates:

        * REX prefix: `0x48` (for 64-bit operands, W=1. R=0 for RCX, B=0 for
        RAX).

        * Opcode: `0x01` (ADD r/m64, r64).

        * ModR/M: `0xC8` (Mod=11 for reg-reg, Reg=RCX(001), R/M=RAX(000)).

        * **Resulting bytes:** `48 01 C8` (ADD RAX, RCX)

* **ARM64 (AArch64) (`lib/trans/arm64.inc`)**

    * `emit-native-reg? :r1` -> `1` (X1)

    * `emit-native-reg? :r0` -> `0` (X0)

    * `emit-add-rr src_reg_num dest_reg_num` calls `(arm64-rrr 0x8b000000
    src_reg_num dest_reg_num dest_reg_num)` (assuming `src_reg_num` is not SP).

    * `(arm64-rrr 0x8b000000 1 0 0)` (for `ADD X0, X0, X1`):

        * Base opcode `0x8b000000` (ADD Xd, Xn, Xm, 64-bit).

        * Rd (dest): X0 (0) -> `0` in bits 0-4.

        * Rn (first src): X0 (0) -> `0` in bits 5-9.

        * Rm (second src): X1 (1) -> `1` in bits 16-20.

        * **Resulting bytes (example):** `00 00 02 8B` (Hex for `ADD X0, X0,
        X1`)

* **RISC-V 64 (`lib/trans/riscv64.inc`)**

    * `emit-native-reg? :r1` -> `11` (a1)

    * `emit-native-reg? :r0` -> `10` (a0)

    * `emit-add-rr src_reg_num dest_reg_num` calls `(riscv64-r 0x33
    dest_reg_num dest_reg_num src_reg_num 0x0 0x0)`.

    * `(riscv64-r 0x33 10 10 11 0x0 0x0)` (for `ADD a0, a0, a1`):

        * Opcode `0x33` (OP).

        * rd: a0 (10).

        * funct3: `0x0` (ADD/SUB).

        * rs1: a0 (10).

        * rs2: a1 (11).

        * funct7: `0x00` (ADD).

        * **Resulting bytes (example):** `93 05 B5 00` (Hex for `ADD a0, a0,
        a1`)

* **VP64 (`lib/trans/vp64.inc`)**

    * `emit-native-reg? :r1` -> `1` (index for :r1)

    * `emit-native-reg? :r0` -> `0` (index for :r0)

    * `emit-add-rr src_idx dest_idx` calls `(emitm-short (+ (next-opcode) (<<
    dest_idx 8) (<< src_idx 12)))`.

    * If `VP64_ADD_RR` opcode is, say, `36`:

        * `36 | (0 << 8) | (1 << 12)` -> `36 | 0 | 0x1000` -> `0x1024`.

        * **Resulting short:** `0x1024`

**VP Instruction:** `(vp-cpy-cr 100 :r2)` *Semantics: Copy constant 100 into
register `:r2`.*

* **Lisp form:** `(emit-cpy-cr 100 (emit-native-reg? :r2))`

* **x86-64:**

    * `emit-native-reg? :r2` -> `2` (RDX)

    * `emit-cpy-cr 100 2`:

        * Since 100 fits in a 32-bit immediate sign-extended to 64 bits:

        * REX prefix: `0x48` (W=1).

        * Opcode: `0xC7`.

        * ModR/M: `0xC2` (Mod=11 for reg, R/M=RDX(010)).

        * Immediate: `0x00000064` (32-bit).

        * **Resulting bytes:** `48 C7 C2 64 00 00 00` (MOV RDX, 100)

* **ARM64:**

    * `emit-native-reg? :r2` -> `2` (X2)

    * `emit-cpy-cr 100 2`:

        * `arm64-mov-cr 100 2` (since 100 is small):

        * `MOVZ X2, #100, LSL #0` (Opcode `0xD2800000` + immediate + Rd).

        * 100 is `0xC80` shifted by 0. `(0xC80 << 5) | 2`.

        * **Resulting bytes (example):** `42 0C 80 D2` (Hex for `MOV X2, #100`)

* **RISC-V 64:**

    * `emit-native-reg? :r2` -> `12` (a2)

    * `emit-cpy-cr 100 12`:

        * `riscv64-cr 100 12`:

        * `ADDI a2, zero, 100` (Opcode `0x13`, rd=a2, rs1=zero, imm=100).

        * **Resulting bytes (example):** `64 A6 00 01` (Hex for `ADDI a2, x0,
        100`)

* **VP64:**

    * `emit-native-reg? :r2` -> `2`

    * `emit-cpy-cr 100 2`:

        * `vp64-cr (VP64_CPY_CR_0_opcode) 100 2`.

        * 100 fits in 4 bits (0-15) if sign-extended? No, 100 is `0b01100100`.

        * It will use `VP64_CPY_CR_1` (constant fits in 4+16=20 bits).

        * Opcode for `VP64_CPY_CR_1`.

        * First short: `opcode | (2 << 8) | ((100 & 0xF) << 12)`.

        * Second short: `100 >> 4`.

        * **Resulting shorts (example):** `[opcode_cpy_cr_1 | 0x0200 | 0x4000,
        0x0006]`

### Multi-Pass Translation for Forward References

The `emit-translate` function in `lib/trans/vp.inc` uses a multi-pass approach
primarily to resolve forward branches and jumps.

1.  **Problem:** When a branch instruction like `(vp-beq-rr :r0 :r1 my_label)` is encountered, `my_label` might be defined later in the code. The translator doesn't know the address of `my_label` on the first pass.

2. **Solution:**

    * **Pass 1:**

        * Labels are assigned placeholder addresses (e.g., 0).

        * Branch instructions are emitted with an estimated offset (e.g., a
        short branch if possible, or a placeholder for a long branch).
        Information about the branch (its location, target label, current
        offset size) is stored (e.g., in `*offsets*`).

    * **Subsequent Passes:**

        * The code is re-emitted.

        * `emit-label` now sets the *correct* address for labels based on the
        code emitted so far in *this* pass.

        * Branch emitters recalculate the offset to the target label using its
        updated address.

        * If a branch offset was initially too small (e.g., a short branch was
        emitted, but the target is now too far), the emitter might need to emit
        a longer form of the branch. This change in instruction size can shift
        subsequent label addresses.

        * The `*offsets*` list in the x86-64 and RISC-V emitters helps track
        the largest offset needed for a branch across passes, ensuring the
        final pass uses an adequate encoding.

    * **Convergence:** The process repeats until a pass produces a byte stream
    identical to the previous pass. This indicates all label addresses and
    branch offsets have stabilized.

### Conclusion

The ChrysaLisp VP and its translation system provide a flexible and portable
way to compile code. The VP offers a consistent target, while the CPU-specific
emitter Lisp files handle the complexities of native instruction encoding and
ABI conventions. The multi-pass translation approach robustly handles forward
references, a common challenge in assemblers and compilers. This architecture
allows ChrysaLisp to be potentially retargeted to new CPUs by primarily writing
a new `lib/trans/<cpu>.inc` file.
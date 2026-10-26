# The ChrysaLisp Virtual Processor (VP) and Translation System

## Introduction

The ChrysaLisp Virtual Processor (VP) is an imaginary 64-bit RISC-like
processor. It serves as the primary compilation target for all ChrysaLisp
code, whether written directly in VP assembly or generated from higher-level
constructs like vpx (via the `(assign)` macro).

The main purposes of the VP are:

1. **Portability:** By targeting a single, well-defined virtual architecture,
   ChrysaLisp code can be written once and then translated to various native
   hardware architectures.

2. **Simplification:** It provides a simpler and more orthogonal instruction
   set than many complex native CPUs, making the assembler and compiler
   backends easier to develop and maintain.

3. **Abstraction:** It allows for high-level concepts (like object method
   calls or string operations) to be eventually broken down into a consistent
   set of low-level operations.

The **Translation System** is responsible for converting this VP bytecode
into executable machine code for a specific target CPU and ABI (Application
Binary Interface).

```image
docs/diagrams/vp_translate.cwb
```

## VP Architecture

* **Registers:** The VP has 16 general-purpose 64-bit registers, and 16 IEEE
  64-bit float registers:

    * `:r0` through `:r14`: General-purpose registers.

    * `:rsp`: Stack Pointer.

    * `:f0` through `:f15`: IEEE float registers.

    These are symbolic names used in VP assembly; the translation system maps
    them to native registers.

* **Instruction Set:** The VP instruction set is designed to be relatively
  simple and orthogonal. Key categories include (refer to [`vp_vm.md`](../vm/vp_vm.md) for a
  full list):

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

## The Compilation and Translation Pipeline

The process of going from ChrysaLisp source to native executable code
involves several stages:

1. **Lisp to VP Assembly:** High-level Lisp code, vpx expressions
   (within `(assign)`), and direct VP assembly macros (`(vp-add-rr ...)`)
   are processed by the ChrysaLisp assembler. This stage generates a list of
   VP instructions. These are Lisp lists, e.g., `(vp-add-rr :r0 :r1)`.

2. **VP Instruction Collection:** During a function's definition (`(def-func
   ...)` in `lib/asm/func.inc`), these VP instructions are collected into an
   internal list (often `*emit_list*`, though this name is more prominent at
   the next stage).

3. **vpx Optimization (if applicable):** If vpx was used, its
   generated VP instructions (still in the `(vp-xxx ...)` form) are
   optimized by `vpxopt.inc` (e.g., constant folding, offset folding).

4. **VP Emitter List Preparation:** The `(vp-xxx ...)` instructions are
   transformed into `(emit-xxx ...)` forms, still with VP register names,
   e.g. `(emit-add-rr :r1 :r0)`.

5. **VP Emitter Optimization:** The `opt-emit-list` function (from
   `lib/asm/vpopt.inc`) performs peephole optimizations on this list of
   `(emit-xxx ...)` instructions (e.g., redundant load/store elimination).

6. **VP Binary:** The list is encoded as VP binary, `vpb-encode` in
   `lib/trans/vpb.inc`, below.

7. **Translation:** The translator of the target CPU, itself a VP
   function, is given the VP binary and gives back the native code as a
   string. `vp-translate-bin` in `lib/trans/vp.inc` calls it.

8. **Output:** The native code is saved to an object file by the
   function's name (e.g., `obj/x86_64/AMD64/sys/mem/alloc`),
   `vp-translate-out`.

## The VP Binary

`lib/trans/vpb.inc` holds the format and its Lisp encoder and decoder. A
function is:

* its name, as a length and its bytes,

* how many labels it has,

* how many instructions it has,

* and each instruction: an op byte, how many operands it has, then each
  operand as a tag and what follows it.

The op byte is the instruction's place in `+vpb_all_ops`,
`lib/trans/vpb_ops.inc`: the VP's own ops, `+vp_emit_ops` of
`lib/asm/ops.inc`, then the fused ops the prepasses of the translators make,
so a translator's records can hold them too.

A number is unsigned, 7 bits a byte, low first, the top bit of a byte saying
another follows. A signed one is written as its zigzag, 0 -1 1 -2 2 ... as
0 1 2 3 4 .... The operand tags are:

* `reg`: a byte, `:r0` to `:rsp` as 0 to 15, `:f0` to `:f15` as 16 to 31.

* `num`: a signed number.

* `label`: the label's number, where it is.

* `label_pc`: the label less where this instruction is.

* `label_diff`: two labels, the first less the second.

* `string`: its length and its bytes.

* `name`: a keyword that is not a register, the class and method a
  `call-i` names, its length and its bytes.

* `pc_label`: where this instruction is less the label.

Labels are numbered from 0 in the order they are first seen. A constant the
compile left as a form, `(+ 24 1)`, is worked out and written as its number.

The functions are:

* `(vpb-encode entry)`: a bundle's entry, `(name (progn inst ...) ...)`, to
  VP binary.

* `(vpb-decode bin)`: back to the entry, the labels named again, for the
  tools and for looking at a binary.

* `(vpb-bundle bins)` and `(vpb-unbundle data)`: the binaries of a file's
  functions as one string, each after its length, and back.

* `(vpb-name bin)`: the function's name, without decoding the rest.

## The Translators

There is one translator for each target, `lib/trans/<cpu>.vp`:
`arm64.vp`, `x86_64.vp`, `riscv64.vp`, `la64.vp` and `vp64.vp`. Each is a
single VP function, built like any other into
`obj/<cpu>/<abi>/lib/trans/`:

* `vpb_arm64`

* `vpb_x86_64_amd64` and `vpb_x86_64_win64`, from the one body in
  `x86_64.vp`, made for each ABI: they differ only in where a host call has
  its args.

* `vpb_riscv64`

* `vpb_la64`

* `vpb_vp64`, to VP64 code for the emulator.

`lib/trans/vp.inc` binds it with `ffi` and calls it with the binary, as
`(vpb-<cpu> bin)`, `(vpb-x86-64-amd64 bin)` and `(vpb-x86-64-win64 bin)`
for x86_64. It gives back the native code as a string, or `:nil` if it
cannot. The decode, the prepass, the passes and the output are all in VP.

### The Shared Driver

`lib/trans/vpb_trans.inc` is included by every translator:

* `(vpb-driver)`: the entry, the decode of the binary to a record of each
  instruction and its operands, the call of the translator's `prepass`,
  then the passes, and the output as a string.

* `(handler op label)`: registers the routine at `label` as the one of
  `op`. An op with no routine goes to `h_none`, which marks the function as
  one the translator cannot do.

* `(vpb-helpers)`: routines the prepasses use to look at operands.

* `(vpb-tables name)`: the entry's error, and the table of each op's
  routine, in the order of `+vpb_all_ops`.

* `(vpb-call-fusion)` and `(vpb-call-fusion-tables cpu)`: a generic call
  fusion prepass for CPUs with a link register, used by riscv64 and la64.

A routine of an op is called with `:r0` to `:r3` its first operands' values,
`:r4` all of them, `:r5` how many, `:r6` the operands as decoded and `:r7`
its record. A register operand comes to it already in the CPU's own number,
from the translator's `reg_map`, 16 bytes for `:r0` to `:rsp` then 16 for
`:f0` to `:f15`. A routine may trash `:r0` to `:r13`, never `:r14`, the
state, which has `+vpbt_pc` where the output is and `+vpbt_pass` the pass.
It writes its bytes with `emit8`, `emit16`, `emit32` or `emit64`.

For example, the riscv64 routines of the register to register ALU ops:

```vdu
(each (lambda ((op label f3 f7 test))
		;(s d), d = d op s
		(vp-label label)
		(when test (vpif '(:r0 = :r1)) (vp-ret) (endif))
		(rv-r 0x33 :r1 :r1 :r0 f3 f7)
		(vp-ret)
		(handler op label))
	'(... (emit-add-rr e_add_rr 0 0 :nil) ...))
```

`rv-r` adds up the fixed fields of the instruction when the translator is
built, and the register fields when it runs, then calls `emit32`.
`(vp-add-rr :r1 :r0)` comes to `e_add_rr` as `:r0` = 11 and `:r1` = 10,
a1 and a0, and makes `add a0, a0, a1`.

### Prepasses

Before the passes a translator may rewrite its records, `prepass`, making
an instruction into a fused op of `vpb_ops.inc` or into `emit-nop`:

* arm64: call fusion, load and store pair fusion to `ldp` and `stp`, and
  copy fusion, a copy and the ALU op after it to one three register op.

* x86_64: a load and the ALU op of what it loaded, that register dead
  after, to the ALU op from memory, `emit-x64-alu-ir`; a copy and an add,
  or a shift by 1, of what it copied to, to a `lea`.

* riscv64 and la64: the generic call fusion of `vpb_trans.inc`.

* vp64: none.

### Multi-Pass Translation

A branch to a label later in the code does not know where it is on the
first pass. So the driver emits the whole function, pass after pass, each
operand's value worked out from the labels as the pass before left them.
`emit-label` sets where a label is as the pass reaches it.

A branch that may take a short or a long form keeps in its record, `+vpbr_m`,
the most it has had to reach on any pass, and is sized by that, so it only
ever grows, and the passes settle. They stop when no label has moved from
the pass before, after 2 passes at the least. After 1000 passes the
translator gives up and gives back `:nil`.

A routine that is given something it cannot make, an op it has no routine
for, a shift by a constant outside 0 to 63, a branch out of reach, marks the
function and the translator gives back `:nil`. `vp-translate-bin` then
throws `The <cpu> translator failed on this function !` with its name. Any
constant is handled, a translator makes one that does not fit an
instruction some other way, so it is a fault in the translator, not the VP.

## What Each Translator Does

Some things are the same on all five:

* `(vp-div-rrr rs rdh rdl)` and `vp-div-rrr-u` are as
  [`vp_vm.md`](../vm/vp_vm.md) has them: `rdh` is not read, a divide by 0
  gives a quotient of 0 and the dividend as the remainder, and the most
  negative number by -1 gives itself. Below they are `(s d1 d2)`, `d1` the
  remainder and `d2` the dividend and quotient.

* `vp-cvt-fr` truncates towards 0.

* A float compare with a NaN in it is false for every branch but
  `bne-ff`, which is taken.

* `emit-stack-init`, `(s f x)`, makes the stack of a new task at `s`, so
  that the first switch to it runs `f` with `x` as where `f` returns to.

### arm64

`:r0` to `:r7` are x0 to x7, `:r8` to `:r14` x19 to x25, `:f0` to `:f15`
d8 to d23. `:rsp` is x28, not the hardware sp, so the VP stack is free of
the 16 byte rule; sp is made from x28 only for a host call. It keeps x29
as its temp and x30 as the link register.

A constant is a logical immediate if it can be one, `orr d, xzr, #imm`.
Else it is `movz`, or `movn` for one below 0, of the highest 16 bit part,
then a `movk` for each lower part that is not all 0s, or all 1s for
`movn`: 1 to 4 instructions. `add-cr` and `sub-cr` of up to 24 bits are
an `add` or `sub` of the low 12 bits and one of the high 12 shifted by 12,
either left out if 0, a negative constant taking the other op. `and-cr`,
`or-cr` and `xor-cr` are the immediate form if the constant is a logical
immediate. `mul-cr` by 0, 1, -1 or a power of 2 is an `eor`, nothing, a
`neg` or a shift. A compare with 0 to 0xfff is `cmp`, with -0xfff to -1
`cmn`. Any other constant goes to x29 and the register form is used.

Loads and stores take an offset of -256 to 255 as `ldur` and `stur`, then
a whole number of the size, up to 4095 of them, as the scaled `ldr` and
`str`. Any other offset goes to x29 and the register offset form,
`[xn, x29]`. `-b`, `-s` and `-i` loads are `ldrsb`, `ldrsh` and `ldrsw`;
`-ui` is `ldr w`. `-ub` and `-us` are `ldrb` and `ldrh`, with an `eor` of
the destination before, or a `ubfx` after when it is the base. The
indexed forms are the register offset form. A float load or store takes
-256 to 255, then 0 to 0x7ff8 in 8s, else x29 is made the address.

`push` and `pop` are `stp` and `ldp` pairs on x28, the first `stp` with
pre-index writeback of the whole size, the last `ldp` with post-index, an
odd one a `str` or `ldr`. `emit-save-state` and `emit-load-state` push and
pop x30. `alloc` and `free` are a `sub` or `add` of x28.

A conditional branch is a `cmp`, `cmn` or `fcmp` and a `b.cond`, reach
±1MB. `beq-cr` and `bne-cr` of 0 are `cbz` and `cbnz`. There is no long
form: a branch out of reach fails. `jmp` is `b`, ±128MB. The float
branches use the conditions `eq`, `ne`, `ge`, `gt`, `ls` and `mi`.
`seq` to `sge` are the compare and a `cset`.

`call` is `str x30, [x28, #-8]!`, `bl` (±128MB), then
`ldr x30, [x28], #8`. `call-r` is the same round a `blr`. `call-i` loads the address into
x29 from `[b, #c]` and `call-p` by `ldr` literal (±1MB), then they are a
`call-r` of x29. `ret` is `ret`, `sync` is `dmb sy`. `lea-p` is `adr`,
`cpy-pr` the `ldr` literal, both ±1MB.

`mul-rr` is `madd` with xzr. A divide is `sdiv` or `udiv` into x29, an
`msub` for the remainder, and a copy of x29 to `d2`. A shift by a
constant is `ubfm` or `sbfm`, 0 nothing, below 0 or above 63 fails.
`min` and `max` are `cmp` and `csel`. The float ops are the `d` forms;
`cvt-rf` is `scvtf`, `cvt-fr` `fcvtzs`.

`e_entry_abi` copies sp to x28. `e_call_abi`, `(r b c n)`, takes up to 8
args, already in x0 to x7. It pushes x1 to x7 and x19, sets sp to x28
rounded down to 16, keeps x30 in x29, loads `r` from `[b, #c]`, `blr r`,
then puts x30 back and pops. `e_stack_init` takes 24 from `s`, puts `f`
and `x` at 8 and 16 from it, and at 0 the address of a stub, after a `b`
over it, which pops x29 and x30 from x28 and jumps to x29.

The prepass goes from instruction 8 to the one before the last:

* Call fusion: calls in a straight line keep the link on the stack
  between them, the first a push and `bl`, the middle ones only `bl`, the
  last `bl` and a pop. Each `:rsp` access between them is moved a slot
  deeper, a copy of `:rsp` becoming a `lea-i` of 8. A label, branch,
  jump, `ret`, `push`, `pop`, `alloc`, `free`, `sync`, save or load state
  or ABI op ends it, so does an `:rsp` access that cannot be moved, or a
  call that uses `:rsp`.

* Pair fusion: two loads or stores in a row of the same op, 8 byte, int
  or float, the same base, both offsets numbers, one a size past the
  other, the lower a whole number of the size and in the reach of `ldp`,
  -64 to 63 of the size, less a slot from `:rsp` while a call fusion is
  open. Not if the two destinations are the same, either is `:rsp`, or a
  load is into its own base. The first becomes `ldp`, `ldpsw`, `ldp w`,
  `ldp d`, `stp`, `stp w` or `stp d`, the second nothing.

* Copy fusion: an `add`, `sub`, `and`, `or` or `xor` of two registers, a
  shift by a constant, an `add-cr` or `sub-cr` of up to 0xfff, a `mul-cr`
  by -1, or a logical op of a logical immediate, with a `cpy-rr` to its
  destination up to 6 instructions back. Nothing between may use the
  destination, or the other source, or be a barrier as above, and the
  copy is not from the destination or `:rsp`. The copy becomes the three
  register op, the op nothing.

### x86_64

`:r0` to `:r3` are rax, rcx, rdx and rbx, `:r4` to `:r6` rbp, rsi and
rdi, `:r7` to `:r14` r8 to r15, `:rsp` rsp, `:f0` to `:f15` xmm0 to xmm15.
It keeps no register: a temp it needs is pushed and popped round the use,
or a constant is put in the code.

A constant of 0 is `xor r32, r32`, -1 `or r, -1`. 0 to 0xffffffff is
`mov r32, imm32`, a signed 32 bit one `mov r64, imm32`, any other
`mov r64, imm64`. An ALU op of a constant takes an imm8, then an imm32,
the short form for rax. A bigger one is a `jmp` over the 8 bytes of the
constant, then the op of `[rip - 15]`. `add-cr` and `sub-cr` of 1 are
`inc` and `dec`, `and-cr` of 0 is `xor`, a compare with 0 `test r, r`.
`mul-cr` by 0, 1, -1 or a power of 2 is `xor`, nothing, `neg` or `shl`,
else `imul` with an imm8 or imm32, or of the constant in the code.

Loads and stores take no displacement, a disp8 or a disp32, with the SIB
byte rsp and r12 need and the 0 disp rbp and r13 need. An offset beyond
32 bits is put in the first of rax, rcx, rdx and rbx that is not the data
or the base, pushed and popped round it, and the indexed form used. `-b`
and `-s` loads are `movsx`, `-i` `movsxd`, `-ub` and `-us` `movzx`,
`-ui` `mov r32`. A short store has the 0x66 prefix. The float ones are
`movsd`. `lea-i` and `lea-d` are `lea`, `lea-p` and `cpy-pr` the rip
relative `lea` and `mov`, ±2GB.

`push` and `pop` are one `push` or `pop` each. `emit-save-state` and
`emit-load-state` are nothing, the return address being on the stack.

A conditional branch is `cmp` or `test` and a `jcc`: rel8 if the most it
has reached fits, else rel32. `jmp` is `eb` rel8 or `e9` rel32. Out of
32 bits fails. A float branch is `ucomisd` then: `beq-ff` a `jp` over a
`je`, `bne-ff` a `jp` and a `jne`, `bgt-ff` and `bge-ff` `ja` and `jae`,
`blt-ff` and `ble-ff` the same with the operands swapped. `seq` to `sge`
are the compare, `setcc` and `movzx`.

`call` is `e8` rel32. `call-r` is `call r`, `call-i` `call [b + c]`,
`call-p` `call [rip + disp32]`, the path's slot. The jumps are the same
with `jmp`. `ret` is `ret`, `sync` `mfence`.

`mul-rr` is `imul`. A divide with `d1` rdx and `d2` rax is done in place,
with them the other way round an `xchg` either side; else the operands,
rdx, rcx and rax are pushed, the divide done in rax by rcx, and the
results stored in the operands' slots before the pops. The signed one
tests for a divisor of 0, quotient 0 and remainder the dividend, and of
-1, a `neg`, before `cqo` and `idiv`; the unsigned one tests for 0 before
an `xor edx, edx` and `div`. A shift by a constant is nothing for 0, the
short form for 1, `imm8` for 2 to 63, and fails for any other. A shift by
a register is by cl, with an `xchg` either side if the destination is
rcx, else the operands, rax and rcx pushed and the shift done in rax.
`min-rr` and `max-rr` are `cmp` and `cmovg` or `cmovl`, the constant
forms a `jcc` over a `mov`. `abs-rr` is `test`, `jns`, `neg`. `ext-rr`
from rax to rdx is `cqo`. The float ops are the `sd` forms, `cvt-rf`
`cvtsi2sd`, `cvt-fr` `cvttsd2si`. `abs-ff` shifts the xmm left 1 and
right 1, `neg-ff` flips bit 63 with `btc` through rax.

The two bodies differ only in the ABI. `vp_args`, `:r0` to `:r5`, go to
`rdi rsi rdx rcx r8 r9` on AMD64, `rcx rdx r8 r9` on WIN64:

* `e_entry_abi`, `(n)`, moves the host's args to `:r0` on, by pushes and
  pops: 6 at most on AMD64, 4 on WIN64.

* `e_call_abi`, `(r b c n)`, 6 args at most. `r` must be a register the
  ABI keeps, rbx rbp r12 to r15, and rsi rdi on WIN64, and not `b`. It
  pushes the caller saved registers, moves the args, keeps rsp in `r`,
  `and rsp, -16`, 8 more if an odd number of args go on the stack, pushes
  them (WIN64 past 4), and on WIN64 32 bytes of shadow space. Then
  `call [b + c]`, rsp from `r`, and the pops.

* `e_stack_init` puts `f` and `x` at `s` less 16, so a `ret` goes to `f`.

The prepass goes from instruction 8 to the last:

* A `cpy-ir` of 8 bytes, a number offset in 32 bits, its base not `d`,
  up to 6 instructions back, then an `add`, `sub`, `and`, `or`, `xor` or
  `mul` of two registers from what it loaded, `(s d)`, neither `:rsp`.
  Nothing between may be a barrier, write its last operand, or use `s`
  or `d`. `s` must be dead after: written, not read, in the 15 that
  follow, before any barrier. The load becomes nothing, the op
  `emit-x64-alu-ir`, `op d, [base + offset]`.

* A `cpy-rr` to `d` up to 6 back, then an `add-rr`, an `add-cr` or
  `sub-cr` in 32 bits, or a `shl-cr` by 1, of `d`. The copy becomes `lea
  d, [s + x]`, `[s + c]` or `[s + s]`, the op nothing, by the same rules
  as the arm64 copy fusion.

### riscv64

`:r0` to `:r7` are a0 to a7 (x10 to x17), `:r8` to `:r14` s2 to s8 (x18
to x24), `:rsp` sp (x2). `:f0` to `:f15` are fs0 to fs11 (f8, f9, f18 to
f27) and ft0 to ft3 (f0 to f3). It keeps x31 (t6) as its first temp, x30
(t5) and x29 (t4) as others, x27 and x26 (s11, s10) for sp and ra over a
host call, and x1 as ra.

A constant of 12 bits is `addi r, zero, c`. One of 32 bits is `lui` of the
upper part, rounded for the sign of the low, and `addiw` of the low 12 if
not 0. A bigger one is split into the low 32 bits signed and the high, one
more if the low is below 0. With a low of 0 it is the high made as above
and `slli 32`, else the high in x30, `slli 32`, the low in `r`, and an
`add`: up to 6 instructions.

* `add-cr`, `xor-cr`, `or-cr` and `and-cr` of 12 bits signed are `addi`,
  `xori`, `ori` and `andi`. `sub-cr` is `add-cr` of the negative.

* `mul-cr` by 0, 1, -1 or a power of 2 is `xor`, nothing, `sub` from zero
  or `slli`.

* Any other constant goes to x31, or x29 for the sets, and the register
  form is used.

Loads are `lb lh lw ld lbu lhu lwu`, stores `sb sh sw sd`, `fld` and
`fsd` the floats, with a 12 bit signed offset. A bigger offset is made in
x29, x31 for a float, added to the base, and the access is at 0 from it.
The indexed forms are an `add` to x29, or x31, and the access at 0.
`lea-i` is `addi`, else `add` of x29.

`push` is one `addi sp, sp, -8n` and an `sd` of each, the first deepest,
`pop` the `ld`s and the `addi`. `emit-save-state` and `emit-load-state`
push and pop ra.

A conditional branch is `beq`, `bne`, `blt` or `bge` on two registers,
`bgt` and `ble` with them swapped. A constant of 0 is the zero register,
any other is made in x31 first. The short form reaches ±4KB; past that,
by the most it has reached, it is the condition the other way round
branching over a `jal zero` of ±1MB. A float branch is `feq.d`, `flt.d`
or `fle.d` to x31, then `bne x31, zero`, or `beq` for `bne-ff`.

`call` is a push of ra, `jal ra` (±1MB), and a pop of ra. `call-r` is the
same round `jalr ra, 0(r)`. `call-i` loads the address into x31 from
`c(b)`, `call-p` by `auipc` and `ld`, then a `call-r` of x31. `jmp` is
`jal zero`, ±1MB, `ret` `jalr zero, 0(ra)`. `lea-p` is `auipc` and
`addi`, `cpy-pr` `auipc` and `ld`, ±2GB. `sync` is `fence`.

The divide is `div` or `divu` to x31, then `sltu` and `sub` make a mask of
`s` not 0, the `and` of it makes the quotient 0 on a divisor of 0, `mul`
and `sub` give the remainder. A shift by a constant is `slli`, `srli` or
`srai`, 0 to 63, any other fails. `seq` and `sne` are a `sub` and
`seqz` or `snez`, `slt` and `sgt` `slt`, `sle` and `sge` an `slt`, a
`seqz` and an `or`. `lnot` is `seqz`, `land` two `snez` and an `and`.
`min`, `max` and `abs` branch over a copy or a negate. The float ops are
the `.d` forms; `cvt-fr` is `fcvt.l.d` rounding towards 0, `abs-ff` and
`neg-ff` `fsgnjx.d` and `fsgnjn.d`.

`e_entry_abi` is nothing, the args already in a0 on. `e_call_abi`, `(r b
c n)`, takes up to 8 args. It pushes `:r1` to `:r8`, keeps sp in x27 and
ra in x26, `andi sp, sp, -16`, loads x31 from `c(b)`, `jalr ra, x31`,
then puts sp and ra back and pops. `e_stack_init` is as on arm64, the
stub after a `jal` over it popping x31 and ra and jumping to x31.

The prepass is the generic call fusion of `vpb_trans.inc`, instructions 8
to the last, the same rules as the arm64 call fusion, making
`emit-riscv64-call-h`, `-m` and `-t` and their `-r`, `-i` and `-p` forms.

### la64

`:r0` to `:r7` are a0 to a7 (r4 to r11), `:r8` to `:r14` s0 to s6 (r23
to r29), `:rsp` sp (r3), `:f0` to `:f15` f0 to f15. It keeps r12 (t0) as
its first temp, r13 and r14 (t1, t2) as others, r30 and r31 (s7, s8) for
sp and ra over a host call, and r1 as ra.

A constant of 12 bits signed is `addi.d r, zero, c`. One of 32 bits is
`lu12i.w` of bits 12 to 31 and `ori` of the low 12 if not 0. A bigger one
adds `lu32i.d` of bits 32 to 51, then `lu52i.d` of the top 12 unless the
sign extension of `lu32i.d` has made them already: up to 4 instructions.

* `add-cr` of 12 bits signed is `addi.d`, `sub-cr` is `add-cr` of the
  negative.

* `and-cr`, `or-cr` and `xor-cr` of 0 to 0xfff are `andi`, `ori` and
  `xori`, the immediates being unsigned.

* `mul-cr` by 0, 1, -1 or a power of 2 is `xor`, nothing, `sub.d` from
  zero or `slli.d`. `slt-cr` of 12 bits is `slti`.

* Any other constant goes to r12, or r14 for the sets, and the register
  form is used.

Loads are `ld.b ld.h ld.w ld.d ld.bu ld.hu ld.wu`, stores `st.b st.h st.w
st.d`, `fld.d` and `fst.d` the floats, with a 12 bit signed offset. A
bigger offset is made in r14 and added to the base, or for a float the
`fldx.d` or `fstx.d` of r12. The indexed forms are one `ldx`, `stx`,
`fldx.d` or `fstx.d`. `lea-i` is `addi.d`, else `add.d` of r14.

`push`, `pop`, `emit-save-state` and `emit-load-state` are as on riscv64.

A conditional branch is `beq`, `bne`, `blt` or `bge`, `bgt` and `ble`
with the registers swapped, a constant of 0 the zero register, any other
in r12. The short form reaches ±128KB; past that it is the condition the
other way round branching over a `b` of ±128MB. A float branch is
`fcmp.ceq.d`, `fcmp.clt.d` or `fcmp.cle.d` to `$fcc0`, then `bcnez`, or
`bceqz` for `bne-ff`, reach ±4MB, with no long form.

`call` is a push of ra, `bl` (±128MB), and a pop of ra. `call-r` is the
same round `jirl ra, r, 0`. `call-i` loads the address into r12, `call-p`
by `pcaddu12i` and `ld.d`, then a `call-r` of r12. `jmp` is `b`, ±128MB,
`ret` `jirl zero, ra, 0`. `lea-p` is `pcaddu12i` and `addi.d`, `cpy-pr`
`pcaddu12i` and `ld.d`, ±2GB. `sync` is `dbar 0`.

The divide is `div.d` or `div.du` to r12, `maskeqz` to make it 0 on a
divisor of 0, then `mul.d` and `sub.d` for the remainder. A shift by a
constant is `slli.d`, `srli.d` or `srai.d`, 0 to 63, any other fails. The
sets are as on riscv64, with `sltui` for `seqz`, `sltu` from zero for
`snez`, and `sle` and `sge` an `slt` and an `xori` of 1. `min`, `max`
and `abs` branch over a copy or a negate. The float ops are the `.d`
forms. `cvt-rf` is `movgr2fr.d` and `ffint.d.l`; `cvt-fr` keeps `s` in
r12, does `ftintrz.l.d` in place, moves the result out and puts `s` back.

`e_entry_abi` is nothing. `e_call_abi` is as on riscv64, pushing `:r1` to
`:r8`, keeping sp and ra in r30 and r31, sp `and`ed with -16 made in r12,
the address loaded into r12, the call `jirl ra, r12, 0`. `e_stack_init`
is as on riscv64, with r12.

The prepass is the generic call fusion, making `emit-la64-call-h` and
the rest.

### vp64

There is no native CPU: the output is VP64 code for the emulator,
`src/host/vp64.cpp`, in 16 bit words. The registers are the VP's own,
`:r0` to `:r14` 0 to 14, `:rsp` 15, the floats 0 to 15. There is no
prepass.

The op numbers are the order of `enum Opcodes` in `vp64.cpp`, 191 of them,
which `vp64_ops` in `vp64.vp` follows by name, an op of 2 or 4 forms
taking as many numbers. The first word is the op in its low 8 bits, then
a register at bits 8 to 11 and another, or 4 bits of a constant, at bits
12 to 15.

* A constant op, `cpy-cr`, the ALU ops, `cmp-cr`, `min-cr`, `max-cr` and
  the sets, has 4 forms by size: 4 bits signed in the first word, 20 bits
  with one more word, 36 with two, else all 64 bits in four words after.

* The register ops are one word, `d` at bit 8 and `s` at 12.

* A shift by a constant is a word and the count, 0 nothing, below 0 or
  above 63 fails.

* A load or store with an offset is a word and a 16 bit signed offset, or
  the next op number and the offset in four words.

* An indexed load or store, `lea-d` and the divides have the third
  register in a word of their own.

* `call-i` and `jmp-i` take a 20 bit signed offset, `cpy-pr` and `lea-p`
  a 20 bit one from past the instruction; past that they fail.

* `call-abi` is `b` and `n` and a word of `c`, 0 to 0xfff8; `r` is not
  used.

A compare is its own op, `cmp-cr`, `cmp-rr` or `cmp-f`, setting the
emulator's compare pair, then the branch op tests it. A branch, `call`,
`jmp`, `call-p` and `jmp-p` have 2 forms: the offset in the top byte of
the word, from past it, or the next op number with 24 bits, the low 8 in
the word and the rest in the next, from past both. Past 24 bits fails. A
conditional branch is sized by the most it has reached; `call`, `jmp`,
`call-p` and `jmp-p` by the reach on this pass.

`push` is a `sub-cr` of `:rsp` and a `cpy-ri` of each, `pop` the reverse.
`emit-entry-abi`, `emit-save-state` and `emit-load-state` are nothing,
`call` pushing the return pc as x86_64 does. `e_stack_init` puts `f` and
`x` at `s` less 16, as x86_64.

## Binding a Translator, `lib/trans/vp.inc`

`lib/trans/vp.inc` binds the translator of the target with `ffi`, from
`obj/<running platform>/lib/trans/vpb_<cpu>`, with `_<abi>` for x86_64, or
from the boot image if it has it, `image-has?` in `lib/boot/image.inc`. A
dev tree that has neither gets `No translator to this CPU here, make
install makes them !`. It has:

* `(emit-translate emit_code)`: a list of `(emit-xxx ...)` to native code.

* `(vp-translate-bin bin)`: a function's VP binary to native code.

* `(vp-translate-save entry)`: a build's, a bundle entry to native code,
  saved by its name, its VP binary given back.

* `(vp-translate-file file dir)`: a build's translate stage of one file.

* `(vp-translate-out name out)`: the native code saved in
  `obj/<cpu>/<abi>/`.

## Build and Install

The VP stage of a build writes the bundle of each file as text,
`obj/vp/<file>`, and as binary beside it, `<file>b`. The translate
stage of each target reads the binary bundle, falling back to the text if it
is older. The whole tree translates in about 7 ms, against 162 ms for the
old Lisp translators.

### Translators Made First

A build translates with the translators of the platform it runs on, the
functions made of `lib/trans/<cpu>.vp` in `obj/<cpu>/<abi>/lib/trans/`. When
one of those sources changes, `(make)` in `lib/asm/asm.inc` makes the
translators before anything else:

* Those out of date are made first, by the translator there is now.

* If this platform's own translator was one of them, it made all the
  others, so all of them are made again, by the new one. A translator made
  by its new self is what a clean build would give.

* The stages after leave them alone: their own make would be newer than
  them, and everything would be made again on the next build.

`make platforms` and `make all platforms` build the platform they run on
first, so the translators to every target are new before any target is
translated.

A target's functions are translated again when its translator's sources,
`lib/trans/<cpu>.vp` and `vp.inc`, `vpb.inc`, `vpb_ops.inc` and
`vpb_trans.inc`, are newer than them. A change to the RISC-V translator
makes the RISC-V code again, and of the other targets only their own copy of
the RISC-V translator. A change to one of the classes it includes does not,
as it does not change what the translator gives.

### A Translator Bound Fresh

A node binds a function once and keeps it. A node that ran a build before
has the old translator in its function list, and a worker of the next build
started there would bind that, not the one just made. So each worker of the
farm, `lib/asm/asm.lisp`, before its first job loads the translator's file
as it is now and places it in a function server of its own, named by its
hash, `(task-server)` and `(task-place)`. Its bind of the translator finds
that first. The server goes when the worker ends.

Other users of `emit-translate`, `@Source` and a shader's compile, bind the
translator as any function is bound, and see a new one when their node is
started again, as with any VP function made again.

### The Loader's Own

A build keeps each function's VP binary beside its native,
`obj/<cpu>/<abi>/<name>.vpb`. The loader, `(:sys_load :load)`, links the
translator of its CPU, so every boot image holds it, and calls it with no
Lisp: `:r0` 0, `:r1` the VP binary and `:r2` its length, giving back the
str of the native or 0. A native that is not there, or not as long as it
says, as when another node is writing it, is translated from its `.vpb`,
copied into the block as a file would be read, and kept for the next. See
[code_delivery_design.md](code_delivery_design.md).

### Install

The VP64 boot image in `snapshot.zip` carries all six translators already
translated to VP64 code, `+boot_extras` in `lib/boot/image.inc`. So `make
install`, running on the emulator, translates to the native CPU with them,
and never needs a translator in Lisp. The VP64 has no translator files of
its own then, so the translators are not made first; the native build makes
the native ones, and the next native build uses them.

## A New CPU

A new CPU port means:

1. Writing `lib/trans/<cpu>.vp`, starting from a copy of the nearest one,
   `riscv64.vp` say: its `reg_map`, a routine for each op, its
   `e_call_abi` and `e_entry_abi` for the host's ABI, and its `prepass`.

2. Adding its fused ops, if it has any, to `lib/trans/vpb_ops.inc`.

3. Adding it to `+boot_extras` in `lib/boot/image.inc`, so installs can
   translate to it.

The emulator, VP64, needs no new translator, so the PII of a new host is
still tested first, see [`porting.md`](porting.md).

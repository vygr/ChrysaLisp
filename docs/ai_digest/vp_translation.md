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

It is called from Lisp as `(vpb-<cpu> bin)` and gives back the native code
as a string, or `:nil` if it cannot. The decode, the prepass, the passes
and the output are all in VP.

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
the pass before, after 2 passes at the least.

A routine that is given something it cannot make, an op it has no routine
for, a shift by a constant outside 0 to 63, a branch out of reach, marks the
function and the translator gives back `:nil`. `vp-translate-bin` then
throws `The <cpu> translator failed on this function !` with its name. Any
constant is handled, a translator makes one that does not fit an
instruction some other way, so it is a fault in the translator, not the VP.

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
`obj/vp/<mode>/<file>`, and as binary beside it, `<file>b`. The translate
stage of each target reads the binary bundle, falling back to the text if it
is older. The whole tree translates in about 7 ms, against 162 ms for the
old Lisp translators.

The VP64 boot image in `snapshot.zip` carries all six translators already
translated to VP64 code, `+boot_extras` in `lib/boot/image.inc`. So `make
install`, running on the emulator, translates to the native CPU with them,
and never needs a translator in Lisp. A native build then has its own.

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

# Virtual Machine

This document covers a variety of subjects to do with the VM that ChrysaLisp
employs. Some of them are not exactly about the VM but they are all to do with
how code gets executed, called and so forth.

## Virtual Processor

The lowest level of ChrysaLisp is the Virtual Processor (VP), this is an
imaginary RISC like processor that the assembler and script compiler target.

It is, currently, a very simple 64 bit 32 register (16 int, 16 IEEE float),
load/store machine. It may be extended in the future with features like vector
instructions, but for now I'm preferring to see just how far you can push such a
simple design.

It supports a very orthogonal logic and arithmetic instruction set and a few
simple load/store addressing modes.

### Registers

```vdu
:r0,:r1,:r2,:r3,:r4,:r5,:r6,:r7,:r8,:r9,:r10,:r11,:r12,:r13,:r14,:rsp
:f0,:f1,:f2,:f3,:f4,:f5,:f6,:f7,:f8,:f9,:f10,:f11,:f12,:f13,:f14,:f15
```

These are mapped to real physical registers by the target processor's
translator, the `reg_map` of `lib/trans/x86_64.vp` and `lib/trans/arm64.vp`. On certain
processors, like the x86_64, it's worth knowing that `:r0` and `:r2` are mapped
to rax and rdx when it comes to scheduling VP div and rem code ! It makes no
difference to the arm64 translator, so one does tend to make VP divide code
use `:r0` and `:r2` as it really helps the x86_64 code generation quality.

```image
docs/diagrams/vp_registers.cwb
```

You can use the `(vp-rdef)` and `(vp-fdef)` macros to assign register equated
symbols to help your source look nice. Or bind symbols to registers, via
`(method-input)` and `(method-output)`, that match function entry/exit
parameters if you desire. A great example of this is the `:canvas :fpoly`, or
the `:pixmap :resize_2` functions.

### What the machine is

A word is 64 bits and memory is little endian, on every target. A load of
`-b`, `-s` or `-i` sign extends the byte, short or int to a word, of `-ub`,
`-us` or `-ui` zero extends it, and a store of one of those writes its low
bytes and no more. A load or a store is at its natural alignment, a word at
an address that is a whole number of words, an int of 4 bytes, a short of 2;
one that is not is not defined, and `run_tui.sh -V`, the VP64 emulator in
validation mode, says each place that is not, the function and where in it.
The float registers hold IEEE doubles. In memory a float is a double, 8
bytes, or with `-i` a single, 4 bytes, as a GPU has its floats:
`vp-cpy-if-i` loads one and makes it a double, `vp-cpy-fi-i` stores the
nearest single to a register, which is not changed. The `-i` is of the
memory, as `vp-cpy-ir-i` is; the register is a double either way.

A constant, and the offset of a load or a store, may be any 64 bit value: a
translator makes one that does not fit an instruction of its CPU some other
way, a temp register, a register put by round it, a constant in the code, a
wider form, and the VP code never knows.

Arithmetic is of words and wraps: a multiply keeps the low 64 bits of the
product, `vp-abs-rr` of the most negative number is itself. A shift by a
register uses the low 6 bits of the count; a shift by a constant takes a
constant of 0 to 63, and a translator fails on one outside that: it gives
back nothing for the function, and `vp-translate-bin` throws `The <cpu>
translator failed on this function !` with its name.
`(vp-div-rrr rs rdh rdl)` divides `rdl` by `rs`, the quotient to `rdl` and the
remainder to `rdh`, whatever `rdh` held; the quotient goes towards 0 and the
remainder has the sign of what was divided. A divide by 0 gives a quotient of 0
and the remainder what was divided; the most negative number divided by -1
gives itself and a remainder of 0. `vp-div-rrr-u` is the same of unsigned
words.

`(vp-ext-rr rs rd)` makes `rd` 0 or -1, the sign of `rs`, the high word of `rs`
made two. `(vp-swp-rr rs rd)` swaps them. `vp-min` and `vp-max` of words are
signed. `(vp-cvt-fr fs rd)` takes the whole part, towards 0, of a float in the
range of a word; of one outside it, or of a NaN, what it gives is not defined.
`(vp-cvt-rf rs fd)` is the nearest float. `vp-cpy-rf` and `vp-cpy-fr` copy the
bits as they are. `vp-min-ff` and `vp-max-ff` of a NaN are not defined. Of the float
branches, as IEEE has it, every compare with a NaN is false but `vp-bne-ff`.
`(vp-sync c)` is a full barrier of memory, whatever `c`.

A call leaves one word on the stack under the callee, and what is in it is not
defined: the callee does not read it or write it, or look above it. Some
targets have the return address there, some the link register of the caller,
and a translator may join calls that follow each other on the one word.
`(vp-call-abi rt rb i arg_list)` calls a host function: it gives its result in
`:r0`, keeps every other general register of the VP but `rt`, which is left
with anything in it, and keeps no float register, as a change of task keeps
none. It lines the stack up as the host's ABI wants at the call, so the VP
never does.

### VP Assembler instructions

#### Constant to register

```vdu
(vp-cpy-cr c rd)
(vp-add-cr c rd)
(vp-sub-cr c rd)
(vp-mul-cr c rd)

(vp-and-cr c rd)
(vp-or-cr c rd)
(vp-xor-cr c rd)

(vp-shl-cr c rd)
(vp-shr-cr c rd)
(vp-asr-cr c rd)

(vp-min-cr c rd)
(vp-max-cr c rd)
```

#### Register to register

```vdu
(vp-cpy-rr rs rd)
(vp-add-rr rs rd)
(vp-sub-rr rs rd)
(vp-mul-rr rs rd)

(vp-and-rr rs rd)
(vp-or-rr rs rd)
(vp-xor-rr rs rd)

(vp-shl-rr rs rd)
(vp-shr-rr rs rd)
(vp-asr-rr rs rd)

(vp-div-rrr rs rdh rdl)
(vp-div-rrr-u rs rdh rdl)

(vp-abs-rr rs rd)
(vp-min-rr rs rd)
(vp-max-rr rs rd)
```

#### Logical And, Not

```vdu
(vp-lnot-rr rd)
(vp-land-rr rs rd)
```

`(vp-lnot-rr rd)` makes `rd` 1 if it was 0, else 0. `(vp-land-rr rs rd)`
makes `rd` 1 if both were not 0, else 0, and leaves `rs` as it was. Any
registers may be used.

#### Register to Memory

```vdu
(vp-cpy-ri rs rb i)
(vp-cpy-rd rs rb ri)
(vp-cpy-ri-b rs rb i)
(vp-cpy-rd-b rs rb ri)
(vp-cpy-ri-s rs rb i)
(vp-cpy-rd-s rs rb ri)
(vp-cpy-ri-i rs rb i)
(vp-cpy-rd-i rs rb ri)
```

#### Memory to Register

```vdu
(vp-cpy-pr label rd)
(vp-cpy-ir rb i rd)
(vp-cpy-dr rb ri rd)
(vp-cpy-ir-b rb i rd)
(vp-cpy-dr-b rb ri rd)
(vp-cpy-ir-ub rb i rd)
(vp-cpy-dr-ub rb ri rd)
(vp-cpy-ir-s rb i rd)
(vp-cpy-dr-s rb ri rd)
(vp-cpy-ir-us rb i rd)
(vp-cpy-dr-us rb ri rd)
(vp-cpy-ir-i rb i rd)
(vp-cpy-dr-i rb ri rd)
(vp-cpy-ir-ui rb i rd)
(vp-cpy-dr-ui rb ri rd)
```

#### Branch

```vdu
(vp-beq-cr c rd label)
(vp-bne-cr c rd label)
(vp-blt-cr c rd label)
(vp-ble-cr c rd label)
(vp-bgt-cr c rd label)
(vp-bge-cr c rd label)

(vp-beq-rr rs rd label)
(vp-bne-rr rs rd label)
(vp-blt-rr rs rd label)
(vp-ble-rr rs rd label)
(vp-bgt-rr rs rd label)
(vp-bge-rr rs rd label)

(vp-seq-cr c rd)
(vp-sne-cr c rd)
(vp-slt-cr c rd)
(vp-sle-cr c rd)
(vp-sgt-cr c rd)
(vp-sge-cr c rd)

(vp-seq-rr rs rd)
(vp-sne-rr rs rd)
(vp-slt-rr rs rd)
(vp-sle-rr rs rd)
(vp-sgt-rr rs rd)
(vp-sge-rr rs rd)
```

A compare is signed, there is no unsigned one. A branch goes to its label if
the compare holds. A set, `vp-seq` to `vp-sge`, makes `rd` 1 if it holds and 0
if not, on every target, so the value may be counted or added, not only
tested.

#### Effective address

```vdu
(vp-lea-i rb i rd)
(vp-lea-d rb ri rd)
(vp-lea-p label rd)
```

#### Call, Jump, Ret

```vdu
(vp-call label)
(vp-call-r rd)
(vp-call-i rb i)
(vp-call-p label)
(vp-call-abi rt rb i arg_list)

(vp-jmp label)
(vp-jmp-r rd)
(vp-jmp-i rb i)
(vp-jmp-p label)

(vp-ret)
```

#### Push, Pop

```vdu
(vp-push rs rs ...)
(vp-pop rd rd ...)

(vp-alloc c)
(vp-free c)
```

A push lays its registers on the stack as if each was pushed in turn, a word
each: after `(vp-push :r0 :r1 :r2)` the `:r2` is at `(:rsp 0)`, `:r1` at
`(:rsp +ptr_size)` and `:r0` at `(:rsp (* 2 +ptr_size))`. A pop takes them back
in the same layout, the last first. `(vp-alloc c)` takes c bytes, `(vp-free c)`
gives them back, and c is a whole number of words.

What these put on the stack lies end to end, nothing before, after or
between: a push of n registers is n words, an alloc of c is c bytes. So any
word below `:rsp` that a function put there may be read or written by its
offset, and what was put there may be taken back in any grouping: two pushes
by one pop of both, a push by a free, an alloc by a pop, or the allocs of
nested scopes by one free, as `(return)` does. What is taken back is never
more than was put there, and a function leaves the stack as it found it.

#### Swap, Extend

```vdu
(vp-swp-rr rs rd)
(vp-ext-rr rs rd)
```

#### Memory Sync

```vdu
(vp-sync c)
```

#### Float ops

```
(vp-cpy-ff fs fd)
(vp-cpy-rf rs fd)
(vp-cpy-fr fs rd)
(vp-cvt-rf rs fd)
(vp-cvt-fr fs rd)

(vp-add-ff fs fd)
(vp-sub-ff fs fd)
(vp-mul-ff fs fd)
(vp-div-ff fs fd)

(vp-abs-ff fs fd)
(vp-neg-ff fs fd)
(vp-min-ff fs fd)
(vp-max-ff fs fd)
(vp-sqrt-ff fs fd)

(vp-cpy-if rb i fd)
(vp-cpy-fi fs rb i)
(vp-cpy-df rb ri fd)
(vp-cpy-fd fs rb ri)

(vp-cpy-if-i rb i fd)
(vp-cpy-fi-i fs rb i)
(vp-cpy-df-i rb ri fd)
(vp-cpy-fd-i fs rb ri)

(vp-beq-ff fs fd label)
(vp-bne-ff fs fd label)
(vp-blt-ff fs fd label)
(vp-ble-ff fs fd label)
(vp-bgt-ff fs fd label)
(vp-bge-ff fs fd label)
```

#### Pseudo ops

```vdu
(vp-label label)
(vp-align align [byte])
(vp-string string)
(vp-cstr string)
(vp-byte byte ...)
(vp-short short ...)
(vp-int int ...)
(vp-long long ...)
```

```vdu
(vp-simd op rsv rdv)
```

Register op keys:

* c, constant

* r, register

* f, float register

* i, constant index

* cr, constant to register

* rr, register to register

* rf, register to float register

* fr, float register to register

* rrr, register pair by register to register pair

* rd, destination register

* rs, source register

* fd, float destination register

* fs, float source register

* rdh, destination register, high

* rdl, destination register, low

* rb, base register

* ri, index register

* rt, temp register

* rsv, source register vector

* rdv, destination register vector

Memory op suffix keys:

* ri, register to base indexed, index is a constant byte offset, 8 byte data
item if no qualifier

* rd, register to base indexed, index is a register byte offset, 8 byte
data item if no qualifier

* ir, base indexed to register, index is a constant byte offset, 8 byte
data item if no qualifier

* dr, base indexed to register, index is a register byte offset, 8 byte
data item if no qualifier

Memory op data type qualifier keys:

* b, 1 byte data item, sign extended on read

* s, 2 byte data item, sign extended on read

* i, 4 byte data item, sign extended on read

* ub, 1 byte data item, zero extended on read

* us, 2 byte data item, zero extended on read

* ui, 4 byte data item, zero extended on read

## Calling Convention

Simple answer is there is none. The better answer is that all calls, apart from
host OS ABI calls, take parameters in registers and not via the stack ! All
functions define their register inputs and outputs, and document register
trashes if they can. The `(assign)` function will do any parameter mapping and
copying for you, it'll tell you if it can't due to a circular mapping so you
can add a temp. Assignment will not attempt to spill to the stack or assign
temp registers !

In other systems, OS and Compilers, they adopt some convention for parameter
register layout and stack layout, but suffer performance problems and inability
to support features, like function chaining, as a result. You will often see a
ChrysaLisp function jump out to another function in order to save on stack
space with the eventual return going back to the original caller. Prime example
are deinit methods that chain on to their parent deinit with a direct
`(s-jump)`, but this happens all over the code base where possible.

The `(dec-method)` function takes the lists of input and output parameter
registers. If the list of inputs or outputs is `:nil` this means to inherit the
list from the parent class declaration.

After them it takes the registers a call of the method may leave not as they
were, its trashes, ranges and all: `(:r1-:r8 :r14 :f0-:f15)`, `()` for none.
Of a virtual method it is what any class that may be called so trashes. It is
not written by hand: `trace -w`, which works out what each function really
trashes, writes it, and `lint` says where a declaration is not what the code
does. `(method-trashes class method)` gives the list to the assembler, `:nil`
if it is not said: a method written in line in its `class.inc` has none.

A virtual method has one more list after that, what the one function that is
the method in this class trashes. A call through the vtable trashes the
first, it may be of any class. A call bound to the function trashes the
second.

An example from the array class.inc.

```file
class/array/class.inc "def-class" ":sort"
```

Core functions in the kernel and class libs are very careful to track and
document their register trashing. Higher level functions often use the next
registers available while calling lower functions in order to avoid stack
push/pop or other memory read/write instructions.

Any use of the script expression compiler means all bets are off as regards
register trashing, so you will see that all such functions are documented as
'trashes all'. However the expression compiler can be constrained to use only a
specific set of registers to do it's work, but that's an advanced topic for
specialist code generation, the vector math DSL takes full advantage of this
feature.

## Functions and bundles

### A function as it is loaded

A function is one block of bytes, `obj/<cpu>/<abi>/<path>`, with nothing
outside it to say what it is: a header, its code, its strings, its constants,
the links to the functions it calls and their paths, laid out the same on
every target. `docs/vm/vp_functions.md` has the layout field by field, how the
loader binds the links, and the boot image. A translator never lays it out
itself: the header and everything after the code come to it in the bundle as
data, below, and it makes them by doing what the data says.

### A bundle of VP

What a function is before any target, the VP its source file made, is kept as
text, the bundle of that file, `obj/vp/<file>`, or `obj/vp_validate/<file>`
for a validate build, `lib/asm/asm.inc`, and as VP binary beside
it, `<file>b`, `lib/trans/vpb.inc`. The VP stage writes the bundles once; each
target's translate stage, `(vp-translate-file)` in `lib/trans/vp.inc`, reads
the binary one back and its translator, `lib/trans/<cpu>.vp`, makes its own
code from it, and the tools that look at a function whole, `trace`, `relies`
and the rest, read the text bundles of `debug`.

A bundle is a run of forms, one a function, as `(read)` reads them:

```vdu
("apps/system/netspeed/vops"
(progn
(emit-label '_0)
(emit-long -1)
(emit-short _27 _2 _15 _21 6144)
...)
(locals ...)
(kept ...))
```

The first item is the function's path, the second the `progn` of its
instructions. Each instruction is a call of the `emit-` of a VP instruction,
`(emit-add-cr 1 :r0)` for `(vp-add-cr 1 :r0)`. Labels are symbols, `_0` and
on, set by `(emit-label)` and used before or after it. The header and the
tail are in it too, as data: `emit-byte`, `emit-short`, `emit-int`,
`emit-long`, `emit-string` and `emit-align`, so that a translator makes the
layout above by doing nothing more than its instructions say.

An operand is one of these, and nothing else:

* a register, `:r0` to `:r14`, `:rsp`, `:f0` to `:f15`.

* a number, a `num`, a whole number of 64 bits.

* a label, `_4`, or quoted where `(emit-label)` sets it, `'_4`.

* the distance between two places, `(- _4 *pc*)`, `(- *pc* _4)` or
`(- _4 _2)`.

* a string, its bytes.

* a name, a keyword that is not a register, the class and method a call-i
names.

Not a `fixed` nor a `real`: a table of fixed point numbers is written as their
16.16 bits, `(vp-int -12288 28672)`, not `(vp-int -0.1875 0.4375)`. Not a
form left to be worked out: a constant the source works out is the number it
came to by the time it is emitted. A form in a quoted list of operands is not
worked out, and a macro in one is only expanded, so `'((& :r4 (inc
+str_data)))` emits `(#add 24 1)`; write `` `((& :r4 ,(inc +str_data))) ``.
The encoder of VP binary, `lib/trans/vpb_encode.vp`, refuses any other
operand, and the build stops with the function's name.

After the instructions there may be `(locals ...)`, the frames of named locals
the function had, and `(kept ...)`, those of them kept in registers. They are
for the tools; a translator does not read them.

## VP function example

This is the system level string compare function. `:sys_str :compare`

Register inputs and outputs are declared in the `sys/str/class.inc` file.

```file
sys/str/class.inc ":compare" "dec-method"
```

So this function will take the C style input char*'s in registers `:r0` and
`:r1`, and will return the comparison value in register `:r0`.

Implementation of the function is defined in the `sys/str/class.vp` file.

```file
sys/str/class.vp ":compare" "def-method"
```

So let's go through the important lines in this function.

First of all the `(def-method :sys_str :compare)` is doing the same job as a
`(def-func)` would do, it's a wrapper function to simplify writing the
`(def-func)` that also does some extra checks to make sure you actually do have
a `(dec-method)` for it in the include file. The `(def-func-end)` just wraps
the function, matching any `(def-func)` or `(def-method)`. If you want to dive
into what these calls do to get your function compiled and written out, look in
`lib/asm/func.inc` where all the magic happens.

Next there is a section of documentation, this format can be parsed out by the
`make docs` command line tool. Parsed documentation ends up in the
`docs/reference/vp_classes/` folder.

The `(entry :sys_str :compare '(:r0 :r1))` and `(exit :sys_str :compare
'(:r2))` calls are helpers to make sure input and output parameters get copied
to the correct registers. They enforce the `(def-method)` input and output
register declarations by use of two `(assign)` calls. The register lists
provided here are auto assigned from and to the declared register input and
output lists ! In this case the entry of `'(:r0 :r1)` turns into an `(assign
'(:r0 :r1) '(:r0 :r1))` which ends up emitting no code, but the exit of
`'(:r2)` does an `(assign '(:r2) '(:r0))` which emits a `(vp-cpy-rr :r2 :r0)`
ensuring that the result in `:r2` ends up copied to the declared output `:r0`.
So entry and exit helpers ensure your function sticks to its declared contract
with the outside world.

The other lines that are not basic VP code instructions are `(loop-start)`,
`(loop-end)` and `(breakif)` functions. These are structured coding functions
defined within the `lib/asm/code.inc` file. There are many such helper
functions that allow all the basic structured code concepts to be used, even
within VP code as well as vpx level code. These will be covered in detail
in [other documents](vp_structure.md), but here the use is fairly obvious.

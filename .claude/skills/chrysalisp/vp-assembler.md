# ChrysaLisp VP Assembler Guide

Part of the `chrysalisp` skill. Read this in full before writing or changing a
`.vp` file.

## Contents

*	[Register Architecture](#register-architecture)

*	[VP Method Definition Boilerplate](#vp-method-definition-boilerplate)

*	[Generated Function Headers (`gen-create`, `gen-type`)](#generated-function-headers-gen-create-gen-type)

*	[The `assign` Macro & Frames](#the-assign-macro--frames)

*	[Field Helpers & Sorted Memory Transfers (`load-fields`, `save-fields`, `assign-fields`)](#field-helpers--sorted-memory-transfers-load-fields-save-fields-assign-fields)

*	[Hot Loops](#hot-loops)

*	[Symbols, Registers and Slots](#symbols-registers-and-slots)

*	[Calling Conventions & Register Introspection](#calling-conventions--register-introspection)

*	[Systematic VP Lowering Workflow](#systematic-vp-lowering-workflow)

*	[Declared Trashes, And What May Lean On Them](#declared-trashes-and-what-may-lean-on-them)

*	[Entry And Exit](#entry-and-exit)

*	[Push, Pop And Alloc](#push-pop-and-alloc)

*	[`sys/math` And Its Contract](#sysmath-and-its-contract)

*	[Loops Over Things That Are Let Go Of](#loops-over-things-that-are-let-go-of)

*	[The Shape Of Structured Code](#the-shape-of-structured-code)

*	[A Slot Is Memory, And No Passes](#a-slot-is-memory-and-no-passes)

*	[Every Load And Store Naturally Aligned](#every-load-and-store-naturally-aligned)
*	[Boot Image Functions Run From The Image](#boot-image-functions-run-from-the-image)

*	[A New Native And Its `ffi`](#a-new-native-and-its-ffi)

*	[Validate Functions Mix](#validate-functions-mix)

*	[VP Ops, The Text Format And The Translators](#vp-ops-the-text-format-and-the-translators)

*	[Two Traps In The Lisp Of The Assembler](#two-traps-in-the-lisp-of-the-assembler)

When writing or modifying `.vp` files, you target ChrysaLisp's register and
execution model.

## Register Architecture

*	15 General-Purpose registers: `:r0` through `:r14`.

*	1 Dedicated Stack Pointer: `:rsp`.

*	16 Floating-Point registers: `:d0` through `:d15`.

*	**ABSOLUTE VOLATILITY (No Callee-Saved Registers):** ChrysaLisp has NO
	callee-saved registers. Any call may trash registers. The `;trashes`
	header above each function is the sole source of truth.

## VP Method Definition Boilerplate

Every method must follow standard boundary and scoping conventions:

(def-method :class :method)
	;inputs
	;:r0 = this (ptr)
	;:r1 = arg (num)
	;outputs
	;:r0 = result (num)
	;trashes
	;:r1-:r3

	(vp-rdef (this arg res))

	(entry :class :method '(:r0 :r1))

	; ... method body ...

	(exit :class :method '(:r0))
	(vp-ret)

(def-func-end)

## Generated Function Headers (`gen-create`, `gen-type`)

`(gen-create :class [name])` and `(gen-type :class)` generate the
`class/x/create` and `class/x/type` functions. They are documented in the
same way as a method, by a header comment directly under the call:

(gen-type :list)
	;inputs
	;:r0 = list object (ptr)
	;outputs
	;:r0 = list object (ptr)
	;:r1 = type list object (ptr)
	;trashes
	;:r1-:r5, :d0-:d15

The doc scanner (`lib/files/info.inc`), `make docs` and `trace` all read
it, and `trace -i -l -w` keeps the `;trashes` line correct. When adding a
class, add these headers too, with `;none` as the trashes, and let
`trace -w` fill it in. Leave the `;inputs` section out of a `gen-create`
header when the class's `:vcreate` is declared with different inputs to its
`:create`, as both map to the one function and the scanner checks each.

## The `assign` Macro & Frames

`(assign ...)` moves registers, memory and constants to registers, and a
register to memory, many at once, in an order that is safe
(`(assign '((:r0 +str_length) (:r0 +str_data)) '(:r1 :r0))`). The width and
sign of a field come from its type, `<field>_t`, so a `uint` field is a
32 bit load that zero extends. An offset that is worked out needs its
width said, `(:rsp (+ slot_rect host_gui_rect_x) i)`. An address is
`(& :rsp slot_x)`, a label's `($ label)`, a function's `(@ path)`.

*	**No Memory-to-Memory or Constant-to-Memory:** `assign` throws for
	either. Go by a register: load it, then store it. A swap of two
	registers in one `assign` is a copy cycle, and throws too.

*	**No conversion.** `assign` moves a value as it is: a 32 bit `i` field
	to or from a single `:fN`, a word to or from a double `:dN`. A single
	and a double, either way, throws; write the `(vp-cvt-fd)` or
	`(vp-cvt-df)`. `(vp-reg-single :d4)` is `:f4`, to load a single into the
	register a double is named by. Nothing in the tree converts a type
	quietly.

*	**A value not wanted is `&`.** An output of a call, or any destination
	of `assign`, written `&` is left where it is: `(call :obj :type '(:r0)
	'(& :r1))`. `_` there is refused, it is a name, not a skip.

*	**A frame is a structure.** A function's frame is
	`(structure slot 0 ...)`, its fields `slot_x`, made by
	`(vp-alloc slot_size)` and freed by `(vp-free slot_size)`. `vp-alloc`
	and `vp-free` round up to whole longs; never put an `(align)` at the end
	of a structure for it, that upsets the packing of objects. A field may
	not be named `size`, nor `fields` in a frame: `slot_size` and
	`slot_fields` are the structure's own, and it throws.

		(structure slot 0
			(ptr this list)
			(uint count))

		(vp-alloc slot_size)
		(entry :class :method '((:rsp slot_this) (:rsp slot_list)))
		...
		(exit :class :method '((:rsp slot_this) :r1))
		(vp-free slot_size)
		(vp-ret)

*	**One frame.** Registers to keep over a call are slots of the frame,
	saved by `save-fields`, never pushed and then a frame made below them.
	A subroutine or callback of its own may have its own frame, a
	structure of another name or `slot` again, or push and pop.

*	**A jump out frees the frame first.** `jump`, `f-jmp`, `s-jump` and the
	like do not free it: load what they take from the frame, `vp-free`,
	then jump. An error path that has no frame yet, before the
	`vp-alloc`, needs none.

*	**Several at once.** Several registers to registers are one `assign`.
	Several slots to or from registers are `load-fields` or `save-fields`
	of `:rsp` (sorted, so arm64 pairs them), and so are several fields of
	any one base, unless that base is also a destination. `entry` and
	`exit` take slots, `'((:rsp slot_this) :r1)`.

*	**Conditions.** A run of `breakif`s is one `breakif` of several
	conditions, any of them breaks. `vpif` and `loop-until` of several are
	all of them. An or for a `vpif` is the branches the other way round,
	or `gotoif`s to the part they lead to. A `vpif` or `elseif` test may use
	registers that are live at the test. `break` and `breakif` leave the
	innermost `switch`, and a `vpif` is one: inside a `vpif` in a loop name
	the loop, or `goto` a label.

*	**Constants.** A fixed point or float constant is its bits,
	`,(logior 0 +fp_2pi)` or `(logior 0 0.5)`. One of more than 31 bits goes
	in a register first, x64 has no larger immediate.

## Field Helpers & Sorted Memory Transfers (`load-fields`, `save-fields`, `assign-fields`)

Defined in `class/obj/class.inc`, `load-fields`, `save-fields`, and
`assign-fields` are essential helpers for loading, storing, and copying
structured fields between memory objects and VP registers:

*	**`load-fields`:** `(load-fields base fields tmps)`

*	**`save-fields`:** `(save-fields base fields tmps)`

*	**`assign-fields`:** `(assign-fields src src_fields dst dst_fields tmps)`

### Why Sorted Memory Transfers Are Critical

*	**Compile-Time Offset Sorting:** Both `load-fields` and `save-fields`
	evaluate the supplied field expressions at expansion time via
	`(map (const eval) fields)` and sort the transfers by ascending memory
	offset using `(sort ... (# (- (last %0) (last %1))))`.

*	**Cache Locality & Streaming Prefetch:** By accessing memory strictly in
	ascending address order, transfers exhibit optimal spatial locality and
	cooperate with CPU hardware streaming prefetchers.

*	**Arm64 LDP/STP Instruction Fusion:** ChrysaLisp's ARM64 backend
	translator features a peephole optimization pass (the `prepass` routine
	in `lib/trans/arm64.vp`). When consecutive VP memory instructions share the
	same base register and access contiguous aligned offsets (e.g. `c` and
	`c + 8`), the translator fuses two separate 64-bit loads into an ARM64
	`ldp` (Load Pair) or two stores into `stp` (Store Pair). Out-of-order or
	interleaved memory accesses prevent the prepass from fusing them, it only
	pairs an instruction with the next. Sorting guarantees that contiguous
	struct fields end up adjacent in `emit_list`, maximizing `ldp`/`stp` pairing to halve memory
	instruction count.

*	**Layout Independence Across Heterogeneous Structs:** When transferring
	data between different structures (e.g. from a network link fragment to an
	IPC message), `assign-fields` sorts the loads by the source structure's
	layout, and then sorts the stores by the destination structure's layout.
	Both operations independently achieve maximal memory ordering and `ldp`/
	`stp` pairing.

*	**Multi-Field vs Single-Field Transfers:** Field helpers are designed
	specifically for multi-field transfers where sorting offsets and
	achieving hardware instruction pairing (such as ARM64 `ldp`/`stp`)
	provides significant performance advantages. For single-field loads or
	stores, field helpers add unnecessary overhead; use direct
	`(assign `(,reg) `((,base offset)))` or `(assign `((,base offset)) `(,reg))`
	instead.

### Practical Usage Pattern

	;copy frag data
	(vp-rdef (msg rx_frag t0 t1 t2 t3 t4 t5 t6 t7 t8))
	(assign {msg, frag_buf} `(,msg ,rx_frag))
	(assign-fields
		rx_frag
			`(lk_frag_length lk_frag_offset lk_frag_total
			,(+ lk_frag_dest +net_id_mbox_id)
			,(+ lk_frag_dest +net_id_node_id +node_id_node1)
			,(+ lk_frag_dest +net_id_node_id +node_id_node2)
			,(+ lk_frag_src +net_id_mbox_id)
			,(+ lk_frag_src +net_id_node_id +node_id_node1)
			,(+ lk_frag_src +net_id_node_id +node_id_node2))
		msg
			`(+msg_length +msg_offset +msg_total
			,(+ +msg_dest +net_id_mbox_id)
			,(+ +msg_dest +net_id_node_id +node_id_node1)
			,(+ +msg_dest +net_id_node_id +node_id_node2)
			,(+ +msg_src +net_id_mbox_id)
			,(+ +msg_src +net_id_node_id +node_id_node1)
			,(+ +msg_src +net_id_node_id +node_id_node2))
		`(,t0 ,t1 ,t2 ,t3 ,t4 ,t5 ,t6 ,t7 ,t8))

*Note:* `assign-fields` enforces `(if (find dst tmps) (throw ...))` at
compile time to prevent accidental destination register clobbering by
intermediate temporaries.

## Hot Loops

A loop with no call in it runs in registers:

1.	Name the registers: `(vp-rdef (buf mask i))`, or say them, `:r5`.

2.	Load what the loop needs from the frame at once:
	`(load-fields :rsp '(slot_buf slot_mask slot_i) `(,buf ,mask ,i))`.

3.	Hand-code the loop using `loop-start`, `loop-until`, `breakif`, and
	raw instructions (`vp-load-rrr-ub`, `vp-add-cr`, etc.).

4.	Save what it changed: `(assign `(,i) '((:rsp slot_i)))`.

5.	A function that may trash few registers saves `:r7` to `:r14` in slots
	at entry and gives them back at exit, and its loops have them all.

6.	If a call inside a loop trashes registers needed for loop control,
	keep loop counters in registers outside the call's `;trashes` set, or
	advance loop counters *before* the call, or keep them in slots.

## Symbols, Registers and Slots

*	*Raw Symbols* (`(vp-load-rcr adr 80 cnt)`): names given to registers by
	`vp-rdef`, `:r0` - `:r14`.

*	*Slots* (`(:rsp slot_x)`): a field of the frame, as any field of any
	base is.

*	*Quasi-Quoted Lists* (`` `(,adr ,cnt) ``): commas evaluate register
	symbols and constants inside an `assign`.

		(structure slot 0
			(pubyte buf)
			(long pos))

		(vp-rdef (buf pos))
		(vp-alloc slot_size)
		(list-bind-args args `(,buf ,pos) '(:obj :num))
		(save-fields :rsp '(slot_buf slot_pos) `(,buf ,pos))

*	*Arguments* (`(list-bind-args args regs types)`): a native's arguments
	out of their list, each to its register by its type, `:num` `:fixed`
	`:double` the value, `:str` the address of its data, `:char` its first
	byte, any other the pointer. A type of `&` is an argument passed over,
	as in a `bind`, with no register for it: `'(:view & :num :num)`. The
	registers are those that are loaded and no others, so of a method's
	inputs with one that is not an argument, `(rest in)` or `(most in)`;
	there is no `&&`. The last of them is what the list is walked by.

## Calling Conventions & Register Introspection

*	Inspect register contracts dynamically at compile time:

	*	`(method-input :class :method)`: Returns expected input registers.

	*	`(method-output :class :method)`: Returns output registers.

*	**Dynamic Call Mapping:** Pass `(method-input ...)` directly to `vp-rdef`
	to bind argument aliases without hardcoding volatile registers:

	(vp-rdef (cp_src cp_dst cp_len) (method-input :sys_mem :copy_to_ring))

	Assign loop state into these argument aliases immediately before the call.

*	Preserve live state across calls in slots of the frame, or by
	`(vp-push reg)` / `(vp-pop reg)` where there is no frame.

## Systematic VP Lowering Workflow

Follow this 7-step discipline when designing VP routines:

1.	**Draft the Algorithm:** Specify control flow and data transitions.

2.	**Identify Call Boundaries:** Enumerate every sub-call in the logic.

3.	**Inspect Call Contracts & Trashed Sets:** Review `;trashes` and
	`(method-input ...)` for each dependency.

4.	**Evaluate Stack Spill Necessity:** Check if persistent state can fit
	in registers above the trashed set, avoiding stack manipulation.

5.	**Map Variable Lifespans:** Group variables into persistent (must
	survive calls) and ephemeral (local to call intervals).

6.	**Formulate Compact `vp-rdef` Mappings:** Assign persistent state to
	safe registers and reuse volatile low registers across intervals.

7.	**Registers or Slots:** Keep state in registers when it fits
	there; give the function a frame of slots when it must live over
	calls.

## Declared Trashes, And What May Lean On Them

What a function trashes is in the `;trashes` line of its comment, written by
`trace -w` from a build, never by hand. Not in the `(dec-method)`. A call by
the vtable may trash what any class called so does.

*	**Never make a function trash more than it does.** Hand-tuned callers
	lean on what it leaves. To trash less is always safe.

*	**The rare path pays.** A declaration is of the worst path through a
	function, and every caller pays for it on every path. Where the worst
	path is rare, a grow, a refill, a last reference, have that path push
	and pop what it uses beyond the common path. `:obj :destroy`,
	`:array :set_cap`, `:stream :read_char` and `:sys_task :sleep` do. A
	deref trashes `:r0` to `:r2`, a push to an array only what it gives
	back, a read of a char two registers.

*	**`lint` means two things.** No function trashes more than is declared,
	`trace -l`, and no function reads a register after a call declared to
	trash it, `relies`. Both must be clean. `relies -s` is the other way
	round, a register saved round a call that leaves it: a tool, not in
	`lint`.

*	**`trace -w` is one go**, `files obj/vp/ | trace -i -l -w`, then
	`lint`. If it ever says the same thing twice, the class database was
	taken as fresh when it was not: `(database-stale?)` in
	`lib/files/info.inc`.

*	**A call by the vtable** goes through `:r14` unless it is given another.
	Give one only having looked at the trashes comments of the method, of
	every class called so. `relies` says if a register is read after a call
	that went through it.

*	**A change of task keeps nothing** but what the translator needs for
	its stack. `:sys_task :sleep` and `:sys_mail :read` push their caller's
	registers round it. `:suspend` and `:defer` trash all.

## Entry And Exit

`(entry ...)` and `(exit ...)` tie a function to the registers its
`dec-method` declares.

*	Every method has an entry, and an exit at each return, `'()` where it
	takes or gives nothing. One that ends by a `jump` needs no exit.

*	A callback inside a function uses the callback's own declaration,
	`:sys_task :task_callback`, `:hmap :each_callback`, `:array
	:sort_callback`. A local subroutine reached by `(call 'label ...)` uses
	the one argument form, `(entry '(...))`, `(exit '(...))`, mapping `:r0`,
	`:r1` in order. One reached by a raw `(vp-call 'x)`, or working on the
	registers of the function round it, is private and needs neither.

*	**An entry takes an input only to where the code after it reads it.**
	Never have the entry move an input to a slot or another register and
	then go on reading the declared register: change the declaration and
	that breaks, where an honest entry would emit one more copy. If both
	the register and a saved copy are wanted, the entry names the register
	and the copy is a plain `assign` after it. A register passed as it is to
	an input the callee does not read is right.

*	The argument checks, `gotoif-lisp-args-len`, `-sig`, `-match`, `-type`,
	take a register or a `vp-rdef` name, the one the args are in when the
	check runs: check before the args are moved, with the entry
	`'(:r0 :r1)`, and move them after.

*	`-sig`, `-match` and `-type` leave their answer in the name `tmp`,
	which they bind to the register it comes back in, `:r2`, whatever
	`vp-rdef` gave `tmp`: `(call ... '(& tmp))`. A value kept in `tmp`
	after one is lost at the next call that loads `:r2` with an argument,
	and a node dies as it starts with nothing said. Keep it in a name of
	its own. Found by reading `obj/vp/class/obj/lisp.vp`, the VP the
	function was made into, beside the last good one.

*	A change to a core native is built and tried in a clone of the tree,
	and a boot image that will not start is mended on the emulator, `echo
	"make all boot" | ./run_tui.sh -e -n 4 -f`, which remakes this
	machine's native code alone: the emulator's `(cpu)` and `(abi)` are
	the host's.

*	An entry or exit change is proved by the binary: build the old and new
	trees each with `make all platforms boot` and `diff -rq` every
	`obj/<cpu>`; only `sys/boot_id`, a hash of the VP source, may differ.

## Push, Pop And Alloc

Pushes and allocs lie end to end, whole words, nothing before, after or
between, as `docs/vm/vp_vm.md` says under Push, Pop.

*	A push lays its registers as if pushed in turn, a word each: after
	`(vp-push :r0 :r1 :r2)`, `:r2` is at `(:rsp 0)`.

*	So anything may be taken back in any grouping: two pushes by one pop,
	a push by a free, an alloc by a pop, nested scopes by one free. Words
	put there may be read and written by offset.

*	What must hold: never take back more than was put on, leave the stack
	as found, allocs whole words. `trace` checks the first and last along
	each path, and `lint` prints them.

## `sys/math` And Its Contract

`sys/math` holds the unboxed functions of a number, `q_` fixed, `d_`
double, `f_` single, a value in a register in and a value out. The
classes of numbers and the shader compiler call them directly.

*	Every one of them trashes no more than `:r0` to `:r2` and `:d0` to
	`:d3` (`:f0` to `:f3` the same registers), `+sys_math_trashes` in
	`sys/math/trashes.inc`, which the shader compiler spills by and `lint`
	checks. `i_rand`, `q_intersect` and `q_dist_sqd` are outside it,
	`+sys_math_free`.

*	So a caller keeps what it wants in `:r3` up and `:d4` up across them,
	and a new one fits the set, saving what else it uses itself. This is
	that family's rule, not the tree's: other VP reads each callee's
	`;trashes`.

## Loops Over Things That Are Let Go Of

*	Not `(loop-while)`: `(vpif '(a /= b)) (loop-start) ...
	(loop-until '(a = b)) (endif)`. A test at the top of every turn is
	hard on branch predictors.

*	Hold the place in registers above `:r2`, a deref leaves them. Do not
	store it to the stack before the call and read it back after: that
	shape inside a called function cost 5 ns a call on the M4, and not on
	x64 or the Pi. If the function may not trash those registers, push
	them once at entry and pop at exit.

*	`(class/obj/deref-live obj tmp 'label)` lets go of a thing that lives
	on in line, no call, and goes to the label for the last reference,
	where the code calls `:obj :deref` and jumps back. `:list :clear` and
	the iterators do.

*	Never put code after a function's `(signature ...)`. It is a table of
	shorts and what follows can be two bytes off: fine on x64, a fault on
	arm64. A side path goes before the error labels.

*	**A side path frees what it made.** A side path moved in front of the
	error labels frees the frame itself if it returns, and the error code
	that follows frees it before its jump. A callback run on the host's
	stack, `:sys_task :callback`, is written in plain VP with the `slot`
	structure of its caller, as the natives of `service/audio/lisp.vp` and
	`:lisp_shade` in `gui/canvas/lisp.vp` are:

		(vp-label 'callback)
			;inputs
			;:r0 = user data (ptr)

			(entry :sys_task :task_callback '(:r0))
			(vp-push :r0)
			(host-gui-call :host_gui :video_close '((:r0 slot_handle)))
			(vp-pop :r1)
			(vp-ret)

		(vp-label 'error)
			(jump :lisp :repl_error ...)
			(signature ...)

		(def-func-end)

## The Shape Of Structured Code

The forms are `lib/asm/code.inc`, each with an optional loop name symbol
last.

*	`vpif`, `elseif`, `loop-while`: all of several conditions hold.
	`vpifnot`, `elseifnot`, `loop-whilenot`: none holds. `breakif`,
	`continueif`: any holds; `breakifnot`, `continueifnot`: any fails.
	`loop-until`: ends when all hold. `gotoif` and `gotoifnot` take one.

*	Of reals, `:d` or `:f` registers, a test that fails is not the opposite
	test holding: a NaN is neither `<` nor `>=`. So `gotoifnot` of reals,
	and every form above that goes by it, is the test's own branch past a
	jump to its label, one more instruction, and a NaN goes as a test that
	fails, as a GPU has it. Of whole numbers it is the opposite branch, as
	it always was.

*	Data in a function, a `(vp-cstr)` string, goes after all its code, at its
	end before `(def-func-end)`. In the middle, a string of a length that is
	not a whole number of instructions leaves the code after it out of line:
	arm64 then runs the tail of the string as an instruction, a SIGILL at
	boot, where x86 and the emulator, which do not care, run it fine.

*	A `vpif` is a switch of its own: a `break`, `breakif` or `continue`
	inside one inside a loop goes to the end of the `vpif`. Name the loop,
	`(loop-start 'name)`, and `(break 'name)`, `(breakif c 'name)`,
	`(continue 'name)`.

*	A loop guarded by a `vpif` sits one level inside the `vpif`. A `vpif`
	whose body is only a jump is a `gotoif`. A guard whose fall through
	lands on a jump or its label is a `gotoifnot`, never a jump to a jump.
	Never nest two `vpif`s with no `else` between them.

*	A loop that stops on either of two things is `(breakif a)` then
	`(loop-until b)` at its foot. `breakif` with `(loop-end)` costs a jump
	more each time round.

*	Negate a register by `(vp-mul-cr -1 r)`; the emitter makes it a
	negate.

*	A missing or extra bracket shows as an odd error at the end of a form,
	`symbol_not_bound *switch*` when an `(endif)` falls to top level: run
	`brackets <file>` first.

## A Slot Is Memory, And No Passes

No pass keeps the slots of a frame in registers: what is written is what
is assembled, and `lib/asm/vpopt.inc` is the only optimiser. A value
wanted more than once between calls is loaded once to a register and used
from there, not read from its slot again. What an `(entry)` gives in a
register is used from the register, and saved to its slot only if it is
wanted after a call.

*	`assign` does one thing beyond emitting, a topology sort against
	hazards. It does not put loads and stores in memory order;
	`load-fields` and `save-fields` do, before it. Add no analysis or
	reordering to `assign`, `call`, `entry`, `exit` or a function's end:
	better code is a change to the `.vp` source.

*	Order the source by hand where it gains: reads and writes forward
	through memory, neighbours together so the arm64 prepass (`pp_pair`
	in `lib/trans/arm64.vp`) pairs them, and the slots of a frame laid in
	the order a call or entry takes them. Not for order alone: count the
	pairs before and after, and add no lines that gain none.

*	A change to VP source not yet known to be right, a script's above all,
	is built in a clone (`git clone`, copy `obj/` in), never the live tree:
	`make` writes what it compiles to `obj/`, and wrong natives there stop
	every session. Only sources whose VP comes out the same, or was read
	and understood, come back.

## Every Load And Store Naturally Aligned

The VP's rule, the user's, 2026-11-22 and again 2026-11-23: a load or a
store of 2, 4 or 8 bytes is at an address of 2, 4 or 8. Of a file's, a
packet's or a caller's bytes at any place, it is bytes one at a time, or
a copy to the stack first (`sys/crypto/aead_open`), or bytes up to a
place of 8 and then 8 at a time (`gui/canvas/fpoly`). A table of the code
of a function is put on its place by `(vp-align +int_size)` before its
label.

*	Why, as the CPUs are: x86-64 never faults; arm64 does not for an
	ordinary load or store of user code, only for its atomics. A RISC-V
	core may fault, the ISA lets it, and on Linux firmware or the kernel
	then does it in software, hundreds of times slower; a LoongArch core
	without it in hardware raises an address error, that the kernel
	does the same for. A misaligned atomic fails on all of them. So
	broken, it is slow on real RISC-V, not wrong, and on the M4 and the
	x64 nothing shows.

*	Code is placed too. No translator moves an instruction that comes
	after data, a table or the strings of a function: it is where the
	data's bytes leave it, and off a place of 4 an arm64, RISC-V or
	LoongArch instruction faults as it runs, a VP64 one off 2 breaks the
	rule. Put the data after the function's last instruction, or `(vp-align
	+int_size)` before the code. `lib/xml/parse` had its error path after 43
	bytes of strings: a wrong argument took an M4 node down, `Illegal
	instruction`, and every VP64 node that translated it said `VP64
	UNALIGNED lib/trans/vp64 +858`, 2026-11-23.
	`tests/system/test_code_place.lisp` looks at every function of
	`obj/vp/` for it.

*	Nothing of ours shows a misaligned load or store but `./run_tui.sh
	-V`, the VP64 emulator validating: QEMU's user mode, as the RISC-V and
	LoongArch suites run, does one without a word, and so does the
	emulator without `-V`. A misplaced instruction QEMU does catch. Prove a change both ways, the dev-cycle skill's point
	6: the code as it was says `VP64 UNALIGNED` with the place, as it is
	says nothing.

## Boot Image Functions Run From The Image

The loader, the statics, the kernel and all they call are in the boot
image, `lib/boot/image.inc`, and a node runs those from it, not from their
files. A change to one is not tested till `make` and `make all boot` have
made the image again: a test of it before that tests the old code. The
library natives an image holds are the same, `lib/streams/lisp.vp`,
`lib/image/lisp.vp` and the like: `(ffi ...)` takes the image's copy, so an
edited one is tested only after `make all boot`; a new one runs from its
file at once.

## A New Native And Its `ffi`

*	**Build, then bind.** A new native bound by `(ffi ...)` in
	`class/lisp/root.inc`, or in anything a node reads as it boots: add the
	function and its declarations, build and boot, and only then add the
	`ffi` line and build again. An `ffi` of a function with no native yet
	throws at boot, every node stops, the builder too. If a build is stuck
	on one, take the line out, let the node's timeout end it, build, put it
	back.

*	**Kept natives are not stamped.** The loader translates a function
	that has no native and keeps the result at `obj/<cpu>/<abi>/<path>`,
	loaded as found by the next node. `make` retranslates when the
	translator sources are newer, but only for the platform it builds.
	After an op or translator change, `make all platforms boot` and `make
	apps`. An emulator crash with no message at the same address every
	time, in one tree and not a clone: suspect what `obj/` holds first.

## Validate Functions Mix

A validate function and a normal one must run together, so `make
validate` with no `boot` gives a system that works.

*	A `(validatecase)` check saves what it uses and changes no structure
	or layout another function reads. If checks need state shared between
	functions, those functions all go in the boot image, as
	`:sys_heap :free` and its guard words do.

*	`(validatecase)` goes round checks, never round a `def-method` or
	`def-func`: no function is made by one mode alone.

*	A validate make writes validate natives over the normal ones. Make one
	in the live tree only for a validate system, with `boot`; try build
	options in a clone.

## VP Ops, The Text Format And The Translators

*	**An op's place never moves.** An op's number is its place in
	`+vp_emit_ops`, `lib/asm/ops.inc`. One make builds the new translators
	with the old, whose dispatch tables are baked in: move an op the
	translators use and they come out garbage. Rename an op in its slot,
	reuse a retired slot, or append before `emit-vp-nop`. A slot may change
	meaning only for ops no translator's code uses: check with
	`grep -o "emit-[a-z-]*" obj/vp/lib/trans/*.vp`. A pure rename builds
	every object byte identical but `sys/boot_id`.

*	**VP64's opcodes** are one list, `VP64_OPS` in `src/host/vp64.cpp`, and
	`vp64_ops` in `lib/trans/vp64.vp` follows it: renumber both together,
	with a new host and snapshot, then `make all boot platforms` and `make
	apps`.

*	**The VP text is the format**, what tools `read`, what @Source sends and
	what the loader translates: a raw list of bare op names, `(add-cr 1
	:r0)`, no `progn`, no `emit-`. `lib/trans/vp_parse.vp` parses it as
	bytes, strictly, into the translators' records, no Lisp in the load,
	translate and bind path. Size does not matter.

*	**A build uses the translator it has just built**, so a broken one
	breaks the build at once, and with `boot` the image: recover by `make
	install`. A build that fails to translate one function still ends
	"Done" and keeps that function's old native: grep a make's output for
	"Error", or run `lint`, never judge it by the last line.

## Two Traps In The Lisp Of The Assembler

*	`:nil` is a symbol, and a symbol is a sequence: `(first :nil)` is ":"
	and `(third :nil)` is "i", both true. Test a thing is there before
	taking a part of it.

*	`(# ...)` with no `%0` in it takes no arguments. Given to `each`, `some`
	or `sort` it fails. Use `(lambda (x) ...)`.


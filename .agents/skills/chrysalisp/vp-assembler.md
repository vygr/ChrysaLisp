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

*	**A frame is a structure.** vpx, `def-vars`, `push-scope`,
	`pop-scope` and `return` are gone (2026-11-08). A function's frame is
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

## Declared Trashes, And What May Lean On Them (2026-10-13)

What a method trashes is in its declaration, `(dec-method name function type
in out trashes own)` in the `class.inc`, and in the `;trashes` line of the
function. `trashes` is what a call may trash, of a virtual method what any
class called so may. `own` is what the one function does. Both are written
by `trace -w` from a build, never by hand. `(method-trashes class
method)` gives the registers.

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
	`lint`. It needed two
	till 2026-10-13: the database of classes was taken to be fresh when a
	source had been written in the second it was made in, which is when
	`trace -w` writes them. If it ever says the same thing twice, look
	there, `(database-stale?)` in `lib/files/info.inc`.

*	**A call by the vtable** goes through a register the method trashes
	anyway and does not take, `(dispatch-reg)`, not `:r14`. Give one at the
	call only for a reason.

*	**A change of task keeps nothing** but what the translator needs for
	its stack. `:sys_task :sleep` and `:sys_mail :read` push their caller's
	registers round it. `:suspend` and `:defer` trash all.

## Loops Over Things That Are Let Go Of

*	Not `(loop-while)` in a hot loop: `(vpif '(a /= b)) (loop-start) ...
	(loop-until '(a = b)) (endif)`. Chris: "loop-whiles are genrally bad...
	give branch predicators a hard time."

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

## Slots Kept In Registers

Every function with a frame, `(vp-alloc slot_size)` of a `(structure slot
...)`, has its slots kept in registers where they can be, when it has all
been assembled: `lib/asm/slots.inc`, that was `vpxreg.inc`. A slot whose
address is taken, or that is a struct, or is read or written as less than
a long, stays in memory. The allocator is for what was C-Script, not to be
taken further unasked.

## Boot Image Functions Run From The Image

The loader, the statics, the kernel and all they call are in the boot
image, `lib/boot/image.inc`, and a node runs those from it, not from their
files. A change to one is not tested till `make` and `make all boot` have
made the image again: a test of it before that tests the old code.

## Two Traps In The Lisp Of The Assembler

*	`:nil` is a symbol, and a symbol is a sequence: `(first :nil)` is ":"
	and `(third :nil)` is "i", both true. Test a thing is there before
	taking a part of it.

*	`(# ...)` with no `%0` in it takes no arguments. Given to `each`, `some`
	or `sort` it fails. Use `(lambda (x) ...)`.


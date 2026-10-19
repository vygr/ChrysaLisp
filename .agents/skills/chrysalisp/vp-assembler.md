# ChrysaLisp VP Assembler & vpx Guide

Part of the `chrysalisp` skill. Read this in full before writing or changing a
`.vp` file.

## Contents

*	[Register Architecture](#register-architecture)

*	[VP Method Definition Boilerplate](#vp-method-definition-boilerplate)

*	[Generated Function Headers (`gen-create`, `gen-type`)](#generated-function-headers-gen-create-gen-type)

*	[The `assign` Macro & vpx Memory Rules](#the-assign-macro--vpx-memory-rules)

*	[Field Helpers & Sorted Memory Transfers (`load-fields`, `save-fields`, `assign-fields`)](#field-helpers--sorted-memory-transfers-load-fields-save-fields-assign-fields)

*	[Raw VP Loop Optimization (Bypassing vpx in Hot Paths)](#raw-vp-loop-optimization-bypassing-vpx-in-hot-paths)

*	[Mixed VP Assembler & vpx Syntax Duality](#mixed-vp-assembler--vpx-syntax-duality)

*	[Calling Conventions & Register Introspection](#calling-conventions--register-introspection)

*	[Systematic VP Lowering Workflow](#systematic-vp-lowering-workflow)

When writing or modifying `.vp` files, you target ChrysaLisp's register and
execution model.

## Register Architecture

*	15 General-Purpose registers: `:r0` through `:r14`.

*	1 Dedicated Stack Pointer: `:rsp`.

*	16 Floating-Point registers: `:f0` through `:f15`.

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
	;:r1-:r5, :f0-:f15

The doc scanner (`lib/files/info.inc`), `make docs` and `trace` all read
it, and `trace -i -l -w` keeps the `;trashes` line correct. When adding a
class, add these headers too, with `;none` as the trashes, and let
`trace -w` fill it in. Leave the `;inputs` section out of a `gen-create`
header when the class's `:vcreate` is declared with different inputs to its
`:create`, as both map to the one function and the scanner checks each.

## The `assign` Macro & vpx Memory Rules

`(assign ...)` evaluates expressions, moves data, and loads/stores memory
simultaneously (e.g. `(assign '((:r0 +str_length)) '(:r1))`):

*	**Typed Declarations:** Define stack variables with `(def-vars ...)`.
	Supported types include `ptr`, `pubyte`, `long`, `uint`, etc.
	Dereferencing `(vpx (* src_ptr))` generates appropriate byte or word transfers
	based on variable type.

*	**vpx Scope Discipline (CRITICAL):**

	*	Open scope with `(push-scope)` at function start.

	*	Place `(pop-scope-syms)` at the end of the `def-func` block (after
		`errorcase` and `signature` sections) to cleanly purge compiler
		tracking.

*	**vpx Stack Packing & Unions:**

	*	Order variables in `def-vars` from largest (`long`, `ptr`, `netid`)
		to smallest (`uint`, `ubyte`) to prevent alignment padding waste.

	*	Use `(union ...)` to share stack space between variables with
		mutually exclusive lifetimes. Do not create redundant variables
		merely to avoid type casts.

*	**Assignment Fusion & Hazards:**

	*	Fuse assignments to minimize temporaries: `(assign {a, b} {x, y})`.

	*	**Evaluation Order:** Sources compile left-to-right (pushed to
		value stack); destinations compile right-to-left (popped from value
		stack).

	*	**Mutation Hazard:** Never mutate a variable in a fused assignment
		if that variable calculates the address of a destination further to
		the left (e.g. `(assign {val, p + 1} {p[0], p})` is invalid). Split
		such updates into sequential `assign` statements.

	*	**No Memory-to-Memory or Constant-to-Memory:** In VP assembler,
		`(assign ...)` does not support memory-to-memory transfers
		(e.g. `(assign `((,r0 off1)) `((,r1 off2)))`) or constant-to-memory
		transfers (e.g. `(assign '(10) `((,r1 off)))`). All memory reads and
		writes must route through an intermediate register (e.g. load memory
		to register, then store register to memory).

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
	translator features a peephole optimization pass (`emit-prepass` in
	`lib/trans/arm64.inc`). When consecutive VP memory instructions share the
	same base register and access contiguous aligned offsets (e.g. `c` and
	`c + 8`), the translator fuses two separate 64-bit loads into an ARM64
	`ldp` (Load Pair) or two stores into `stp` (Store Pair). Out-of-order or
	interleaved memory accesses prevent the translator's lookback window
	from fusing them. Sorting guarantees that contiguous struct fields end up
	adjacent in `emit_list`, maximizing `ldp`/`stp` pairing to halve memory
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

## Raw VP Loop Optimization (Bypassing vpx in Hot Paths)

When vpx's value stack overhead is unacceptable in inner loops, drop to
pure VP assembler:

1.	Declare raw registers: `(vp-rdef (r_buf r_mask r_i))`.

2.	Extract vpx variables into registers with mixed `assign`:
	`(assign (vpx buf mask ptr_i) `(,r_buf ,r_mask ,r_i))`.

3.	Hand-code the loop using `loop-start`, `loop-until`, `breakif`, and
	raw instructions (`vp-cpy-dr-ub`, `vp-add-cr`, etc.).

4.	Restore results to vpx variables: `(assign `(,r_i) (vpx ptr_i))`.

5.	If a call inside a loop trashes registers needed for loop control,
	keep loop counters in registers outside the call's `;trashes` set, or
	advance loop counters *before* the call, or spill persistent registers
	(`this`, `args`) to stack slots.

## Mixed VP Assembler & vpx Syntax Duality

*	**Forms, not strings:** A vpx expression is a form inside `(vpx ...)`,
	`(vpx (+ a b))`, and `(vpx a b)` is a list of them. Before 2026-10-12
	it was a string in curly braces, `{a + b}`. That is gone.

*	**Three Symbol Contexts:**

	*	*Raw Symbols* (`(vp-cpy-ir adr 80 cnt)`): Resolve directly to VP
		registers (`:r0` - `:r14`).

	*	*vpx Forms* (`(vpx adr cnt)`): Resolve to stack frame offsets
		(`[rsp + offset]`).

	*	*Quasi-Quoted Lists* (`` `(,adr ,cnt) ``): Commas explicitly evaluate
		register symbols inside macros.

*	**Bridging Pattern:**

	; 1. Define vpx stack variables
	(def-vars
		(pubyte buf)
		(long pos))

	; 2. Define VP registers with matching names
	(vp-rdef (buf pos))

	; 3. Extract inputs into VP registers
	(list-bind-args args `(,buf ,pos) '(:obj :num))

	; 4. Map VP registers to vpx variables
	(assign `(,buf ,pos) (vpx buf pos))

## Calling Conventions & Register Introspection

*	Inspect register contracts dynamically at compile time:

	*	`(method-input :class :method)`: Returns expected input registers.

	*	`(method-output :class :method)`: Returns output registers.

*	**Dynamic Call Mapping:** Pass `(method-input ...)` directly to `vp-rdef`
	to bind argument aliases without hardcoding volatile registers:

	(vp-rdef (cp_src cp_dst cp_len) (method-input :sys_mem :copy_to_ring))

	Assign loop state into these argument aliases immediately before the call.

*	Preserve live state across calls via explicit `(vp-push reg)` /
	`(vp-pop reg)` or by saving to vpx stack slots.

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

7.	**vpx vs. Pure VP Selection:** Use pure VP when state fits
	entirely in registers; use vpx when named stack slots are required.

## Declared Trashes, And What May Lean On Them (2026-10-13)

What a method trashes is in its declaration, `(dec-method name function type
in out trashes own)` in the `class.inc`, and in the `;trashes` line of the
function. `trashes` is what a call may trash, of a virtual method what any
class called so may. `own` is what the one function does. Both are written
by `trace -w` from a debug build, never by hand. `(method-trashes class
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

*	**`trace -w` is one go**, on a debug build, then `lint`. It needed two
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
	arm64. A side path goes before the `(errorcase` that holds the error
	labels.

*	**The scope stays till after the errorcase.** A side path moved in
	front of the errorcase does not take the function's `(pop-scope-syms)`
	with it: the error code that follows still wants the scope. Never pop
	the scope early to make room for a side path, and never give a side
	path a `(push-scope)` of its own inside the function's. A callback run
	on the host's stack, `:sys_task :callback`, is written in plain VP with
	the `local` structure of its caller, as the natives of
	`service/audio/lisp.vp` and `:lisp_shade` in `gui/canvas/lisp.vp` are:

		(vp-label 'callback)
			;inputs
			;:r0 = user data (ptr)

			(entry :sys_task :task_callback '(:r0))
			(vp-cpy-rr :rsp :r1)
			(vp-alloc 16)
			(vp-and-cr -16 :rsp)
			(assign '(:r0 :r1) '((:rsp 0) (:rsp 8)))
			(host-gui-call :host_gui :video_close '((:r0 local_handle)))
			(assign '((:rsp 8)) '(:rsp))
			(vp-ret)

		(errorcase
		(vp-label 'error)
			(jump :lisp :repl_error ...)
			(signature ...))

		(def-func-end)

## Named Locals

Every function with named locals, `(def-vars)` and `(push-scope)`, has them
kept in registers where they can be, when it has all been assembled:
`lib/asm/vpxreg.inc`. `(vpx-stack)` before the `(push-scope)` keeps them on
the stack as written, none does. A local whose address is taken, or that is
a struct, stays in memory. The allocator is for what was C-Script, not to be
taken further unasked.

## Two Traps In The Lisp Of The Assembler

*	`:nil` is a symbol, and a symbol is a sequence: `(first :nil)` is ":"
	and `(third :nil)` is "i", both true. Test a thing is there before
	taking a part of it.

*	`(# ...)` with no `%0` in it takes no arguments. Given to `each`, `some`
	or `sort` it fails. Use `(lambda (x) ...)`.


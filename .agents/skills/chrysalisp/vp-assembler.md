# ChrysaLisp VP Assembler & CScript Guide

Part of the `chrysalisp` skill. Read this in full before writing or changing a
`.vp` file.

## Contents

*	[Register Architecture](#register-architecture)

*	[VP Method Definition Boilerplate](#vp-method-definition-boilerplate)

*	[Generated Function Headers (`gen-create`, `gen-type`)](#generated-function-headers-gen-create-gen-type)

*	[The `assign` Macro & CScript Memory Rules](#the-assign-macro--cscript-memory-rules)

*	[Field Helpers & Sorted Memory Transfers (`load-fields`, `save-fields`, `assign-fields`)](#field-helpers--sorted-memory-transfers-load-fields-save-fields-assign-fields)

*	[Raw VP Loop Optimization (Bypassing CScript in Hot Paths)](#raw-vp-loop-optimization-bypassing-cscript-in-hot-paths)

*	[Mixed VP Assembler & CScript Syntax Duality](#mixed-vp-assembler--cscript-syntax-duality)

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

## The `assign` Macro & CScript Memory Rules

`(assign ...)` evaluates expressions, moves data, and loads/stores memory
simultaneously (e.g. `(assign '((:r0 +str_length)) '(:r1))`):

*	**Typed Declarations:** Define stack variables with `(def-vars ...)`.
	Supported types include `ptr`, `pubyte`, `long`, `uint`, etc.
	Dereferencing `{*src_ptr}` generates appropriate byte or word transfers
	based on variable type.

*	**CScript Scope Discipline (CRITICAL):**

	*	Open scope with `(push-scope)` at function start.

	*	Place `(pop-scope-syms)` at the end of the `def-func` block (after
		`errorcase` and `signature` sections) to cleanly purge compiler
		tracking.

*	**CScript Stack Packing & Unions:**

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

## Raw VP Loop Optimization (Bypassing CScript in Hot Paths)

When CScript's value stack overhead is unacceptable in inner loops, drop to
pure VP assembler:

1.	Declare raw registers: `(vp-rdef (r_buf r_mask r_i))`.

2.	Extract CScript variables into registers with mixed `assign`:
	`(assign {buf, mask, ptr_i} `(,r_buf ,r_mask ,r_i))`.

3.	Hand-code the loop using `loop-start`, `loop-until`, `breakif`, and
	raw instructions (`vp-cpy-dr-ub`, `vp-add-cr`, etc.).

4.	Restore results to CScript variables: `(assign `(,r_i) {ptr_i})`.

5.	If a call inside a loop trashes registers needed for loop control,
	keep loop counters in registers outside the call's `;trashes` set, or
	advance loop counters *before* the call, or spill persistent registers
	(`this`, `args`) to stack slots.

## Mixed VP Assembler & CScript Syntax Duality

*	**String Literal Equivalence:** Curly braces `{...}` and double quotes
	`"..."` are identical string constructors in ChrysaLisp. `{...}` is
	favored for CScript code to allow embedded quotes (`\q`) without escapes.

*	**Three Symbol Contexts:**

	*	*Raw Symbols* (`(vp-cpy-ir adr 80 cnt)`): Resolve directly to VP
		registers (`:r0` - `:r14`).

	*	*CScript Strings* (`{adr, cnt}`): Resolve to stack frame offsets
		(`[rsp + offset]`).

	*	*Quasi-Quoted Lists* (`` `(,adr ,cnt) ``): Commas explicitly evaluate
		register symbols inside macros.

*	**Bridging Pattern:**

	; 1. Define CScript stack variables
	(def-vars
		(pubyte buf)
		(long pos))

	; 2. Define VP registers with matching names
	(vp-rdef (buf pos))

	; 3. Extract inputs into VP registers
	(list-bind-args args `(,buf ,pos) '(:obj :num))

	; 4. Map VP registers to CScript variables
	(assign `(,buf ,pos) {buf, pos})

## Calling Conventions & Register Introspection

*	Inspect register contracts dynamically at compile time:

	*	`(method-input :class :method)`: Returns expected input registers.

	*	`(method-output :class :method)`: Returns output registers.

*	**Dynamic Call Mapping:** Pass `(method-input ...)` directly to `vp-rdef`
	to bind argument aliases without hardcoding volatile registers:

	(vp-rdef (cp_src cp_dst cp_len) (method-input :sys_mem :copy_to_ring))

	Assign loop state into these argument aliases immediately before the call.

*	Preserve live state across calls via explicit `(vp-push reg)` /
	`(vp-pop reg)` or by saving to CScript stack slots.

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

7.	**CScript vs. Pure VP Selection:** Use pure VP when state fits
	entirely in registers; use CScript when named stack slots are required.

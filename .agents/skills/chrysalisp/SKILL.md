---
name: chrysalisp
display-name: ChrysaLisp
description: Use when writing, reviewing, or modifying ChrysaLisp code (Lisp .lisp/.inc, VP assembler .vp, CScript). Covers idioms, primitives, and architecture.
---

# ChrysaLisp Coding Skill

ChrysaLisp is a unique, LISP-like, distributed, message-passing, parallel
processing MIMD OS and language. It features a portable Virtual Processor (VP)
architecture, its own build tools, native translators, a hyper-fast Lisp
interpreter, and extensive class libraries at both the VP assembler and Lisp
levels.

The system is a direct evolution of its bare-metal predecessor, Taos OS.
Its features and syntax emerged from first-principles engineering focused on
creating a high-performance, distributed system; convergence with Lisp was a
discovery, not a design goal. It fundamentally rejects traditional Lisp
implementation details (such as cons cells and tracing garbage collection) in
favor of vector primitives that map directly to high-performance hardware.
ChrysaLisp treats the host OS as a set of drivers via its Platform
Implementation Interface (PII), rather than acting as a runtime dependent on
one.

## Documentation Index & AI Digest

For comprehensive architectural guides, design rationale, and system
deep-dives, consult `LLM.md` in the workspace root. `LLM.md` is the master
index and reading guide for all 70 technical documents in `docs/ai_digest/`,
organized by category:

*	**Core Architecture:** Genesis, philosophies, memory model, and object
	hierarchies (`0–8`).

*	**Virtual Processor (VP):** Translation, classes, functions, SIMD, and
	emulator (`9–13`).

*	**Lisp Language & Primitives:** Modern Lisp, Four Horsemen sequence
	transformations, flow-through, and built-ins (`14–20`).

*	**Advanced Lisp:** Dual vtables, closure-less design, O(1) calling,
	modules, REPL/JIT, and CScript compiler (`21–28`).

*	**Data Types & Streams:** Numerics, text parsing, regexps, streams, pipes,
	and slicing (`29–36`).

*	**GUI Framework:** Views, widgets, compositor, vector graphics, and text
	stack (`37–44`).

*	**Distributed OS & Networking:** Fault tolerance, dynamic code, IPC, task
	farming, and streaming pipelines (`45–50`, `68–69`).

## Core Architectural Philosophies

### Philosophy 1: "Well, Don't Do That Then!"

ChrysaLisp pragmatically avoids common systems programming problems rather
than engineering complex machinery to manage them:

*	**Concurrency Without Race Conditions:** Sidesteps shared-memory race
	conditions by running completely isolated tasks communicating via
	message passing rather than shared-memory threads.

*	**Performance Without GC Pauses:** Eliminates garbage collection pauses
	entirely by using reference counting and a memory model built strictly
	on vector primitives instead of traditional `cons` cells.

*	**Security Without Complex Memory Protection:** Avoids self-modifying
	native code and complex W^X policies. The native code engine is
	immutable and ROMable. Dynamic optimizations and runtime changes occur
	by patching Lisp data structures (the "script") in RAM, which the engine
	executes.

*	**Radical Simplicity and Speed:** The entire boot image for a RISC CPU
	is around 200 KB (fitting inside L1 cache), and a full OS rebuild
	completes in under 0.1 seconds on a modern laptop.

### Philosophy 2: "Be Formless, Shapeless, Like Water"

The system is engineered for fluid adaptability and distributed scalability:

*	**Formless Network:** The network is an emergent entity defined solely
	by active nodes and links. Communication is completely location-
	transparent: `(mail-send)` behaves identically whether the target task
	is on the same core, a separate core, or a remote machine.

*	**Dual-Mode Task Placement:**

	*	**Application-Directed Placement:** Programs query `(lisp-nodes)`
		and use libraries like `lib/task/farm.inc` to explicitly distribute
		workloads (e.g. random or round-robin placement).

	*	**Kernel-Assisted Emergent Placement:** When spawning a task with
		`+kn_call_child`, the kernel initiates a decentralized load-
		balancing search. It compares its local `task_count` against
		immediate network neighbors. If a neighbor is less loaded, the
		entire spawn request flows "downhill" to that node without
		spawning locally. This cascades across hops until settling in a
		local minimum ("valley"), where the task finally spawns.

### Philosophy 3: "Know Thyself" — Cooperative Internals

Internal primitives are designed with intimate awareness of the cooperative
execution model:

*	**Cooperative Tasks and Small Stacks:** Tasks are cooperatively
	scheduled and non-preemptible, yielding only at explicit points (e.g.
	`task-sleep`, `mail-read`, `task-slice`). This guarantee allows tasks to
	operate safely with small, fixed stacks (~8 KB), minimizing footprint
	and enabling high concurrency.

*	**The Iterative Idiom:** Deep recursion on machine stacks is strictly
	prohibited by the small stack size. ChrysaLisp pervasively uses
	**iteration with an explicit heap-allocated `list` as a work stack**
	(used in `lisp :read`, `host_gui :composite`, etc.).

*	**Synergy with O(1) Cache Performance:** Flatter iterative lexical
	scopes keep symbol bindings stable, preventing the churn of deep
	scope chains and maximizing cache hits in the symbol engine.

*	**Dual HMap Tree Duality:** Every object instance is fundamentally an
	`hmap` participating simultaneously in two hierarchies:

	*	The dynamic **Containment Hierarchy** traversed at runtime via the
		`:parent` key for inherited appearance attributes (such as
		`:color` or `:font`).

	*	The static **Class Hierarchy** resolved via the `:vtable` key
		pointing to a compile-time-composed, flattened `hmap` of method
		pointers. Method dispatch (`(. obj :method)`) is an immediate
		two-step O(1) lookup without any runtime inheritance traversal.

*	**Lock-Free, State-Aware Algorithms:** Non-preemption permits safe,
	lock-free data updates:

	*	*Atomic Swap Pattern:* Shared caches (such as `font :flush`)
		prepare changes on a private copy and commit via an atomic pointer
		swap across non-yielding sequences.

	*	*Robust Iterator Pattern:* Iterators like `hmap :each` support
		in-place deletion of the current item via swap-and-pop `erase`,
		intelligently resynchronizing state after callback execution.

### The Unambiguous, Ephemeral `netid`

Network identity is based on the tuple `(mailbox_id, node_id)`:

*	**`node_id` (Ephemeral Node Identity):** Generated randomly each time a
	node boots. If a node restarts, its `node_id` changes, preventing
	messages meant for a previous incarnation from ever delivering to the
	new one.

*	**`mailbox_id` (Disposable Mailbox Identity):** A 64-bit monotonically
	increasing counter allocated via `(mail-alloc-mbox)`. **Mailbox IDs are
	never reused.** When `(mail-free-mbox)` is invoked, the ID is permanently
	invalidated; `mail:validate` drops subsequent messages addressed to it.

*	**Practical Pattern:** For any distinct conversation, transaction, or
	request-response cycle, allocate a fresh mailbox. Freeing the mailbox
	instantly drops late-arriving responses, eliminating stale messages,
	sequence-number tracking, and zombie tasks.

## The Lisp Implementation & Symbol Engine

ChrysaLisp's interpreter is self-hosted (`class/lisp/`, root environment in
`class/lisp/root.inc`) and built for maximum throughput:

*	**Vector-Based Architecture:** The system entirely dispenses with cons
	cells, `car`, and `cdr`. All Lisp sequences are vectors operated on via
	indexed primitives (`(!)` forms).

*	**No-Layer FFI:** Direct zero-overhead calling between Lisp forms and
	underlying Virtual Processor (VP) machine instructions.

*	**O(1) Symbol Lookup & Single-Bucket HMaps:**

	*	Environments are trees of single-bucket `hmap` objects. Single-bucket
		tables avoid hash division/modulo math and minimize allocation
		costs.

	*	Interned symbol objects carry a cached field: `str_hashslot`.

	*	When a symbol is bound (via `defq` or argument binding), the
		runtime stores the binding and immediately writes the slot index
		into the symbol's `str_hashslot`. Lookups do not perform initial
		linear scans; they execute as direct indexed reads.

	*	If shadowing invalidates a cached slot, `hmap:find` performs a
		one-time linear recovery scan upon exiting the shadow, and
		immediately rewrites `str_hashslot` with the recovered index to
		resume O(1) performance.

*	**Linkerless VP Code Generation:** VP compilation emits symbolic
	dependency paths in a links section. The `boot-image` tool resolves
	these to relative offsets, and `sys/load/init` performs runtime
	rebinding to absolute addresses in fractions of a second.

## Core Lisp Disciplines (Must Follow)

*	**Tab Style:** Always use leading 4-space tab characters for indentation
	in source code and documentation, with spaces afterwards if needed.

*	**Type-Dependent Equality with `eql`:**

	*	`eql` performs deep content comparison on scalar numbers, strings,
		and typed numeric vectors (`nums`, `fixeds`, `reals`).

	*	`eql` performs pointer identity comparison on general containers
		(`list`, `hmap`). To test list content equivalence, use:

		(and (= (length l1) (length l2)) (every eql l1 l2))

*	**No Lexical Closures:** `lambda` forms are pure code templates that
	execute strictly inside the environment of their caller. They do not
	capture defining scopes. All context must be supplied explicitly via
	arguments or pre-exist in the caller's environment.

*	**Static `'()` vs Independent `(list)`:** `'()` evaluates to a shared,
	static empty list instance. Mutating `'()` (e.g. via `push`) corrupts
	every reference in the system. Always use `(list)` to create a fresh,
	independent, mutable empty list.

*	**Control Flow and Error Prevention:** There is no `return` keyword;
	a function yields the result of its last evaluated expression.
	Non-local exits (`catch`, `throw`) are strictly for debugging. Code must
	prioritize input validation to prevent invalid states from ever
	occurring, rather than catching errors after the fact.

*	**Lists as LIFO Stacks:**

	*	`(push list elem)` appends to the end of `list`.

	*	`(pop list)` removes and returns the final element.

	*	`(first list)` is `(elem-get list 0)`.

	*	`(last list)` is `(elem-get list -2)`.

	*	`(slice list 1 -2)` returns the list minus first and last items.

*	**Pragmatic Lambda Usage (`#` vs `lambda`):**

	*	Use `(# ...)` for compact, performance-critical callbacks. Positional
		symbols `%0`, `%1`, etc., are globally interned with permanent
		`str_hashslot` cache indices.

	*	Use `(lambda ...)` when destructuring arguments or when explicit
		naming clarifies complex logic.

	*	**NEVER** use `(bind ...)` inside an anaphoric `#` lambda
		(anti-pattern: `(# (bind '(k v) %0) ...)`). Doing so introduces
		local symbols that destroy the cache benefits of `%0` while adding
		`bind` overhead. Use `(lambda ((k v)) ...)` instead.

*	**Variable Binding and Shadowing:**

	*	**Variable Naming (Snake_case Only):** Variables, arguments, and
		parameters must ALWAYS use snake_case with underscores (`_`), never
		hyphens (`-`). Hyphenated/kebab-case symbols are strictly reserved
		for callable forms (functions and macros).

	*	Do not nest `let` or `let*`. Use `defq` to declare and bind multiple
		variables simultaneously in the current scope:

		(defq a 1 b 2 c (list))

	*	Never use built-in function names as variables (e.g. `path`, `str`).
		Functions and variables share the same symbol environment.

	*	Use `bind` for destructuring sequences and return tuples:

		(bind '(w h) (. view :get_size))
		(bind '(x y &rest tail) my_list)
		(bind '(x y &ignore) my_list)
		(bind '((x0 x1 &ignore) (y0 y1 &ignore) &ignore) nested_list)

*	**Object Syntax:**

	*	Method call: `(. obj :method arg1 arg2)`.

	*	Chained method call returning `this`: `.->` macro:

		(.-> *canvas* (:set_color +argb_black) (:fill 0))

	*	Property access: `(get :prop obj)`.

	*	Property create or mutate: `(def obj :prop val)`.

	*	Property mutate only: `(set obj :prop val)`.

*	**Anaphoric Loop Index `(!)`:** In `each`, `each!`, `map`, and `lines!`,
	the form `(!)` evaluates to the current zero-based loop index:

	(each (# (print "Item " (!) ": " %0)) my_list)

*	**Compile-Time Constants:** Force compile-time arithmetic or lookups
	using `(const ...)`:

	(* x (const (/ 180.0 +fp_pi)))

*	**GUI Application Event Loop Pattern:**

	(defun main ()
		(defq select (task-mboxes +select_size) *running* :t)
		; ... setup widgets / window ...
		(gui-add-front-rpc *window*)
		(while *running*
			(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
			(cond
				((= idx +select_main)
					(if (= (getf msg +ev_msg_target_id) +event_close)
						(setq *running* :nil)
						(. *window* :event msg)))
				(:t (. *window* :event msg))))
		(gui-sub-rpc *window*))

*	**Short-Circuiting, Embedded Binding, and Branching (`and` / `or`):**

	*	`and` expands to `condn` (testing for `:nil`), and `or` expands to
		`cond`. Write predicates for zero wasted interpreter evaluations.

	*	*Embed `defq` Inside Consumer Predicates:* Do not write standalone
		`(defq ...)` clauses in an `and` chain. Bind variables directly
		inside the comparison that uses them:

		(eql (third %1) (defq d (third %0)))

	*	*Order Clauses for Fastest Failure:* Place the most discriminative
		test earliest to lazily abort execution before further bindings.

	*	*Leverage `cond` Branch Conditions:* Put `(and ...)` directly as
		the `cond` branch condition; do not wrap the body in `(when ...)`.

	*	*Use `when` Naturally for Multi-Statement Blocks:* Inside `case` or
		unconditional blocks, use `(when tst a1 a2)` directly.

	*	*Anti-Pattern:*

		(cond
			((eql op0 'emit-cpy-rr)
				(when (and (defq cpy_info (pfind +map (first %1)))
						(defq d (third %0))
						(eql (third %1) d)
						(defq s (second %0))
						(nql s d))
					(case (first cpy_info)
						(:cr
							(elem-set emit_list (!) (list (second cpy_info) (second %1) s d))
							(elem-set emit_list (inc (!)) '(emit-nop)))))))

	*	*Correct Pattern:*

		(cond
			((and (eql op0 'emit-cpy-rr)
					(defq cpy_info (pfind +map (first %1)))
					(eql (third %1) (defq d (third %0)))
					(nql d :rsp)
					(nql (defq s (second %0)) d)
					(nql s :rsp))
				(case (first cpy_info)
					(:cr
						(elem-set emit_list (!) (list (second cpy_info) (second %1) s d))
						(elem-set emit_list (inc (!)) '(emit-nop))))))

## Virtual Processor (VP) Assembler & CScript Guide

When writing or modifying `.vp` files, you target ChrysaLisp's register and
execution model.

### Register Architecture

*	15 General-Purpose registers: `:r0` through `:r14`.

*	1 Dedicated Stack Pointer: `:rsp`.

*	16 Floating-Point registers: `:f0` through `:f15`.

*	**ABSOLUTE VOLATILITY (No Callee-Saved Registers):** ChrysaLisp has NO
	callee-saved registers. Any call may trash registers. The `;trashes`
	header above each function is the sole source of truth.

### VP Method Definition Boilerplate

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

### The `assign` Macro

`(assign ...)` evaluates expressions, moves data, and loads/stores memory
simultaneously:

(assign '((:r0 +str_length)) '(:r1))

### CScript Integration & Memory Rules

CScript provides high-level typed variables and pointer expressions within
`{...}` blocks:

*	**Typed Declarations:** Define stack variables with `(def-vars ...)`.
	Supported types include `ptr`, `pubyte`, `long`, `uint`, etc.
	Dereferencing `{*src_ptr}` generates appropriate byte or word transfers
	based on variable type.

*	**CScript Scope Discipline (CRITICAL):**

	*	Open scope with `(push-scope)` at function start.

	*	**NEVER call `(pop-scope)` before `(return)` or `(jump)`**. Both
		macros automatically emit scope unwinding instructions. Calling
		`pop-scope` manually creates a double-free of the stack frame.

	*	Place `(pop-scope-syms)` at the end of the `def-func` block (after
		the `errorcase` and `signature` sections) to cleanly purge compiler
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

### Raw VP Loop Optimization (Bypassing CScript in Hot Paths)

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

### Mixed VP Assembler & CScript Syntax Duality

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

### Calling Conventions & Register Introspection

*	Inspect register contracts dynamically at compile time:

	*	`(method-input :class :method)`: Returns expected input registers.

	*	`(method-output :class :method)`: Returns output registers.

*	**Dynamic Call Mapping:** Pass `(method-input ...)` directly to `vp-rdef`
	to bind argument aliases without hardcoding volatile registers:

	(vp-rdef (cp_src cp_dst cp_len) (method-input :sys_mem :copy_to_ring))

	Assign loop state into these argument aliases immediately before the call.
	If the call trashes registers used for loop progression, keep counters in
	registers outside the call's `;trashes` set, or advance them before the call.

*	Preserve live state across calls via explicit `(vp-push reg)` /
	`(vp-pop reg)` or by saving to CScript stack slots.

### Systematic VP Lowering Workflow

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

## Numerical Representations & Systems

ChrysaLisp natively supports three numerical types:

*	**Integers (`num`):** 64-bit signed integers.

*	**Fixed-Point Reals (`fixed`, `fixeds`):** 48.16 signed fixed-point
	values (64-bit word with 16 fractional bits). Uses `+fp_frac_mask` and
	`+fp_shift 16`. Trigonometric and transcendental math operate on fixed.

*	**Double-Precision Reals (`real`, `reals`):** Standard 64-bit IEEE 754
	double-precision floating-point values.

*	**Conversions:**

	*	`(n2i x)`: Converts `fixed` or `real` to integer `num`.

	*	`(n2f x)`: Converts `num` or `real` to 48.16 `fixed`.

	*	`(n2r x)`: Converts `num` or `fixed` to IEEE double `real`.

## Subsystems & Architecture Reference

### Shared Memory (SHMEM) Link Protocol

Independent ChrysaLisp instances communicate over shared memory using
`sys_link` ring buffers:

*	**Negotiation Towel:** Nodes race to write their `node_id` into the
	`host_a` field of `chan_1`. After `(task-sleep 100)`, the surviving ID
	becomes the owner (transmits on `chan_1`, receives on `chan_2`). The
	other node writes to `host_b` (transmits on `chan_2`, receives on
	`chan_1`).

*	**Buffer Slot Statuses:** `lk_chan_status_ready` (free),
	`lk_chan_status_ping` (routing heartbeat), `lk_chan_status_frag`
	(message data fragment), and `lk_chan_status_skip` (buffer wrap marker).

### Distributed JIT Compilation Pipeline

Dynamic VP compilation (`lisp.vp`) is protected by network locking:

(lock-claim-rpc obj_prefix)
(when (some (# (> file_age (age (cat obj_prefix %0)))) products)
	(catch (within-compile-env (# (include file))) :nil))
(lock-release-rpc obj_prefix)

### Two-Pass GUI Layout System

GUI rendering executes in two deterministic passes:

1.	**Constraint Pass (`:constraint`):** Traverses top-down to compute
	minimum bounding dimensions based on content.

2.	**Layout Pass (`:layout`):** Traverses bottom-up to assign final
	coordinates and bounds to widgets.

*	Flags like `+flow_stack_fill` and `+flow_down_fill` use `lastw` and
	`lasth` to absorb remaining container dimensions.

## File & Naming Conventions

*	`.lisp`: Executable ChrysaLisp programs.

*	`.inc`: Library files included via `(import ...)`.

*	`.vp`: Virtual Processor assembly source.

*	`.tre`: Application configuration and state trees.

*	`class.inc`: VP assembler include definitions (`(include ...)`).

*	`class.vp`: VP assembler method implementations.

*	`lisp.inc`: Lisp FFI bindings and Lisp versions of VP structures.

*	`lisp.vp`: VP implementations of `:lisp_xxx` static primitives.

*	**Identifier Naming Rules:**

	*	**Functions and Macros:** Kebab-case with hyphens (`foo-bar`). ONLY
		callable code (functions and macros) may use kebab style.

	*	**Variables and Parameters:** Snake_case with underscores (`foo_bar`).
		NEVER use hyphens in variable or parameter names.

	*	**Constants:** Plus prefix (`+foo_bar`).

	*	**Globals:** Asterisk-wrapped (`*foo_bar*`).

	*	**Properties and Keywords:** Colon prefix (`:foo_bar`).

## Output Directives (Mandatory)

*	Do not emit entire unmodified source files. Provide focused diffs or
	concise cut-and-paste snippets indicating exact locations.

*	Always use 4-space tab indentation in ChrysaLisp source code and
	documentation.

*	Always place a blank line between all documentation elements in `.md`
	files, including between individual bullet points and sub-bullets.

*	Always wrap ChrysaLisp `.md` documentation at 80 columns (do not wrap
	source code blocks).

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

## Contents

Find the task below and read that section, or that file, in full before acting,
rather than skimming. Sections marked mandatory apply to every task.

Reference files, in this folder, read on demand:

*	**[lisp-disciplines.md](lisp-disciplines.md)**
	Mandatory before writing or reviewing any Lisp code. The 23 Lisp rules in
	full, each with its reason and examples.

*	**[vp-assembler.md](vp-assembler.md)**
	Read before touching a `.vp` file. Registers, method boilerplate,
	`gen-create` / `gen-type` headers, `assign` and CScript memory rules, field
	helpers, raw VP loops, calling conventions, and the lowering workflow.

Sections of this file:

*	**[Output Directives (Mandatory)](#output-directives-mandatory)**
	Mandatory. How to present work: diffs not whole files, tab indentation,
	`.md` wrapped at 80 columns with blank lines between elements.

*	**[Documentation Index & AI Digest](#documentation-index--ai-digest)**
	Where the 70 deep-dive documents are, through `LLM.md`. Go there for design
	rationale this skill only summarises.

*	**[Core Architectural Philosophies](#core-architectural-philosophies)**
	The three philosophies and the ephemeral `netid`. Read before designing
	anything new, they explain why the usual machinery is absent.

*	**[The Lisp Implementation & Symbol Engine](#the-lisp-implementation--symbol-engine)**
	How the interpreter is built: vector primitives, symbols, environments and
	lookup. Read when performance or evaluation order matters.

*	**[Core Lisp Disciplines (Must Follow)](#core-lisp-disciplines-must-follow)**
	Mandatory for any Lisp code. A one line summary of each of the 23 rules.
	The full text is in `lisp-disciplines.md`.

*	**[Virtual Processor (VP) Assembler & CScript Guide](#virtual-processor-vp-assembler--cscript-guide)**
	What the VP guide covers. The guide itself is in `vp-assembler.md`.

*	**[Numerical Representations & Systems](#numerical-representations--systems)**
	The three number types, `num`, `fixed` and `real`, and their vector forms.

*	**[Subsystems & Architecture Reference](#subsystems--architecture-reference)**
	The shared memory link protocol, the distributed JIT build pipeline, and
	the two-pass GUI layout.

*	**[File & Naming Conventions](#file--naming-conventions)**
	What each file extension is for and how functions, variables, constants and
	globals are named.

*	**[Direct REPL Access (`lisp -r`)](#direct-repl-access-lisp--r)**
	How to try a snippet from the host shell before it goes into a file.

*	**[Building & Binary Verification](#building--binary-verification)**
	The `make` command and its options, the lint tools, and the checks to run
	before a commit.

## Output Directives (Mandatory)

*	Do not emit entire unmodified source files. Provide focused diffs or
	concise cut-and-paste snippets indicating exact locations.

*	Always use 4-space tab indentation in ChrysaLisp source code and
	documentation.

*	Always place a blank line between all documentation elements in `.md`
	files, including between individual bullet points and sub-bullets.

*	Always wrap ChrysaLisp `.md` documentation at 80 columns (do not wrap
	source code blocks).

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

*	**No Paranoid Guarding Against Things That Cannot Happen:** Validate
	data strictly at the boundary (e.g. `*config_version*` when loading
	persisted state, or protocol validation when receiving network packets).
	Once verified at the boundary, trust internal invariants. Never sprinkle
	defensive type checks like `(if (not (str? x)) ...)` on internally
	produced or schema-guaranteed values; redundant checks waste performance
	and clutter code.

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
		`+kn_call_run`, the kernel initiates a decentralized load-
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

*	**Vector-Based Primitives:** Sequences are flat vectors rather than
	linked lists; all traversal and manipulation map to indexed vector
	primitives (`elem-get`, `slice`, `splice`).

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

Mandatory for any Lisp code. Before you write or review Lisp, read
[lisp-disciplines.md](lisp-disciplines.md) in full. It gives the reason for
each rule and worked examples of the right and wrong way. In short:

*	**Tab Style:**
	Indent with tab characters, 4 wide, in source and in documentation.

*	**Sensible `defq` and `setq` Line Wrapping:**
	Pack several variable and value pairs per line, up to about 80 to 100
	columns, and combine consecutive `defq` forms into one.

*	**Type-Dependent Equality with `eql`:**
	`eql` compares content for numbers, strings and numeric vectors, but
	identity for `list` and `hmap`.

*	**Multi-Argument (N-ary) Comparisons & Range Checks:**
	Comparisons take any number of arguments, so write a range test as one
	chain, `(<= lo x hi)`, never `(and (>= ...) (<= ...))`, and name the limits
	as constants.

*	**Dynamic Scoping (No Lexical Closures):**
	A function runs in its caller's environment and captures nothing, so pass
	context in or have it exist in the caller.

*	**Manual Scope Control (`env-push` / `env-pop`):**
	`(env-push [env])` and `(env-pop)`, which takes no arguments and returns
	the popped environment. Pair every push with a pop, and note `defq` defines
	into the pushed environment.

*	**Destructuring & Tuple Unpacking (`bind`):**
	Unpack sequences with `bind`, not ladders of `elem-get`. `&` skips one
	element, `&ignore` discards the rest, and leaving it off a longer sequence
	is an error.

*	**DO NOT Shadow Built-in Function Names:**
	Never name a variable `str`, `list`, `path`, `type`, `first` and so on,
	variables and functions share one symbol space.

*	**Sensible Line Wrapping for `defq`, `setq`, and `bind`:**
	Stay within about 80 to 100 columns, neither one pair per line nor one
	sprawling line.

*	**Static `'()` vs Independent `(list)`:**
	`'()` is one shared static list. Use `(list)` for any list you will mutate.

*	**Control Flow and Error Prevention:**
	There is no `return`, the last expression is the result. `catch` and
	`throw` are for debugging, so validate inputs instead.

*	**Conditionals & Branching (`if`, `ifn`, `when`, `unless`):**
	The else of `if` and `ifn` is an implicit `progn`, never wrap it. Use `ifn`
	and `unless` rather than `(if (not ...))`, embed the `defq` in the test,
	use `min` and `max`, and keep a short conditional on one line.

*	**Lists as LIFO Stacks & The Rocinante Collector Pattern:**
	`push` returns the list and takes several elements, so build a list with
	`reduce` and `push`, not `each` and `push` into a temporary.

*	**Pragmatic Lambda Usage (`#` vs `lambda`):**
	`#` for compact callbacks, `lambda` to destructure or name arguments, and
	never `bind` inside a `#`.

*	**Variable Binding and Shadowing:**
	`defq` rather than nested `let`. `bind` defines new local bindings and does
	not update outer variables, use `setq` for that. `gather` pulls several
	values from a map.

*	**Object Syntax & Sensible Wrapping:**
	`(. obj :method)`, `.->` chains, `get`, `def` and `set` for properties,
	with property pairs flowed two or three to a line.

*	**Anaphoric Loop Index `(!)`:**
	`(!)` is the current index inside `each`, `each!`, `map` and `lines!`.

*	**Short Anaphoric Lambdas `(# ...)` vs `(lambda ...)`:**
	Use `(# ...)` with `%0`, `%1`, and never write `(lambda (%0) ...)`.

*	**Compile-Time Constants:**
	Wrap arithmetic or lookups on constants in `(const ...)`.

*	**Top-Level `defun` Definitions (Prebinder Rule):**
	Never nest a `defun` inside another form, the prebinder only sees top-level
	forms.

*	**GUI Application Event Loop Pattern:**
	The standard shape of a GUI app `main`, a mailbox select loop.

*	**NEVER Run GUI Code from TUI or Test Environment:**
	GUI classes do not exist under `./run_tui.sh`. Pipe a snippet to `./run.sh
	-n 1 -f` instead, see the `chrysalisp-gui-apps` skill.

*	**Short-Circuiting, Embedded Binding, and Branching (`and` / `or`):**
	Bind inside the predicate that uses the value, order tests for the fastest
	failure, and put the `(and ...)` straight in the `cond` test.

## Virtual Processor (VP) Assembler & CScript Guide

Before you write or change a `.vp` file, read
[vp-assembler.md](vp-assembler.md) in full. It covers:

*	Register Architecture

*	VP Method Definition Boilerplate

*	Generated Function Headers (`gen-create`, `gen-type`)

*	The `assign` Macro & CScript Memory Rules

*	Field Helpers & Sorted Memory Transfers (`load-fields`, `save-fields`,
	`assign-fields`)

*	Raw VP Loop Optimization (Bypassing CScript in Hot Paths)

*	Mixed VP Assembler & CScript Syntax Duality

*	Calling Conventions & Register Introspection

*	Systematic VP Lowering Workflow

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

*	**Prebinder Constant Evaluation & AST Substitution (`+xxxxx` and `''(...)` vs `'(...)`):**
	In the prebind stage of the REPL, any symbol beginning with `+` (`+xxxxx`) is evaluated in the current environment, and **what it evaluates to is directly substituted into the AST**:
	*	For numbers/atoms (e.g. `(defq +width 32)`), the atom `32` is substituted directly.
	*	For data lists, if defined with a single quote `(defq +my_list '(1 2 3))` or unquoted `(defq +my_list (list 1 2 3))`, `+my_list` evaluates to the raw list `(1 2 3)`. When this raw list is directly substituted into the AST, it forms an unquoted list `(1 2 3)`. At runtime, the evaluator interprets the first element `1` as a function call, failing with `not_a_function ! Obj: 1`.
	*	Therefore, constant lists must always evaluate to a quoted form: use double quotes `(defq +my_list ''(1 2 3))` for pure literals, or quasiquote `(defq +my_list `'(,+item1 ,+item2))` when referencing symbols. Prebind evaluates these to `'(1 2 3)` (i.e. `(quote (1 2 3))`), substituting the `(quote ...)` form into the AST, which evaluates at runtime to the literal data list.

## Direct REPL Access (`lisp -r`)

The `lisp` command (`cmd/lisp.lisp`) has a `-r` / `--repl` option that reads
the remainder of the command line into the REPL. Use it to try raw ChrysaLisp
code, check a primitive's behaviour, or verify a snippet before putting it in
source or documentation:

`echo "lisp -r (print (* 123 456))" | ./run_tui.sh -n 1 -f`

*	`./run_tui.sh -n 1 -f` — single node, TUI only.

*	`./run.sh -n 1 -f` — single node GUI, with a TUI attached to the host. Use
	this when the code depends on GUI classes or libs, which only exist in the
	GUI boot image. A View tree built this way can be dumped to a file with
	`(ui-save stream view)` for inspection (see the `chrysalisp-gui-apps`
	skill).

*	Several forms can follow `-r`; they are evaluated in order in the `lisp`
	command's `main` environment. Output must be explicitly `print`ed. An
	error prints as `Error: ... !` with the offending object.

*	**Use `{}` for strings on the command line.** `{...}` and `"..."` are
	identical string constructors, but the command line parser strips double
	quotes, so `(print "hello world")` fails with `symbol_not_bound`:

	`echo "lisp -r (print {hello world})" | ./run_tui.sh -n 1 -f`

*	`(read)` does escape processing inside both string forms, so `\n`, `\t`,
	`\\` and `\q` (a double quote) all work inside `{}`. Use `\q` when a
	snippet needs a double quote character. Single quote the host shell
	`echo` so the backslashes reach the TUI:

	`echo 'lisp -r (print {a\tb\nc \qquoted\q})' | ./run_tui.sh -n 1 -f`

*	To run a script file use `-r` with an import, so the command exits when
	done: `lisp -r (import {tests/scratch/probe.lisp})`. A bare
	`lisp file.lisp` imports the file and then waits in the stdin REPL.

*	Start with one simple expression and build up. Do not batch many untested
	snippets into one run.

*	Keep `(env-push)` / `(env-pop)` balanced within a snippet.

*	For anything longer than a line, write a script in `tests/scratch/` and
	run it with `-s` instead (see the `chrysalisp-tests` skill).

## Building & Binary Verification

With the full TUI environment accessible via piped stdin or interactive
sessions, developers and LLMs have direct access to the standard ChrysaLisp
`make` command (`cmd/make.lisp`) and all its options:

*	`make` — incremental compile of modified host `.vp` files.

*	`make all boot` — recompile all host `.vp` files and regenerate the native
	boot image (`obj/<arch>/<OS>/sys/boot_image`).

*	`make it` — canonical full multi-platform rebuild of all target platforms:
	native targets (`AMD64`, `WIN64`, `ARM64`, `RISCV64`, `LA64`) are built in
	debug mode (`*build_mode* = 1`), while `VP64` is specifically built in release
	mode (`*build_mode* = 0`). Also regenerates reference documentation under
	`docs/reference/`.

*	`make vp` — compile the debug VP64 emulator image (`*build_mode* = 1`) used
	with `trace` (`cmd/trace.lisp`) for register clobber analysis. (Must never
	go into `snapshot.zip`, which is executed by `make install` to cross-compile
	the native host and needs the full speed of a release build; always restore
	the release image with `make it`).

*	`make apps debug` — compile the `apps/` VP functions with a debug VP64
	build, which `make vp` does not do. Needed before `trace` so the app
	functions are linted too; restore with `make apps`.

*	`make docs` — scan source files and regenerate all Markdown reference
	documentation under `docs/reference/`.

*	`make apps` — recompile GUI and desktop applications.

From the host shell or automated tool invocations, pipe commands directly to
`./run_tui.sh -f` or `./run.sh -f`:

*	**Native Boot Image Rebuild:**

	`echo "make all boot | time -s" | ./run_tui.sh -f`

*	**Full Multi-Platform Rebuild (`make it`):**

	`echo "make it | time -s" | ./run_tui.sh -f`

*	**Binary-to-Binary Verification & Diff Testing:**
	The reference directory `../ChrysaLisp_copy/obj/` is used to verify exactly
	what binaries changed across builds and to confirm that the VP64 emulator
	(`-e`) produces bit-for-bit identical output to the native host:

	1.	Build all platforms natively with `make it`:

		`echo "make it" | ./run_tui.sh -f`

	2.	Sync `obj/` to `../ChrysaLisp_copy/obj/`:

		`rsync -av --delete obj/ ../ChrysaLisp_copy/obj/`

	3.	Rebuild under the VP64 emulator:

		`echo "make it" | ./run_tui.sh -e -f`

	4.	Verify bit-for-bit identity using host `diff`:

		`diff -r obj/ ../ChrysaLisp_copy/obj/`

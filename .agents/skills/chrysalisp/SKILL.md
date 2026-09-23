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

*	**Tab Style:** Always use leading 4-space tab characters for indentation
	in source code and documentation, with spaces afterwards if needed.

*	**Sensible `defq` and `setq` Line Wrapping:**
	Do not place only one variable setting per line when defining multiple
	variables with `(defq ...)` or mutating with `(setq ...)`. Putting one
	variable-value pair per line is excessive and inflates line count.
	Pack related variable-value pairs onto lines up to a reasonable column width
	(~80-100 characters) and flow across lines cleanly:

	; Good: Flowed, logically grouped variable settings
	(defq +font_title (create-font "fonts/OpenSans-Bold.ctf" 14) +font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
		+font_bold (create-font "fonts/OpenSans-Bold.ctf" 13) +font_small (create-font "fonts/OpenSans-Regular.ctf" 11)
		*config* :nil *config_version* 1 *config_file* (cat *env_home* "news.tre")
		*selected_category* "top" *selected_id* 0 *current_stories* (list))

	; Avoid: 1 variable pair per line bloating vertical space
	(defq
		+font_title (create-font "fonts/OpenSans-Bold.ctf" 14)
		+font_btn (create-font "fonts/OpenSans-Regular.ctf" 13)
		*config* :nil
		*config_version* 1
		*selected_category* "top")

	Combine local variable bindings within functions rather than writing multiple
	consecutive `(defq ...)` calls:

	; Good: Combined and flowed
	(defq idx (!) is_selected (= id sel_id) card_flow (Flow) title_btn (Button)
		meta_flow (Flow) score_lbl (Label) meta_lbl (Label))

*	**Type-Dependent Equality with `eql`:**

	*	`eql` performs deep content comparison on scalar numbers, strings,
		and typed numeric vectors (`nums`, `fixeds`, `reals`).

	*	`eql` performs pointer identity comparison on general containers
		(`list`, `hmap`). To test list content equivalence, use:

		(and (= (length l1) (length l2)) (every eql l1 l2))

*	**Multi-Argument (N-ary) Comparisons & Range Checks (`<=`, `=`, `>=`, `<`, `>`, `/=`):**

	*	All comparison operators accept an arbitrary number of arguments:
		`(= a b c ...)`, `(<= a b c ...)`, `(< a b c ...)`, `(>= a b c ...)`.

	*	*Range & "Within" Tests (Never `(and (>= ...) (<= ...))`):*
		Never write multiple comparisons joined by `and`:
		```lisp
		;; ANTI-PATTERN (evaluates id twice, allocates and executes 'and'):
		(and (>= id +event_city_0) (<= id +event_city_5))
		```
		Always use a single chained comparison:
		```lisp
		;; THE CHRYSALISP WAY (single primitive call, evaluates id once):
		(<= +event_city_0 id +event_city_5)
		```

	*	*Named Constants for Event Ranges & Capacity Limits:*
		Never scatter magic numbers (e.g. `50`, `20`) inside event range checks or loops.
		Always declare an explicit named constant for the capacity limit (e.g. `+max_stories 50`),
		and evaluate the upper bound at compile time with flat $N$-ary addition `(const (+ +event_story_0 +max_stories -1))`:
		```lisp
		(defq +max_stories 50)
		...
		(<= +event_story_0 id (const (+ +event_story_0 +max_stories -1)))
		```

	*	*Monotonic Ordering:*
		*	`(<= a b c)` tests `a <= b <= c`.
		*	`(< a b c)` tests `a < b < c`.
		*	`(= a b c)` tests `a = b = c`.
		*	`(< -1 idx (length items))` tests `0 <= idx < (length items)` in one step!

*	**Dynamic Scoping (No Lexical Closures):** Functions and lambdas
	execute strictly inside the environment of their caller. They do not
	capture defining scopes. All context must be supplied explicitly via
	arguments or pre-exist in the caller's environment.

*	**Destructuring & Tuple Unpacking (`bind` vs Manual `elem-get`):**
	Always use `(bind '(var1 var2 ...) seq)` to unpack lists, tuples, or function return sequences.
	Never write multi-line ladders of `(elem-get seq 0)`, `(elem-get seq 1)` in a `defq`:
	```lisp
	;; ANTI-PATTERN (manual elem-get unpacking ladder in defq):
	(defq sym (elem-get selected_coin 0)
		name (elem-get selected_coin 1)
		price (elem-get selected_coin 2)
		change (elem-get selected_coin 3)
		rank (elem-get selected_coin 4)
		spark (elem-get selected_coin 5))

	;; THE CHRYSALISP WAY (single bind statement):
	(bind '(coin_sym name price change rank spark) selected_coin)
	```

	*	**`&` (Single Skip) vs `&ignore` (Trailing Discard):**
		- `&` skips **exactly one** element (1-to-1 placeholder).
		- `&ignore` terminates binding immediately and discards **all remaining** elements in the sequence.
		If a sequence has trailing elements not accounted for in the pattern, omitting `&ignore` causes an `Error: (bind (param ...) seq) wrong_num_of_args !`.
		```lisp
		;; WRONG: (date) returns 7 elements (sec min hr day mo yr dotw),
		;; so (& cmin chr &) only consumes 4 elements -> wrong_num_of_args!
		(bind '(& cmin chr &) (date city_sec))

		;; CORRECT: &ignore consumes and discards all remaining elements:
		(bind '(& cmin chr &ignore) (date city_sec))
		```

*	**DO NOT Shadow Built-in Function Names:**
	Never use built-in function or primitive names as variable or argument identifiers.
	Symbols such as `sym`, `str`, `num`, `char`, `type`, `list`, `first`, `rest`,
	`last`, `find`, `slice`, `map`, `each`, `eval`, `read`, `print`, `length`,
	`format`, `min`, `max`, `abs`, `sort`, `filter`, `range`, `path`, etc., are
	core language primitives. Shadowing them breaks lexical/global lookups and
	introduces subtle bugs. Always use descriptive names (e.g. `coin_sym`,
	`item_name`, `val_str`).

*	**Sensible Line Wrapping for `defq`, `setq`, and `bind`:**
	Keep code readable within ~80-100 columns. Avoid both extreme vertical
	ladders (one pair per line) and sprawling multi-variable lines:
	```lisp
	;; IDIOMATIC (sensible groupings by category, ~80-100 columns):
	(defq *config* :nil *config_version* 1
		*config_file* (cat *env_home* "crypto.tre")
		*selected_symbol* "BTC" *coins_data* (list)
		*canvas_width* 360 *canvas_height* 110
		*btn_coin_0* :nil *btn_coin_1* :nil *btn_coin_2* :nil
		*btn_coin_3* :nil *btn_coin_4* :nil *btn_coin_5* :nil)

	;; Wrap long list constructors across multiple lines:
	(defq btns (list
		*btn_coin_0* *btn_coin_1* *btn_coin_2*
		*btn_coin_3* *btn_coin_4* *btn_coin_5*))
	```

*	**Static `'()` vs Independent `(list)`:** `'()` evaluates to a shared,
	static empty list instance. Mutating `'()` (e.g. via `push`) corrupts
	every reference in the system. Always use `(list)` to create a fresh,
	independent, mutable empty list.

*	**Control Flow and Error Prevention:** There is no `return` keyword;
	a function yields the result of its last evaluated expression.
	Non-local exits (`catch`, `throw`) are strictly for debugging. Code must
	prioritize input validation to prevent invalid states from ever
	occurring, rather than catching errors after the fact.

*	**Conditionals & Branching (`if`, `ifn`, `when`, `unless`):**

	*	*Implicit Progn on Else Clauses:* Both `(if tst then else_1 else_2 ...)` and
		`(ifn tst then else_1 else_2 ...)` treat the `then` branch as a single form,
		but treat all subsequent expressions in the `else` position as an **implicit `progn`**
		(evaluated sequentially via `:lisp :repl_progn`).

	*	*NEVER Wrap Else Clauses in `(progn ...)`:*
		Wrapping the `else` clause in `(progn ...)` is completely redundant:
		```lisp
		;; ANTI-PATTERN: (if (not ...)) + redundant (progn ...) in the else clause:
		(if (not entry)
			(progn
				(defq md (Md))
				(def md :page_width page_w :zoom 1.0 :base_font_size 14)
				(. *right_container* :add_child md)
				(. md :populate_lines '("# Select an algorithm" "" "*No algorithm selected.*")))
			(progn
				(defq md_top (Md))
				(def md_top :page_width page_w :zoom 1.0 :base_font_size 14)
				(. *right_container* :add_child md_top)
				(. md_top :populate_lines (catalog-overview-markdown entry))
				...))
		```
		Instead, use `ifn` with the fallback in the single-form `then` branch, and let the entire main block flow into the `else` clause with zero `progn` wrapper:
		```lisp
		;; THE CHRYSALISP WAY: ifn + implicit progn in else clause + (def (defq ...)):
		(ifn entry
			(progn
				(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
				(. *right_container* :add_child md)
				(. md :populate_lines '("# Select an algorithm" "" "*No algorithm selected.*")))
			;; ELSE clause has implicit progn - no (progn ...) wrapper!
			(def (defq md_top (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md_top)
			(. md_top :populate_lines (catalog-overview-markdown entry))
			(defq code_vdu (create-code-vdu (elem-get entry 7) page_w))
			(. *right_container* :add_child code_vdu)
			(when (defq idm (catalog-idioms-markdown entry))
				(def (defq lbl_space (Label)) :min_height 8 :border 0)
				(. *right_container* :add_child lbl_space)
				(def (defq md_bot (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
				(. *right_container* :add_child md_bot)
				(. md_bot :populate_lines idm))
			(def (defq lbl_pad (Label)) :min_height 16 :border 0)
			(. *right_container* :add_child lbl_pad))
		```

	*	*Test-Result Passthrough:* When no `else` clause is provided:

		*	`(if tst then)` returns `:nil` when `tst` evaluates to falsy.

		*	`(ifn tst then)` returns `tst` (the non-nil test result) when `tst`
			evaluates to truthy.

	*	*Macro Architecture of `when` and `unless`:*

		*	Single-form `(when tst form)` expands directly to `(if tst form)`
			for zero-overhead evaluation.

		*	Multi-form `when` expands to `(ifn tst :nil body ...)`, routing the
			body through the else-clause implicit `progn`.

		*	`(unless tst body ...)` expands to `(if tst :nil body ...)` for both
			single- and multi-form bodies.

		*	Both `when` and `unless` strictly guarantee returning `:nil`
			whenever the body is not executed, completely avoiding the
			fallback overhead of intermediate `cond` or `condn` structures.

	*	*Idiomatic Negative Conditionals (Never `(if (not ...))`):*
		Never write `(if (not cond) ...)`. Always use ChrysaLisp's native negative
		conditional forms:
		*	Use `(ifn cond then [else ...])` when branching on a falsy condition.
		*	Use `(unless cond body ...)` for single-branch side effects or guard clauses without an `else`.

	*	*Short True Branch on Same Line (Else Flows Below):*
		A very common ChrysaLisp idiom is to place a short `then` branch of `if`/`ifn` on the same line as the test, with the `else` (implicit `progn`) block flowing below:
		```lisp
		(ifn (str? price_str) "$0.00"
			(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
				(defq int_part (slice price_str 0 dot)
					frac_part (slice price_str (+ dot 1) -1))
				(if (eql int_part "0")
					(cat "$0." (slice (cat frac_part "0000") 0 4))
					(cat "$" int_part "." (slice (cat frac_part "00") 0 2)))))

		(if (starts-with "-" chg_str) (cat chg_str "%")
			(cat "+" chg_str "%"))
		```

	*	*Don't Waste Statements When Return Values Can Be Used Directly:*
		In ChrysaLisp, binding and evaluation forms (`defq`, `setq`, `push`, etc.) evaluate directly to their resulting value. Never waste a separate binding statement immediately before a conditional check:
		```lisp
		;; ANTI-PATTERN (wasting statements):
		(defq dot (find "." price_str))
		(ifn dot ...)

		(defq resp (http-get url))
		(when resp ...)

		;; THE CHRYSALISP WAY (embed return value directly in condition):
		(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
			...)

		(when (defq resp (http-get url))
			...)

		(when (defq stream (file-stream path))
			...)
		```

	*	*Use Built-in `min` and `max` Primitives:*
		ChrysaLisp has built-in `(min num num ...)` and `(max num num ...)` primitives that accept variable arguments:
		```lisp
		;; ANTI-PATTERN (manual comparison branching):
		(each (#
			(if (< %0 min_val) (setq min_val %0))
			(if (> %0 max_val) (setq max_val %0)))
			flt_pts)

		;; THE CHRYSALISP WAY:
		(each (# (setq min_val (min min_val %0) max_val (max max_val %0))) flt_pts)
		```

	*	*Single-Line `if` / `ifn` / `when` / `unless` for Simple Forms:*
		When a conditional has a short test and concise form, keep it on a **single line**:
		```lisp
		;; IDIOMATIC (single-line for simple guard/init):
		(ifn *config* (setq *config* (Emap)))
		(unless *syntax* (setq *syntax* (Syntax)))
		(if (empty? items) (return :nil))

		;; ANTI-PATTERN (vertical sprawl for a trivial branch):
		(ifn *config*
			(setq *config* (Emap)))
		```

*	**Lists as LIFO Stacks & The Rocinante Collector Pattern:**

	*	`(push list elem ...)` appends to the end of `list` **and returns the list** as its return value!

	*	*Avoid Imperative `(each ... (push l ...))` Accumulation:*
		Never pre-allocate an empty list and use an imperative loop to populate it:
		```lisp
		;; ANTI-PATTERN (redundant variables and environment lookups):
		(defq results (list) items '((1 2) (3 4)))
		(each (lambda ((a b)) (push results (calc a b))) items)
		```
		Instead, use the Rocinante collector pattern with `reduce`:
		```lisp
		;; THE ROCINANTE WAY (zero temp bindings, direct pipeline expression):
		(reduce (lambda (p (a b)) (push p (calc a b))) '((1 2) (3 4)) (list))
		```
		Because `(push p ...)` returns `p` directly, the accumulator flows seamlessly through each iteration of `reduce` with no intermediate variables, returning the completed list as the expression's value.

	*	*Variadic `push` (Multiple Values in One Statement):*
		`push` accepts multiple elements: `(push list elem0 elem1 ...)`.
		Never write consecutive `(push l a) (push l b)` statements; write `(push l a b)`.
		In `reduce` collectors, you can push multiple items into the accumulator in a single step:
		`(push p item1 item2)`.

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

	*	Do not nest `let` or `let*`. Use `defq` to declare and bind multiple
		variables simultaneously in scope:

		(defq a 1 b 2 c (list))

	*	Never use built-in function names as variables (e.g. `path`, `str`).
		Variables, functions, and macros share a single symbol environment.

	*	Use `bind` for destructuring sequences and return tuples:

		(bind '(w h) (. view :get_size))
		(bind '(x y &rest tail) my_list)
		(bind '(x y &ignore) my_list)
		(bind '((x0 x1 &ignore) (y0 y1 &ignore) &ignore) nested_list)

		Note: `&` is a 1-to-1 placeholder skipping a single element. `&ignore` terminates binding immediately and ignores all remaining trailing elements.

	*	**`bind` Semantics (`(def (env) ...)` vs `(set (env) ...)`):**
		`(bind '(var1 var2 ...) seq)` directly inserts new bindings into the current local environment `(env)`. It performs `(def (env) ...)`, **NOT** `(set (env) ...)`.
		Because it defines variables in the local frame, `bind` does **not** update or mutate existing outer bindings or global variables (`*...*`).
		To update pre-existing outer or global variables, always use `setq`:
		```lisp
		(setq *selected_id* (. *config* :find :selected_id)
			*selected_cat* (. *config* :find :selected_cat)
			*search_query* (. *config* :find :search_query))
		```

	*	**Extracting Map Values with `gather`:**
		To extract multiple values from a map or tree in a single call, use `(gather map :key1 :key2 ...)`. It returns a list of the resolved values:
		```lisp
		;; Extract multiple values from a map directly into local bindings:
		(bind '(x y width height) (gather *config* :x :y :width :height))
		(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
		```
		This eliminates repetitive individual `(. map :find :key)` calls when populating local variables.

*	**Object Syntax & Sensible Wrapping:**

	*	Method call: `(. obj :method arg1 arg2)`.

	*	Chained method call returning `this`: `.->` macro:

		(.-> *canvas* (:set_color +argb_black) (:fill 0))

	*	Property access: `(get :prop obj)`.

	*	Property create or mutate: `(def obj :prop val)`.

	*	Property mutate only: `(set obj :prop val)`.

	*	**Sensible Wrapping for `(def)` and `(set)`:**
		Avoid the vertical ladder anti-pattern where every single property and value occupies its own indented line. Instead, flow property pairs sensibly across lines, grouping 2 to 3 related pairs per line within ~80–100 columns, or keep the form inline on a single line if compact:
		```lisp
		;; ANTI-PATTERN: Excessive vertical sprawl (1 pair per line):
		(def (defq vdu (Vdu))
			:font +font_code
			:vdu_width 80
			:vdu_height h
			:color 0
			:ink_color +argb_black)

		;; IDIOMATIC: Sensible wrapping (2-3 pairs per line, ~80-100 columns):
		(def (defq vdu (Vdu))
			:font +font_code :vdu_width 80 :vdu_height h
			:color 0 :ink_color +argb_black)

		;; IDIOMATIC: Compact forms kept on a single line:
		(def (defq backdrop (Backdrop)) :color +argb_grey1 :min_width (max pad_w page_w) :min_height rh)
		(def (defq scroll (Scroll +scroll_flag_horizontal)) :min_width page_w :min_height rh)
		```

*	**Anaphoric Loop Index `(!)`:** In `each`, `each!`, `map`, and `lines!`,
	the form `(!)` evaluates to the current zero-based loop index:

	(each (# (print "Item " (!) ": " %0)) my_list)

*	**Short Anaphoric Lambdas `(# ...)` vs `(lambda ...)`:**
	Always use `(# ...)` when using positional arguments (`%0`, `%1`, etc.):

	(map (# (path-transform m %0 (cat %0))) paths)

	NEVER write `(lambda (%0) ...)`. The `lambda` form is strictly reserved for
	explicitly named parameter lists: `(lambda (item) ...)`.

*	**Compile-Time Constants:** Force compile-time arithmetic or lookups
	using `(const ...)`:

	(* x (const (/ 180.0 +fp_pi)))

*	**Top-Level `defun` Definitions (Prebinder Rule):**
	**NEVER** nest `defun` inside another `defun`, `progn`, `catch`, or other expressions!
	The ChrysaLisp prebinder scans top-level forms to discover functions, prebind symbols, assign frames, and optimize call sites.
	If a `defun` is placed inside `(progn ...)`, `(catch ...)`, or another function, the prebinder cannot see it, causing `symbol_not_bound`, `wrong_num_of_args`, or compilation failure.
	Always define all functions with `defun` at the top level of your file.

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

*	**NEVER Run GUI Code from TUI or Test Environment:**
	GUI classes (`View`, `Window`, `Vdu`, `Flow`, `Button`, `Label`, `Md`, etc.)
	and desktop apps (`apps/desktop/`, etc.) require full system mode with the
	GUI subsystem, compositor, and window manager (`./run.sh -f`). They are NOT
	present in the headless test environment or the TUI boot image
	(`./run_tui.sh`); attempting to reference or instantiate GUI classes from
	test scripts or TUI causes immediate `symbol_not_bound` errors.

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

### The `assign` Macro & CScript Memory Rules

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

### Field Helpers & Sorted Memory Transfers (`load-fields`, `save-fields`, `assign-fields`)

Defined in `class/obj/class.inc`, `load-fields`, `save-fields`, and
`assign-fields` are essential helpers for loading, storing, and copying
structured fields between memory objects and VP registers:

*	**`load-fields`:** `(load-fields base fields tmps)`

*	**`save-fields`:** `(save-fields base fields tmps)`

*	**`assign-fields`:** `(assign-fields src src_fields dst dst_fields tmps)`

#### Why Sorted Memory Transfers Are Critical

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

#### Practical Usage Pattern

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

*	**Prebinder Quoted Lists (`''(...)` vs `'(...)`):**
	When defining top-level constant or variable data lists with `(defq ...)`,
	always use double quotes: `(defq +my_list ''(...))` if you expect the
	prebinder to embed it as a literal list. A single quote
	`(defq +my_list '(...))` has its quote consumed during prebinding, causing
	the list to be evaluated as a function call at load time (e.g. producing a
	`not_a_function` error).

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

## Output Directives (Mandatory)

*	Do not emit entire unmodified source files. Provide focused diffs or
	concise cut-and-paste snippets indicating exact locations.

*	Always use 4-space tab indentation in ChrysaLisp source code and
	documentation.

*	Always place a blank line between all documentation elements in `.md`
	files, including between individual bullet points and sub-bullets.

*	Always wrap ChrysaLisp `.md` documentation at 80 columns (do not wrap
	source code blocks).

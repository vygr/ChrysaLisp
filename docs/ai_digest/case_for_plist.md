# Plist, Case, and Collections

ChrysaLisp's architecture is a network of cooperating ideas, built from
first-principles engineering. At the center of its performance characteristics
is the property list (`:plist`).

This document describes how ChrysaLisp uses the `:plist` to power compile-time
optimizations (such as the `pcase` macro), how the collection library is
constructed upon it, and how it compares to the lexical environment (`:hmap`)
system.

## 1. The `:plist` Primitive and `str_hashslot` Caching

The `:plist` (property list) is a flat, contiguous vector of interleaved
key-value pairs. It inherits directly from `:list` and `:array`.

The core performance of `:plist` is achieved through the `+str_hashslot` caching
mechanism on globally interned symbols (`symbol` objects).

### The Lookup Process in `pfind`

When the virtual processor executes `:plist :pfind` to search for a key, it
follows a highly optimized execution path:

* **VTable Verification**:

	The engine checks if the search key's vtable is equal to the global `:sym`
	vtable.

* **Direct Index Access**:

	If the key is a symbol, the engine reads the symbol's cached `+str_hashslot`
	value.

* **Bounds Checking**:

	The engine offsets the base pointer of the `:plist` by the cached slot value
	and verifies that this address is within the boundaries of the active list.

* **Cache Hit**:

	If the key stored at the calculated offset matches the search key, the engine
	immediately returns the value pointer. This results in an O(1) search.

* **Cache Miss and Self-Repair**:

	If there is a mismatch (caused by a collision or a lookup in a different
	scope), the engine performs a linear scan of the `:plist`. Once found, it
	updates the symbol's `+str_hashslot` with the new index. All subsequent lookups
	of this symbol in the same context resolve at O(1) speed.

* **Polymorphic Fallback**:

	If the key is not a symbol, the engine falls back to calling the virtual `:eql`
	method. This allows the `:plist` to compare numbers, strings, and other objects
	as keys.

## 2. Compile-Time Optimization via `pcase` and `case`

Traditional Lisps compile `case` macros into nested O(N) linear chains of `cond`
or `if` statements. ChrysaLisp avoids this overhead by converting the branching
table into a compile-time `:plist`.

### The Compilation and Dispatch of `pcase`

When the compiler parses a `pcase` block, it constructs a literal `:plist`
containing the mapped branches and keys.

At runtime, the program executes a single `:plist` lookup:

```vdu
(pfind ,pl ,key)
```

If the branches of the `pcase` statement are atomic values, the macro emits a
direct lookup:

```vdu
(ifn (pfind ,pl ,key) ,val)
```

If the branches contain executable code blocks, the macro quotes the bodies and
evaluates the returned block at runtime:

```vdu
(eval (ifn (pfind ,pl ,key) ',val))
```

```file
class/lisp/root.inc "(defmacro pcase" ";;;;" 
```

### Pre-population of Symbols via `pkeys`

The `pcase` macro signature is defined as:

```vdu
(defmacro pcase (key pkeys &rest body) ... )
```

The `pkeys` argument accepts a pre-arranged list of symbols (such as the
`+pkeys2` list used in the register assigner `assign-asm-to-asm`).

This pre-population mechanism provides several engineering advantages:

* **Memory Pre-allocation**:

	The backing array of the generated `:plist` is pre-sized to hold all potential
	matching keys, eliminating dynamic allocation overhead during macro expansion.

* **Stable Index Layout**:

	By pre-inserting the complete set of valid symbols in a fixed order before the
	actual case branches are mapped, the indices of these symbols remain identical
	across every `pcase` block generated in the system.

* **Cache Preservation**:

	Because the layout is identical across different compiled `pcase` tables, the
	`+str_hashslot` of the dispatching symbols is never invalidated when the
	compiler transitions from compiling one function to another. This keeps the
	lookup cache hot and preserves O(1) execution speeds.

```file
lib/asm/assign.inc "assign-asm-to-asm" "" 
```

## 3. The Collections Hierarchy Built on `:plist`

The collections library is built recursively on top of the `:plist` primitive.
Rather than defining complex, specialized data structures for each collection
type, ChrysaLisp wraps or arrays `:plist` buckets.

* **`Lmap` (Linear Map)**:

	A direct wrapper around a single `:plist` instance. It uses `pfind` and
	`pinsert` to manage keys and values.

* **`Fmap` (Fast Map)**:

	An array of `:plist` buckets. A hash of the key is computed to select the
	bucket, and `pfind` is called on that bucket.

* **`Lset` (Linear Set)**:

	A `:plist` where member elements are stored as keys mapping to `:t`.

* **`Fset` (Fast Set)**:

	An array of `:plist` buckets where member elements are stored as keys mapping
	to `:t`.

### Architectural Benefits

This design yields several advantages:

* **Contiguous Memory Layout**:

	Because `:plist` is backed by a flat, contiguous vector, the collections have
	high cache locality. There is no pointer overhead for node wrappers, which
	minimizes L1/L2 cache misses.

* **Shared Optimization**:

	Any collection built on top of `:plist` automatically benefits from the O(1)
	`str_hashslot` optimization for symbol lookups.

* **Minimal Code Footprint**:

	The entire collections library is exceptionally small and maintainable, as the
	core logic of insertion, deletion, and search is centralized within `:plist`.

## 4. Comparison to `(env 1)` Lexical Environments

Lexical environments in ChrysaLisp are created as `:hmap` objects. While an
`(env 1)` instance and a `:plist` are conceptually similar, they are not
identical.

### Similarities

* **Shared Caching Engine**:

	Both structures utilize the same globally interned symbol `+str_hashslot` cache
	to resolve symbol bindings at O(1) speed.

* **Single-Bucket Structure**:

	When a local environment is created via `(env 1)`, the system instantiates a
	single-bucket `:hmap`. This eliminates the modulo math of bucket selection,
	making the lookup path functionally identical to a flat `:plist` scan.

### Differences

* **Traversable Hierarchy**:

	An `:hmap` contains a `+hmap_parent` pointer linking it to its parent scope,
	which allows the lookup engine to traverse up the lexical scope tree. A
	`:plist` is a flat, self-contained sequence with no hierarchy or parent
	pointers.

* **Bucket Distribution**:

	An `:hmap` can be configured with multiple buckets (e.g., the *compile_env*
	environment is sized with many buckets to reduce collisions). A `:plist` is
	always a single sequential list.

*   **Mutability and Compilation**:

	`:hmap` instances are typically populated dynamically at runtime as variables
	are bound and shadowed. `:plist` instances are frequently constructed as static
	literals during compilation (such as the jump tables generated by `pcase`) and
	remain immutable throughout execution.

## 5. Use of `:plist` in Symbolic Register Tracing (`trace` command)

The `trace` command uses ChrysaLisp’s `:plist` to track register states during
symbolic execution. This utility simulates program flow to calculate transitive
register clobbering (register trashing) across function calls, relying on a core
plist-backed structure: `vpmap` (register mappings).

### The `vpmap` (Register Set) Primitive

In the tracer, a `vpmap` represents a set of hardware registers. It is modeled
entirely as a `:plist` where register symbols map to themselves (e.g., `:r0 ->
:r0`) for a preserved state, or `:nil` (e.g., `:r0 -> :nil`) for a clobbered
state.

By leveraging `:plist` primitives, set algebra and clobbering states are mapped
directly to fast list operations:

* **Initialization**:

	A new register set is initialized with each register mapped to itself,
	representing a fully preserved state:

	```vdu
	(defmacro vpmap ()
		(static-qq (vpmap-copy (const (reduce (lambda (%0 %1) (pinsert %0 %1 %1)) +vp_regs (plist))))))
	```

* **Symmetric Merge (`vpmap-merge`)**:

	To combine register states from independent merging execution paths (e.g., at
	labels), `vpmap-merge` compares register states pairwise and conservatively
	marks any mismatched registers as clobbered (`:nil`):

	```vdu
	(defmacro vpmap-merge (%0 %1)
		(static-qq (reduce (lambda (%0 (%1 %2)) (if (nql (pfind %0 %1) %2) (pinsert %0 %1 :nil) %0)) (partition ,%1 2) ,%0)))
	```

* **Asymmetric Clobber Application (`vpmap-clobber`)**:

	To apply a known clobber set (a delta, such as a function call's clobbers) to
	the active `trace_map`, `vpmap-clobber` marks any register as clobbered (`:nil`)
	if its state in the callee map is not in its default self-mapped state:

	```vdu
	(defmacro vpmap-clobber (%0 %1)
		(static-qq (reduce (lambda (%0 (%1 %2)) (if (nql %1 %2) (pinsert %0 %1 :nil) %0)) (partition ,%1 2) ,%0)))
	```

* **Loop Stability (`vpmap-changed?`)**:

	To prevent infinite loops when tracing loop back-edges where registers are
	restored or swapped in the body, `vpmap-changed?` only triggers a loop
	back-edge if the active state would further widen/degrade the label state:

	```vdu
	(defmacro vpmap-changed? (%0 %1)
		(static-qq (some (lambda (v0 v1) (and (nql v1 :nil) (nql v0 v1))) ,%0 ,%1)))
	```

### The `vpmap` (Register Value Map)

During symbolic simulation, the tracer must track the origin or value of each
register to detect when a spilled register is restored. This is managed by
`vpmap`, which maps each register symbol to its current symbolic state.

* **Initialization**:

	The map is initialized with each register mapping to itself:

	```vdu
	vpmap (copy (const (reduce (# (pinsert %0 %1 %1)) +all_regs (plist))))
	```

* **State Tracking**:

	As instructions are simulated (such as `emit-cpy-rr`), `pfind` resolves the
	symbolic source, and `pinsert` updates the destination register's mapping in
	the plist:

	```vdu
	((emit-cpy-rr emit-cpy-ff)
		(def-reg (last inst) (pfind vpmap (second inst))))
	```

### Caching and Simulation Performance

Because the keys of `vpmap` are globally interned register symbols (e.g., `:r0`,
`:r1`, `:f0`), this symbolic simulator achieves high execution speeds.

During the iterations of the data-flow analysis, almost every lookup in `vpmap`
hits the `str_hashslot` cache. This minimizes the overhead of the tracer,
allowing it to calculate register-trashing behaviors across thousands of
instructions in a fraction of a second.

```file
lib/asm/regs.inc "; (plist)" ""
```

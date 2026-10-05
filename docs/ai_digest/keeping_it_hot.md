# The Anatomy of a ChrysaLisp Function Call

In conventional interpreted Lisp systems, function invocation is plagued by
significant performance bottlenecks: recursive evaluation walks, heavy heap
allocations for environment frames, hash map collision resolution, expensive
division/modulo operations, and tracing garbage collection pauses.

ChrysaLisp rejects these paradigms from first principles. By combining
fixed-size heap cell recycling, embedded inline array storage, single-bucket
lexical environments, proactive symbol hash-slot caching, and deterministic
reference counting, ChrysaLisp executes dynamic function calls almost entirely
within **hot CPU L1 cache**.

This document traces the complete round trip of a ChrysaLisp function call from
call-site dispatch to stack tear-down.

## 1. The L1 Memory Engine: High-Speed Cell Recycling

Every function invocation requires storage for its lexical environment. In
ChrysaLisp, this allocation never touches the host OS kernel or a general-purpose
heap manager during standard execution.

```
                  +-----------------------------------+
                  |      Heap Free List (LIFO)        |
                  |  [ Cell 0 ] -> [ Cell 1 ] -> ...  |
                  +-----------------------------------+
                       |                         ^
            (env-push) |                         | (env-pop)
                       v                         |
                  +-----------------------------------+
                  |         Hot L1 Data Cache         |
                  |     Active Environment Frame      |
                  +-----------------------------------+
```

### Power-of-Two Cell Heaps

*	Memory is organized by `sys_mem` and `sys_heap` into discrete heaps of
	power-of-two cell sizes (`24`, `48`, `96`, `192`, `384` ... bytes).

*	Each heap manages its blocks with a singly linked, LIFO free list
	(`+hp_heap_free_flist`).

### Instant Allocation and Cache Re-use

*	Allocating a cell (`(call :sys_heap :alloc)`) simply unlinks the head node
	from the free list.

*	Freeing a cell (`(call :sys_heap :free)`) pushes the pointer back onto the
	top of the free list.

*	Because function calls allocate and deallocate scopes in a strict
	last-in, first-out sequence, the system continuously recycles the **exact
	same physical memory addresses**.

*	These memory blocks remain permanently resident in the CPU's L1 data
	cache, eliminating cache misses and memory bus latency.

## 2. Compact Scope Representation: Embedded Inline Array Storage

Traditional systems allocate an environment record and then allocate a separate
backing array for variable bindings. ChrysaLisp eliminates this second pointer
indirection through **embedded inline storage**.

```
+-----------------------------------------------------------------------+
|                         Single Heap Cell                              |
| +------------------------------------+------------------------------+ |
| |       HMap / Object Header         |     Inline Storage Array     | |
| | (vtable, count, parent, cap, len)  | [Key 0 | Val 0 | Key 1 ... ] | |
| +------------------------------------+------------------------------+ |
+-----------------------------------------------------------------------+
```

*	The `:hmap` class inherits from `:list` and `:array`.

*	The object header reserves a dedicated section (`+hmap_elems`) mapping
	directly into the remainder of the cell allocated from the heap.

*	When an environment is allocated for a function call via `env_push`, both
	the environment header and its initial key-value slots reside in a **single
	contiguous memory block**.

*	A standard function environment requires only one cell allocation and zero
	pointer chasing to access its bindings.

## 3. Eliminating the Modulo: Single-Bucket Lexical Scopes

In standard hash table implementations, looking up a key requires calculating a
hash code and performing an integer modulo operation (`hash % num_buckets`) to
find the target bucket. On modern CPUs, integer division and modulo instructions
can take 10 to 40 cycles.

ChrysaLisp completely circumvents this overhead for execution scopes:

*	Function execution frames are instantiated with **`num_buckets = 1`**.

*	With a single bucket, the bucket selection logic collapses to a no-op,
	bypassing hashing and modulo math entirely.

*	All local bindings reside in a single contiguous list of `(key, value)`
	pairs within the inline storage of the cell.

## 4. Proactive Slot Caching: `str_hashslot` and Direct O(1) Access

A single bucket with linear scanning would typically degrade performance to
`O(N)`. ChrysaLisp achieves strict **`O(1)` access** through its self-repairing
**`str_hashslot`** symbol cache.

```
Symbol Object (:sym)
+------------------------------------+
|  ... | +str_hashslot: [ Index 2 ]  |
+------------------------------------+
                   |
                   | (Direct Index Dereference)
                   v
Environment HMap (+hmap_elems)
+------------+------------+-------------------------+------------+
| Key0, Val0 | Key1, Val1 | Key2 (:sym), Val2 (42)  | Key3, Val3 |
+------------+------------+-------------------------+------------+
```

### Proactive Cache Population

*	Symbols in ChrysaLisp are globally interned immutable strings (`:sym`).
	Every symbol contains a cached slot index field: `+str_hashslot`.

*	When a variable is bound during function entry (`env_bind` / `pinsert`),
	the engine knows the exact array offset where the binding is placed.

*	The engine **proactively writes this slot index directly into the
	interned symbol's `+str_hashslot` field**.

### The Zero-Search Read

*	When a symbol is evaluated inside the function body, `:hmap :pfind` reads the
	cached `+str_hashslot` from the symbol.

*	It multiplies the slot index by the element size and reads the key at that
	exact array offset.

*	If the key matches the symbol pointer (pointer equality), the value is
	immediately returned.

*	There is **no hash calculation, no modulo division, and no search
	iteration**. It is a direct array read.

### Self-Repairing Resolution

*	If a symbol is shadowed by an inner scope or unshadowed upon scope exit,
	the cached index may point to a stale position.

*	On a cache miss, `:hmap :pfind` performs a one-time linear scan of the
	single bucket to find the correct binding.

*	Upon finding the key, it **immediately updates the symbol's
	`+str_hashslot`** with the new index.

*	All subsequent lookups for that symbol in that scope immediately resume at
	`O(1)` speed.

## 5. The Complete Function Call Lifecycle

Here is the step-by-step execution flow when evaluating `(my-func arg0 arg1)`:

```
[ Call Site: repl_eval ]
       |
       v
[ 1. Look at the Operator ] ---> Special form (:func)? ---> [ Jump Native Code ]
       |
       v (Built in :func, or Lambda Template)
[ 2. Evaluate Arguments ]        <--- Takes an args list from the chain
       |
       +---> Built in (:func)? ---> [ Call Native Code ] ---> [ Args list back to the chain ]
       |
       v (Lambda Template)
[ 3. Take Scope (env_push) ]     <--- Takes an environment from the chain
       |
       v
[ 4. Bind Parameters (env_bind) ] ---> Sets symbol +str_hashslot
       |
       v
[ Args list back to the chain ]
       |
       v
[ 5. Execute Body (repl_progn) ]  ---> O(1) cached variable reads
       |
       v
[ 6. Give back Scope (env_pop) ]  ---> Environment back to the chain
       |
       v
[ Return Value to Caller ]
```

All of this is in line in `:lisp :repl_eval`, which is the heart of the
recursion, so the aim is not only speed, it is to hold as little as it can on
the stack over each call it makes:

| Form                          | Held on the stack, plus the return address |
|-------------------------------|--------------------------------------------|
| symbol, or evals to itself    | nothing, no call is made that recurses     |
| special form                  | nothing, it is a jump                      |
| built in, while args eval     | the form and the args list, 16 bytes       |
| built in, while it runs       | the args list, 8 bytes                     |
| lambda, while args eval       | the form and the args list, 16 bytes       |
| lambda, while its body runs   | nothing                                    |

The Lisp object, `this`, is not held on the stack at all. By contract each
method gives it back in `:r0`, and code that has lost it, after a call that
trashes every register, fetches it again from the task control block, with
`(lisp-this reg)`.

### Phase 1: Operator Resolution (`class/lisp/repl_eval.vp`)

*	The first element of the form is nearly always prebound, by `:lisp
	:repl_bind` at read time, so it is not evaluated, the engine only looks at
	what it is. The form holds it, so it needs no reference of its own, and
	no stack slot, it is found again from the form when wanted.

*	**Special form (`:func`, with type bits set):** The engine extracts the
	function pointer from `+num_value` and jumps to it, with the form as its
	args. There is no frame, and the value it returns is returned direct to
	the caller of `:repl_eval`.

*	**Built in function (`:func`):** The args are evaluated, then the
	function pointer is called direct, not through `:repl_apply`.

*	**Lambda Template (`:list`):** If the operator is a list that starts with
	the prebound `lambda`, it is a user-defined function, it would only
	evaluate to itself, so that step is skipped.

*	**Anything else:** The operator is evaluated, the general case. That
	result does hold a reference, in a third stack slot, and is applied with
	`:repl_apply`. This is rare.

### Phase 2: Argument Evaluation (`class/lisp/repl_eval.vp`)

*	The argument expressions are evaluated, each against the caller's current
	environment, straight into the args list, which takes each value as it is
	given, with no extra reference.

*	The args list is not created for the call. The Lisp object keeps a chain
	of empty lists, `+lisp_args_pool`, the link held in the first element of
	each. One is taken from it, and when the call is done, if no other has a
	reference to the list, and it has not grown past its own storage, its
	values are dropped, in line where they live on, and it is put back. Only
	the first call at each depth of nesting creates one.

*	The place in the form is not kept, it is found from the length of the args
	list so far. So the frame is just the form and the args list.

### Phase 3: Scope Instantiation (`class/lisp/env_bind.vp`)

*	`env_push` takes the new local execution environment from a second chain
	the Lisp object keeps, `+lisp_env_pool`, of empty single-bucket `:hmap`
	objects, the link held as the parent of each. Only if the chain is empty
	is one created:

	```vdu
	(call :hmap :create '(1) '(:r1))
	```

*	The new environment's `+hmap_parent` pointer is linked to the caller's
	current environment, and takes over the reference the Lisp object had to
	it.

### Phase 4: Parameter Binding (`class/lisp/env_bind.vp`)

*	`env_bind` matches formal parameters against evaluated arguments.

*	The usual call, a list of plain symbols, bound to as many values, in the
	empty environment `env_push` just gave, needs no search. Each key-value
	pair is put in place in the inline array, and the symbol's `+str_hashslot`
	is set, with no call and no stack frame.

*	Anything else, `&optional`, `&rest`, `&most`, `&ignore`, a list to
	destructure, a symbol given twice, or an environment that is not empty, as
	`(bind)` may be given, goes to the general code, which uses `:hmap
	:pinsert`.

*	Once bound, the values are held by the environment, so the args list goes
	back to its chain before the body runs.

### Phase 5: Body Evaluation (`class/lisp/lisp_progn.vp`)

*	`repl_progn` iterates over the expressions in the function body.

*	Expressions access local variables via `:hmap :get`, which hits the
	proactively cached `+str_hashslot` for instantaneous `O(1)` reads.

*	**Tail-Call Optimization:** If the body contains multiple expressions, all
	leading expressions are evaluated and discarded, with a frame of two
	iterators. The final expression is evaluated via a direct jump to
	`repl_eval`, with no frame. A body of one expression, which most are, uses
	no stack at all.

*	The flow control forms follow the same rule. `(if)` holds one slot, only
	over its test, `(while)` one slot, `(cond)` two, and the branch taken is a
	jump.

### Phase 6: Scope Tear-down (`class/lisp/env_bind.vp`)

*	When execution finishes, `env_pop` restores the parent environment.

*	If no other has a reference to the local environment, and it is still a
	single bucket in its own storage, its keys and values are dropped, in line
	where they live on, and it goes back on the chain.

*	If not, say it was captured with `(env)`, or resized, it is given a
	`deref` as any object, and is freed when its last reference goes.

*	Any temporary objects whose reference counts reach zero during the call are
	immediately freed in-place without garbage collector pauses.

## 6. The Virtuous Circle of L1 Cache Locality

ChrysaLisp's speed is the result of architectural synergy across all layers of
the OS:

1.	**Iterative Core Design:** By utilizing small task stacks (8KB) and
	enforcing iteration over recursion, lexical scopes remain shallow and flat.

2.	**Stable Cache Slots:** Flat lexical scopes ensure that symbols' cached
	`+str_hashslot` indices remain valid across many iterations, maximizing
	O(1) cache hits.

3.	**Cell Recycling:** Rapid LIFO allocation ensures that environment frames,
	strings, and temporary vectors reuse the same physical cache lines.

4.	**Micro-Engine Footprint:** The entire ChrysaLisp kernel, interpreter, and
	runtime engine fit within ~200 KB—small enough to sit entirely in the L1
	instruction cache of modern processors.

By systematically eliminating memory allocation overhead, hash collisions, and
pointer traversal, ChrysaLisp delivers execution speeds approaching compiled
languages while maintaining the flexibility of a purely dynamic, interpreted
Lisp environment.

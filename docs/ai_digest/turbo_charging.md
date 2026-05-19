# Translator Turbo Charging: Mastering O(1) Map Alignment

The ChrysaLisp compilation and translation pipeline is engineered for extreme
speed, capable of rebuilding the entire OS and libraries for 5 different
platforms in 0.38 seconds (or a single platform like ARM64 in 0.074 seconds).

Achieving this speed requires stripping away all hidden O(N) bottlenecks. A
prime example of this is how the translator manages its instruction lookups
across multiple environments. By leveraging ChrysaLisp's unique `str_hashslot`
cache mechanics, the system transforms what would normally be an O(N²)
translation pass into a strictly O(1) operation.

## The ChrysaLisp Hash Map Architecture

To understand the optimization, you must first understand how ChrysaLisp
achieves O(1) symbol lookups in its hash maps (`:hmap` / `env`):

* **Direct Indexing:** ChrysaLisp environments do not rely on traditional
  hashing algorithms at runtime. Instead, the globally interned symbol object
  itself contains a cached integer field called `str_hashslot`.

* **Proactive Cache-Setting:** When a symbol is inserted into a map (via `def`,
  `bind` or `defq`), the system immediately pushes the binding into the bucket's
  underlying array and directly updates the symbol's `str_hashslot` to that new
  array index.

* **Instant Retrieval:** When looking up a symbol, the system simply reads the
  symbol's `str_hashslot`, jumps directly to that index in the array, and
  verifies the key. If it matches, the value is returned instantly in O(1) time.
  There is no "first-time" linear scan penalty.

## The Performance Pitfall: The Cost of "Not Found"

While this caching mechanism is incredibly fast for *existing* keys, it has a
weakness when queried for *missing* keys.

* If you use `(def? symbol map)` to check if a symbol exists, and it is not in
  the map, the system experiences a cache miss.

* It is forced to perform a full linear scan of the bucket's array.

* Because the symbol is ultimately not found, there is no valid index to cache.
  The `str_hashslot` cannot be updated.

* Consequently, if you check for that same missing symbol again, it will trigger
  *another* full linear scan.

During the translator's `emit-prepass` (which scans thousands of instructions
for fusion opportunities like `LDP`/`STP` on ARM64) or the main `emit-translate`
loop, checking an instruction against a map of valid targets could result in
thousands of linear scans if the instruction doesn't belong in that map.

## The Solution: Padding with `:nil`

To prevent these linear scans, ChrysaLisp employs a highly disciplined
application of the "Know Thyself" philosophy: **shape the data to fit the
architecture.**

* Instead of only storing the instructions that actually apply to a specific
  map, the translator inserts *every single instruction* into the map.

* For instructions that are relevant (e.g., `emit-jmp` inside the `+emit_jmps`
  map), it stores the actual data or a `:t` flag.

* For instructions that are irrelevant (e.g., `emit-add-rr` inside the
  `+emit_jmps` map), it explicitly stores a `:nil` value.

* Because every instruction is now physically present in the map, querying
  `(def? op map)` always results in a cache hit. It instantly returns `:nil` in
  strictly O(1) time, completely bypassing the linear scan penalty.

## The Masterstroke: Cross-Map Index Alignment

The optimization goes one step further. During the translation loop, an
instruction symbol might be checked against multiple different maps
sequentially.

For example, `emit-translate` checks `+emit_jmps` and then `+emit_funcs`. Later,
`emit-prepass` in ARM64 checks `+arm64_fuse_map`.

If the symbol `emit-add-rr` lived at index `42` in `+emit_jmps` but index `15`
in `+emit_funcs`, querying them sequentially would cause the `str_hashslot`
cache to thrash. It would constantly overwrite the cached index, causing linear
scans on every check.

To solve this, ChrysaLisp perfectly aligns the underlying memory layout of all
the maps:

* **Single-Bucket Maps:** The maps are initialized as `(env 1)`. This tells the
  `:hmap` to use exactly 1 bucket, eliminating any hashing or modulo division
  overhead. The map is effectively just a flat array.

* **Identical Insertion Order:** The translator gathers a master list of all
  emit functions (`emit_list` or `+emit_funcs`). Every single map is then
  populated by iterating over this exact same master list.

* Because the maps have a single bucket, and because they are populated with the
  exact same keys in the exact same order, **a symbol will possess the exact
  same array index across all maps**.

## Step-by-Step Execution

Here is how this sequence plays out in the code:

### 1. Building the Master List (`vp.inc`)

The system gathers every emit function into a single, definitive map
(`+emit_funcs`):

```vdu
(defq +emit_funcs (reduce (lambda (e (k v)) (if (starts-with "emit-" k) (def e k v)) e)
	(tolist (env)) (env 1)))
```

### 2. Building the Jumps Map (`vp.inc`)

The jumps map is built by iterating directly over the master list. Non-jump
instructions are inserted, but their `find` evaluates to `:nil`:

```vdu
(defq +emit_jmps (reduce (lambda (e (k v)) (def e k (find k
		'(emit-jmp emit-beq-cr ...)))) e)
	(tolist +emit_funcs) (env 1))
```

### 3. Building the Architecture-Specific Map (`arm64.inc`)

The ARM64 backend gathers the master list again, appending its own specific
instructions, and uses `reduce!` to populate its fusion map in the exact same
order:

```vdu
(defq emit_list (merge (filter (# (starts-with "emit-" %0)) ...) '(emit-arm64-ldp ...))
	+arm64_fuse_map
		(reduce (#
			(def %0 %1 (when (defq i (find %1 '(emit-cpy-ir ...)))
				... fusion data ...)) %0)
			emit_list (env 1)))
```

## The Result

Through this careful orchestration, the sequence of execution for a symbol like
`emit-add-rr` is flawlessly optimized:

* `emit-add-rr` is `def`'d into `+emit_funcs` at index `42`. Its global
  `str_hashslot` is instantly set to `42`.

* It is `def`'d into `+emit_jmps` (as `:nil`) at index `42`. Its `str_hashslot`
  is overwritten with `42`.

* It is `def`'d into `+arm64_fuse_map` (as `:nil`) at index `42`. Its
  `str_hashslot` is overwritten with `42`.

* During compilation, when `(def? 'emit-add-rr +arm64_fuse_map)` is evaluated,
  the `:hmap` looks at the symbol's cache, sees `42`, checks index `42` in the
  array, finds the match, and returns `:nil`.

* Zero cache misses. Zero linear scans. Absolute O(1) performance across
  millions of instruction evaluations.

## Conclusion

This technique beautifully demonstrates the ChrysaLisp philosophy. Rather than
building a complex, heavy runtime to profile and optimize map lookups
heuristically, the developer is given simple, transparent, hyper-fast
primitives.

By writing code that is intimately aware of how those primitives physically
operate in memory, the developer can arrange their data to exploit the
architecture, achieving bare-metal C performance entirely from within a
high-level Lisp environment.
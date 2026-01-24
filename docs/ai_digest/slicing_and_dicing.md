# Deep Analysis: Vectorized Slicing and Dicing in ChrysaLisp

This document expands upon previous analysis. It is crucial to understand that
**ChrysaLisp is fundamentally a vector-based Lisp**. Unlike traditional Lisps
where a `list` is a chain of cons cells (linked list), a ChrysaLisp `:list`
inherits from `:array`.

Both `:str` (byte strings) and `:list` (object vectors) are contiguous blocks of
memory. This architecture dictates that sequence manipulation is not done via
pointer swapping (`set-cdr!`), but through high-performance memory block
operations (`memcpy`, `memmove`).

The system relies on a trinity of primitives—**cat**, **slice**, and
**splice**—which serve as the foundation for the standard library macros and the
advanced Regular Expression replacement compiler.

## 1. The Core Primitives

These functions are polymorphic methods defined on the `:seq` (Sequence) class.
Because `:list`, `:array`, `:nums`, `:fixeds`, and `:reals` all share the
underlying contiguous memory structure of `:array`, they share the exact same
O(N) performance characteristics as `:str`.

### 1.1 `cat` (Concatenate)

**Signature:** `(cat seq ...)`

**VP Implementation:** `class/array/class.vp` -> `:cat`

`cat` joins multiple sequences into a new sequence of the same type.

* **Mechanism:**

    1. **Measure:** Iterates inputs to calculate total element count.

    2. **Allocate:** Allocates a single contiguous memory block for the result.

    3. **Blit:** Performs a linear copy of each source buffer into the
       destination buffer.

* **Vector implication:** Unlike `append` in a linked-list Lisp (which might
  just link the last cdr of list A to list B), `cat` here creates a brand new
  vector. It is strictly functional (immutable inputs, new output).

### 1.2 `slice` (Subsequence)

**Signature:** `(slice seq start end)`

**VP Implementation:** `class/array/class.vp` -> `:slice`

`slice` creates a new vector containing a copy of elements from `start`
(inclusive) to `end` (exclusive).

* **Pointer Arithmetic:** Because the data is contiguous, finding the start
  point is an O(1) multiplication (`start * element_size`), not an O(N)
  traversal.

* **Memory Copy:** It allocates `(end - start) * element_size` bytes and
  performs a single `memcpy`.

* **Directionality:** If `start` > `end`, the slice is returned **reversed**.
  This is done during the copy pass, eliminating the need for a separate
  post-allocation reverse step.

### 1.3 `splice` (The Scatter/Gather Engine)

**Signature:** `(splice src1 src2 index_vector)`

**VP Implementation:** `class/array/class.vp` -> `:splice`

This is the topological workhorse of ChrysaLisp. It constructs a new vector by
alternating between chunks of `src1` and `src2` based on the `index_vector`.

* **The Index Vector:** A `:nums` object (vector of integers) containing pairs
  `(start end)`.

    * **Even Indices (0, 2..):** Ranges from `src1`.

    * **Odd Indices (1, 3..):** Ranges from `src2`.

* **Vector Efficiency:** Because lists are vectors, `splice` does not need to
  traverse nodes to find split points. It jumps directly to memory offsets.

* **Execution:**

    1. **Pre-flight:** Sums the lengths of all defined ranges in the
       `index_vector`.

    2. **Allocate:** Allocates the exact result buffer.

    3. **Gather:** Loops through the index vector, `memcpy`ing chunks from
       `src1` and `src2` directly into the result.

**Implication for Lists:**

In a linked-list Lisp, splicing often involves mutating pointers (O(1) if you
have the cons cell). In ChrysaLisp, `splice` allows you to construct a modified
version of a list without mutating the original, and because of CPU cache
locality and hardware `memcpy` optimization, this is often faster than pointer
chasing for small to medium lists.

## 2. The Macro Layer

ChrysaLisp uses macros to map high-level editing concepts (insert, erase) into
specific `splice` topological vectors. These macros work identically for strings
and lists.

Definitions found in `class/lisp/root.inc`.

### 2.1 `erase` (Delete range)

**Code:**

```vdu
(defmacro erase (s b e)
    (static-qq (splice ,s ,s (nums 0 ,b ,e -1))))
```

* **Topological Map:** `[0...b]` from Source 1, `[e...EOF]` from Source 1.

* **Vector Op:** This effectively memmoves the tail of the vector over the
  deleted section into a new buffer.

### 2.2 `insert` (Inject data)

**Code:**

```vdu
(defmacro insert (s p i)
    (static-qq (splice ,s ,i (nums 0 ,p 0 -1 ,p -1))))
```

* **Topological Map:**

    1. `src1` (Original): `0` to `p`.

    2. `src2` (Insertion): `0` to `-1` (All).

    3. `src1` (Original): `p` to `-1`.

* **Performance:** A single allocation and three `memcpy` operations.

### 2.3 `replace` (Overwrite range)

**Code:**

```vdu
(defmacro replace (s b e i)
    (static-qq (splice ,s ,i (nums 0 ,b 0 -1 ,e -1))))
```

* **Topological Map:** Prefix of `s`, entire `i`, Suffix of `s`.

### 2.4 `rotate` (Permute ranges)

**Code:**

```vdu
(defmacro rotate (s i j k)
    (static-qq (splice ,s ,s (nums 0 ,i ,j ,k ,i ,j ,k -1))))
```

* **Topological Map:**

    1. `0..i` (Head)

    2. `j..k` (The middle section moved forward)

    3. `i..j` (The start section moved backward)

    4. `k..-1` (Tail)

* **Significance:** Performing a rotation on a linked list requires changing
  multiple pointer links and potentially traversing to finding `k`. On a vector,
  this is simply copying 4 memory blocks into a new container.

## 3. The Search & Replace Compiler

Located in `lib/text/searching.inc`. This system exploits the vector nature of
ChrysaLisp strings to perform global Regex replacements in linear time with a
single allocation.

### 3.1 `replace-compile`

Compiles a replacement template (e.g., `"Value: $1"`) into a vector "blueprint".

* **Output:** A `z_nums` (Zero Nums) vector. This acts as a pre-allocated
  structure for the `splice` index vector.

### 3.2 `replace-matches`

Because strings are vectors, we cannot insert text in-place efficiently.
`replace-matches` solves this by calculating the topology of the *entire* final
string before creating it.

**The Algorithm:**

1. **Scan:** Regex search finds all match indices (start/end points).

2. **Vector Construction:** It builds a massive `index_vector` for `splice`.

    * It identifies ranges of the *original* text to keep (between matches).

    * It patches the `z_nums` blueprint with the indices of capture groups found
      in the match.

3. **Execution:** `(splice text replacement_template huge_index_vector)`

    * `src1`: The original text.

    * `src2`: The replacement template string.

    * `index_vector`: A map interleaving chunks of original text with chunks of
      the template (or capture groups from the original text).

**Why Vectors Win Here:**

If this were a linked-list of characters, this operation would be horrific. As
vectors, the `splice` command effectively becomes a "Scatter-Gather DMA"
operation for the CPU, streaming data from two source pointers into one
destination pointer sequentially.

## 4. Interaction with Rosinante (Iterators)

The "Rosinante" primitives (`each`, `map`, `filter`, `reduce`) are designed to
work over these contiguous memory blocks.

### 4.1 The Cost of `rest`

In a traditional linked-list Lisp, `(rest list)` (or `cdr`) is O(1)—it just
returns the pointer to the next cell.

In ChrysaLisp, `(rest list)` calls `(slice list 1 -1)`.

* **Cost:** O(N). It allocates a new vector and copies the data.

* **Impact:** Recursive algorithms that consume a list one item at a time (`func
  (first list) (recurse (rest list))`) are **O(N^2)** in ChrysaLisp (Triangle
  number memory usage).

### 4.2 The Solution: Iterators and Indices

To compliment the vector architecture, Rosinante iterators do not consume the
list via `rest`. They iterate an **index** or a **pointer** across the
contiguous memory block.

* **`each`:** Iterates a pointer from `array_begin` to `array_begin + length`.
  Zero allocation.

* **`map`:** Allocates the result vector *once* (size is known), then iterates
  pointers filling it.

### 4.3 Slicing as "Windowing"

When you need to process a sub-section of a list, the iteration primitives all
multiple sequences and take optional slice indices ! Even allow reverse
iteration of multi sequences.

See the document specifically on the "Rosinante" primitives for all the details.

```vdu
(each! print (list my_list my_nums my_strings) 20 10)
```

## 5. Summary

ChrysaLisp's identity as a **Vector Lisp** fundamentally changes the performance
profile of sequence operations.

1. **Identity:** Lists are Arrays. Arrays are Memory Blocks.

2. **`splice`:** The universal primitive for reshaping memory blocks. It allows
   complex permutations (insert, delete, rotate, regex-replace) to be expressed
   as a single allocation and copy pass.

3. **Efficiency:** It trades the O(1) insert/delete of linked lists for the
   cache-friendly, pre-fetchable, linear access patterns of vectors.

4. **Macros:** The high-level editing macros (`insert`, `erase`) serve to
   abstract the complexity of generating `splice` vectors, providing a familiar
   Lisp API over a flat-memory architecture.
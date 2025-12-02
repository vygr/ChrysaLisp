# ChrysaLisp Sequence Indexing & Memory Model

In ChrysaLisp, `seq` is the base class for all ordered collections, including
`array`, `list`, `str`, `nums`, `fixeds`, and `reals`. Because they share this
lineage, they share a unified set of manipulation functions and a unique
indexing logic designed for high-performance string and buffer manipulation.

## The Golden Rule of Slicing

To understand ChrysaLisp indexing, you only need to answer one question: **"How
do I slice a sequence to remove just the first and last elements, without
calculating its length?"**

The answer defines the system:

```vdu
(slice seq 1 -2)
```

This single line implies the three fundamental rules of the coordinate system:

1. **0** is the **Start** of the sequence.

2. **-1** is the **End Boundary** (the Length), *not* the last element.

3. **-2** is the **Last Element**.

## The Index Number Line

Unlike many languages where `-1` represents the last item, ChrysaLisp uses `-1`
to represent the "Fence Post" immediately following the last item. This allows
it to serve as an exclusive upper bound for slices.

Given a sequence of length **5** (e.g., "HELLO"):

```
Item:		 H	 E	 L	 L	 O	 <Boundary>
Positive:	 0	 1	 2	 3	 4	 5
Negative:	-6	-5	-4	-3	-2	-1
```

### The Resolution Formula

When the system receives a negative index, it resolves the absolute index using
this logic: RealIndex = Length + InputIndex + 1.

* Input `-1`: 5 + (-1) + 1 = 5 (The Length / End Boundary).

* Input `-2`: 5 + (-2) + 1 = 4 (The Last Item).

## Slicing and Reference Optimization

The `(slice seq start end)` function returns a sequence representing the
requested range. The `start` is inclusive, and `end` is exclusive.

### The Optimization: Reference vs. Copy

A critical feature of ChrysaLisp's `slice` implementation is **Identity
Detection**.

If you request a slice that covers the entire sequence range:

```vdu
(slice seq 0 -1)
```

The system detects that the result is identical to the source. Instead of
allocating new memory and copying data, it simply **increments the reference
count** of the original object and returns it.

### Forcing a Copy

Because `slice` may return a reference, if you strictly require a new, distinct
copy of a sequence (e.g., to modify it without affecting the original), you must
use `cat`:

```vdu
(defq my-copy (cat seq))
```

`(cat)` with a single argument guarantees the creation of a new container with
the same content.

### Common Slicing Idioms

* **Identity (Ref):** `(slice seq 0 -1)`

* **Trim First/Last:** `(slice seq 1 -2)`

* **Head (First 3):** `(slice seq 0 3)`

* **Tail (All but first):** `(slice seq 1 -1)`

* **Empty (No elements, but same type):** `(slice seq 0 0)`

## Search and Reverse Search

The search functions are engineered to return indices that feed directly into
`slice` as arguments, eliminating the need for `inc` or `dec` math.

### `find` (Forward Search)

Scans from left to right.

* **Returns:** The index of the **start** of the match.

* **Slicing utility:** Perfect for the `start` argument.

### `rfind` (Reverse Search)

Scans from right to left.

* **Returns:** The index of the element **immediately following** the match.

* **Slicing utility:** Perfect for the `end` argument.

**Example: Parsing a File Path**

Consider: `(defq path "home/user/dev/main.lisp")`

1.  **Get the Filename:**

	We want everything *after* the last slash. `rfind` searches backwards for `/`.
	It finds it at index 13, but returns **14** (the index of 'm').

	```lisp
	(defq path "lib/asm/file.inc")
	(slice path (rfind "/" path) -1)
	```

2.  **Get the Directory:**

	We want everything *up to* the last slash.

	```lisp
	(defq path "lib/asm/file.inc")
	(slice path 0 (dec (rfind "/" path)))
	```

## Element Access

When accessing a single element via `(elem-get seq idx)`, the index must point
to an actual item, not a boundary.

*   **(first seq)** is:

	```vdu
	(elem-get seq 0)
	```

*   **(last seq)** is:

	```vdu
	(elem-get seq -2)
	```

*Warning: Attempting `(elem-get seq -1)` is an Out of Bounds error, because `-1`
resolves to the slot after the data ends.*

## Iteration: The `(!)` Operator

When iterating using high-level functions like `(map!)`, `(each!)`, or
`(filter!)`, the current index operator `(!)` provides the **absolute hard
index**.

* It always returns a positive integer.

* It starts at 0.

* It allows you to cross-reference other arrays/lists during iteration without
  calculating relative offsets.

```lisp
(map (lambda (item)
	(list (cat item " ->") (!)))
	"test")
```

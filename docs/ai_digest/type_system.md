# The ChrysaLisp Type System: Architecture of a Zero-Overhead Metacircular Interpreter

The ChrysaLisp type system is a masterclass in first-principles software
engineering, perfectly encapsulating the system's core philosophies: "Know
Thyself" and "Well, Don't Do That Then!".

It achieves a seemingly impossible feat: a complete rebuild of the entire OS and
libraries from source in under 0.1 seconds. Incredibly, this worst-case metric
occurs in `*build_mode* 1` (debug mode), where the Lisp interpreter dynamically
type-checks every function call signature at runtime. In `*build_mode* 0`
(release mode), this entire type-checking apparatus compiles completely out of
existence, yielding bare-metal speeds.

The secret to this performance lies in rejecting traditional Run-Time Type
Information (RTTI) and bloated object headers in favor of radically simple,
hyper-optimized primitives that map directly to the Virtual Processor (VP)
architecture.

Here is a detailed breakdown of how the ChrysaLisp type system is engineered.

## The Object-VTable-Parent Triad: O(d) Type Resolution

Traditional dynamic type systems often rely on complex hash maps, deep metadata
structures, or expensive runtime lookups to determine if an object is an
instance of a specific class. ChrysaLisp sidesteps this completely using a
universal, zero-overhead memory layout.

* **Universal Slot 0 (`+obj_vtable`):** Every single VP object reserves its very
  first field (slot 0) as a pointer to its class vtable. This dictates the
  object's type.

* **The VTable Parent Pointer:** Crucially, slot 0 of the *vtable itself* is a
  pointer to its *parent's* vtable.

* **The Root Class:** The hierarchy terminates cleanly at the root `:obj` class,
  whose vtable slot 0 simply contains a `0` (null pointer).

Because of this strict geometric arrangement, the path from an object instance
all the way to the root of the inheritance tree is just a simple singly-linked
list.

When the VP needs to perform a type check (via the `class/obj/inst_of` method),
it does not execute a complex algorithm. It simply walks this linked list,
performing rapid pointer comparisons. Because inheritance trees in ChrysaLisp
are engineered to be shallow, this traversal takes only a few CPU cycles and
operates entirely within hot L1 cache data, making dynamic subtyping
exceptionally fast.

## VTables as Code-less Functions

A fascinating manifestation of the "Know Thyself" philosophy is how ChrysaLisp
implements the vtables themselves. Rather than inventing a bespoke data
structure for class method dispatch, ChrysaLisp reuses its existing executable
binary format.

* **Link Tables without Code:** A VP class vtable is, structurally, just a
  standard VP function. It shares the exact same `fn_header` format.

* **Native Linker Integration:** The only difference is that a vtable has no
  code section. The link pointer table naturally sits at the start of the
  "function."

* **Trivial Resolution:** Because they are structurally identical to functions,
  the boot linker (`sys/load/init`) doesn't need to care about the difference
  between a class type and an executable routine. It resolves vtable
  dependencies with the exact same linker logic used for function calls.

## Compile-Time Signatures and Runtime Resolution

When operating in debug mode, functions validate their arguments against
declared signatures (e.g., `(signature '(:str :num))`).

* **Static Offsets:** The VP assembler converts these signature declarations
  into an array of 16-bit relative offsets pointing into the VP function's link
  table.

* **Linkerless Predictability:** Because ChrysaLisp is effectively "linkerless"
  (code sections never change size after compilation), these relative offsets
  are perfectly known at VP assembly time.

* **Fast Runtime Evaluation:** When a method like `:lisp :env_args_sig` or
  `:lisp :env_args_type` executes, it simply walks this compact signature block,
  uses the 16-bit offset to find the absolute link entry address, and passes
  that directly into the `:obj :inst_of` linked-list traversal mentioned above.

## The `errorcase` Macro: Zero-Cost Abstractions

The magic behind the system's dual-mode performance is the ubiquitous
`errorcase` (and `validatecase`) macro, which physically removes the type system
from the execution path in production environments.

* **Debug Mode (`*build_mode* 1`):** The `errorcase` macro expands its contents
  normally. Signature arrays are embedded in the binary, and argument
  verification logic is compiled into the VP assembler streams, providing robust
  safety during development.

* **Release Mode (`*build_mode* 0`):** The `errorcase` macro expands to nothing.
  The entire Abstract Syntax Tree (AST) node representing the type
  check—including the generation of the `(signature ...)` block itself—is
  completely ignored by the Lisp parser and VP compiler.

The result is a codebase that guarantees type safety when requested, but incurs
literally zero bytes of memory overhead and zero cycles of CPU overhead in
deployment.

## Allocation-Free Type Vectors

When Lisp code queries an object's type using the `(type-of)` predicate, the
system returns a list of static symbols representing the inheritance chain
(e.g., `'(:obj :str :sym)`). In traditional Lisp runtimes, returning a dynamic
list requires hitting the heap allocator, generating memory fragmentation and
triggering garbage collection.

ChrysaLisp implements this without external allocations:

* **Flexible Base Blocks:** The `:list` class inherits from the `:array` class.

* **Inline Capacity:** The `:array` class is designed to utilize the remaining
  padding space within its base memory heap block as internal storage space
  before it ever attempts an external allocation.

* **Zero-Allocation Execution:** There is room for 8 elements inline. Because VP
  inheritance chains are essentially never deeper than 8 levels, the type vector
  generated by `(type-of)` fits entirely inside the base object footprint.

## Bridging VP-Level and Script-Level Inheritance

ChrysaLisp features two distinct architectural realms: the underlying bare-metal
VP classes (implemented in `.vp` assembler) and the Script/GUI level classes
(implemented in Lisp via `hmap` vtable composition). The type system gracefully
unites these two domains.

* **VP-Level `(type-of)`:** When you call the standard Lisp `(type-of)`
  predicate on a Lisp-level object, it queries the underlying VP implementation.
  This returns the native VP inheritance chain.

* **Script-Level `(. :type_of)`:** The `lib/class/class.inc` library defines a
  script-level `:type_of` method on Lisp objects. When invoked, this method
  calls `(.super this :type_of)` to get the base VP types, and then elegantly
  pushes its own Lisp-level class name onto the end of the list.

The result is a unified type inspector. For example, querying a GUI `Button`
object reveals exactly how the system bridges native memory structures and
high-level Lisp compositions:

```vdu
; Querying the native VP memory structure
(type-of (Button))
-> (:hset :hmap)

; Querying the full unified type hierarchy
(. (Button) :type_of)
-> (:hset :hmap :View :Label :Button)
```

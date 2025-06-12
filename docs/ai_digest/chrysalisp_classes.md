# The Dual VTable: A Study of ChrysaLisp's Static and Dynamic Object Models

ChrysaLisp is an operating system and language built on a unique and highly
efficient object-oriented foundation. A key to its design is the use of two
distinct yet conceptually related class systems, each employing a Virtual Method
Table (vtable) for polymorphism. The first system, operating at the low-level
Virtual Processor (VP) layer, is a static, pre-compiled model optimized for the
raw performance required by an OS kernel. The second, operating at the
high-level "Script" layer, is a fully dynamic Lisp model that, through a
brilliant caching strategy, achieves the same O(1) performance characteristics
as its static counterpart while unlocking powerful runtime capabilities like
live code modification and mixins. This document provides a detailed study of
these two parallel systems, illustrating how ChrysaLisp delivers the best of
both worlds without compromise.

## The VP-Level Class System: The Compiled "Engine"

The foundation of ChrysaLisp is its kernel, built from a set of high-performance
primitives for memory management (`sys_mem`), task scheduling (`sys_task`), and
inter-process communication (`sys_mail`). These core components are implemented
using the VP-level class system, a framework designed for speed, predictability,
and minimal overhead, analogous to C++.

**Implementation and VTable Structure**

At compile time, the ChrysaLisp assembler processes class definitions written
using macros like `def-class` and `dec-method`. It generates a vtable for each
class that is a simple, **static array of function pointers**, stored in the
final executable boot image. The layout of this array is fixed; each virtual
method corresponds to a specific, known offset within the table.

An object instance at this level is a simple C-style `struct` in memory. The
first field of this struct is a pointer to its class's static vtable.

**Inheritance and Layout Compatibility**

Inheritance in the VP system is a compile-time process that ensures layout
compatibility. When a subclass is defined, the assembler first copies the vtable
layout from the parent class. It then overwrites the function pointer slots for
any methods the subclass overrides. This guarantees that a method like `:start`
is at the same offset in the vtable for both a base class and its descendants,
making polymorphic calls safe and direct. Declaring a virtual method in a base
class effectively reserves a slot at a fixed offset for all subclasses to use.

**Method Dispatch: The Speed of a Fixed Offset**

When a virtual method is called on a VP-level object, the dispatch is extremely
fast:

1.  The system retrieves the `vtable` pointer from the object's header (offset
    0).

2.  It adds the fixed, compile-time offset for the desired method to this
    pointer.

3.  It performs an indirect jump to the resulting function address.

This process is mechanically identical to a C++ virtual function call, carrying
almost no overhead. It provides the performance necessary for the core "Engine"
of the operating system.

## The Lisp-Level Class System: The Dynamic "Script"

Built atop this high-performance engine is the dynamic Lisp environment where
applications are written. This "Script" layer uses its own class system that
implements the same *concept* of vtable-based polymorphism but with a completely
different, radically more flexible implementation.

**Implementation and Object Representation**

In the Lisp world, an object is not a `struct` with a fixed layout; it **is an
`hmap`** (hash map). All of its properties, from `:color` and `:text` in a GUI
widget to instance variables, are key-value pairs within this `hmap`.

The vtable for a Lisp class is **also an `hmap`**. When a class is defined at
runtime with `(defclass Button ...)` a global `hmap` named `*class_Button*` is
created to serve as its vtable. Methods defined with `(defmethod :draw ...)` are
inserted into this `hmap` as a key-value pair, where the key is the symbol
`:draw` and the value is the compiled lambda function for the method body.

**Inheritance: Compile-Time Composition**

Lisp-level class inheritance is a powerful act of **compile-time composition**,
not runtime traversal. When `(defclass Button () (Label))` is executed, the
`defclass` macro performs these steps:

1.  It creates a new, empty `hmap` to be the vtable for `Button`
    (`*class_Button*`).

2.  It **copies all key-value pairs** from the parent's vtable (`*class_Label*`)
    into `*class_Button*`.

3.  It then inserts or overwrites methods defined specifically within the
    `Button` class body.

The resulting `*class_Button*` vtable is a "flattened," self-contained `hmap`
that includes all necessary methods, both its own and those inherited. This
makes method dispatch extremely fast.

**Method Dispatch: The Magic of O(1) Dynamic Lookup**

ChrysaLisp's dynamic dispatch is deceptively fast, achieving O(1) performance
through the **proactive `str_hashslot` caching mechanism**.

1.  **Proactive Cache Setting:** When any key (a symbol) is inserted into any
    `hmap`—be it a class vtable via `defmethod` or an object property via
    `def`—the `hmap :insert` function **immediately writes the index of that
    key's location within the `hmap`'s internal bucket into the global symbol's
    `str_hashslot` field.** This cache is set at definition time, not on first
    use.

2.  **The `mcall` (`.`) Dispatch:** A call like `(. my_button :draw)` is a
    two-step O(1) process:

    * **Find the VTable:** The system needs the object's vtable. By a strict,
        low-level convention, the `:vtable` key is **always the first entry** in
        any Lisp object's `hmap`. Its `str_hashslot` is therefore always `0`.
        The lookup for the vtable is a direct, O(1) indexed read to slot 0 of
        the object's `hmap`, which returns a pointer to the `*class_Button*`
        `hmap`.

    * **Find the Method:** The system then searches the `*class_Button*` `hmap`
        for the key `:draw`. The `str_hashslot` on the `:draw` symbol was also
        proactively set when the class was defined. This second lookup is also a
        direct, O(1) indexed read to the correct slot, which returns the
        method's lambda function.

The result is a fully dynamic, hash-map-based object system that achieves the
O(1) method dispatch performance of a static, compiled language.

## The Magic Spin-Off: Runtime Flexibility Without a Performance Penalty

Because the script-level vtable is a mutable `hmap` and dispatch is resolved at
runtime, ChrysaLisp's application layer gains immense power.

*   **Live Code Modification (Monkey Patching):** A developer can connect to a
    running system and redefine a method simply by calling
    `(def *class_Button* :draw <new_lambda>)`. This `def` (which is
    `hmap :insert`) will update the class `hmap` and, crucially, update the
    `str_hashslot` on the `:draw` symbol to point to its new location. The next
    time any button is drawn, `mcall` will instantly and efficiently find and
    execute the new code.

*   **Dynamic Mixins:** This power extends to modifying the inheritance chain
    itself. A developer can create a new composite vtable for a single object
    instance at runtime by creating a new `hmap`, copying methods into it from
    multiple source classes, and assigning this new vtable to an object. The
    caching mechanism adapts seamlessly, and performance remains O(1).

## Conclusion

ChrysaLisp's dual vtable architecture is a masterstroke of pragmatic
engineering. It does not force a single object model on the developer but
provides the right tool for each layer of the system.

*   The **VP-level "Engine"** uses a **static vtable array** for maximum
    performance and predictability in core OS functions.

*   The **Lisp-level "Script"** uses a **dynamic `hmap` vtable** built via
    **compile-time composition** for maximum flexibility in applications.

Crucially, through a proactive caching system (`str_hashslot`), the dynamic
script layer achieves the same O(1) lookup performance as the static engine.
This allows developers to employ powerful, high-level programming patterns like
live patching and dynamic mixins without the performance penalties typically
associated with such features. It is a system that elegantly unifies the
concepts of compilation and interpretation, providing both raw power and dynamic
grace.

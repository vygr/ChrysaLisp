# ChrysaLisp

ChrysaLisp is built from functions. Ok, so that's just about the most obvious
thing to say about any computer program but they are organised in a slightly
different way than other systems.

They are not bound into larger groupings such as libraries. More often than not
ChrysaLisp functions are collected into classes. Classes implement a general
concept and the methods of that class are individual related but unbound
functions.

All external function calling goes via indirect calls through the local
function Vtable. Local subroutines use standard relative call instructions.

Even a class Vtable is implemented as a function that has no code but just a
local Vtable !

## Function format

Functions consist of 7 sections, header, code, string pool, atom table,
external function Vtable, atom string pool and external function path string
pool. In some circumstances these sections may not be present, for example in
the case of a class Vtable the code section is empty, plus a string pool or
table section may be empty if there are no entries required. When a function is
in bound format sections such as the path and or atom string pool may have been
stripped after binding in order to save memory space.

The total size of a function and the start of the code, atom table and path
Vtable sections are aligned to a ptr_size.

### Header fields

* ulong/ptr ln_fnode: In a bound function the address of the next function in
the function list. When unbound -1.

* ushort fn_header_length: Total length of the function in bytes.

* ushort fn_header_entry: Offset in bytes to the code entry point.

* ushort fn_header_atoms: Offset in bytes to the atom table. Atom table entries
are ptr_size. In unbound format they are relative offsets to entries in the
atom string pool. When bound and resolved by the atom intern functions they are
pointers to symbol objects.

* ushort fn_header_links: Offset in bytes to the external function Vtable.
Vtable entries are ptr_size. In unbound format they are relative offsets to
entries in the path pool. In bound format they are pointers to the code section
of the external functions.

* ushort fn_header_syms: Offset in bytes to the atom string pool. 0 terminated
C style strings. After interning of the atom symbols this offset is set to 0.
Resolution of the atom symbols takes place via calling 'symbol 'intern_atoms.
Any function that uses atoms must call this method at least once, subsequent
calls will detect, via this offset now being 0, that resolution has already
happened.

* ushort fn_header_paths: Offset in bytes to the path string pool. 0 terminated
C style strings.

* ushort fn_header_stack: Stack amount in bytes required by function. Currently
this is a default value for all unless specified as an optional argument via
def-func. A process main function uses this value to allocate the initial stack
requirement but in future a debug mode stack check will check at each function
entry point that there is enough stack remaining for the function invocation.

* offset fn_header_pathname: The actual pathname of the function. 0 terminated
C style string, followed by padding bytes, of at least a single byte, of the
offset from the code section to the string start.

## Low level Vtable, string, and atom creation, and calling/jumping

You do not normally need to use these low level features, but higher level
calling and binding operations do. These are the Lisp functions that create
entries in the local Vtable, add strings to the local string pool, add atom
references to the local atom pool etc. They de-duplicate and in special
situations allow duplication where that is essential to create a class Vtable
correctly.

### fn-add-string

Add a string to the string pool, if not already present, and return its unique
index value.

### fn-add-sym

Add an atom string to the atom pool, if not already present, and return its
unique index value.

### fn-add-path

Add an external function path to the path pool, if not already present, and
return its unique index.

### fn-add-link

Push an entry onto the Vtable, referring to a path added using fn-add-path.

### fn-add-atom

Push an entry onto the atom table, referring to an atom string added using
fn-add-sym.

### fn-find-link

Search the current Vtable for a path and add a new entry if needed using
fn-add-link. Return the unique index.

### fn-find-atom

Search the current atom table for a atom string and add a new entry if needed
using fn-add-atom. Return the unique index.

### fn-string

Load register with string address via the local string pool.

### fn-atom

Load register with interned atom string objject via the local atom table.

### fn-bind

Load register with external function address via the local Vtable.

### fn-call

Call external function address indirectly via the local Vtable.

### fn-jmp

Jump to external function address indirectly via the local Vtable.

## Higher level function calling

### f-call

Call method statically.

### f-jmp

Jump to method statically.

### f-bind

Bind to method statically.

### s-call

Super call method statically.

### s-jmp

Super jump to method statically.

### s-bind

Super bind to method statically.

### v-call

Virtual call method.

### v-jmp

Virtual jump to method.

### v-bind

Virtual bind to method.

### d-call

De-Virtualise call method.

### d-jmp

De-Virtualise jump to method.

### d-bind

De-Virtualise bind to method.

### r-call

Register dispatched call method.

### l-call

Local subroutine call.

## Function info helpers

### f-path

Return method function path.

### s-path

Return super method function path.

## Function entry and exit helpers

### f-entry

Method entry args.

### f-exit

Method exit args.

### l-entry

Local subroutine entry args.

### l-exit

Local subroutine exit args.

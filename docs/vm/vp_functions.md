# VP Functions

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

Functions consist of 6 sections, header, code, string pool, constant pool,
external function Vtable and external function path string pool. In some
circumstances these sections may not be present, for example in the case of a
class Vtable the code section is empty, plus a string pool, constant pool or
table section may be empty if there are no entries required. The constant pool
is the words `(fn-const)` pooled, constants the code reads where it can not
hold them in an instruction. When a function is in bound format sections such
as the path string pool may have been stripped after binding in order to save
memory space.

```image
docs/diagrams/vp_function.cwb
```

The total size of a function and the start of the code, constant pool and
Vtable sections are aligned to a `+ptr_size`. The offsets in the header are 16
bits, so a function is at most 64KB, and the translator throws at one larger.

### Header fields

```vdu
(structure +fn_header +ln_fnode_size
	(ushort length)
	(ushort entry)
	(ushort links)
	(ushort paths)
	(ushort stack)
	(offset pathname))
```

* `long/ptr ln_fnode`: In a bound function the address of the next function in
the function list. When unbound -1.

* `ushort +fn_header_length`: Total length of the function in bytes, including
header, code and extra data.

* `ushort +fn_header_entry`: Offset in bytes, from the header start, to the
code entry point.

* `ushort +fn_header_links`: Offset in bytes, from the header start, to the
external function Vtable. Vtable entries are `+ptr_size`. In unbound format
they are relative offsets to entries in the path pool. In bound format they are
pointers to the code section of the external functions.

* `ushort +fn_header_paths`: Offset in bytes, from the header start, to the
path string pool. 0 terminated C style strings.

* `ushort +fn_header_stack`: Stack amount in bytes required by function.
Currently this is a default value for all unless specified as an optional
argument via `(def-func)`. A tasks main function uses this value to allocate
the initial stack requirement. In a `validate` build each function but the
`sys/` ones and Vtables calls `:sys_task :stack` at its entry, which dumps the
task if its stack has run into its margin.

* `offset +fn_header_pathname`: The actual pathname of the function. 0
terminated C style string, followed by padding bytes, of at least a single
byte, of the offset from the code section to the string start.

### Loading and the boot image

The loader, `sys/load/class.vp`, reads a function from
`obj/<cpu>/<abi>/<path>` and binds it: each Vtable entry, an offset from
itself to a path, is replaced by the address of the code of the function of
that path, loading that one first if it has to. Nothing else in a function is
moved, every other reference in it is relative to where it is.

The boot image, `obj/<cpu>/<abi>/sys/boot_image`, made by `lib/boot/image.inc`,
is the functions the kernel needs end to end, `sys/load/init` first and
`sys/statics/statics` second. Each is kept without its path string pool: its
`length` is where the pool would have started, and each Vtable entry holds the
offset to the `pathname` of the function it names, in the image. The first
word of the image, the first function's `ln_fnode`, is the length of the whole
image, and after the last function a `length` of 0 ends it.

## Low level Vtable, string, and calling/jumping

You do not normally need to use these low level features, but higher level
calling and binding operations do. These are the Lisp functions that create
entries in the local Vtable, add strings to the local string pool etc. They
de-duplicate and in special situations allow duplication where that's essential
to create a class Vtable correctly.

### fn-add-string

Add a string to the string pool, if not already present, and return its unique
index value.

### fn-add-path

Add an external function path to the path pool, if not already present, and
return its unique index.

### fn-add-link

Push an entry onto the Vtable, referring to a path added using fn-add-path.

### fn-find-link

Search the current Vtable for a path and add a new entry if needed using
fn-add-link. Return the unique index.

### fn-string

Load register with string address via the local string pool.

### fn-bind

Load register with external function address via the local Vtable.

### fn-call

Call external function address indirectly via the local Vtable.

### fn-jump

Jump to external function address indirectly via the local Vtable.

## Higher level function calling

### f-call

Call method statically.

### f-jump

Jump to method statically.

### f-bind

Bind to method statically.

### s-call

Super call method statically.

### s-jump

Super jump to method statically.

### s-bind

Super bind to method statically.

### v-call

Virtual call method.

### v-jump

Virtual jump to method.

### v-bind

Virtual bind to method.

### d-call

De-Virtualize call method.

### d-jump

De-Virtualize jump to method.

### d-bind

De-Virtualize bind to method.

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

# ChrysaLisp

ChrysaLisp is built from functions. Ok, so that's just about the most obvious
thing to say about any computer program but they are organised in a slightly
different way that most other systems.

They are not bound into larger groupings such as libraries. More often than not
ChrysaLisp functions are collected into classes. Classes perform general
concepts and the methods of that class are individual functions.

All external function calling goes via indirect calls through the local
function Vtable. Local subroutines use standard relative call instructions.

Even a class Vtable is actually implemented as a function that has no code but
only a local Vtable !

## Low level Vtable, string, and atom creation, and calling/jumping

You do not normally use these low level features, but higher level calling and
binding operations do. These are the functions that create entries in the local
Vtable, add strings to the local string pool, add atom references to the local
atom pool etc. They de-duplicate multiple uses and in special situations allow
duplication where that is essential to create a class Vtable correctly.

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

Load register with atom string address via the local atom table.

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

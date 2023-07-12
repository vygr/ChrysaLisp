# Tasks, Nodes and Networks

What is the underlying VP machine abstraction ? What is a VP task, VP node, VP
network ? How are these ideas mapped onto `real` hardware or host OS's ?

## Tasks

A VP task is a 'thread' of execution of VP instructions. Don't get hung up on
that word thread, it doesn't imply anything beyond a running sequence of VP
instructions. Process implies too much and too heavy a concept, likewise the
word Thread ! ChrysaLisp Tasks are not processes or threads.

Each task has a control block created for it, this control block, `TCB`,
contains a few items that each stack needs to maintain and an area that is used
for the `stack`, your `:rsp` register is pointing at the end of this area when
it is created. These Task Control Blocks are held on a list, one list for each
task priority. When a task is inactive, due to scheduling or blocking on a
message read etc, the VP register state for that task is stored pushed onto the
stack. When a task becomes active that VP register state is popped from the
stack and execution of this tasks activity continues.

The structure of the TCB and the priority lists are detailed in
`sys/task/class.inc`, the implementation of the task methods in
`sys/task/class.vp`.

Tasks are started/created with the `'sys_task :start` method and stopped via
the `'sys_task :stop` method. If a task eventually returns from it's entry
point without calling `'sys_task :stop` this will happen automatically. A
'call' to `'sys_task :stop` is pushed onto the task stack on creation of the
task TCB.

During the lifetime of a task it can change state from active to inactive,
suspended, sleeping etc. Based on the method name you can probably guess the features available, so do look through the `sys/task/class.*` files.

## Networks

A VP network consists of a group of VP nodes, each connected to a neighbor by a
point to point link, each node executing a group of VP tasks. These tasks are
communicating with each other via sending and receiving messages, see the
`COMMS.md` document for that discussion.

This is an abstract model ! On an particular native machine or collection of
native machines, a VP node might be a Host OS process, OS Thread, or be running
on the bare metal, it can even be running as a C/C++/Rust VP64 EMU !

In either case, a VP node can execute a number of VP tasks, and it does this
via a priority based co-op task scheduling policy.

# Tasks, Nodes and Networks

What is the underlying VP machine abstraction ? What is a VP task, VP node, VP
network ? How are these ideas mapped onto `real` hardware or host OS's ?

The VP machine is a `sea` of nodes. So how does one sail upon this sea ?

## Tasks

A VP task is a 'thread' of execution of VP instructions. Don't get hung up on
that word thread, it doesn't imply anything beyond a running sequence of VP
instructions. Process implies too much and too heavy a concept, likewise the
word Thread ! ChrysaLisp Tasks are not processes or threads.

Each task has a control block created for it, this control block, `TCB`,
contains a few items that each task needs to maintain and an area that is used
for the `stack`, your `:rsp` register is pointing at the end of this area when
it's created. These `Task Control Blocks` are held on a list, one list for each
task priority. When a task is inactive, due to scheduling or blocking on a
message read etc, the VP register state for that task is stored pushed onto the
stack. When a task becomes active the VP register state is popped from the
stack and execution of this tasks activity continues.

The structure of the TCB and the priority lists are detailed in
`sys/task/class.inc`, the implementation of the task methods in
`sys/task/class.vp`.

Tasks are created/started with the `'sys_task :start` method and stopped via
the `'sys_task :stop` method. If a task eventually returns from its entry point
without calling `'sys_task :stop` this will happen automatically. A 'call' to
`'sys_task :stop` is pushed onto the task stack on creation of the TCB.

During the lifetime of a task it can change state from active to inactive,
suspended, sleeping etc. Based on the method names you can probably guess the
features available, so do look through the `sys/task/class.*` files.

## Nodes

A VP node consists of a group of VP tasks being scheduled for execution along
with a hi priority `Kernel` task. This Kernel task is no different to any other
task, other than it's running with the highest priority AND has been allocated
mailbox ID 0 ! You can always talk to the Kernel task via mailbox ID 0, it's
guaranteed to be there.

The Kernel task is responsible for scheduling the other tasks, for maintaining
the distributed `Service Directory`, distributing task start requests and
handling callbacks to the initial OS process/thread/emu/warp/weft/queef that
started the VP node `boot_image`. A lot of host OS calls require that you only
call them on their own `stack` !

You can view a VP node as what happens when you run a `boot_image`, doesn't
matter how your running it ! From an host OS thread, process, bare metal, EMU,
it's all irrelevant. If you `launched` a `boot_image` then you are running a VP
node.

## Networks

A VP network consists of a group of VP nodes, each connected to a neighbor by a
point to point link, each node is executing a group of VP tasks. These tasks
are communicating with each other via sending and receiving messages, see the
`COMMS.md` document for that discussion.

This is an abstract model ! On a particular native machine or collection of
native machines, a VP node might be a Host OS process, OS Thread, or be running
on the bare metal, it can even be running as a C/C++/Rust VP64 EMU !

In either case, a VP node can execute a number of VP tasks, and it does this
via a priority based co-op task scheduling policy.

## Link tasks

A link task, or driver, is a task that registers itself with the Kernel as a task that can send message data to a neighbor node.

It runs at the link driver priority, lower than the Kernel, but higher than the rest of the system.

When a link starts up, it exchanges a `hello` with the node it's connected to.
And then starts to transmit message data.

A link doesn't get involved in anything other than sending and receiving data
to/from its neighbor. It does work to split and combine large messages into
smaller packets, but keeps itself to itself.

## Lisp tasks

A Lisp task is a task stared by the Kernel via launching `class/lisp/run.vp`.
It's just the same as any other VP level task, but this task is passed the path
of the `.lisp` file it should start interpreting as its first message.

The `run.vp` task creates an instance of the Lisp class and then calls the
`'lisp :repl` method with the file name it received from the Kernel.

## Service tasks

A service task is one that `declares` its mailbox in the `Service Directory`. There is nothing special apart from that action.

The Kernel maintains a directory of all the local VP node's services, and via
talking to neighboring Kernels, via the link drivers, a network wide directory
of these named mailboxes.

You can create, destroy and search for the mailbox/s for a service/s task/s of
interest, via the `'sys_mail :enquire`, `'sys_mail :declare` and `'sys_mail
:forget` method calls.

# Tasks, Nodes and Networks

What is the underlying VP machine abstraction ? What is a VP task, VP node, VP
network ? How are these ideas mapped onto `real` hardware or host OS's ?

The VP machine is a `sea` of nodes. So how does one sail upon this sea ? Maybe
it's more of a `soup`, there are things in this soup that call themselves
carrots, carrots have a carrot service API, croutons implement the `crispy`
service API.

The contents of the soup can change, the soup base can shrink and expand, you
are sitting in the soup and need to proceed...

## Tasks

A VP task is a 'thread' of execution of VP instructions. Don't get hung up on
that word thread, it doesn't imply anything beyond a running sequence of VP
instructions. Process implies too much and too heavy a concept, likewise the
word Thread ! ChrysaLisp Tasks are not processes or threads.

Each task has a control block created for it, this control block, `TCB`,
contains a few items that each task needs to maintain and an area that is used
for the `stack`, your `:rsp` register is pointing at the end of this area when
it's created.

These `Task Control Blocks` are held on a list, one list for each task
priority. When a task is inactive, due to scheduling or blocking on a message
read etc, the VP register state for that task is stored pushed onto the stack.
When a task becomes active the VP register state is popped from the stack and
execution of this tasks activity continues.

The structure of the TCB and the priority lists are detailed in
`sys/task/class.inc`, the implementation of the task methods in
`sys/task/class.vp`.

Tasks are created/started with the `'sys_task :start` method and stopped via
the `'sys_task :stop` method. If a task eventually returns from its entry point
without calling `'sys_task :stop` this will happen automatically. A 'call' to
`'sys_task :stop` is pushed onto the task stack on creation of the TCB.

When a task is created it's assigned a none repeating 64 bit `mailbox_id`, this
ID is combined with a 128 bit `node_id` to create the network wide `net_id`
that is used to address a message.

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
call them via their own `stack` !

You can view a VP node as what happens when you run a `boot_image`, doesn't
matter how your running it ! From an host OS thread, process, bare metal, EMU,
it's all irrelevant. If you `launched` a `boot_image` then you are running a VP
node.

When a node is launched it's assigned a 128 bit UUID `node_id`. Messages are
routed first of all to the correct VP node and then into the mailbox of the
corresponding `mailbox_id`.

## Networks

A VP network consists of a group of VP nodes, each connected to a neighbor by a
point to point link. Each node is executing a group of VP tasks. These tasks
are communicating with each other via sending and receiving messages, see the
`COMMS.md` document for that discussion.

This is an abstract model ! On a particular native machine or collection of
native machines, a VP node might be a Host OS process, OS Thread, or be running
on the bare metal, it can even be running as a C/C++/Rust VP64 EMU !

In either case, a VP node can execute a number of VP tasks, and it does this
via a priority based co-op task scheduling policy.

Networks can `morph` dynamically ! The `sea` of VP nodes, and services, within
your horizon, can and does, change during your apps lifetime... more on this
later.

## Kernel task

The Kernel task is the highest priority task running on a VP node. It's
initially given the command line arguments after a `boot_image` launch. These
arguments are usually commands to start up link drivers and/or the TUI/GUI
services.

Once the Kernel launches all the command line tasks, it enters a loop
monitoring messages to it to distribute new tasks, register new link driver
tasks, service declarations from neighbors and beyond !

The Kernel task also launches a `ping` task. This task sits in the background
and every so often sends out the local `Service Directory` entries to its
neighbors. In this way the network wide routing and service tasks info is
distributed.

The Kernel task is responsible for distribution of task creation requests via
sending a message to it on `mailbox_id` 0. This method of starting a task
passes the task creation request from Kernel to neighboring Kernel until the
decision is made to call `'sys_task :start` by one of the Kernels, then the
`net_id` of the new task is returned to the original requester.

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

A service task is one that `declares` its `net_id` in the `Service Directory`.
There is nothing special apart from that action.

The Kernel maintains a directory of all the local VP node's services, and via
talking to neighboring Kernels, via the link drivers, a network wide directory
of these named mailboxes.

You can create, destroy and search for the mailbox/s for a service/s task/s of
interest, via the `'sys_mail :enquire`, `'sys_mail :declare` and `'sys_mail
:forget` method calls.

## Does the VP OS even exist ?

I was going to say that the system is an emergent OS... but that leads to a
whole pile of other discussions.

Are you trying to tell me, the OS and Network just emerge from the
interactions... ? YES !

This was something that hit me back when I coded Onslaught on the ST/Amiga...
The entire game was an emergent effect of the sprite rules. I spent most of the
time playing the game and tweaking the rules till it did what I thought was a
good game. And then I thought "This also applies to a distributed OS !" ...

As soon as we launch a `boot_image` we have a VP node running. A VP node is a
single thread (on the host OS maybe), that does it's own co-op scheduling, so
it can run many VP tasks, on a single host OS thread.

If we launch several `boot_image` we have several VP nodes (like the run
scripts do). At this point although we have several VP nodes running, they all
think they are a network of 1 node ! As the link driver tasks start up and they
introduce themselves to the node next door, the VP nodes find that they are
part of a bigger network, and they join together into a bigger group of VP
nodes. That takes a few seconds depending on the link driver communication
speed and the host OS process scheduling.

That's on a single Host machine ! But you could have nodes started on other
machines, and a link driver comes up that 'groups' those VP nodes together...

As VP nodes group, they sync up their copies of the Services Directory (and
sort out the message routing situation).

All this works in reverse as well, as VP nodes go out of contact, the nodes
'ungroup', and split apart. The Service directory of the separated parts
updates, and the routing situation updates.

This grouping and ungrouping of VP nodes can go on ALL the time ! In a mostly
stable situation it doesn't do it that often, but in theory it can be like a
quantum foam ;) Applications live on this 'sea' of VP nodes. And they have to
live with the fact that things can and do come and go dynamically.

You have classes provided like `Global` and `Farm` that take a lot of the pain
away from you, they monitor the network state and call back to you as VP nodes
go away or appear. The assembler, like the Raymarch demo is fault tolerant, in
that, it creates a farm of children to do work for it, but these children may
live on VP nodes that die. Any child that dies is restarted on the current
'sea', and you try again. Keep going till all the files are compiled, then
stop.

A robust app, allows for the fact that its tasks may go out of contact, as a
link is pulled out, or maybe somebody pulls the power cable out from a node or
group of nodes.

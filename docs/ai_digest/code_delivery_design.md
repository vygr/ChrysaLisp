# Code Delivery, Design Notes

How code reaches the node that runs it: what install does, what runtime
does, how VP is translated and linked, and how a task sent out onto the
network gets the code it needs. Settled on 2026-10-24, from first principles,
after an experiment with nodes that ran emulated and native code at once.
What is built now is [source_service.md](source_service.md) and the function
servers of [vp_functions.md](../vm/vp_functions.md); where they differ from
these notes, the notes say where the design is going.

## Simplicity First

The experiment worked: a node could boot with no native code, run VP64, and
have native code take over function by function. It needed trampolines in
the host, a stub per function, a register kept for a dispatcher, task
switching that served both, and all of it again for each CPU. And it bought
little: translating the whole system took under two seconds. It mixed up two
stages that are better apart, install and runtime. It is kept on a branch as
a record, and not used.

## Nodes

A node runs 100% native code or 100% emulated, VP64. Never both at once.

* A native node is as a node is now.
* An emulated node runs the VP64 image, as `-e` does now. It is for install,
  and for a CPU we have no translator for, which runs as a VP64 node.
* A network can have both kinds of node working together.

## The Baseline

Install establishes a baseline: the common system shared by every system on
the network, the VP and the Lisp. For now the baseline is the current flooded
snapshot, the waypoint, and install and sync keep every machine level with
it. A machine that has moved off the waypoint, as a development machine does,
still works with the others: functions are compared by the hash of their VP,
so less is common and more is sent.

The baseline is source 0. It is what server zero, the node's own function
list, holds.

## Install And Runtime

Each has a minimum boot image, and they do not hold the same things.

| | VP64 image | Native image |
|---|---|---|
| Code | VP64, one for every machine | native, one for each machine |
| Made by | the snapshot, shipped | install, on the machine |
| Its job | install, and run an emulated node | start the loader |
| Holds | loader, kernel, Lisp, streams, the translator | the loader, the kernel, what they call |

The VP64 image serves install, and any node that runs emulated only, as on a
CPU with no translator yet; that is acceptable. Install boots it emulated,
translates the baseline for this machine, writes the translations, and the
machine boots native from then on. There is no handing over from VP64 to
native code while running.

The native image can hold less because once install has done its job the
loader can load, and translate, what it lacks on demand. The installer can
not rely on that for the things it needs to do the translating, so the VP64
image holds them.

Install has two inputs, a snapshot on the machine or the network, and does
the same with either.

## Translation And Linking

Two separate things.

* Translation turns a VP function into native code with its links unbound,
  only their names. It depends on the VP, the CPU, the ABI and the
  translator, and nothing else, so its result is cached by those, in memory
  and on the machine's own storage where that is allowed, and the work can
  be shared out over the nodes of a machine.
* Linking binds those names, in the context of a task: its own function
  server, then the node's. One translated function can be linked to
  different functions for different tasks. Linking is done on the node, in
  memory, and is never cached or sent.

A function's name is its contract: the same name takes the same inputs and
returns the same outputs, it achieves its defined function, whatever it links
to, more things or fewer. Any implementation of a name is safe for a link to
it; which one a link gets is decided by the source. A function is not linked
once, with every call to it then getting that entry point: a task of another
system may use the node's algorithm and give it a function of its own, its
own way of doing one part. So the node's copy of a function that calls, all
the way down, a function the task's system has its own way of, is linked
again for the task, in its server: the same translation, other links. The
rest the task shares with the node.

VP is translated, to native or to VP64, on demand.

## Sources

A task binds to two sources:

1. Source 0, the baseline.
2. Its originator, the system that sent it, with its own function list,
   named by its system id.

Code from other systems, a third source, is not designed yet.

A task sent out onto the network carries both common code and code unique to
its originator. The node it runs on is sent the hashes first, and asks for
what it does not have, in one transaction. The originator is the system that
must deliver any code the destination has not cached. Shared pools of code
are possible one day, as a quicker place to look, but they never change what
is run.

A source is keyed by its system id, which survives a reboot. The launch of a
task carries both the system id and the mailbox of its `@Source`. If the
mailbox does not answer, as after a reboot, the task asks a kernel of that
system, mailbox 0 of any of its nodes, what its `@Source` is now, and keeps
the answer. `@` names are still not advertised beyond their system.

## A Machine With No Disk

Dumb compute. It is flashed with the host and the install image. When it
boots it connects, installs the current snapshot over the network, into
memory, then translates and runs native code for tasks as any other machine
does, its translations kept in memory, within reason. Network sync keeps it at
the waypoint. It is an ordinary machine whose storage is memory.

## Still To Build

What is on master now does some of this. The rest:

* A task sent from another system that sends a task of its own should name
  its originator, not the system it runs on.
* The kernel query for a system's `@Source`, and the retry by it.
* A translation cache in memory, for each machine.
* Translation shared out over a machine's nodes.
* The loader asking for a translation of a baseline function it does not
  have, not failing.
* Lisp found by hash, as VP is; now each file is sent whole each time.
* The install and runtime images apart.
* Network install.
* The emulator running on every host; it does not boot on the Pi.

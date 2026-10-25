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
the network, the VP and the Lisp. The baseline is everything of the tree
that is not under `apps/`. An application is not part of it: install builds
none of its VP, see [Applications](#applications). For now the baseline is the current flooded
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

What a node has of its own, server zero, the baseline, is bound by the
kernel's loader as any other: what is not there yet is made on demand, the
loader waiting for it as below. All other VP, an application's of this
system or code of another, is made on demand by the `@Source` of the machine
it runs on, to native or to VP64.

## A Bind That Waits

Decided on 2026-10-25, in place of the day before's "server zero is always
bound from what install translated". It is how Taos did it.

A bind looks for a function in this order, and only the last step waits:

1. The function list: it is there, it is bound.
2. A translation cached on the machine, for its CPU and ABI, under the hash
   of the VP: loaded, as a load from `obj/` is now.
3. A native made by hand for this CPU, where there is one: loaded, and its
   VP is never translated.
4. None of these: a stub is put in the function list in its place, and the
   task waits.

The translator is only ever called when no native is there, cached or made by
hand. The loader never translates: it is VP, and the translator is Lisp. It
puts the stub in, asks, and holds the task.

A stub says "it is coming, hold on". It has the name and a queue of the
tasks that want it. A second task that misses the same name joins the queue,
no second request is sent. The request is a translate job with its product
to go to `:cache`; when it is done the native is loaded from the cache into
a new block, linked, made runnable, and put in the function list in place of
the stub, and the tasks of the queue carry on.

Closure first. A function is placed only when every function it calls, all
the way down, is there or placed with it: its links are entry points written
once into a block that is never written again, so there is nothing to patch
later and no trampolines. A task is held until the whole chain it needs is
loaded and translated, and the request asks for that chain in one go, by the
hashes, as a task sent from another system does.

Who may wait. A task waits in its own bind. The kernel task never waits on a
stub, every task of its node would wait with it: a task to be started from a
function that is not there yet is started, and it binds, and waits, itself.

What can never miss. The loader, the kernel, Lisp, the translator, and what
asks for and delivers a translation, each with everything it calls: a miss in
those could never be filled, as what would fill it is what is missing. That
is what the native image holds, [Install And Runtime](#install-and-runtime),
and a miss there is fatal, as a missing file is now.

When it goes wrong. On the ideal path the request is answered, the chain is
placed and the tasks go on, all by messages. Should the answers stop, its
service gone or a translation failed, a stub with nothing heard for long
enough dissolves: the binds held on it give 0, an `(ffi)` waiting on one
throws, and the stub is taken out, so a bind after asks again. A long
translation that is still going is not inactivity. Nothing waits for ever.

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

## Applications

An application's VP is not built by install, and nothing of it is kept in
`obj/` by name: two versions of one function, a development machine's and
the waypoint's, or two systems', would fight over one file. It goes the way
another system's code goes, through the machine's `@Source`, with this
system as the originator.

* Lisp reaches an application's VP only by `(ffi)`, so that is the one
  place a miss is caught: the bind is tried, and on a miss the local
  `@Source` is asked for the function, what it is given is placed, and the
  bind is tried again. No application changes for it.
* `(source-task)` keeps one meaning: a task that may be sent to another
  system, whose code is read from home.
* What a system has that is not the baseline is linked in the function
  server named by its system id, on each node, so every task of it there
  shares one copy. Server zero stays the baseline alone.

## Three Stages, Each Pure

| Stage | Takes | Gives | Done |
|---|---|---|---|
| Compile | a `.vp` file | the VP of its functions, an entry each | once for each hash of the file and all it includes, and the mode, at the originator, which alone has the source |
| Translate | a function's VP | native, or VP64 | once for each hash of the VP and of the translator, CPU and ABI, on each machine, shared by its nodes |
| Link | natives and names | a bound function | in each task's server, on its node, in memory |

`(vp-compile)` gives the entries and `(emit-translate)` the native, and
neither saves anything: what is done with a product is for whoever asked.
In one task that is simply the value returned, used as the caller likes.

A product is named by its inputs, so the same key gives the same bytes
wherever it is made. Any node may make it, any cache may give it, and two
nodes making the same one at once is work wasted, never a fault: no locks,
no order.

## Jobs And Where Products Go

Compiles and translations are farmed as jobs over a machine's nodes, with
the herd the build already uses. A job is a message, so where its product
goes is said in it as data, one of a fixed set the worker knows, never a
function the worker would have to run:

| Destination | The worker | Used by |
|---|---|---|
| `:obj` | writes into `obj/<cpu>/<abi>/` by the function's name | the build of the baseline |
| `:cache` | writes into the machine's cache under the name its key gives, by a temporary name and a rename | `@Source` |
| `:mail` | sends the product to a mailbox | a machine with no disk, or one that may keep nothing; to the node of the task that wants it |

The worker works out the path itself, so a job can not send a write outside
`obj/` or the cache. Products go from the child that made them to its own
machine's disk; what goes back to the farm is a few dozen bytes, done and
the key, or an error. The only bulk that crosses the network is VP from the
originator, once for each function a machine has not seen.

`@Source` is one task to a system, so anything heavy in it is serial. It
only keeps the books: a request in, the hashes looked up, a job started for
each product that is missing, and every waiter told when the products are
there, asked once however many wait. It never handles a product on a
machine with a disk; a task's node loads from the cache what it needs.

## A Machine With No Disk

Dumb compute. It is flashed with the host and the install image. When it
boots it connects, installs the current snapshot over the network, into
memory, then translates and runs native code for tasks as any other machine
does, its translations kept in memory, within reason. Network sync keeps it at
the waypoint. It is an ordinary machine whose storage is memory.

## Still To Build

Done since these notes were written: a task that sends one of its own names
its originator; Lisp found by hash, each machine's `@Source` asking once for
a file and all it imports and keeping the answers,
[services.md](services.md); the install and native images apart; the
emulator on every host, the Pi's had only stale files. The rest:

* Compiling apart from translating and saving: done, `(vp-compile)`.
* The kernel query for a system's `@Source`, and the retry by it: done,
  [source_service.md](source_service.md).
* Jobs for compile and translate, with their destinations, and `@Source`
  only keeping the books: the translating and hashing it does in its own
  loop now move out.
* The bind that waits: the stub and its queue in the loader, filled by
  those jobs, [A Bind That Waits](#a-bind-that-waits). Then install need
  translate no more than the native image holds.
* Applications: `(ffi)` asks `@Source` on a miss, install and `make` build
  only the baseline, `make apps` and the applications' products in `obj/`
  go.
* A translation cache in memory, for a machine that may not keep one on disk.
* Network install.

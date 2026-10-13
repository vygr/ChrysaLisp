# Alien nodes: a node need not be ChrysaLisp

This is a policy, and what follows from it. Where a thing is not built, or
not decided, it says so.

## The policy

A ChrysaLisp network is not a network of Lisp nodes. It is a network of nodes
that speak the link protocol.

Nothing in principle stops a node being written in another language, with
another compiler, on a machine this tree has never been built for. Such a node
is called alien here. It can route for the network, and it can give services
to it, and the nodes that are ChrysaLisp use those services as they use each
other's.

The one thing an alien node may not be able to do is run Lisp that is sent to
it. That is allowed. The network is to cope with a node that does not, and
must not send it any.

So:

* The link protocol is the surface that other systems meet. It is to be
  written down well enough to be built from the writing, with no reading of
  this source.

* A change to the link protocol is a change to every alien node there is. It
  is made with that in mind, and said in the release notes.

* Nothing above the link is to take it that the node at the other end is
  Lisp, unless it has asked.

## What a node is, to the network

Four things, each on top of the one before. They are described in
`messages_and_routing.md` and `mesh_net.md`, and those are the start of
the writing down.

1. **A link.** Between nodes of one machine it is a ring of bytes in shared
   memory, `sys/link`. Between machines it is a TCP connection, opened
   through the door, `service/net/door.inc`: both ends prove they hold the
   same `mesh_key`, with nonces and HMAC-SHA256, and after that it is the
   same stream of messages.

2. **Mail.** A message has a destination, a net id, which is a node id and a
   mailbox on that node. One that is longer than a link carries in one go is
   sent as fragments and put together at the far end, `sys/mail`.

3. **The kernel's mailbox.** Every node has one, and what is sent to it is a
   kernel call, `sys/kernel/struct.inc`: `exit`, `callback`, `signal`, `pin`,
   `run`, `want`, `ping`, `ping_sys`. The pings are how the network is known:
   each node says who it is, the system it is on, how far it has come, and a
   hash of its services, and a node that hears one floods it on by the rule
   in `messages_and_routing.md`. A node that does not have the services for a
   hash asks with `want`.

4. **Services.** A service is a name and a mailbox, carried in the pings. A
   name that begins `*` is told to every node. One that begins `@` is told
   only to the nodes of the same system. What is sent to a service's mailbox
   is that service's own protocol.

An alien node has to do the first three well enough to be routed through, and
may then declare services.

## What an alien node may leave out

`pin` and `run` ask a kernel to start a task, by the path of its file. That
file is Lisp, or VP made by this tree. A node that has neither can not start
it.

`callback` and `signal` are of use only to tasks on the node itself.

So an alien node may be one of three kinds, and all three are welcome:

* **It routes and serves.** It starts no task for anybody. A box that is a
  database, a camera, a printer, a file store, behind a service name.

* **It also starts tasks of its own, by name.** It answers `pin` for the
  paths it knows, with its own code behind them.

* **It runs Lisp.** Another implementation of the whole. To the network it is
  not alien at all.

## What keeps tasks off an alien node now

The system id. Every node says in its ping what system it is on, and a system
is what shares a file system.

* `run`, which lets the kernel choose the node, only spreads to nodes of the
  same system id (`+kn_call_run` in `sys/kernel/class.vp`). An alien node has
  a system id of its own, so no `run` is ever handed to it.

* `@` services are told only within a system. A Lisp node does not see an
  alien node's `@` services, nor the alien node its own.

* A node started with `-sid` is a system of its own already, and tasks are
  not spread to it or from it. An alien node is the same case, come by
  another way.

So the local task system is local. Nothing that is written to work within its
own system needs to change.

## What does not, and has to be built

`pin` names the node. An application that takes the nodes of the whole
network and starts a task on each will start one on an alien node.

Those applications are the ones that farm work out over every node there is:
the network monitor, the ray marcher, the Mandelbrot, and anything made with
the global task farm.

They all ask the one question, `(lisp-nodes)` in `class/lisp/root.inc`. The
task libraries `lib/task/global.inc`, `lib/task/farm.inc` and
`lib/task/local.inc` ask it and nothing else, and so does the network
monitor. Today its answer is every node that has been heard from.

**To build:** a node says whether it runs Lisp tasks, and `(lisp-nodes)`
gives only those that do. With that one change, in one function, every farm
there is leaves alien nodes alone, and no application is touched.

How a node says it is not decided. Two ways that use what is carried already:

* **A service name.** The services of every node reach every other node in
  the pings. A node that runs Lisp declares a name that says so, and
  `(lisp-nodes)` keeps the nodes that have it. Nothing in the ping changes.

* **A field in the ping.** One more number beside the system id. Cheaper to
  ask, but it changes `+kn_msg_ping`, which is the link protocol.

Either way it is the node that runs Lisp that says so, not the alien node
that says it does not. A node that says nothing is then left alone, which is
the safe way to be wrong.

## Not written down yet

* The link protocol as a specification: the bytes of a link, the door, a
  fragment, each kernel call, a ping. The two documents named above describe
  them from the inside. What an implementer needs is the layout and the
  rules, and a way to test against them.

* What a kernel answers to a `pin` it can not do. An alien node that starts
  tasks by name will be asked for ones it does not have, and what asked must
  be told so and not left waiting.

* The version of the protocol, and what two nodes do that do not agree on it.

* What goes over a link is in the clear once the door is passed. An alien
  node holds the same `mesh_key` as every other, and is trusted as they are.

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

```image
docs/diagrams/link_packet.cwb
```

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

* `pin` and `run` are both sent to a kernel, the node the asker names.
  `pin` starts the task on that node. `run` starts it there, or on the
  least busy of the nodes that kernel is linked to on the same system,
  whichever has the fewest tasks (`+kn_call_run` in `sys/kernel/class.vp`).
  So a `run` is aimed: sent to the kernel of the node that has the service
  a task wants, the task runs on that node or one beside it, near what it
  uses. That is by design. What it does not do is pass a task on to a node
  of another system id, and an alien node has one of its own, so a `run`
  sent elsewhere never lands on it. One sent to an alien kernel by name is
  that kernel's to answer, as a `pin` is.

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

They get their nodes from `(lisp-nodes)` in `class/lisp/root.inc`, most of
them through the task libraries, `lib/task/global.inc` and
`lib/task/farm.inc`. Its answer is every node that has been heard from.

**Decided:** nothing is declared. A node does not say what it is, the ping
does not change, and no node holds a thing about every other node to know
it. The task libraries find out, by asking, and manage it. An application
that uses them is not involved, and nor is any node.

What is there for that already:

* A kernel that is asked to `pin` a task it does not have answers with an
  id of 0, and whoever asked is told so. That is the answer an alien node
  gives to every `pin` it can not do. It is part of the protocol as it
  stands.

* The answer says who gives it. An id of 0 has no node in it, so after the
  id is the node of the kernel that answers, `+kn_msg_reply_node` in
  `sys/kernel/struct.inc`, the task started or not. What asked for a task on
  many nodes knows which of them could not. What is sent to ask is as it
  was, and the answer is only longer, so a node that does not know of this
  still asks and is still answered.

* An application gives the libraries a time to wait, `(. farm :refresh
  timeout)`, and an entry whose task has not come in that time is started
  again. So a node that answers 0, or does not answer, costs one wait and is
  asked again.

* A farm that picks any node for each task, as the chess does, and as
  `(Jobs)` is made on, `(Farm)` in `lib/task/farm.inc`, is given the
  kernel's answer too, `(. farm :reply msg)`. A node that could not start
  a child is not picked again while that farm lasts, and the child is
  asked for of another at once, with no wait. `(. jobs :launched msg)`
  does it for what uses `(Jobs)`.

One that keeps a task on every node, `(Global)`, would ask the same node
again after every wait, for ever. So the application gives it the kernel's
answer, `(. global :reply msg)`, and a node that could not start the task
is not asked again while that `Global` lasts. The network monitor, the
network speed and the network map do.

It is of the one `Global` or `Farm`, and gone with it, Chris: "they would
ignore that node from then on. It's only for this specific instance, so not
permenant." Nothing is kept of a node between one application and the next,
or one start of it and the next.

What it costs is one `pin`, for each alien node, for each
application that farms over the whole network, each time it is started.

## Not written down yet

* The link protocol is written as a specification,
  [link_protocol.md](link_protocol.md): the bytes of a link, the door, a
  fragment, each kernel call, a ping. Not yet: a way to test another
  implementation against it.

* The version of the protocol, and what two nodes do that do not agree on it.

* A link the dialler made over the internet is sealed once the door is
  passed, ChaCha20-Poly1305 records, [`mesh_net.md`](mesh_net.md); one on
  the LAN is in the clear. An alien node holds the same `mesh_key` as every
  other, and is trusted as they are.

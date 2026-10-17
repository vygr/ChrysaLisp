# The Mesh: how the machines find each other and stay joined

Machines that run ChrysaLisp on one network, and have the same key, join
into one. Nobody tells them to. This is how that is done, by the Net
service each session starts, `service/net/`. The guide for someone who only
wants it to work is [`docs/intro/mesh.md`](../intro/mesh.md).

## The parts

```image
docs/diagrams/net_parts.cwb
```

The Net service is one task, `service/net/app_impl.lisp`, declared as
`@Net`. What it starts are tasks of their own:

* **The gate**, `service/net/gate.lisp`, listens on TCP port 3333. It is
  sent the key and the port, says if it has the port, and gives each
  connection it accepts to a door.

* **A door**, `service/net/door.lisp`, is one connection, dialled or
  accepted, proving itself. Through, it is a link. If not, it is closed.

* **A link**, `service/net/link.vp`, is three tasks, one that waits and one
  each way, and a record the kernel routes by, `lk_node`. `remote` in that
  record is 1 for a link over the network, 0 for one of shared memory to a
  node of the same machine.

The beacon and discovery are sockets of the service itself, both UDP port
3334: every two seconds it says who it is and where it listens, and it
hears the others say the same.

## The key

The key is the file `mesh_key` at the root of the tree. `make install`
makes one if there is none, so a machine has its own from the first day,
and is by itself. To join machines, give them the same file.

```image
docs/diagrams/mesh_states.cwb
```

The service looks at the age of the file every two seconds. If it is
another, or has gone, or has come, what was made with the old key ends:
the gate is told to go, the two sockets are closed, the peers are
forgotten, and every link over the network is dropped, `(net-drop)`. Then
it joins again with the key it has now. With none it stays off the
network, and `link` says why it will not make one.

## Proving it

```image
docs/diagrams/door_proof.cwb
```

`service/net/door.inc`. A key of 64 hex digits is used as it is. Any other
text is a passphrase, and is made into a key the slow way, PBKDF2, so that
it is dear to guess.

## Who dials

```image
docs/diagrams/mesh_dial.cwb
```

`service/net/mesh.inc` is the logic alone, with no mail and no sockets in
it, and is tested as that. The service does what it says.

## More than one session on a machine

```image
docs/diagrams/mesh_sessions.cwb
```

Only one process can listen on a port, and that settles it: the first
session to ask has port 3333, and is the machine's member. A session that
finds it taken asks again every ten seconds.

A session started with `-a`, alone, does not ask at all. A run of the
tests is one, and every session a `rack` run starts: or, with no member
up, the tests would become the machine's member for as long as they ran,
link to the other machines, and two machines testing at once would see
each other's services.

A subnet started as a system of its own, `nodes -s cube -o`, is not the
machine's. `(node-start)` tells each of its nodes the system id they have,
and a Net service there has none of the machine's key.

## What is seen from where

```image
docs/diagrams/services_scope.cwb
```

The Net services find each other by the name `*Mesh`, and so does the
`mesh` command, to say if this machine has joined.

## The parts, in files

* `service/net/app_impl.lisp`, the service: its requests, and the pump
  that looks at the key, the sockets and the links.

* `service/net/mesh.inc`, who is known, who dials, who is forgotten.

* `service/net/door.inc`, `door.lisp`, `gate.lisp`, the key and the proof.

* `service/net/link.vp`, a link's tasks. `service/net/lisp.vp`, the calls
  the service makes of the host, with `(net-links)` and `(net-drop)`.

* `cmd/mesh.lisp` and `cmd/link.lisp`, the commands.

* `tests/solo/test_tcp_link.lisp`, two networks and a real connection
  between them: mail over it, the link dropped, and first come first
  served of a port.

## Others' machines

The key is all there is: a machine that has it, a friend's server joined
over the internet by `mesh -t host`, is trusted as one's own, with any
service, and to start a task on any node. A sync is guarded apart from
that, the Sync service takes one only from its own machine, those named in
its `sync_from` and the herd at waypoints, [`sync.md`](sync.md), "And from whom",
and `rack -o` names the machines a run touches. Tasks are not guarded.

## Not here yet

* A guest link: one that carries mail to the services a machine offers and
  starts no task on its nodes, and a machine that says which it is to
  others. Today a key is all or nothing.

* What goes over a link is in the clear. The door says who may link, and
  no more.

* On Windows a second listener on a port may not be refused, so first
  come first served is not yet known to hold there.

* A session that is not the member has no way to the mesh. It could have
  one through the member, and does not.

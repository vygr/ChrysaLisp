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
and is by itself. To join machines, give them the same key.

A machine keeps the keys it holds by name, its keyring, the files of
`keys/`, `service/net/keys.inc`, and the `key` command lists them with
their fingerprints, makes, takes in, says, renames and forgets them, and
puts one in use by copying it to `mesh_key`. It will not put another in use
while the one in use is in no keyring file, which would lose it. A
fingerprint is four groups of four hex digits of a hash of the key's 32
bytes, so the same key written in hex or as its passphrase has the one
print. `keys/` and `mesh_key` are never committed, and a sync, from
whoever, never writes nor removes them, nor `sync_from` and `waypoint`:
`(sync-safe?)` refuses them on the side that takes it.

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

## Sealed over the internet

A link the dialler made to a host outside the private addresses is sealed.
Inside are IPv4 10/8, 172.16/12, 192.168/16, 127/8 and 169.254/16, IPv6
loopback, link local and unique local, `localhost`, and names that end
`.local`, `.lan`, `.home` or `.internal`; anything else is outside,
`(door-outside? host)`. `link -s host` seals a link inside as well.

The dialler's hello says whether it is sealed, one byte, and the proof is of
that byte and both X25519 shares as well as the nonces, so one in between
who has the key can neither swap shares nor say the link is plain. Each end
works out the shared secret, refuses one of nothing, and with HKDF over it
and the key, salted with the two nonces, makes a key each way. The keys are
new each link.

On a sealed link each packet, wire header and all, goes as a record: 8 bytes
of its length, the packet encrypted, a 16 byte tag, ChaCha20-Poly1305 of RFC
8439 with no aad, `sys/crypto/class.vp`. The nonce is a count of the
records each way, so one sent again, dropped, or out of order fails to open.
A record that does not open drops the link, and the mesh dials again. The
out task seals in a buffer of its own and the in task opens in one, so a
plain link runs the same code it did. The cipher does some 350 MB/s on an
M4, and a sealed link between two networks on one machine moved mail as
fast as a plain one.

The handshake is CLDOOR2. A machine on CLDOOR1 links to none on CLDOOR2,
its hello is too short and the door waits it out.

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

But a desktop comes first. A session with a GUI on one of its own nodes
that finds the port taken sends `YIELD` and its instance to the holder, by
UDP to 127.0.0.1 on port 3334, which the holder hears beacons on, and asks
for the port again in a second and a half. A holder that is not a desktop,
a member left up for `rack`, steps aside as it would for a new key: its
listener, beacon and links let go of, its store stopped. It asks for the
port every ten seconds as any other, and has it again once the desktop has
gone. A holder that is a desktop does not step aside: of two desktops, the
first keeps the port. Sessions of one machine are not linked to each
other, the mesh knows a machine once, so the one that gives way is off the
mesh while the desktop is up.

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
  between them, sealed: mail over it, the link dropped, and first come
  first served of a port.

* `sys/crypto/`, ChaCha20, Poly1305 and the AEAD for VP code, which the
  Lisp natives of `lib/crypto/lisp.vp` call as well.

## Others' machines

The key is all there is: a machine that has it, a friend's server joined
over the internet by `mesh -t host`, is trusted as one's own, with any
service, and to start a task on any node. A sync is guarded apart from
that, the Sync service takes one only from its own machine, those named in
its `sync_from` and the herd at waypoints, [`sync.md`](sync.md), "And from whom",
and `rack -o` names the machines a run touches. Tasks are not guarded.

## Machines on other builds

A beacon says the build of the machine, the id of what its boot image was
made from, `lib/boot/id.inc`, and `mesh` names one that differs. It is only
said: such a machine links and is used as any other. Tried with a machine
given a false id, and with one whose boot image was built from changed VP
source: each linked, its services were seen, it ran `rack` work and took a
sync. So machines at different waypoints of the herd keep in touch while
they update, one at a time. What would part them is a change to what goes
between nodes, the mail and link messages; such a change has to reach every
machine before they talk again, by some other way than the mesh.

## Not here yet

* A guest link: one that carries mail to the services a machine offers and
  starts no task on its nodes, and a machine that says which it is to
  others. Today a key is all or nothing.

* On Windows a second listener on a port may not be refused, so first
  come first served is not yet known to hold there.

* A session that is not the member has no way to the mesh. It could have
  one through the member, and does not.

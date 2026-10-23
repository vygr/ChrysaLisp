# The link protocol

What one node sends another over a link, byte by byte, so that something that
is not this code, an alien node ([alien_nodes.md](alien_nodes.md)) or a
ChrysaLisp written again, could talk to it. It says what the code does now,
as of v7.4 and after. What was found wrong in the code as this was written,
and has been put right since, is under
[Faults found writing this](#faults-found-writing-this).

How it all hangs together, and why, is in
[messages_and_routing.md](messages_and_routing.md) and
[mesh_net.md](mesh_net.md). This is the wire.

A TCP link says the version of the link it speaks as it is made, in the
door. This document is of version 1. See [Versions](#versions).

## Conventions

* Every number is in the byte order of the host, and every host ChrysaLisp
  runs on is little endian. Nothing is turned round on the way out. The only
  big endian numbers are the ports inside a socket address, which are the
  host's.
* Sizes: byte 1, short 2, int and uint 4, long and ptr 8. A field is at an
  offset that is a whole number of its own size. A node id and a net id are
  bytes, and are not aligned.
* A node id is 16 bytes, two longs, `node1` then `node2`, random, made as the
  node boots.
* A system id is 16 bytes: the machine's id, `/etc/machine-id` taken from hex,
  or the file `.system_id`, or what `-sid` says, up to 16 bytes, the rest 0.
  Every node of one machine has the same one.
* A net id is 24 bytes, a mailbox id, a long, then a node id. A mailbox id
  is a uint in the long's low half; only the low 32 bits are looked at as
  mail is given out.
* The most a fragment carries, `lk_data_size`, is 6104 bytes
  (`sys/link/class.inc`): a heap cell of 6144 less what a string and the
  mail header take of it.

## The two kinds of link

A shared memory link joins two nodes of one machine. A TCP link joins two
machines, and is made by the Net service, `service/net/`. Both carry the same
two things, pings and fragments, and above them the same mail.

## A shared memory link

### Making it

A node started with `-l name`, or told `(node-link name)`, opens the link of
that name. A name is seven characters, `[0-9a-z]{3}-[0-9a-z]{3}`, and the two
nodes of a link are each given it.

The memory is a file `/tmp/<name>` on POSIX, opened to create and to fail if
it is there; the first to open it sets its size, the other waits till it is
that size and opens it, for 5 seconds at most. On Windows it is a mapping of
that name. Its size is `lk_shmem_size`, 63648, and both ends must agree on it
to the byte: an end that finds the file another size for 5 seconds gives the
link up, and says `Link: no shared memory, or the other end's is another
size, another build !`. The first end, which made it, waits for a second for
ever, as it waits for any. The end that closes a link takes the file away.

### The memory

```
offset   size    what
0        30880   chan_1, the owner's to send on
30880    16      host_a, the owner's node id
30896    16      host_b, the guest's node id
32768    30880   chan_2, the guest's to send on
```

`chan_2` is at the next 4096 after `host_b`.

```image
docs/diagrams/link_shmem.cwb
```

The first to find `host_a.node1` 0 writes its node id there and is the
owner; the other, finding another's there, writes its own in `host_b` and is
the guest. The owner sends on `chan_1` and reads `chan_2`, the guest the
other way. Nothing else is said: who the other is comes in its first ping.

### A channel

A channel is a ring of 30880 bytes of records, each a header and a body:

```
lk_buf, 8 bytes
0   uint  status   0 ready, 1 ping, 2 frag, 3 skip
4   uint  length   of the whole record, header and body
```

The writer puts the record's length, moves its head on by it, writes
`status 0` at the new head, makes sure the memory is seen (`vp-sync`), and
only then writes the record's own status, which is what makes it the
reader's. The reader waits for a status that is not 0, reads the length,
and when it has the record sets its status back to 0, which is what gives
the writer the room again. There is no acknowledgement and no sequence: the
ring is the flow control. A writer counts the room as what lies between its
head and the first record not yet given back, and the whole ring when all
are.

* A ping record is 64 bytes: the header, then 56 bytes of `lk_node`.
* A frag record is the header, the 64 byte fragment header, and the data,
  rounded up to a multiple of 8. The data may run round the end of the ring
  and on from its start.
* A header never runs round. Where the 72 bytes of the two headers would, a
  skip record is written whose length is what is left to the end, and the
  frag starts at the ring's start.
* A writer sends a ping only with 64 bytes free, and a fragment only with
  6184.

### A ping, shared memory

```
lk_node, 56 bytes
0    16  node id of the sender
16   16  system id of the sender
32   uint  task count: its tasks, and its lag, so many a 32 ms
36   uint  process id of the sender's host process
40   16  not written, not read
```

It is sent when the count changes, and once a second as a heartbeat once
the other end has been heard and the link is up.

### Fragments, both kinds of link

```
lk_frag, 64 bytes
0    24  dest   net id
24   24  src    net id
48   uint  length   of this fragment's data
52   uint  offset   of this fragment's data in the whole message
56   uint  total    length of the whole message, 0 for a message of one fragment
60   4   pad, 0
```

A message of one fragment has `src` and `offset` 0: they are set only for
the fragments of a parcel, the mail service's own for each. A message is
made with them 0, as the heap block it is made in may have held a
fragment of a parcel before.

## A TCP link

### Ports, and who dials

The Net service listens on TCP port 3333, IPv4. Sockets do not block and
have `TCP_NODELAY`. Two machines that can each dial the other do not both:
the lower system id dials, the higher waits 15 seconds for it before it
dials itself (`service/net/mesh.inc`). A failed dial is tried again after
10 s, 20, 40 and then each 80.

### The door

Every link the Net service makes goes through the door,
`service/net/door.inc`. Each end, the dialler and the listener alike, does
this, and either end that finds anything wrong closes the socket, and says
nothing. All of it must be done in 5 seconds.

```image
docs/diagrams/door_proof.cwb
```

**1. Each sends its hello, 60 bytes:**

```
0    7   "CLDOOR3"
7    uint  the version of the link it speaks, 1
11   16  nonce, random
27   32  X25519 public share of a secret of 32 random bytes
59   1   1 if this end dialled and seals the link, else 0
```

**2. Each reads the other's first 11 bytes**, and they must be its own:
`CLDOOR3` and the version it speaks. If not, it closes at once, without
waiting for more. Then the other 49. The other's nonce must not be the one
this end sent. A listener seals if the
dialler's last byte is 1; a dialler seals by its own choice, below.

**3. Each makes what was said, 97 bytes:** the dialler's nonce, the
listener's nonce, the dialler's share, the listener's share, and the seal
byte, that order whichever end it is.

**4. Each sends its proof, 32 bytes:**
`HMAC-SHA256(mesh key, the first 11 bytes of a hello + "D" or "L" + what was said)`, `D` from the
dialler, `L` from the listener.

**5. Each reads the other's 32 bytes** and compares them with what the other's
proof must be, in time that does not depend on where they differ. So each
end knows the other has the key, and the hellos were the ones sent.

**6. The keys of the link.** Not sealed: both are 32 zero bytes, and are not
used. Sealed:

```
shared = X25519(this end's secret, the other's share)      all zero: close
prk    = HKDF-Extract(salt = the two nonces, 32 bytes,
                      ikm  = shared + mesh key)
k_dl   = HKDF-Expand(prk, "ChrysaLisp link, dialler to listener", 32)
k_ld   = HKDF-Expand(prk, "ChrysaLisp link, listener to dialler", 32)
```

The dialler seals with `k_dl` and opens with `k_ld`, the listener the other
way. HKDF is RFC 5869 over HMAC-SHA256.

**The mesh key** is the file `mesh_key`, trimmed: 64 hex digits are its 32
bytes as they are, anything else is
`PBKDF2-SHA256(text, "ChrysaLisp mesh key", 100000, 32)`. A machine with no
key makes no links.

**Who seals.** The dialler does, when told to (`!host:port`, `link -s`), or
when the place it dials is outside, by `(door-outside?)`: inside is
`localhost`, a name ending `.local` `.lan` `.home` `.internal`, IPv4
10/8 127/8 172.16/12 192.168/16 169.254/16, and IPv6 `::1` `fe80:` `fc` `fd`.

Then each end's out task sends a ping at once.

### What goes over it

Every packet starts with a wire header:

```
0   uint  length   of what follows the wire header
4   uint  type     1 ping, 2 frag
```

**A ping, 48 bytes:**

```
0    8   wire header, length 40, type 1
8    16  node id of the sender
24   16  system id of the sender
40   uint  task count, its tasks, with no lag
44   4   pad, 0
```

**A frag, 72 bytes and the data:**

```
0    8   wire header, length 64 + data, type 2
8    64  lk_frag, as above
72   ..  the data, length of it, not padded
```

A packet of an unknown type is dropped, and a frag of more than 6104 bytes
of data. An end sends a ping as it starts, and then only when it has had
nothing to send for 5 seconds.

**Sealed**, each packet, ping or frag, wire header and all, goes as a record:

```
0    8   n, the length of the packet, in the clear
8    n   the packet, ChaCha20-Poly1305 (RFC 8439), no additional data
8+n  16  the tag
```

The nonce is 12 bytes, a count, 8 bytes, then 4 zero bytes. Each way has its
own key and its own count, from 0, one on for every record. A record with n
less than 8 or more than 6176, a tag that is not right, or a frag whose
header says more data than the record has, ends the link.

## Messages, over either

A message of no more than 6104 bytes goes as one fragment, offset 0, total
0, src the sender's mailbox.

A bigger one is cut into fragments of 6104 or less. Each has the message's
dest, a src whose mailbox is a parcel number, made new for the message, and
whose node is the sender's, its offset in the message, and the whole length.
A node that is the dest gathers them by src into one message, in any order,
and has it when it has as many bytes as the total. The sender's own mailbox
is not sent with a big message.

A fragment for another node is sent on, unchanged, towards it.

### Calls of the kernel

Mail to mailbox 0 of a node is to its kernel. The first long is what is
asked:

| | | |
|---|---|---|
| 0 exit | 1 callback | 2 signal |
| 3 pin, a task started there | 4 run, a task started on the least busy | 5 want, send the services |
| 6 ping | 7 ping of the system | |

```
kn_msg, 40 bytes, the start of every call
0    long  func
8    long  key, said back in the reply
16   24    reply_id, net id

open, pin or run: kn_msg, then the path of the task, ending 0
reply: kn_msg, then 16, the node id of the node that answered
```

A pin, or a run done where it was asked, is always answered with a reply.
`reply_id` is the new task's net id, or all 0 if it was not started, a file
not there say. `run` sent on to another node is the same message with only
the dest node changed.

A want is a `kn_msg` of func 5 and nothing else said, to mailbox 0 of the
node whose services are wanted. That node answers with a full ping, no more
than one a second.

### Pings of the network

These are mail, not link pings, and are flooded over every link:

```
kn_msg_ping, 80 bytes, then the services
0    long  func, 6 or 7
8    16    via, the node it came from last
24   16    origin, the node that sent it first
40   16    the origin's system id
56   uint  session, one more each time the origin pings
60   uint  hops, one more each node it passes
64   uint  hash of the services, 0 for none
68   uint  full, 1 if the services follow
72   uint  interval, microseconds, till the next
76   uint  kick, 1 if the origin saw a link come or go
80   ..    services, if full, each "name,<net id in hex>,<system id in hex>,<info>", a line each
```

A ping (6) has the services whose names start `*`, a ping of the system (7)
those that start `@`, and goes only to nodes of the same system. A node that
gets a newer session than it knew floods it on, one with the same session and
fewer hops too. A node that gets a hash it does not have sends a want. A ping
comes again in the interval, less up to a quarter at random, and the
interval goes from 1 s up to 64 s by doubling.

## When a link is gone

Shared memory: while the other's host process is known, it is looked for
each second, and the link is down when it has gone, or has taken nothing
from the ring for 60 seconds. With none known, the link is down when no
ping has come in the timeout, which starts at 3 seconds and comes to twice
the longest gap seen, no more than 60.

TCP: there is no timeout. The link is down when the socket fails or is
closed, a packet or record is wrong, or it is told to drop.

## The mesh

What the Net services say to find each other is not on a link.

* A beacon, UDP to port 3334, to 255.255.255.255 and 127.0.0.1, every 2
  seconds, text:
  `CHRYSA_BEACON:<port>:<system id, 32 hex>:<instance, 32 hex>[:D][:B<boot id, up to 16 hex>][:P<version>]`.
  `D` if it can be dialled, `P` the version of the link it speaks. One with
  no `P` is of before there were versions.
* A hello, mail to each `*Mesh` service every 2 seconds: a 24 byte reply net
  id, a uint type 6, then the text
  `CHRYSA_HELLO <system id>` and a line `<system id> <ip> <port>` for each
  machine it has heard of in the last 30 seconds.

## Versions

The version of the link is `+door_version` in `service/net/door.inc`, 1 now,
for all that this document says goes over a link once it is through the
door. It is one more each time any of that changes, and two machines that
speak different versions do not link: each reads the other's version first
in the hello and closes. What says one build from another:

* The version in the hello, after `CLDOOR3`. A v7.4 node says `CLDOOR2`, and
  one of v7.3 `CLDOOR1`, and neither links to a node of now. They are closed
  on as soon as their first 11 bytes are read.
* The `P` field of a beacon, the same version, so a machine heard and not
  linked can be said to speak another, by `mesh`, before it is dialled. A
  beacon with no `P` is of before there were versions.
* The `B` field of a beacon, the boot image's id, shown, not acted on.
* "Net Service 0.1" and "Mesh 0.1", the info of two services.

A shared memory link has no version. Two nodes of builds whose
`lk_shmem_size` differ do not link: the second end to open gives up after 5
seconds. Two builds of the same size and another layout would.

What two builds must agree on is all of this document: every offset, the
enums (record status, wire type, kernel calls), `lk_data_size`, how the
services are hashed, and the byte order.

## Faults found writing this

Found reading the code for this document, 2026-10-21, and put right the
same day. A node from before then does these things, and one that talks to
it must take them:

* **Bytes not written went out.** The 4 pad bytes at the end of a TCP ping (44
  to 47), and of a fragment header (60 to 63, 68 to 71 of a TCP frag packet),
  were never set. They carried what was in that memory before: on a shared
  memory link the ring's old data, on an unsealed TCP link stack bytes on the
  wire. They are 0 now. A reader is not to look at them.
* **A TCP link with no door read a seal it was not given.** `:host_net :link`,
  the link made with no Net service (`net-link-rpc`, the loopback tests), sent
  its connection task a message of 16 bytes, the handle, and the task reads 80,
  the handle, a seal flag and two keys. It took for the seal what the rest of
  the message's cell had. It sends all 80 now, 0 but the handle: not sealed.
* **A shared memory file was left behind.** `pii_close_shared` unlinked the
  name it was given, not `/tmp/<name>` where the file was made. A file left
  behind and opened again has the node ids of its last two ends in it, and the
  ends that open it then are both guests. The scripts that start nodes take
  the files away; now a link that closes does too.

## Tested

`tests/solo/test_link_wire.lisp` holds the TCP link to this document. A
node's real end, the door and the link tasks, meets a bare socket in
`tests/solo/link_wire_peer.lisp`, which goes through the door by the
formulas here and takes apart what it is sent: the hello field by field, the
proof, the first ping, and a fragment, with their offsets written out as
numbers, so a change to the code that this document does not have fails it.
A shared memory link can not be tapped from Lisp: its size check is
`tests/solo/test_link_size.lisp`.

## The files

| | |
|---|---|
| `sys/link/class.inc`, `sys/link/class.vp` | the shared memory link |
| `service/net/class.inc`, `service/net/link.vp` | the TCP link: wire header, ping, frag, sealing |
| `service/net/door.inc`, `door.lisp`, `gate.lisp` | the door |
| `service/net/mesh.inc`, `app_impl.lisp` | beacons, hellos, who dials |
| `sys/mail/struct.inc`, `sys/mail/out.vp`, `sys/mail/in.vp` | net ids, cutting and gathering messages |
| `sys/kernel/struct.inc`, `sys/kernel/class.vp` | kernel calls, pings, routes |
| `src/host/net.cpp`, `src/host/pii_*.cpp` | sockets, the shared memory |

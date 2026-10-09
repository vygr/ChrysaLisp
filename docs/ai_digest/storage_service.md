# The Storage Service, Notes On An Idea

None of this is built. These are notes from a conversation between Chris and
Claude on 2026-10-07, to put some flesh on the bones of the idea, and the
reason the file system work, `lib/fs/exfat.inc` and `lib/fs/fat32.inc`, is
being done. Where a thing is Chris's wish it says so. The rest is what was
talked over, and is open.

## The Idea

In Chris's words, "a distributed raid, with the kicker of sending the tasks to
the data".

A `@Storage` service, one for each system, a system being a machine. The
services talk to each other and arrange to distribute, replicate and migrate
what is stored, and to keep it available, with nobody asking them to.

* An app sends a request to read or to write, and gets the data back.
* An app can also send a query, code, a task, and it is sent to where the
  data is. The data is not brought to the asker. The reply goes straight back
  to the asker.
* Each service keeps a file system image on its machine. It stands apart from
  the host's file system, though the image may be a file of the host, held
  with the exFAT driver.
* It heals itself, blocks that are lost are made again from what is left.
* Like Hadoop, in what it is for.

## What Chris Wants Of It

* No primary and no secondary. Every service is able to carry on for any of
  the others.
* It works when there is only the one service.
* A new machine is plugged in, a new `@Storage` appears, blocks start to
  migrate to it, and the space and the speed of reading go up by what the
  machine brings. His example, the assets service of a games company.
* A machine goes down, and what is left is copied till there are three
  copies of it again, say three, to bring the resilience back.
* Encryption, probably. Error correction of blocks, maybe.

## An Object Is A File

Probably. So it has a path, and so there has to be agreement, between
services that may not all be up, on what is where.

It helps to see two maps, which need very different amounts of agreement.

* Path to object. An object is the list of the blocks of a file. This map is
  small, and it is the one that needs real agreement, two machines must not
  differ on what a path is now.
* Block to who holds it. This needs none. If a block is named by its content,
  see below, it never changes and it checks itself, so a wrong answer does no
  harm, the one asked says no, or sends what does not match its name, and
  another is asked. A service can just say what it holds, now and then.

Hadoop is this split, its name node has the paths and its data nodes report
their blocks. It has one name node, with a standby to take over, which is
what is not wanted here.

For the path map, two ways to agree.

* A leader. The services choose one of themselves, any of them can be it,
  and every change to a path goes through it. A reader never sees two
  answers. Nothing can be written by a machine that can not reach most of
  the others, and two machines are an awkward number.
* Versions, and no leader. Every service has the whole map, an entry has a
  version and who wrote it, and changes spread. A machine that is cut off
  can go on writing. When it is back there can be two versions of a path,
  and one is chosen, or both are kept. This is nearer to how the directory
  of services already works.

For a store that is mostly read, the versions look the better fit. The
question under it all is what is to happen when two machines have both
written a path while apart. That is to be decided, and most of the rest
follows from it.

## A Block Is Named By Its Content

The name of a block is a hash of what is in it. One choice, and it gives
three things.

* Deduplication. Two objects with a block the same have the one name in
  their lists, and it is stored once.
* Integrity. The name is the checksum. A block that does not hash to its
  name is bad.
* Cheap replication. Two services compare lists of names and send only what
  the other has not got. Migration is the same.

What it costs.

* A block can only go when no object anywhere has it in its list. That is
  counts of references, or a sweep, over machines, and is likely the most
  work in the whole of it.
* The hash has to be one that can be trusted to mean the same block, SHA-256
  or BLAKE2.
* An index, from name to where the block is on the volume.
* It pulls against encryption, see below.

How blocks are cut. Of a set size is simple, and finds whole objects that
are the same and the parts of a large one that did not change. It misses all
that follows an insertion, the data has moved along. Cuts placed by the
content, a rolling hash says where, do not, and cost blocks of all sizes and
more work on a write. Start with a set size, the other can come later with no
change to the model.

A write never changes a thing where it is. It makes new blocks and a new
object, and the path is pointed at it. So an old version is only its list of
blocks, kept, and a snapshot is nearly free. That fits the last known good
image of a machine that boots into ChrysaLisp, it is a path that is not let
go of.

## Where A Block Is Kept

By a rule, not by a table. For a block, each service ranks every service
that is up by a hash of the name of the block and the id of the service. The
first three hold it. Every service works out the same answer from the list
of members and nothing else, so there is no table of placements and nobody
in charge of one.

* A new machine comes out in the first three for its share of the blocks,
  and only those move. Rank with a weight for the size of the disk, and a
  bigger machine takes more.
* A machine goes, and for each block it held the next service in that
  block's ranking is now one of the three, and fetches a copy.
* With one service the first three of one is itself. The rule is three, or
  as many as there are.
* A read can go to any that hold the block, the nearest, or the least busy.
  A query goes to one of them the same way. So three copies are three
  places a query can run, replication is speed as well as safety.

Migration, and copying to get back to three, run behind the reads and
writes of the apps.

## Who Is A Member

The question that is always there, how long to wait before the worst is
assumed. Here a wrong guess costs copying that was not needed, and never
wrong data. A machine that comes back still has good blocks, they check
themselves, it says what it holds and the copies there are now too many of
are trimmed later. And while the wait goes on every block is still read from
the two copies that are left, and a block written in that time gets its
three from the machines that answer.

So there need not be the one timer.

* Suspect soon, act late. After a few seconds of silence no reads are sent
  to the machine. That is cheap and undone at once. Copying is what costs,
  and waits longer. Hadoop waits about ten minutes.
* The danger sets the hurry. A block with two copies can wait. A block down
  to its last copy is copied now.
* A machine that is shut down on purpose says so, and there is no wait.
* A machine that has come and gone a lot is waited for longer than one that
  was steady for weeks.

It is three copies that buy the slack. With two the same wait is a gamble.

## Encryption And Error Correction

* A cipher that authenticates puts a tag on a block, and a block that fails
  its tag is known to be bad.
* Correction goes outside the encryption, it is worked out over the blocks
  as they are stored, so a block can be mended before anything tries to
  read it.
* The name, or the tag, says which block is bad, so correction is filling in
  a block that is missing, not hunting for a fault. One block of XOR parity
  for a group mends any one block of the group. Reed-Solomon is for more
  than one.
* Replication mends from another machine. Correction mends on the one
  machine, with nobody to ask, which is what a system of one machine has.
* Encryption and deduplication pull apart. A cipher makes two blocks that
  are the same look different. For them to be found the same, the key or
  the nonce of a block has to come from its content, and then whoever can
  see the store can tell that two blocks are the same, and can test for a
  file they have. Whether that matters depends on who can see a store.

There is a hash now, SHA-256, and a cipher, ChaCha20 with Poly1305,
[`docs/ai_digest/crypto.md`](crypto.md). There is no field arithmetic yet. They are VP, a
block at a time is too much for Lisp. ChaCha20 with Poly1305 is add, rotate
and xor, no tables and nothing of any one CPU. The same cipher and hash
would serve TLS.

Where the keys are kept, and whether the machines share one, is open.

## Tasks To The Data

This is where ChrysaLisp has it over what it is like. Elsewhere the compute
was bolted on to the store afterwards. Here, to start a task on a chosen node
and have it answer the asker is what the kernel does already, and what
`lib/task/jobs.inc` does every day. The store only has to say which node
holds the data.

A query is Lisp source, or a task named by its path, and each node runs it
as native code for the CPU it has, as a shader is assembled for each machine
the first time it meets it. The reply is only the answer, a search of a 3MB
asset sends back a few bytes, so a slow link matters far less. And a new
kind of query needs nothing new in the service, which does not know what a
query does, only that it is to run beside the data.

## Trust

To run code that came in over a link is fine among your own machines. Chris,
on the rest: security and signatures are a topic and a library of their own,
and ChrysaLisp is not trying to be a protected mode system, not yet, though
a trusted build could run in ring 0 with a ChrysaLisp for each user in user
land. The store is not to wait for that.

It leaves room for it. Names that come from content, and a small path map,
are what a signature is put on, sign the entries of the path map and all
that is under them is covered. And a node is only ever reached by messages
over its links, so a link is where trust would be checked.

## What Exists

* `lib/fs/exfat.inc`, exFAT read, written and formatted, on a file of the
  host or a memory stream, with a cache of blocks, batches of writes, and a
  mark for a volume that was not let go of cleanly.
* `lib/fs/fat32.inc`, FAT32, read only.
* `lib/crypto/sha256.inc`, SHA-256, what would name a block by its content,
  and HMAC on it.
* `lib/crypto/aead.inc`, ChaCha20 with Poly1305, to seal a block and open
  it.
* The directory of services, `(mail-declare)` and `(mail-enquire)`, by which
  the services would find each other.
* `lib/task/jobs.inc`, and `(open-task)`, tasks to a chosen node with the
  answer sent to the asker.

## Open

* What happens when two machines have both written a path while apart.
* A leader for the path map, or versions.
* How a block that nothing refers to is found, over machines.
* Whether the store may show that two blocks are the same.
* Where the keys are kept.
* How an object is cut into blocks.
* What an app's request looks like, and whether a path is all an object is
  known by.

# The Storage Service, Notes On An Idea

These began as notes from a conversation between Chris and Claude on
2026-10-07, to put some flesh on the bones of the idea, and the reason the
file system work, `lib/fs/exfat.inc` and `lib/fs/fat32.inc`, is being done.
Where a thing is Chris's wish it says so.

On 2026-10-11 Chris said to start building it, and the questions were gone
through. What was settled then is under Settled, and what is built is under
Built, each as it lands. The sections between are the talk of the 7th, left
as they were said: where they and Settled differ, Settled is how it is.

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

```image
docs/diagrams/storage_blocks.cwb
```

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

## Settled

Chris, 2026-10-11, in his words where they are quoted.

**Who is a member.** "system_id is member ?" Yes. One service a system, so a
member is a system id, and the members of a store are a list of them. A
machine becomes one by being told to join a store, is one while it is away,
and stops being one only when it is retired on purpose: a laptop taken home
is still one of the five. And "the image carries the store id and member
id": a volume opened by another member than the one it was, a machine given a
new id, a disk moved to another machine, has that member take the place of
the one it was. There is not one more member, and nothing is copied.

**Two machines write a path while apart.** He asked of three things, not
taking any as given: a lock service for the whole store, a time all agree on,
and transactions.

* A lock every machine honours is the same agreement again: who has it, and
  when the lock of one that has died is no lock. It can be had with no
  primary, by most of the members saying yes, and then a machine that can
  not reach most of them can not write.
* A time, from the clocks, no: they differ, and the later of two is a guess
  that throws a write away when it is wrong. A count, yes: how many times
  each member has written the path, as far as the writer knew. It says what
  no time can, whether one write was made knowing of the other, or the two
  were made apart.
* A transaction, the path set to B provided it is A, stops a write being
  lost between machines that can reach each other, with no lock held. It
  does not settle the machine that is cut off.

Then his: "If the cut off machine does a write and the Storage node he asks,
directs his write to a node he is still in contact with ... does he ever know
he was cut off ?" No. Being cut off looks the same as the others being
switched off. Of the blocks it does not matter, a block is right wherever it
is. Of the path it is known only by counting heads before, or by comparing
the counts after.

So:

* There is one way to write, a path is set to an object provided it is now
  what the writer says it is, and every entry has the counts.
* The store always takes the write, and says how many members have it:
  seen by 1 of 5.
* When members meet, an entry that most members had keeps the path over one
  that they had not. "so your going with the majority wins the write ?" Yes:
  at most one side of a split can have most of the members, so a writer who
  waited for most is promised its write stays. Between two that neither had,
  the counts, then the member's id, only so that all choose the same.
* The one that does not keep the path is kept under another name. It is
  never thrown away.
* It only bites where both sides wrote the same path. "if I pull a laptop
  from the rack, and take it home, when I come back, I really ought to be on
  the losing side of any stuff I write locally", yes, and "I could work on my
  user/me/xxxxx and then come in to work and nothing is going to go wrong at
  all ?", yes.

**Pins.** A machine holds only its share of the blocks, so away from the rack
part of anything would be out of reach. A member can pin the start of a
path, `user/me/`, and holds all of every object under it beside the copies
there are, and does not let go of it. It can say if what is pinned is all
there yet. Chris: "yes, pinning is how I'd expect it to work".

**A query is a thing that walks.** Chris asked what a query does that reads
more than one block: "does it get to the end of a block say on machine a, and
then hits the 'next_buffer' and get moved over to where the next block is on
machine b.... that could be interesting ?" A task that is running can not be
moved, what it is doing is in its stack. What can be moved is a function and
what it has so far. So a query is that: what it has, and a function given
that and a block, which gives what it has now and which block it wants next,
or that it is done. A member runs it over the blocks it holds, and when the
next is one it does not hold, sends the function and what it has on to a
member that does. The last sends the answer straight to who asked.

Chris: "exactly ! ... you have to form a query task, and that has
rules/packaged_state as well as the programm that runs the state machine and
decides when it found it result or finish it's search etc. Provided we can
ensure the rule, we can transport the task all over the place from block to
block.... now that would make other systems weep !" and "It's like you send
off a gopher task in to the network and it just runs around until it gets
what it wants and then comes directly back to you !"

Because the function says where to go next, it need not go in order. A search
of a sorted object by halves reads three or four blocks of it and no more,
wherever they are.

**Taken as they were put, not yet talked over:** blocks of 64KB cut at fixed
places, named by SHA-256; three copies; where a block is kept worked out from
the members by a hash rule, no table; five minutes before the blocks of a
machine that has gone are made up; no encryption and no error correction
yet; a member sweeps its own blocks that nothing is made of.

## Built

**The store, one member alone**, `lib/storage/store.inc`, 2026-10-11. All
that a member holds, and all there is when there is the one. It knows nothing
of mail or of another member.

An exFAT volume, a file of the host or a disk, with on it:

```
store         what it is: the store's id, the member's, the block size
members       the members, a line each
pins          the paths this member holds all of
paths         the path map, a line an entry
blocks/xx/y   a block, its name in hex, xx the first two of it
```

An object is a manifest, how long it is and the names of its blocks in
order, and is a block itself. A path is set to the name of one. An entry of
the path map is `(path hash bytes vv writer seen deleted)`.

```
(store-make device size id member [block_size])   a new store
(store-open device member) -> store               :was says who it was
(store-put store path data [from])                (:ok entry) (:stale entry) (:full)
(store-commit store path names bytes [from])      of blocks kept already
(store-get store path)   (store-read store path offset len)
(store-walk store path state visit [at])          a block at a time, where it says
(store-fold store path state step)                in order
(store-delete store path [from])   (store-list store [prefix deleted])
(store-sweep store)                               blocks nothing is made of
(store-check store [mend])                        blocks that are not their names
(store-pin store prefix [off])   (store-pins-missing store)
```

`from` is what the writer says the path is now, the name of an object, `""`
for none, and not given, not to say.

What the tests hold, `tests/system/test_store.lisp`: a block is had back as
it was, and one changed on the disk is not given and is found; two objects
of the same bytes are the one; 2MB of a pattern that comes round fits a
store of 1MB; a write from what the path was, by one who did not see it
change, is refused and told what it is now; a deleted path stays as an entry;
a sweep lets go of what nothing is made of and of nothing else; a store
opened by another member is that member's, in the place of the one it was;
and a search by halves of an object of eight blocks reads four at most.

**The service, and a gopher**, `service/storage/`, 2026-10-11. One of a name
for a system, `@Storage` unless given another. It keeps one store and does
what it is asked by mail, a thing at a time, so a write is all done or not
begun as far as another asker can see. `(storage-start image [size id])`
starts it on a file of the host or a disk, and it is not started by itself
yet: nothing of a session changes till someone asks for it.

An object goes to it a block at a time and comes from it so, and only the
blocks it has not got are sent: a mail is never longer than a block and a
little, and one member will ask another for blocks the same way an app does.

```
(storage-put path data [from])    (:ok entry) (:stale entry) (:full) (:lost)
(storage-get path)   (storage-read path offset len)
(storage-stat path)  (storage-list [prefix])   (storage-delete path [from])
(storage-walk path state visit [at wait])      a gopher, see below
(storage-pin prefix [off])   (storage-pins-missing)
(storage-sweep)   (storage-check [mend])   (storage-info)
```

`service/storage/app.inc` is all an app imports. `storage`, the command, is
the same by hand: `storage -s image -m 64`, `storage -p file path`, `storage
-g file path`, `storage`.

A gopher is a task of its own, `service/storage/gopher.lisp`, so that what it
does and how long it takes are not the service's affair: one whose function
throws, or never ends, harms nothing but itself. It is mailed who asked, the
service that has the blocks, and as text the path, the block to begin at,
what it has, and the function. It asks the service for each block it goes
to, and mails what it has at the end straight to who asked.

```
(storage-walk "logs/today" 0
    '(lambda (n data at count)
        (list (+ n (length (filter (# (eql %0 (ascii-char 10))) data))) (inc at))))
-> (13804)
```

The function is written out, quoted, and sent as text: what it has and that
text are all there is of the gopher, which is what lets it be sent on. To be
run a form has its macros done and its names bound, and that is done in the
form itself, so the gopher keeps the function as it came and runs a copy,
`(exec (copy source))`. Chris: "keep the original source so you can send it
on with the state". What it has must print as it reads, numbers, strs, lists
of them.

**On a real disk.** Chris's 4GB flash stick, through its raw device, on the
M4: a store made of the whole of it in 0.6 seconds, eight files of the tree,
1MB, put in 4.0 seconds and had back the same in 0.2, and a gopher that
counted the lines of one of them where its blocks were. Then mounted by
macOS: `blocks`, `members`, `paths`, `pins`, `store` are there to see, a
block's SHA-256 by the Mac's own `shasum` is its name, and the Mac's checker
says the volume is OK.

**Members meeting, the rules of it**, in `lib/storage/store.inc`, 2026-10-11.
No mail yet: what one member does with what another tells it.

```
(store-join store members [retired])   members only grow, and so do the retired
(store-members store)                  who they are meant to be, away or not
(store-meet store entries)             another's path map set against mine
(store-keeps store a b)                of two written apart, which keeps the path
(store-most? store entry)              do most members have it
(store-other-name entry)               path~ and the start of its object's name
(store-digest store)                   two with the same have nothing to tell
(store-holders store name [among])     the members that keep a block
```

An entry's `seen` is the members known to have it. An entry of another's
that comes after mine is taken, one that comes before is not, the same one
makes each know the other has it. Two written apart: one keeps the path,
with the counts of both so that it comes after both, and the other, if it is
an object and not a delete, is kept under `path~` and eight of its object's
name. Which keeps it: the one most members have; else the one written more
times; else the one whose object's name is greater, only so all choose the
same.

Who the members are is two lists that only grow, those joined and those
retired, so two members put theirs together the same whichever way round,
and one that is retired stays so.

Where a block is kept needs no table. Each member is given a number, the
SHA-256 of the block's name and the member's id, and the three with the
greatest keep it. Every member works out the same. When one goes, only what
it kept has another keeper: of 300 blocks over five members each keeps about
180, and with one gone the blocks that move are the ones it kept, each to
one new keeper.

What the tests hold, `tests/system/test_store_meet.lisp`: a laptop's own
paths are taken when it comes back and there are no other names; two that
write a path apart come to the same, and what did not keep the path is had
under its other name on both; what two of three had keeps the path over what
the one away wrote three times; a delete by most beats a write apart, and
the write is kept; a write by most beats a delete apart, and nothing is
kept; three that each wrote a path come to the same in whatever order they
are told, with all three objects there.

**A store of many members**, `service/storage/`, 2026-10-11.

A service is a member. It is known to the others by the name of their
group, `*Storage`, with the store's id and its own after it, on whatever
machine it is: the same directory of services everything else is found by.
Any service of the same store id in the group is a member to it, and who the
members are is put together as they meet. So a machine joins a store by a
store of that id being made on it.

A service only ever answers. It asks nothing of another member and waits,
for two that did, each of the other, would wait for ever. What a member has
to ask is asked by its **tender**, `tender.lisp`, a task of its own, as a
gopher is. Each time it looks, every two seconds, and at once when its member
is written to:

* It finds the other members, and tells its service who is there.
* With each that is not as it is, by the one name of all it has,
  `(store-digest)`, it compares notes: what the other has is taken here, and
  what this one has, with that, is taken there. The two are then the same.
  Chris's picture of it, from the mesh: people with notepads who "just meet
  and compare what they have and swap some pages between them".
* It asks its service for its share, `(store-share)`, among the members that
  are there: every manifest, the blocks it is one of the three keepers of,
  and all the blocks of what it has pinned. What it wants and has not, it
  fetches from a member that has. What it has that is others' to keep, it
  lets go of once each of those says it has it.

A member not heard from is still there for a while, five minutes, before
what it kept is kept by others: a machine turned off and on is no reason to
copy its share.

A write is taken by the one member and is done. The others have it when
they next meet, and `(storage-seen path)` is how far it has got, `(4 5)`,
seen by four of five.

A block the member asked has not got is had from one that has: the service
says who, `(storage-where)`, and it is the asker that goes there.

**The gopher goes to the blocks.** When the block it wants is not kept by the
member it is at, it finds a member that has it, and all there is of it,
where it is, what it has, its function as it was sent, and where it has been,
is mailed to a new gopher task there. The one that sent it is done and says
nothing. The last mails the answer, and the trail, straight to who asked.

```
(storage-walk "big" 0 '(lambda (sum data at count) ...))
-> (4611 ("m-a" "m-c" "m-a" "m-d" "m-b" ...))
```

What the tests hold, `tests/system/test_storage_members.lisp`, five members
on one node, each with a store in a file of its own, their tenders looking
only when told so that every step is done when it says:

* Three members meet and each knows the three. An object of 20 blocks put at
  one is seen by 1 of 3, and when they have met by 3 of 3.
* Two more members: every block is kept by three of the five, no more and no
  fewer, 60 copies of the 20; the manifest by all five; the member it was put
  at has let go of what is others' to keep; it is had whole from any of them.
* A gopher sent to it from a member that has not all of it comes back with
  the right sum, having been to more than one member.
* One member is cut off and writes a path of its own and one the rack also
  writes, twice: 1 of 5, and 4 of 5. It comes back: its own path is on the
  rack, the path both wrote is the rack's, and what it wrote last is under
  the other name on a member that was never away. The five are the same.
* A member pins the object and has all 20 blocks, unpins and every block is
  back to three keepers.
* A member is stopped: every block is kept by three of the four that are
  left, and it is still a member. Started again: three of the five.

**On three machines**, the night of 2026-10-11. A service started in the
standing node of the M4, the x64 Mac and the Raspberry Pi 4, each on a store
of 64MB in a file of its own, the one store id, two copies a block so that no
machine would have all of anything. Nothing else was done to join them.

* Six seconds after they were started each listed the three as members.
* The boot image, 250KB, and `STATUS.md`, 633KB, put at the M4: `1/3`. Eight
  seconds later `3/3` on all three, and 9, 12 and 13 blocks kept, 34, which is
  two copies of the 14 and the two manifests on each.
* The boot image had from the Pi's store and `STATUS.md` from the x64's are
  the M4's files, by SHA-256.
* A gopher sent from the Pi to count the lines of `STATUS.md` came back with
  13,881, which `wc -l` says too, by way of the Pi, the x64, the M4 and the
  x64 again.
* The Pi's service stopped, a file put at the x64: `2/3`. The Pi's started
  again on its store: eight seconds later it has the path, `3/3`, and the
  file is had from it as it was put.

A gopher goes, of the members that have the block it wants, to the one with
the longest run of the blocks after it, so that it is not sent on again till
it must be. How many members keep a block is the store's, said when it is
made, `:copies`, `storage -k`, three if it is not.

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

* It is not started by itself. On the three machines it was started by
  hand, in the standing nodes, and stopped after.
* The whole path map is sent when two members differ. `lib/hash/tree.inc`,
  which sync has, would send only what differs.
* A member is one by a store of that id being made on it. Retiring one is
  `(storage-members "-id")`, and what it kept is then others' to keep.
* How a block that nothing refers to is found, over machines, while a write
  is on its way: a sweep is only when asked.
* A write is done when the one member has it. Waiting till its blocks are on
  three is not there yet, only how many have its path.
* Whether the store may show that two blocks are the same, and where the
  keys are kept, when there is encryption.
* Cutting an object where its content says, not every 64KB.
* A manifest of a very long object is itself long, it is one block of any
  size for now.
* A gopher's function can do anything a task can. What it may do is not
  fenced.

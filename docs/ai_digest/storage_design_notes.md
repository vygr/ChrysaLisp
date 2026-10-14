# The Storage Service, Design Notes

How the Storage service came to be: the talk it began with, what Chris
settled and in his words, what was built, as it landed, and what each
measurement found. A diary, kept as it was written. What the service is
and does now is [storage_service.md](storage_service.md); where the two
differ, that is how it is.

The notes began on 2026-10-07, from a conversation between Chris and Claude,
to put some flesh on the bones of the idea, and the reason the file system
work, `lib/fs/exfat.inc` and `lib/fs/fat32.inc`, was being done. On
2026-10-11 Chris said to start building it.

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

**A machine made a member, and started by itself (2026-10-13).** Chris, of
the choices: a machine is not a member unasked; what it was told is kept as
a fact of the machine; it is started "both TUI and GUI launch"; "mesh_key as
a default store defintely a good one"; three copies "up to"; and of two
sessions both starting it, "we need some way to win the possible race
condition".

* `storage -j`, `(storage-join)`: the machine's part of the store is made,
  `storage/store.img`, 4096 MB at most, a file with holes that takes what is
  in it, and started. What was said is kept in `store_member` at the root of
  the tree, beside `mesh_key` and like it not in git. `storage -f` forgets.
* The id of the store is made from the key, `(storage-key-id)`, a hash of
  it: the same key, the same store, nothing typed. The key can not be had
  from it.
* It is started by `service/storage/auto.lisp`, which the Net service
  starts when its session becomes the machine's member of the mesh and
  tells to go when it is that no longer. Only one process can listen on a
  port, so only one session is the member, and that is the host's doing and
  can not be raced. The list of services could not settle it: two sessions
  can each look, see none, and each declare one.
* A session started alone, a run of the tests, never asks for the port and
  so never starts the store.
* Asked for by hand in another session, what holds two off the one image is
  a note beside it, `store.img.member`, the id of the process that has it:
  `(storage-claim)`. One of a process that has gone is nobody's.
* Three keep a block, or all the members when there are fewer: that was so
  already, `(store-rank)`.

Seen on the Pi: joined with the one command, 780 KB on disk of 4 GB; its
node restarted and the store up again with nothing typed; a second session
asked to join and told another has it.

**Pins.** A machine holds only its share of the blocks, so away from the rack
part of anything would be out of reach. A member can pin the start of a
path, `user/me/`, and holds all of every object under it beside the copies
there are, and does not let go of it. It can say if what is pinned is all
there yet. Chris: "yes, pinning is how I'd expect it to work".

What each member has pinned is known to every member. It is a line for each
member, the prefixes and a count that member alone adds to, and it goes with
the path map when two compare notes, as the list of members does:
`(store-pinned-by store path)`, `(storage-pinned-by path)`. So a gopher is
sent to a member that holds all of the object, and stays there, and has
nobody to ask who that is. Before, it found such a member only when the sum
of who keeps a block happened to send it there. Chris, of a gopher that
went round eight members to get to the one with every block: "yes, put the
pin in the path map". It is with the path map and not in each entry of it:
a pin is of a prefix, every path under it and those to come, and a line a
member has one writer and nothing to settle.

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

**Tasks of one system at one path.** Chris, 2026-10-13: "Did we update policy
to recomen local path accses by tasks from one system_id should use the lock
service ? maybe like \"storage/.../.../.../file.inc\" ? does that sound like
a plan ?" Yes.

* Tasks of one system that read a path of the store, change what they read
  and put it back, take the lock of that path first, from the `@Lock` service
  that system has already, see [lock_service.md](lock_service.md). The key is
  `storage/` and the path in the store: `storage/tree/apps/edit/app.lisp`.
* The keys are paths, so a lock of a folder holds all under it, and the
  `storage/` at the front keeps them apart from the locks of the host's
  files, which are keyed by their paths as they are.
* It is `(with-read-lock)` and `(with-write-lock)` as for a file, and what
  was read is let go of before the lock is.
* It is a courtesy between tasks, and it is this system's only: `@Lock` is
  not seen from another system, so a member on another machine, or a gopher
  that came from one, is not held by it. Nothing in the store asks for it.
* What can be trusted is the store's own rule, above: a path is set to B
  provided it is A. Two tasks that both held the lock never see `:stale` of
  each other. A task that did not ask, or a write from another system, is
  still caught by the rule and by nothing else. So a writer says what the
  path was, lock or no, and reads the answer.
* `(storage-put)`, `(storage-delete)` and `(storage-stream)` do not take the
  lock themselves. The task that holds a lock round a read and a write would
  then wait on itself, the lock is not one a task can take twice; and a
  lock a call took and let go would cover the write and not the read before
  it, which is the part that needs it.

```vdu
(defq key (cat "storage/" path))
(with-write-lock key
	(defq was (storage-stat path) data (storage-get path))
	(storage-put path (change data) (if was (second was) "")))
```

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
(storage-copies path [copies])                 how many members keep its blocks, the dial
(storage-pin prefix [off])   (storage-pins-missing)
(storage-sweep)   (storage-check [mend])   (storage-info)
```

`service/storage/app.inc` is all an app imports. `storage`, the command, is
the same by hand: `storage -s image -m 64`, `storage -p file path`, `storage
-g file path`, `storage`.

Gophers are run by a task of their own, `service/storage/gopher.lisp`, one
for a member, started by its service, so that what a gopher does and how long
it takes are not the service's affair: one whose function throws harms
nothing but itself. It was a task for each gopher till 2026-10-12, see "A
gopher is run where it lands" below. The service passes it what it is sent:
who asked, and as text the path, the block to begin at, what it has, and the
function. It asks the service for each block it goes to, and mails what it
has at the end straight to who asked.

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
seen by four of five. `(storage-wait path [wait want])` waits till most of the
members have it, or as many as are asked for, and is `:nil` if they have
not in the time: a member cut off from most of the others has that every
time, which is how a writer that must not have its write set aside knows.

A block the member asked has not got is had from one that has: the service
says who, `(storage-where)`, and it is the asker that goes there.

**The gopher goes to the blocks.** When the block it wants is not kept by the
member it is at, it finds a member that has it, and all there is of it,
where it is, what it has, its function as it was sent, and where it has been,
is mailed to a new gopher task there. The last mails the answer, and the
trail, straight to who asked.

**A gopher is handed on, and the hand is seen.** `service/storage/gopher.inc`,
2026-10-12, after Chris read the above and asked: what if there is no
answer, does it try the other members that have the block, does one that
is started and sent nothing ever go, does the old one wait to be told?
None of those, as it was. Now:

* The one sent on says so, to the one that sent it, before anything else.
  The one that sent it is done only then.
* If the node does not start a gopher in 2 seconds, one is started at the
  next member that has the block as well, longest run first, and so on.
  They all answer to the one mailbox, and the first to say it is there,
  whichever it is, a late one too, is sent the gopher. Chris: "we don't
  have to do this serialy ? When any replies come in, we can go with that,
  even if it was a late reply for the original choice." Only the one is
  sent anything, the rest go in their own time. If it does not say it has
  what it was sent in 2 more, the next to answer is sent it, till one has
  it or there are none.
* Only then is "none" mailed to who asked, so they hear at once and do not
  wait out their own time.
* A gopher that is started and sent nothing in 10 seconds goes. It waited
  for ever.
* A member is given 1 second to say which blocks it has, it was 10. One
  that does not say, or does not take the gopher, is in a list that goes
  with the gopher, and is not asked again on that walk.

On the x64 Mac and the Pi, a store of the two with one copy of a block: a
gopher from each counted the lines of `STATUS.md`, 14,284 as `wc -l`, by
way of both. The Pi's member stopped: "It did not come back", in 1 second.

**A gopher is run where it lands, and works out its way.** 2026-10-12, the
evening. Measured first, over `apps/media/films/data/raymarch.flm`, 37MB,
568 blocks of 64KB, five members with three copies, all on the M4: a gopher
that added up its bytes took 2.5 seconds and went to a member 126 times,
and 0.67 with the object pinned to one. A stream read it in 1.1. Each time
it was sent on cost about 15ms: 4 asking each member that had the block
what it had of the next 32, one after another; 3.7 for a new task to be
started, read its files and say it had the gopher; 1.1 reading the
object's list of blocks again. 126 is as few as there are for where the
blocks fell, there are 125 runs: it was going the best way, slowly.

Chris, of keeping blocks of a run together to make the runs longer: "I must
admit I like the idea of not having to ask others and just knowing who has
the blocks." So where a block is kept is as it was, and the gopher uses the
same sum. Three changes, his "yes, build those three":

* **Its way is worked out.** `(store-rank name among copies)`, in
  `lib/storage/store.inc`, is the sum every member does, now by itself, and
  `(gopher-route)` does it for the next 32 blocks and each other member that
  is there: no mail. Who is to keep a block and who does are not the same
  while a store is spreading, so a gopher that gets to a member that has
  not the block it came for asks, as it did, `(gopher-order)`, and only if
  nobody has it then is "none" said.
* **One task for a member runs them.** It is up, with its files read, so
  to send a gopher on is a mail to the member's service, which passes it
  on. It does a block of each gopher it has in turn and reads its mail
  between. Sending on does not wait: `(gopher-pass-begin)` and
  `(gopher-pass-turn)`, a step each time round, so a member that does not
  answer holds up only the gopher that was going to it. It is as it was
  otherwise: the first sent it, the next as well after 2 seconds, the
  first of them to say it has it is where it went, the rest are in skip.
  There is the one step now, not start and then send, so one that was slow
  to say so and had it all the same is a second gopher, as before.
* **The list of blocks is kept, and it is of the one object.** A gopher
  carries the name of what the object was when it set out, and the task
  keeps the lists of the last 16 it read, by that name. So it is not read
  again at each member, and a gopher is of the one thing all the way,
  though the path be written while it goes: it was not before.

The same film after: 1.05 seconds, 126 times, a little under 3ms each.
Pinned, 0.70. So a gopher that goes to the blocks is now as quick as the
stream that brings them, 1.1.

What is not as good as it was: a gopher's function that never comes back
holds up every gopher at that member, where it held up only itself.

**An object as a stream.** `(storage-stream path [mode svc ahead])`, 2026-10-12,
to read. To write, see "An object written as a stream" below.
Chris: "Do we have the eqicalent of '(storage-stream path) -> stream' so any
existing code just sees a normal seekable dfile stream ?" I offered the
whole object in a string stream first, and he said: "bring me that entire
4GB mp4, that I may watch it... errrrrr". So it has a block at a time. And
of the blocks after: "We could implament read ahead at the point of
consumtion ?" As the block wanted is read, the next 4 are asked for, each
of a member that has it, a different one in turn where there are several,
so they come from several machines at once and are there, or on their way,
when wanted. A seek makes new mailboxes and asks again from the new place,
what was on its way is to mailboxes nobody reads: "a seek, pays the cost,
but it allways does in any streaming object". A block that did not come in
2 seconds, or is not what its name says, is had from any member that has
it, as `(storage-block)` does.

It is made of a new VP class, `:lstream`, `class/lstream/`, a stream that
is read from Lisp functions, `(lisp-stream fill [seek write])` in
`lib/streams/lisp_stream.inc`. That is all the VP there is of it: how far
ahead, of whom, and what is done about one that does not come is Lisp, to
be changed and measured. `(read-line)` `(read-blk)` `(read-char)`
`(stream-seek)` `(lines!)` and the VP that reads a stream all read it.

It is read only. To put, `(storage-put-stream path stream)` reads a stream
to its end a few blocks at a time, `(storage-put)` and `(storage-put-file)`
are that, and `(storage-same-file? path file)` reads an object as a stream
beside a file of the host and says if they are the same.

**The whole tree, through streams.** Chris: "A good test would be to use the
dumb streams to copy the entire ChrisaLisp tree into the storage system and
then read it back and compare eveything." `storage -T dir path/` puts every
file under a folder and reads each back and compares, `storage -V` the
reading back only, of what another machine put. 2026-10-12, the tree less
obj, deps and .git, 1,971 files, 167MB, the longest a film of 37MB:

	one member, the M4                         put 86 s, read back the same 20 s
	three, M4, x64 Mac, Pi, two copies a block put at the M4 34 s, the same 23 s
	shared out to the other two                about 4 minutes
	read back on the x64 Mac                   1,823 of its 1,936 files the same, 134 s
	read back on the Pi                        1,797 of its 1,845 files the same, 318 s

Each file not the same on the x64 Mac, 113, and on the Pi, 48, was looked
at: its own copy is not the M4's, by MD5. They are what a machine has of
its own, the objects of its host program, a user's settings, a file I had
changed since. No file that is the same on two machines read back
different.

What that run found, and it was the first with more than a few objects:

* A member asked for a block it does not keep went to the volume to find
  that out, two directories read. Every member is asked for many it has
  not got. On the Pi its own share took 11.6 s to work out, the asking of
  it gave up at 10, and the Pi stopped taking blocks altogether. What is
  kept is held in memory: that is asked first. 89ms on the M4.
* The tender fetched a block at a time, each asked of the peers in turn
  till one had it. Chris: "~10 blocks/sec; does that sound fast enough to
  you ? Are hitting the Wifi limit to the x64 ? or somthing deeper ?"
  Measured on the x64 Mac: 61ms a block from the M4 and 29ms to be told
  no by the Pi, where a ping is 17ms and `scp` of 20MB is 1.2MB a second.
  So it was the going and coming of the Wi-Fi, a block at a time, a third
  of what the link carries. Now 8 are asked for at once, each of a peer
  that has said it has it: 28 blocks a second to the x64 Mac, which is the
  link, and 18 to the Pi, which is its SD card, 0.45MB a second to write.
* The digest of a store, asked for by every member each time it looks, was
  worked out from every path each time. It is held till something changes.

**Only what is new is told, and it is written later.** 2026-10-12, found by
the farm: with five members all written to at once the window had one
answer from them in 16 s. Three things, measured one after another.

* Each tender, at every look, fetched the whole path map from each other
  member and gave its own back, and each of those was written to the
  volume whole. A store now counts the changes to its path map from when
  it was opened, `(store-at)`, and has for each path the count at its last
  change. A tender asks for what is new since the count it was last given,
  `(store-since)`, and gives what is new since the count it last gave. The
  counts are of the one opening of a store, which has a name: one asked
  with the count of another opening tells all.
* What is learnt from another member is not written at once,
  `(store-meet store entries :t)`, `(store-later)`: it can be learnt
  again. The service writes it when nothing else is asked of it, or after
  2 s. What a writer was told is kept is written before it is told, as it
  was.
* A member working out its share made a hash and a sort for every block
  it knew of, each time it looked. The keepers of a block are kept once
  worked out, for as long as they are asked of the same members.

And the window's own fault: when a demo was stopped to start another, the
watcher was waiting on the members that had gone, each in turn, a quarter
of a minute. One that says nothing is asked nothing more at that look.

The farm, five members on eight nodes, 1,977 files: put in 10 s and read
back the same in 7 s, as before these. After it, till every path is known
to all five and the film is kept by three: 58 s, now 26 s. Looks the
window was sent while it ran: 1 in 16 s, now 15 in the first 7 s, the
block counts of the five going up together.

**An object written as a stream.** 2026-10-13. Chris: "(storage-stream path)
-> stream, should have read and write modes like the (file-stream) does !"
The mode is the second thing given, as `(file-stream)` has it, and the same
three: `+file_open_read`, or none, to read as before;
`+file_open_write`, a new object of nothing to begin with;
`+file_open_append`, on the end of the object that is there, or a new one.

What is written goes to the store a whole block at a time as there is one,
`(storage-stream-put)`, so an object of any length is written with a block
of it in memory. The path is not touched. When the stream is flushed the
path is set, in the one step there is, to the object of those blocks and a
last short one of what is left over: one who reads it has the old or the
new and never a part. What is left over is kept, so a stream can be
written on after a flush, and the next flush sets the path again.

To append costs nothing for the blocks that are whole: a block is named by
what is in it, so they are had by their names and not read. A last short
block is read, to be written on.

A stream that is let go of is flushed first. I had put it to him as a flush
only, to keep Lisp out of a `:deinit`, and he said: "(stream-flush) ! A
deref of the (storage-stream) read/write should auto flush, as the other
streams do." So it does, and code that writes a stream and lets go of it,
as code written for a file does, sets the path. A flush with nothing
written since the last sets nothing. One opened to write and let go of with
nothing written is an object of nothing, as a file would be. A task that
ends holding one it never flushed: the path is set then.

Whoever sets a path last has it, as with a file. It is not "provided it is
still what it was": that is `(storage-put)` with what it was from.

Under it, `(lisp-stream fill [seek write])`, `class/lstream/`: a stream
given a write function has a buffer, and the function is given each
buffer of what is written, and `:t` with it at a flush. So the
`:deinit` of that class calls Lisp, which mails the store and waits for
it, from wherever the last of the stream was let go. No other class does.
It keeps the registers a stream's `:deinit` was declared to keep.

A fault of mine that the validate build named at once, "Heap overrun !":
the stream's own init wants how long the buffer is, and was given where
it ends, so till the first flush a stream took its buffer to go on past
its end. It showed only in a write longer than the buffer before any
flush. I had told Chris it was the depth of the stack. He asked "are we
really that deep ?", and we were not.

**A function has a second for a block.** 2026-10-13. One task runs every
gopher at a member, a block of each in turn, so a function that did not
come back held up the rest, and the task's mail. Chris: "could it run the
function in a local async task ?", and then, as tasks here give way only
when they choose to, "it should be the case that (time-slice) call if
forced or injected intot he gopher code.. we could just make it a rule",
and "go with the tick alone, one second allowance".

So a function is not run as it was sent. `(gopher-ticked)`, in
`service/storage/gopher.lisp`, does its macros and puts a call of
`(gopher-tick)` first in the body of each `while`, `until` and `lambda`
of it, which is everywhere it can go round. The tick gives way,
`(task-slice)`, and throws if a second has gone since the function was
given the block. What throws was already done with, and who asked told
"none". It is asked once more as the function comes back, for one that
caught what was thrown at it. What is sent on to the next member is the
function as it was written, as before.

What is quoted is not code and is left. And nothing that was there is
written to: each list gone into is a copy, a tick is a new list each time.
The first cut put the one quoted `(gopher-tick)` into every function, and
to be run a form has its names bound in place, so the second gopher found
the tick function itself there, went into it, and put a tick in the tick.
A macro can put a list into a form that is not the form's own, and a tick
in one of those would be in every use of it on the node.

The rules of ChrysaLisp are the rules of a gopher's function, Chris: "just
documenting that ChrysaLisp rules apply, no recursive calling." A function
does not call itself, here or anywhere in the tree. It goes round with a
loop, and the tick is in every loop. Nothing looks for one that does, and
one that went into itself for ever would run its task out of stack and
take the node with it.

Chris: "Injecting code is somthing that we can do easily in Lisp source,
if we ever switch to VP based gophers's we can still inject a VP sequence
to do the same thing."

What it costs, the film of 37MB over five members on the M4, three runs of
each:

| the function                    | as sent | with the tick |
|---------------------------------|---------|---------------|
| the length of each block        | 712 ms  | 710 ms        |
| every 64th byte, the app's      | 766 ms  | 807 ms        |
| every byte, a `while`           | 3.02 s  | 5.09 s        |

A time round a loop is 34ns bare and 70 with the tick. Nearly all of the
36 is the look at the clock and the call, giving way is 4.

**A gopher can become two.** Chris: "you hit the issue with this,
divergence/forkinf of a gopher ! The issue is either to ensure it can't
happen, which may be imposible (or at least too slow as it needs global
control), or we depend on the idea that it is rare, and when it does happen
we mitigate the consiuences." If the new one has what it was sent and its
word back is lost, the old one tries the next member, and there are two.
Both walk to the end and both answer. Who asked reads the first; the second
is to a mailbox nobody reads. For a gopher that only reads, that is all of
it. One that writes is not built, and how what it does is made safe to do
twice is under Open.

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

**The dial: how many copies, a path at a time.** 2026-10-12, late. The
question was whether duplication can be controlled; Chris: "would we do a per
path(file) setting ? deafults to 3, realy important dfiles can be set
higher ? I think that's the crux of the concern", and that it is "both
security of important files, and performance tuning for file that are used
a lot". It is one number, in the entry of the path:

```
(storage-copies path [copies svc])      how many, or set it; 0 the store's
storage -k 5 docs/contract.pdf          the one object
storage -k 2 scratch/                   every object whose path begins so
```

* It is a write of the path like any other: it is counted, told to the
  other members as they meet, and of two set apart the greater is kept.
  Written again, a path has the copies it had.
* Nobody is asked who keeps a block still. The members have an order that
  is the block's own, `(store-rank)`, and the first so many keep it: three,
  or as many as its object says. So to ask for more adds keepers and moves
  nothing, and to ask for fewer takes the last of them away. A member
  working out its share takes the number from the object, and a block two
  objects are made of has as many keepers as the one that asks for more:
  that the same bytes are kept the once is to save room, Chris, "not
  safety or speed", and how many keep them is another thing.
* A gopher carries the number with what its object was, so its way is
  worked out among all that are to have the blocks: of an object every
  member keeps, it goes nowhere.
* An entry is a line as it was, the number after the deleted, `0/5`, and
  one written before reads as not saying.

Seen, five members: set to 5 at one member, every block of the object on
all five and those of another object on three; set to 1 at another, on the
one, and had whole from a member with none of it; set to 0, three again.

Not there: a number for a folder that what is put in it later takes, it
is set on what is there; and anything that counts how much a path is read
and turns the dial, Chris: "We may even be able to do the 'used a lot'
auto tuning later, once we have the dial." Manifests are kept by every
member whatever the dial says, as they were.

**To look at**, `apps/system/storage/app.lisp`, Storage in the launcher under
System. The members of a store down the side, the blocks of one object
across, and a square in a member's colour where it keeps a block: three in
every column. Under it the objects, and how many of the members have each.

It shows the store this system is a member of, if its service is running. If
not, it starts five members of a store of their own on its own node, each
with a store of 4MB in a file under `obj/`, and puts three objects: all that
a store of five machines does, on the one. Then:

```
Put      an object of 10 to 31 blocks is put at one of the members
Next     the next object is the one shown
Gopher   a gopher is sent to it from one of them, to add up its bytes:
         the line drawn is where it went, member to member, block by block
Stop     a member is switched off, and in 3 seconds its blocks are others'
Start    those that are off are on
Cut      a member can reach none of the others, nor they it
Heal     all can reach all
Fork     a path is written at two members at once: with one cut off,
         neither knows of the other, and Heal shows which keeps it
```

Seen on the Pi's desktop: the five rows with three squares to a column,
`5 of 5` against each object, a member that was stopped grey and "not
there" with its blocks made up by the rest, and the line of a gopher through
four members. Its buttons were not pressed by anyone, a copy of it on the Pi
was made to send the gopher and stop the member by itself.

**A journal of the path map.** 2026-10-13, the night, Chris: "look into the
first stage of the jornal work, so we can test and make a judgment call
tomorow." The whole map was written to the volume at every change of it, a
path put, a note taken from another member, half a megabyte a time at 2,000
paths. Now an entry that changes is a line added to the end of a file of its
own, and the map is written whole when that is full. Putting the tree, 2,001
files, on the M4: into one member 26 s to 19 s, into five on a node 123 s to
76 s. Each of the five had written its whole map some 600 times, 16 s of
each. Journalling only a member's own writes gave the five nothing, what
each learnt from the others had to go the same way. And for an hour my
"journal on" runs of the five were off: a sed that turned it on and off had
changed another line. Chris, the next morning: "keep it on".

**Where the rest of the time went.** 2026-10-14, Chris: "profile the one
member put". Timed by request in the service and by step on the asker:
writing each block file, 1.8 ms; looking to see that its folder was there,
0.6 ms; the `info` the asker asks before every put, 1.2 ms, of which the
room left was a count of every bit of the volume's bitmap. The room, the
count of objects and the folders made are kept as they change, 19 s to 13 s.
Then of the 12.5 s a put took with no service between, 10 were finding
names: each name looked for was a read of every entry of each directory on
the way, 256 in the folder of the blocks. A directory is kept once read,
with its names indexed, and read again only once written: one member 13 s
to 6 s, five on a node 76 s to 21 s.

**A recorder.** 2026-10-14, Chris: "move toward the starage control and test
al runn via commands, and the Stirage app moving more towards the realtime
visualizer/debugger ... a recorder for back traceing would be nice." Each
member's service, tender and gopher runner says what it does to a mailbox
that has asked, for 5 seconds unless asked again. Its first run found the
service's own error print failed on a message shorter than 200 characters,
and `check` has `cut` for that now.

**Scenes.** 2026-10-14, Chris, of the app's demo, cut, heal, fork, stop,
start and pin: "not indiviual commands ! Just started from a storage tests
option etc." So they are scripts in `tests/storage/`, each a store of its
own and a thing done to it, `storage -t name`, and recorded with `-r`. Five
members putting the tree by a farm: 2.8 s, read back 1.2. One member, a file
at a time, 15 s, where this morning's script that did the same, its member
on the one node, was 6: not looked into. The first run of the scene test
found its own `said` taken by the runner's mailbox of that name, a name is
found where a function is called from, so `check`'s `capture` now looks at
the names a function makes with a `(defq)` as well as what it takes.

**The app as a player.** 2026-10-14, Chris: "Be nice to have a timeline
scrubber slide ? act as a fast seek through the recording." The app is
built on the recording now: it runs a scene, or records this system's store,
and the timeline is the whole of what was recorded. An event, once done,
keeps what it changed, so a scrub back is undoing and nothing is copied. The
service now also records the blocks an object is made of, as its path is
set, so a recording can draw an object after its store is gone. Three faults
of mine found on the way, each a check now: a list made as the app ran kept
in a + name, `made`; `(const)` of a function made inside another, `inner`;
and before them the capture of a caller's name.

**Hundreds of gophers.** 2026-10-14, Chris: "A test that uses hundreds of
gophers at the same time, and they have to do diffrent things, examples
of some useful stuff, maybe the start of out build in gopher library."
A gopher can be sent and not waited for now, and there is a library of
six. The scene sends 300 at once to six docs over five members, and each
answer is checked against the same function folded over the same blocks
here: all 300 right, 170 ms, 607 moves. Writing the word count I used
`(find word text)`, which looks for the first character of a str, as
`docs/ai_digest/lisp_traps.md` says: I had not read it. And the tail it
carries to the next block was cut short after a near miss at a block's
end, found by folding over blocks of 1 to 7 bytes as well as 4096.

And `tree_one`, 15 s against the 6 s of the script that did the same:
on one node it is 5 s. On eight its member is on another node from the
task that puts, and each block of 64KB goes between the two.

**A late gopher, and no throw.** 2026-10-14, Chris: "In release mode gopher
code can't be injected with tests that throw ... release mode is for fully
tested code, it's should never throw !" Looked at: `throw` and `catch` are
there in a release build, but what passes an error back up is not, the
checks are `errorcase`: a `progn` or an `if` given an error goes on with it
as a value. So the tick's throw would not have ended a loop at all. Now the
tick is in each loop's test and round each lambda's body, and once the time
is up it says no and they all end. A task that ends itself was the other
way, but one stopped from deep inside never lets go of what it holds.

**A band for bots.** 2026-10-14. Chris, of "a second is by the clock":
"should we add a gopher priority band ?", then "that will need
benchmarking ... We are not adding a new band lightly. That band might
end up being a band that is shared with other autonamous 'bots'", and,
measured, "yes, build the bots band, and lets give it a good thrashing."
Measured first, with the gopher runner raised to the GUI's band as a stand
in: 300 gophers on eight nodes each busy with apps, 16 s all at app
priority, 3.4 s with the runners raised, 0.19 s with the services too,
every hand on goes by a service. Built: band 4 of 6, `(task-band)`. The
thrashing found what was feared: a bot gives way only to its own band and
those before it, so 20 gophers that went round for ever held the apps of
their nodes 9.1 s. A bot now rests 1ms every 20ms of work,
`(task-breathe)`: the apps waited 61 ms at most. On the Pi they still
waited 2.1 s: five members on four nodes, two bots of a kind on a node,
and while one rested the other worked. So the bots of a node rest together,
and only when an app is ready, a binding that looks at the kernel's list,
`lisp_apps`. And the tick, in every turn of a gopher's loop, a million in
the scene, was looking up two names of the node from deep in a function,
each a walk up a long chain: idle gophers went from 211 ms to 300. It
looks at one time of its runner's now. M4: 217 ms idle, 234 busy, apps
59 ms at most; the Pi: 3.3 s, 4.5 s, 331 ms.

**Snapshots.** 2026-10-14, Chris: "Snapshots need interface in the
Storage app", and later, "snapshots I think next". The store first: a
snapshot is paths, `snapshots/<name>/<path>`, each set to the object its
path is now, so nothing is copied and every part of the store already
knows how to keep it, share it and sweep round it. Restore sets the paths
back. The scene takes one at one member, changes and deletes, sweeps, and
restores from another; all five the same after.

**Late gophers to the back of the queue.** 2026-10-14, Chris: "late
gophers to the back of the queue". 280 good gophers behind 20 that went
round for ever took 7.2 to 9.3 s on the M4. A first try of 50ms, and one
not done in it tried again after the others with its second, gained
nothing, 6.6 s: a runner is one task, and while it gives a gopher its
second it gives nobody else a turn. A queue that is empty for a moment, a
gopher being sent on, is enough for a looper to have its second, and those
that come then wait behind it. A second runner, the slow lane, in the same
band, gained nothing either, 5.9 s: each time the quick runner gave way, to
wait on the service for a block, the lane had a whole slice. The lane in
the apps' band: 0.69 s, the apps 78 ms at most. What is slow by its nature
waits on the quick and is one more app to the apps. Its second is by the
clock still, so on a node busy with apps it has less of it to work in, to
be watched. `check` has `again`: a `defq` in the runner's loop of `kept`,
a name the runner's own `take` has, gave every gopher "none".

**A service is one task, and the tender's round trips.** 2026-10-14,
Chris: "a service is one task, can it not deligate to a small farm of
task, send a job list out to them ?", then "profile it first". Profiled,
the tree into five members: a service waited for mail nine tenths of the
time, its longest answer 15 ms through the put and 226 ms with the tenders
at work. Of its work, `share` 30%, the volume 28%, hashing 8%. What a
member's state is can not be shared out without locks, and the time was
not the service's: after a put of 3 s every member had its share 45 s on.
Chris: "1 and 2, now we know what we must do." 1: the share is worked out
the once, a plan kept till the path map, pins or who is there change, and
kept up as blocks come and go; a manifest that comes adds its object, a
share went from 44 ms to 4. 2: the tender, timed part by part. Letting go
was half of each look, every holder asked one at a time of each block:
asked the once, 7 s to 0.6 s. The peers asked at once, the puts not
waited for, a window of 32 for 8 (64 was no better), and looking again
while blocks come in place of 8 rounds and a tick: 45 s to 12.9 s, a
service's answer 136 ms at most. `check` has `below`, a call in a module
of one made after it and not exported, which the plan's functions were.

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
* Two members that differ are told what of each other's path map is new
  since they last asked, see Built. One that has never asked, or asks a
  member that has been started again, is sent all of it.
* A member is one by a store of that id being made on it. Retiring one is
  `(storage-members "-id")`, and what it kept is then others' to keep.
* How a block that nothing refers to is found, over machines, while a write
  is on its way: a sweep is only when asked.
* `(storage-wait)` is for how many members have the path. That its blocks
  are on three of them is not asked yet, they follow in a tender's own time.
* Whether the store may show that two blocks are the same, and where the
  keys are kept, when there is encryption.
* Cutting an object where its content says, not every 64KB.
* A manifest of a very long object is itself long, it is one block of any
  size for now.
* A stream that is written sets its path whoever else has since. There is
  no way yet to say what the path was when it was opened.
* How deep the stack is when a written stream is let go of is not
  measured. Its flush is Lisp, with all Lisp needs of the stack, and it is
  run wherever the last of the stream was dropped.
* A service is one task. While it does a thing it answers nothing. Seen
  at 136 ms at most, see Built, and once 0.2 to 4 s, before the volume
  kept what it had read of its folders.
* `(file-stream path)` of a path that is in a store is still a file of the
  host. That a path says where it is, is a thing for root.inc, and Chris.
* `(lisp-stream)` is bound in a library and not root.inc, so that a boot
  image without it can build a tree with it. At a release, with a
  snapshot, it can go to root.inc.
* A gopher's function can do anything a task can. What it may do is not
  fenced.
* A gopher that became two, see Built, and writes. Blocks are safe, the
  same bytes have the same name. Anything else it does is done twice. What
  is to be thought through with Chris: a name for each walk, that a member
  can know it has seen; what a writer must promise; and how often it
  happens at all, which is to be measured and not guessed.
* The tick is two calls of Lisp and one of the host each time round, 36ns
  on the M4. A function that looks at every byte of a block pays it for
  every byte, see Built. One native call, that gives way and knows when
  the time is up, would be a few ns: not written.
* A second is by the clock, not of the function's own running. On a node
  with much else to do a function that gives way is away for some of it.
* Run by a task that stays, a function could be kept compiled between
  gophers, and, Chris: "We can if requred, pass VP (progn ....) code out
  for go faster gopher's and Jit them, if we find we need that." Not
  needed for a function that only adds up: to be measured with one that
  goes over the bytes of every block.

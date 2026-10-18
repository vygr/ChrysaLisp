# The Storage Service

A store of objects spread over the machines of a mesh. Each machine that is
a member keeps a share of the store on a volume of its own, and the members
between them keep every block of every object on more than one machine. An
app asks the member on its own machine for what it wants, and a query can be
sent to where the data is, in place of the data being brought to the app.

This is what it is and how it works. How it came to be, what was decided and
why, and what each measurement found on the way, is
[storage_design_notes.md](storage_design_notes.md).

## The Parts

| file | what it is |
|---|---|
| `lib/storage/store.inc` | a store, what one member holds, on an exFAT volume. Knows nothing of mail or of other members |
| `lib/fs/exfat.inc` | the exFAT volume a store is on, a file of the host or a disk |
| `service/storage/app_impl.lisp` | the service, one for a system: keeps a store and answers what it is asked |
| `service/storage/tender.lisp` | a task the service starts: finds the other members, compares notes, fetches and lets go of blocks |
| `service/storage/gopher.lisp` | a task the service starts: runs the gophers that are at this member |
| `service/storage/gopher.inc` | how a gopher's way is worked out and how it is sent on |
| `service/storage/auto.lisp` | starts the service of a machine that is a member, when its session is the machine's member of the mesh |
| `service/storage/app.inc` | all an app imports: the calls, the asking, the recorder |
| `service/storage/scene.inc` | scenes: a store of its own, a thing done to it, and what came of it |
| `tests/storage/` | the scenes |
| `cmd/storage.lisp` | `storage`, the command |
| `apps/system/storage/` | the Storage app: a store watched as it works, and played back |

```image
docs/diagrams/storage_member_tasks.cwb
```

## Objects, Blocks And Paths

An object is cut into blocks of 64KB, the last shorter. A block is named by
the SHA-256 of what is in it. So a block never changes, two that are the
same are kept once, and one that has gone bad does not match its name and is
known for it.

An object is a manifest: how long it is and the names of its blocks in
order. A manifest is a block too, named the same way.

```image
docs/diagrams/storage_object.cwb
```

A path names one object. The path map, path to manifest, is the one part of
a store that changes, and the one part the members have to agree on. An
entry of it is:

```
(path hash bytes vv writer seen deleted copies)
```

* `hash`, the name of the manifest, `""` for a path deleted.
* `bytes`, how long the object is.
* `vv`, how many times each member has written the path, as far as the
  writer knew: `((member count) ...)`.
* `writer`, the member that made this entry.
* `seen`, the members known to have it.
* `deleted`, the path is deleted. The entry stays, so a member that was away
  does not bring the object back.
* `copies`, how many members keep its blocks, 0 for as many as the store
  says.

A write never changes anything where it is. It makes new blocks and a new
manifest, and the path is pointed at it.

## One Way To Write

A path is set to an object provided it is now the object the writer says it
is: `from`, the name of the object it was, `""` for none, and not given, not
to say. One that does not match is `:stale`, with what the path is now. So a
write is not lost between two writers that can reach the same member, with
no lock held.

```
(:ok entry)        it is set
(:stale entry)     it was not what was said, and is this
(:full :nil)       no room
(:lost :nil)       the service did not answer
```

The store always takes a write, and says how many members have it.
`(storage-seen path)` is `(seen members)`, `(1 5)` seen by one of five.
`(storage-wait path [wait want])` waits till most of the members have it, or
`want` of them, and is `:nil` if they have not in the time: a member cut off
from most of the others has that, and that is how a writer that must not
have its write set aside knows.

## On The Volume

A store is an exFAT volume, a file of the host or a disk. On it:

```image
docs/diagrams/storage_volume.cwb
```

```
store        what it is: its magic, the store's id, the member's, the block size, copies
members      the members, a line each
retired      those that were members and are no more
pins         the paths this member holds all of
pinned       what every member holds all of, as far as is known here
paths        the path map, a line an entry
journal      the entries set since the map was last written whole
blocks/xx/y  a block, its name in hex, xx the first two of it
```

An entry is a line, the path last as it may have spaces in it:

```
<hash hex or -> <bytes> <vv> <writer> <seen or -> <deleted>[/<copies>] <path>
```

The volume is a real exFAT volume. The Mac mounts it, lists those files,
and checks it clean, and a block's SHA-256 by the Mac's own `shasum` is its
name.

### The journal

The path map is not written whole when a path is set. The entry is added as
a line to the end of `journal`, a file of a set length made once and written
where it is, so nothing of the volume but its own bytes changes. The map is
written whole when the journal is full, or when anything else writes it, and
then the journal is emptied. A store that is opened reads `paths` and then
the journal, and an entry of the journal is taken where it is later than the
map's.

The journal begins with a count. A line of it is 8 hex digits, a space, and
the entry: the digits are the start of the SHA-256 of the count and the
entry. Reading stops at the first line that does not match, so a line half
written when the power went, and lines left from before the count last
changed, are not taken. The journal is a 64th of the volume, 1MB at most.
`+store_journal_size` 0 and there is none.

What a member learns from another goes the same way. If the power goes,
what is lost is at most that a member was known to have a path, and that is
learnt again at the next meeting.

### In memory

A store holds its path map, the names of the blocks it keeps, how many
objects there are, the folders of blocks it has made, and the keepers of a
block once worked out. The exFAT volume keeps the blocks of the device it
has read, the room left, and each directory it has read with its names
indexed, read again only once it is written. Without these a put of the
tree, 2,001 files, into one member was 26 s. It is 6 s.

## Members

A member is a system, by its system id. One service a system. The members of
a store are a list of them. A machine becomes one by being told to join, is
one while it is away, and stops being one only when it is retired on
purpose: a laptop taken home is still one of the five. A volume opened by
another member than the one it was, a machine given a new id or a disk moved
to another machine, has that member take the place of the one it was.

Who the members are is two lists that only grow, those joined and those
retired, so two members put theirs together the same whichever way round.

The services of a store find each other by the name of their group,
`*Storage`, declared with the store's id and the member's, on every machine
of the mesh. Any service of that group with the same store id is a member to
the others.

### Joining

```
storage -j          this machine is a member of the store of its mesh, from now on
storage -f          it is not started by itself from now, the store is left as it is
storage -z MB       the store of this member on a volume of that many MB
storage -L on|off   this machine travels, a laptop, or does not
```

`storage -j`, `(storage-join)`, makes the machine's part of the store,
`storage/store.img`, 4096 MB at most, a file with holes that takes what is
in it, and starts it. What was said is kept in `store_member` at the root of
the tree, beside `mesh_key`. The store's id is made from the mesh key,
`(storage-key-id)`, so every machine with the key is of the one store, and
the key can not be had from it.

From then on `service/storage/auto.lisp` starts it. The Net service starts
that when its session becomes the machine's member of the mesh, and tells it
to go when it is that no longer. Only one process can listen on the mesh's
port, so only one session of a machine is its member, and only that one
starts the store. A session started alone, a run of the tests, never does.
Started by hand in another session, a note beside the image,
`store.img.member`, says which process has it, `(storage-claim)`.

### A machine that travels

`storage -L on`, `(storage-member-travels)`, says this machine is one that is
taken away from the others, a laptop. It is kept in `store_member`, and set
at a join, `storage -j -L on`, or after. When its store starts, the area of
the session's user, `usr/<user>/`, is pinned on it, `(storage-user-pin)`: it
holds all of that, wherever else it is kept, so all of it goes with the
machine when it goes, and is read from it with nobody asked. The user is
the session's own, `run.sh -u name`, or the one the machine has,
`usr/current`. A pin does not lapse: a machine that travels is one
person's. `-L off` takes no pin away, `storage -U` does. A desktop or a
server is not one that travels, and holds only its share. Nothing puts a
user's files in the store yet, so the pin holds what is put there.

### Another size

`storage -z MB`, `(storage-resize)`, puts the store of this member on a
volume of that many MB, bigger, or smaller if what it has fits. The service
does it, so it stays in the session it is in: it makes the new volume
beside the old, `storage/store.8192MB.img`, copies every file of the old to
it, the blocks above all, and then writes the path map, the members and
the pins to it as it has them, with a journal of the new volume's size. A
store that was full, that had no room to write its map, loses nothing.
Then the old file is gone, and `store_member` names the new. It is a copy
on the one machine, no other member is asked for anything, and it answers
nothing while it copies. If the new volume can not be made, or what there
is does not fit, it is removed and the store is as it was.

### Where a block is kept

By a rule, not a table. For a block each member is given a number, the
SHA-256 of the block's name and the member's id, and the members in order of
those numbers are the block's own order, `(store-rank)`. The first three
keep it, or as many as its object says, or all the members when there are
fewer. Every member works out the same from the list of members and nothing
else.

```image
docs/diagrams/storage_members.cwb
```

* A new member comes out in the first three for its share of the blocks, and
  only those move to it.
* A member gone, for each block it kept the next in that block's order is
  now a keeper, and fetches a copy.
* Every member keeps every manifest.

### How many copies

`copies` of an entry is the dial, a path at a time: more for what must not
be lost or is read a great deal, fewer for what is big and can be had
again.

```
(storage-copies path [copies svc])      how many, or set it; 0 is the store's
storage -k 5 docs/contract.pdf          the one object
storage -k 2 scratch/                   every object whose path begins so
```

It is a write of the path like any other. To ask for more adds keepers and
moves nothing, to ask for fewer takes the last of them away. A block two
objects are made of has as many keepers as the one that asks for more.

### Pins

A member holds only its share of the blocks, so away from the others part of
anything would be out of reach. A member can pin the start of a path,
`user/me/`, and holds all of every object under it as well as its share,
and does not let go of it.

```
(storage-pin prefix [off svc])      storage -P prefix, -U to unpin
(storage-pins-missing [svc])        blocks of what is pinned that are not here yet
(storage-pinned-by path [svc])      the members that hold all of it
```

What each member has pinned is known to every member: it goes with the path
map when two compare notes. A gopher goes to a member that holds all of an
object and stays there.

## Snapshots

A snapshot is paths. Of each object under a prefix, a path
`snapshots/<name>/<its path>` is set to the same object. No block is
copied: an object is the list of its blocks, which never change, and they
are kept for as long as a path names them. So a snapshot costs an entry of
the path map an object, and every member has it as it has any path: it is
shared, copied as its objects say, swept as they are, and what a snapshot
is of can be had, read as a stream, or sent a gopher.

```image
docs/diagrams/storage_snapshot.cwb
```

```
(storage-snapshot name [prefix svc])   -> count        storage -n name [prefix]
(storage-snapshots [svc])              -> ((name count) ...)   storage -N
(storage-restore name [prefix svc])    -> (restored newer)     storage -R name [prefix]
(storage-snapshot-drop name [svc])     -> count        storage -D name
```

A snapshot is taken at one member in one step, of that member's map as it
is then; the others learn of it as of any write. A restore sets each path
of the snapshot back to the object it was, and one deleted since is had
again; a path made since under the prefix is left, and counted. Dropped,
its paths are deleted, and its blocks go at a sweep if nothing else is
made of them. A name has no / or space. A snapshot of everything is not
of the other snapshots.

## Bots

The service, its tender and its gopher runner are bots, `(task-band :bot)`:
a band of the scheduler between the GUI and apps. One that is ready runs
before any app of its node, so a store at work is not held up by them.
Every 20ms of work the bots of a node all rest at once for 1ms, if an app
of the node is ready to run, `(task-breathe)`, which the store's and the
volume's own giving way calls. All of them at once: one that rests while
another works leaves the apps nothing.

The `bots` scene thrashes it: every node of the machine busy with two apps
and a probe that asks to wake every 10ms, 300 gophers at once, then 300
with 20 that go round for ever. On an M4, eight nodes:

| | gophers, idle | gophers, nodes busy | apps waited | apps waited, 20 for ever |
|---|---|---|---|---|
| all in the band of apps | 180 ms | 16 s | | |
| bots, no rests | 190 ms | 194 ms | 111 ms | 9.1 s |
| bots, resting together | 217 ms | 234 ms | 57 ms | 59 ms |
| the same on a Raspberry Pi 4, four nodes | 3.3 s | 4.5 s | 331 ms | 230 ms |

## The Tender

A service only ever answers. It asks nothing of another member and waits:
two that did, each of the other, would wait for ever. What a member has to
ask is asked by its tender, a task of its own. It looks every 2 seconds,
`+storage_tick`, and at once when its member is written to.

```image
docs/diagrams/storage_put.cwb
```

* It finds the other members that are there, and tells its service who they
  are.
* With each whose digest, one name for its whole path map and members,
  `(store-digest)`, is not its own, it compares notes: what the other has is
  taken here, and what this one has, with that, is taken there. Only what is
  new since the last time is told, `(store-since)`, by a count of changes
  that each store keeps from when it was opened.
* It asks its service for its share, `(store-share)`, among the members that
  are there, 64 blocks of it at most. What it wants and has not got, it
  fetches, 32 at a time, `+tender_window`: each peer is asked at once which
  of them it has, each block is asked of one that has it, all at once, and
  each as it comes is given to its service, not waited for. It asks again
  while what it asked for comes. What it has that is others' to keep, it
  lets go of once each of those says it has it: each peer is asked the
  once of all of them, and those let go of are let go of at once.
* What a member is to keep is worked out the once, `(store-plan)`, and kept
  till its path map, its pins or who is there change. What it wants and
  what is others' are kept as blocks come and go, and a manifest that comes
  adds what its object is made of. So a share is asked in a few ms.

Five members on the M4, the tree, 2015 files, 162 MB, put into them by a
farm in 3 s: every member had its share 12.9 s after, with the tenders
looking every 2 s. It was 45 s. A service answered in 136 ms at most all
the while.

A member not heard from is still counted for 5 minutes, `+storage_grace`,
before what it kept is fetched by others: a machine turned off and on is no
reason to copy its share.

### When two wrote a path apart

When members meet, an entry of the other's that comes after mine, its counts
as many or more of every member's, is taken. One that comes before is not.
Two written apart, each with more of some member's writes:

* The one most members have keeps the path. At most one side of a split can
  have most of the members, so a writer that waited for most is promised
  its write stays.
* Between two that neither had, the one written more times, then the one
  whose manifest's name is greater, only so that all choose the same.
* The one kept has the counts of both, so it comes after both.
* The other, if it is an object and not a delete, is kept under another
  name, `path~` and 8 of its manifest's name in hex. It is never thrown
  away.

Only a path both sides wrote is touched. A laptop's own paths, written while
it was away, are taken when it comes back.

### Locking a path

Tasks of one system that read a path, change it and put it back take the
lock of that path first, from that system's `@Lock` service,
[lock_service.md](lock_service.md). The key is `storage/` and the path:

```vdu
(defq key (cat "storage/" path))
(with-write-lock key
	(defq was (storage-stat path) data (storage-get path))
	(storage-put path (change data) (if was (second was) "")))
```

It is a courtesy between the tasks of one system. A member on another
machine is not held by it. What holds for everyone is the rule above, a path
is set provided it is what the writer says, so a writer says what the path
was, lock or no, and reads the answer. The calls do not take the lock
themselves.

## Gophers

A gopher is a query that goes to the blocks of an object, in place of the
blocks being brought to the asker. It is what it has so far, and a function:
given that, a block, which block it is and how many there are, the function
gives what it has now and the block it wants next, or `:nil`, it is done.

```image
docs/diagrams/storage_gopher.cwb
```

```
(storage-walk "logs/today" 0
    '(lambda (n data at count)
        (list (+ n (length (filter (# (eql %0 (ascii-char 10))) data))) (inc at))))
-> (13804 (("m-a" 0) ("m-c" 12) ...))
```

`(storage-walk path state visit [at wait svc])` sends it, and is `(state
trail)`, what it came back with and where it went, or `:nil` if it did not
come back in the time. `storage -q` is the same from the command line.

* The function is sent as text, quoted. What it has and that text are all
  there is of the gopher, which is what lets it be sent on. What it has must
  print as it reads: numbers, strs and lists of them.
* It goes where the function says, not in order. A search by halves of a
  sorted object reads a few of its blocks and no more.
* At a member it does the blocks that member keeps. For one it does not, its
  way is worked out, `(gopher-route)`, the members that are to keep the next
  32 blocks, by the same rule as where a block is kept, and it is sent to the
  one with the longest run of them. One that does not have the block after
  all asks the members which do, `(gopher-order)`.
* Sending on is a mail. The one sent to says it has it before anything else.
  If it does not in 2 seconds, the next member that has the block is sent it
  as well, and the first of them to say so is where it went. One that does
  not answer goes in a list the gopher carries, and is not asked again on
  that walk.
* It is of the object as it was when it set out: it carries the name of the
  manifest, though the path be written while it goes.
* The last member mails the answer straight to who asked.

One task at a member runs every gopher that is there, a block of each in
turn, and reads its mail between. It keeps the block lists of the last 16
objects it read.

`(storage-walk-send path state visit [at svc])` sends one and does not
wait, it gives the mailbox its answer will come to, and
`(storage-walk-answer mbox [wait])` reads it. So many can be out at once.

### The gopher library

`service/storage/gophers.inc`, the start of one. Each gives what a gopher
sets out with, `(state visit)`:

| | |
|---|---|
| `(gopher-bytes)` | how many bytes |
| `(gopher-lines)` | how many line ends |
| `(gopher-count word)` | how many times a word is there, one cut in two by the end of a block too |
| `(gopher-checksum)` | every byte added up, 32 bits |
| `(gopher-longest)` | the longest line |
| `(gopher-head n)` | the first n bytes, and no block more than it takes |

`(gopher-fold gopher data size)` is what one comes back with from an
object of that data in blocks of that size, worked out here: what one is
checked against. The `gophers` scene sends 300 at once, each a job of the
library, to six objects over five members: all 300 came back right in
170 ms on an M4, with 607 moves between members.

### What a gopher's function may do

ChrysaLisp's rules: it does not call itself, it goes round with `while`,
`until` or a function given to `each` and the like.

A function is not run as it was sent. A call of `(gopher-tick)` is put into
the test of each `while` and `until` of it, and round the body of each
`lambda`, with `if` and `progn`, which are not macros. The tick gives way to
the other tasks of the node, and once the function has had its time for
the block, by the clock, it says no: every loop of the function ends, and a
function of it given to `map` or `each` does nothing more. So it comes back
by itself. Nothing is thrown.

A step is first given 50ms, `+gopher_first`. One not done in that goes to
the back of the queue, `gopher-back`: the slow lane, a second runner the
first starts. It is sent there as a gopher is sent on to another member,
from where it was, and tried again with a second, `+gopher_allowed`. One not
done in the second is done with, `gopher-late`, and who asked is told
`"none"`. The quick runner tells the lane when it comes to have blocks to
do and when it has none, and while it has, the lane waits at its next tick,
and the time it waits is not of its second. So a gopher that loops for ever
holds the others back by its 50ms, once. Measured with `storage -t bots` on
the M4, 280 gophers behind 20 that loop for ever: 7.2 to 9.3s with the one
runner, 0.75s with the lane. In a release build what passes an error back
up is not there, and an error thrown in a loop would be a value it went
round with. The tick costs about 36ns each time round on an M4.

### When a gopher becomes two

If one sent on has the gopher and its word back is lost, the next member is
sent it too, and there are two. Both walk to the end and both answer. Who
asked reads the first, the second is to a mailbox nobody reads, and that is
all: a gopher reads. It is sent out to gather and bring back, and the one
that sent it decides what is to be done with what it brought, and writes
it, with a stream or `(storage-put)`. A gopher does not write: what it
would write it could not take with it, a stream is of one task on one
member, and the gopher goes on from member to member.

## Streams

```
(storage-stream path [mode svc ahead]) -> :nil | stream
```

An object as a stream, for code written for a file. The modes are those of
`(file-stream)`:

* `+file_open_read`, or none. A block at a time. As one is read the next 4,
  `ahead`, are asked for, each of a member that has it, a different one in
  turn, so they come from several machines at once. A seek asks again from
  the new place. A block that does not come in 2 seconds, or is not what its
  name says, is had from any member that has it.
* `+file_open_write`, a new object, of nothing to begin with.
* `+file_open_append`, on the end of the object that is there, or a new one.
  Its whole blocks are had by their names and not read.

What is written goes to the store a whole block at a time. The path is set
when the stream is flushed, and when it is let go of, in one step: who reads
it has the old object or the new and never a part. Whoever sets a path last
has it, as with a file; to set it provided it is what it was is
`(storage-put)` with `from`.

It is made of `(lisp-stream fill [seek write])`, `class/lstream/`, a stream
whose reading, seeking and writing are Lisp functions.

A stream that is written is as a file is: what is written goes as blocks
fill, and one let go of half way, a task that ended, leaves half an object
at its path. What is to come through anything does as programs do with
files: it writes a path of its own, sees that all went as it should, and
renames it to where it is to be.

```
(storage-rename from to [was svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)
```

`to` is given the object `from` is, with its copies, and `from` is then
no more, in one step of the service. Nothing is copied, whatever its
size: a path is only the name of the object it is. `was` is what `to` is
to be now for it to be done, as `from` is to `(storage-put)`, `""` for
none, so two writers of one path can not overwrite each other unknowing.

```
(defq part (cat path ".part") out (storage-stream part +file_open_write))
...
(setq out :nil)
(if (all-went-as-it-should) (storage-rename part path) (storage-delete part))
```

```
(storage-put-stream path stream [from svc])   a stream to its end, a few blocks at a time
(storage-put-file path file [from svc])       a file of the host
(storage-same-file? path file [svc])          is the object the same as the file
```

## The Calls

All in `service/storage/app.inc`. `svc` is a service, `(storage-service)`,
this system's, if not given.

```
(storage-start image [:size :id :name :block :force :group :member :tick :grace :copies :node])
(storage-stop [svc])       (storage-service [name])      (storage-info [svc])
(storage-join [image size id copies force])

(storage-put path data [from svc])           (storage-get path [svc])
(storage-read path offset len [svc])         (storage-stat path [svc])
(storage-list [prefix svc])                  (storage-delete path [from svc])
(storage-copies path [copies svc])           (storage-stream path [mode svc ahead])
(storage-walk path state visit [at wait svc])

(storage-seen path [svc])                    (storage-wait path [wait want svc])
(storage-members [change svc])               (storage-pin prefix [off svc])
(storage-sweep [svc])                        (storage-check [mend svc])

(storage-block name [svc])                   (storage-where name [svc])
(storage-settle [svc wait])                  (storage-pace tick grace [svc])
(storage-cut members [svc])
```

`(storage-sweep)` lets go of blocks no object is made of. `(storage-check)`
names the blocks that are not what their names say, and with `mend` lets go
of them too, to be fetched again. `(storage-settle)`, `(storage-pace)` and
`(storage-cut)` are for tests: have the tender look now, change how often it
looks, and make a member as if it could not reach some others.

A request is a mail: a header, who to answer and what is asked, then what
that takes. An answer is one mail. A mail is never longer than a block and a
little.

## The Command

```
storage                       what the store is and what is in it
storage -l [prefix]           the objects
storage -p file path          put a file of the host
storage -g file path          get, - for stdout
storage -d path               delete
storage -k n [path]           how many copies
storage -P prefix, -U         pin, unpin
storage -M [+id | -id]        the members, join or retire one
storage -q "(lambda ...)" path   send a gopher
storage -w                    sweep
storage -c, -C                check, and mend
storage -T dir path/          put every file under a folder, then read each back and compare
storage -V dir path/          the reading back only
storage -t [scene] [-r file]  the scenes, or run one, and record it
storage -r file [-S secs] [-G group]   record what the members do
storage -n name [prefix], -N, -R name [prefix], -D name   snapshots
storage -j, -f, -s image [-m MB -i id -F], -x, -z MB
```

## The Recorder

What the members of a store do can be recorded, to be watched, played back,
and gone over after. Each member's service, tender and gopher runner says
what it did, a line each, to a mailbox that has asked to be told:

```
1791904249088123 r-a set 1a2b3c4d 12288 doc/one
1791904249090456 r-b meet r-a
1791904249090511 r-b learnt 1a2b3c4d r-a doc/one
1791904249091002 r-b fetched 9f8e7d6c r-a
```

The time in microseconds by the clock of the member's machine, the member,
what was done, and what to, a path last. A block is named by the first 8
bytes of its name, in hex. A gopher is named by a tag, 8 hex digits made
from who it answers, the same at every member it comes to, so the walk of
one of many out at once can be told from the rest.

| what | said by | of |
|---|---|---|
| `set` `delete` `copies`, with `-stale` `-full` `-missing` if not done | service | manifest, length, path |
| `object` | service | a manifest, and its blocks in order, as a path is set |
| `block+` `block-` `block>` | service | a block kept new, let go of, given out |
| `learnt` `learnt-delete` | service | manifest, writer, path, taken from another |
| `pin` `sweep` `cut` `members` `gopher-in` | service | what was asked |
| `snapshot` `restore` `snapshot-drop` | service | how many, the name |
| `peer+` `peer-` `meet` | tender | a member come, gone, compared notes with |
| `fetched` | tender | block, the member it came from |
| `gopher` set out, `gopher-on` come from another member, `gopher-step` `gopher-pass` `gopher-done` `gopher-none` `gopher-back` `gopher-lane` `gopher-late` `gopher-threw` | runner | block number, block, the gopher's tag, path |

A recorder asks each member with `(storage-record-ask svc mbox)`, and is told
for 5 seconds, `+storage_record_lease`, unless it asks again: one that has
gone is not told for long. `(storage-record-ask svc mbox :t)` stops it.
There can be several recorders at once, an app and `storage -r` in a
terminal say, and each is told all of it. `(storage-record-run write until [group stop])`
asks every member of a group each second and gives each line to `write`.
`storage -r file` is that, to a file of the host, for `-S` seconds.
`(storage-record-seed write [group])` gives `write` what the members have
now, as the lines a recording from the start would have left: who each
sees, each path, what each object is made of, and which blocks each keeps.
A recording begun with it has what came before. A task sends what it has
to say once it has done a thing asked of it, in one mail. With no recorder
nothing is said, nor made to be said.

The times of two machines are by two clocks.

## Scenes

A scene is a script in `tests/storage/`. It starts the members of a store
of its own on the nodes of this machine, does what it is for, says what
happens as it goes, and checks what came of it. Then its members are
stopped and its store is gone.

```
storage -t                   the scenes, and what each is for
storage -t split             run one
storage -t split -r file     and record what its members do
```

| scene | what it is |
|---|---|
| `demo` | five members, three objects put at three of them, and a gopher |
| `split` | a member cut off writes, the others write the same path, they heal |
| `stop` | a member switched off and on, three keep every block throughout |
| `pin` | an object pinned at a member, and a gopher goes no further |
| `tree` | every file of the tree put into five members by a farm, read back |
| `tree_one` | every file of the tree put into one member, read back |
| `gophers` | 300 gophers out at once, each a job of the gopher library, every answer checked |
| `bots` | every node busy with apps, 300 gophers, then 20 that never come back, how long the apps wait |
| `snapshot` | a snapshot at one member, things changed, restored from another, every member the same |

A scene is a task, started by `(scene-run name say [record wait])`, which
gives each line it says to `say`, and each line its members record to
`record`, and is `(passed failed)`. It is mailed who to tell and the group
its members are to be of, so that they are recorded from the first. Its
members' tenders look only when told, `(scene-settle)`, so each step is
done when the scene says it is, and it comes out the same each time.

```vdu
(import "service/storage/scene.inc")
(defun main ()
	(defq sc (scene-begin "What the scene is for."))
	(when (scene-check sc "five members start and meet" (scene-members sc +scene_ids))
		(storage-put "big" (scene-bytes 20 "big") :nil (scene-svc sc "m-a"))
		(scene-settle sc)
		(scene-check sc "every block is kept by three" (every (# (= %0 3)) (scene-keepers sc "big"))))
	(scene-end sc))
```

`(scene-start)`, `(scene-stop)`, `(scene-cut)` and `(scene-heal)` switch a
member on and off and cut one off, `(scene-big)` gives the members started
after it room, `(scene-seen?)` and `(scene-keepers)` are what is checked.

## The Storage App

Storage in the launcher, under System. A store watched as it works, and
played back, from a recording of it.

* The scenes are a row of buttons, and Run runs the one chosen: its members
  are recorded from the first, and drawn as they go.
* Live is the store this system is a member of: what it has, and then
  what it does. It is what the app opens on, and what live goes back to
  after a scene.
* Record is whether all of what the store does is kept, and the timeline
  goes back to the start; off, the last 2,000 events. It is kept for the
  user, `usr/<user>/storage.tre`, with the scene chosen, trace and the
  object shown.
* A scene that is run is all recorded, and stays to be gone over when it
  is done.
* Join makes this machine a member, as `storage -j`, and is lit when it is
  one. Travels beside it, the laptop, says this machine is one that is
  taken away, as `storage -L`, and is lit when it is: it pins its user's
  area at once if its store is up.
* Beside Run, Open and Save, with the Files app's picker. Save writes what
  is recorded, a line an event as a recorder says them, `(replay-line)`, so
  a file of `storage -r` and one saved here are the same, and either opens.
  Open plays one back from its start, and what was running stops.
* The snapshots there are, at the event the store is drawn at, are names
  above the objects. A press on one lists its objects, and their blocks
  are drawn as any object's; on it again, the store's own. The bar under
  the toolbar takes a snapshot of the whole store, by the name written or
  the time, puts the store back as it was in the one picked, or drops it.
  Those are of the store of this system, live.
* The members are down the side, the blocks of the object shown across, a
  square where a member keeps one, the first column the object's manifest.
  Under them the objects, a square for each member that has one, grey where
  it has it deleted. A press on one, or the wheel, and that is the object
  shown. Where the last gopher that was done of it went, or the last that
  set out to it if none was, is a line through the members, block by
  block.
* Beside them, what was done, event by event, the one the store is drawn at
  marked, and above, what the scene last said.
* A press on an event of a gopher picks that gopher: its object is shown,
  its way drawn through the members block by block, and the line above
  says which members it went by. With trace, only its events are listed.
  Another event, or the same again, and none is picked.
* The timeline under the buttons is the whole of the recording. Dragged, the
  store is drawn as it was at that event. `<` and `>` go an event back or
  on, Play plays it at the speed it was made. Trace shows only what was done
  to the object shown and its blocks, and `<` and `>` then go from one of
  those to the next.

`apps/system/storage/replay.inc` plays a recording. An event, once done,
keeps what it changed, so it can be undone: to go to any event is to do or
undo those in between, from where it is, and nothing is copied.
`apps/system/storage/run.lisp` runs the scene, or records the store, for the
app, and sends it what happens in batches.

## The Tests

| test | what it holds |
|---|---|
| `tests/system/test_exfat.lisp` | the volume: files, directories, room, a volume let go of uncleanly |
| `tests/system/test_store.lisp` | one member: blocks, objects, the one way to write, sweep, check, pins, the journal |
| `tests/system/test_store_meet.lisp` | the rules of meeting: writes apart, most keeps the path, deletes |
| `tests/system/test_storage.lisp` | the service and its calls, streams, gophers |
| `tests/system/test_storage_members.lisp` | five members on one node: shares, a split and heal, pins, a member stopped |
| `tests/system/test_storage_record.lisp` | the recorder |
| `tests/system/test_storage_scenes.lisp` | the scenes split and stop, run and recorded as `storage -t` does |
| `tests/system/test_storage_snapshot.lisp` | the snapshot scene, five members |
| `tests/system/test_storage_gophers.lisp` | the gopher library over blocks of many sizes, and the gophers scene |
| `tests/system/test_storage_replay.lisp` | a recording played back, forward and back, and a scene run by the app's runner |

## Not Done

* Encryption, and error correction. There is SHA-256 and ChaCha20 with
  Poly1305, [crypto.md](crypto.md), and no field arithmetic. A cipher makes
  two blocks that are the same look different; whether the store may show
  that two blocks are the same, and where the keys are kept, is open.
* A block nothing refers to is let go of only by a sweep, when asked.
* `(storage-wait)` is of how many members have a path, not of how many have
  its blocks.
* Blocks are cut every 64KB, not where their content says.
* The manifest of a very long object is one long block.
* A small object is a manifest and a block, two files of the volume.
* A stream that is written sets its path whoever else has since.
* A service is one task. While it writes a block it answers nothing.
* A gopher's function can do anything a task can. A gopher that writes, and
  becomes two, would write twice.
* A gopher that goes round for ever has its second at its member, and the
  gophers behind it there wait for it: 20 of them held the 280 good ones
  of the bots scene to 9 s.
* A host path and a store path are apart on purpose: `(file-stream)` is of
  the host, `(storage-stream)` of the store.

# Sync

`sync` makes the files of another machine the same as the files of this one,
over the links between them, with no `ssh` and no `rsync`. It is the first
step towards a rack that is updated by updating one machine.

```code
sync -a           ; on a machine that is to be updated
sync              ; on this one, who will take a sync ?
sync -t all -c    ; what would change on them
sync -t all       ; send it
```

```
> sync
8921786E arm64/Darwin . (this machine)
C96A1B10 x86_64/Darwin ., differs from this one
D649CA0E arm64/Linux ., differs from this one
> sync -t all -c
  differs docs/ai_digest/test_cache.md
  differs tests/new_file.txt
C96A1B10 x86_64/Darwin 2 differ, 0 only there, 998ms
D649CA0E arm64/Linux 2 differ, 0 only there, 3287ms
> sync -t all
C96A1B10 x86_64/Darwin 2 sent, 6065 bytes, 0 removed, 263ms
D649CA0E arm64/Linux 2 sent, 6065 bytes, 0 removed, 322ms
```

## How

Two people with notepads who meet, compare pages, and copy across what the
other has not got. It is the model the Net services make their mesh with,
`service/net/mesh.inc`, and the one a storage service would use.

Each side has a tree of hashes of its files, `lib/hash/tree.inc`. A file
has its SHA-256. A folder has the hash of what is in it, the name and the
hash of each file and each folder. So the hash of the top is of everything
below it, one number that says what the whole tree is.

The two tops are compared first. If they are the same the trees are, and
that is all that is said, 64 characters each way for 1,400 files. If they
differ the folders are compared from the top down, and only the folders
whose hashes differ are gone into. One file changed five folders down is
six folders asked for, a few hundred bytes each. Only a file that is not
there, or is not the same, then goes over.

`sync` with no options asks each machine for its top and says if its tree
is the same as this one's. That number is the version of a tree, the same
on every machine that has the same files.

The tree of hashes is a library of its own. It knows nothing of files, a
thing is a name like a path, a hash, and a word more, the mode here. It is
there for what else has trees to compare.

It all goes as mail between the two machines, by whatever links there are.
No task is started on the other machine.

## A machine has to say it will take one

`sync -a` starts the `*Sync` service on a machine, and that is all the say
so is. A machine with the service takes a sync, a machine with none can not
be sent one. `sync -x` stops it, and so does the end of its session. The
name starts with a `*`, a service seen from every machine, where an `@` is a
service for its own machine.

## It can not get out of the ChrysaLisp tree

A sync writes and removes files, so where it can reach is fenced, three
times over.

* **The root is in the tree.** The service's root is the folder ChrysaLisp
  was launched from, or a folder inside it. `sync -a -r /somewhere` is
  refused by the command, and a service started some other way with a root
  outside takes nothing at all.
* **A path can not be written to climb out.** One with a `..` in it, that
  starts with a `/` or a `~`, that has a `:` or a `\`, or a character below
  a space, is refused. So is one into `.git`.
* **A link is never gone through.** Before a file is written or removed,
  each folder on the way to it is asked of the host. It has to be a real
  folder, and the file a real file or not there yet. A symbolic link is
  neither. So a link in the tree that points at your documents is not a way
  to them, not to write, and not to remove. The list of a tree does not go
  through one either.

The worst a sync can do is to the ChrysaLisp install itself, every file in
it written over or, with `-d`, removed. That you can get back from GitHub.

**What this does not fence.** The files a sync writes include the
`Makefile` and the launch scripts. They are run later, by you, or by an
update that does `make install`, and what they run is not held to the tree.
So a sync from a machine you do not trust is that machine running its code
as you, at the next launch. The fence is on the sync, the trust is in who
may send one, and for now that is anyone with a link.

**Any machine that has a link to one that accepts can write its files.**
Who can have a link is what a key settles. With the file `mesh_key` on each
machine, the same on all, a machine links only to one that proves it has
the key, see "A Key" in [`docs/intro/intro.md`](../intro/intro.md). With no key, any machine
that can reach the port can link, and so can write. That is right for
machines alone on their wire, and wrong for any that are not.

## What is sent

Every file under the root, but what the tree's own `.gitignore` leaves out,
so `obj/`, `.system_id`, and the like stay
as each machine has them. Both sides list by the sender's rules. `.git` is
never listed. The rules understood are a name, a `folder/`, a `/path` from
the root, and a `*` in one part of a path. A rule that takes one back, `!`,
is not.

`-d` removes what is there and not here. Without it such files are counted
and left. `-c` changes nothing and says what would be. `-v` names each file.

## Speed

The hash of a file is kept, with its time and size, in
`obj/<cpu>/<abi>/sync_hashes`, and is worked out again only for a file
whose time or size has changed, or that changed in the last two seconds.
The whole tree, 146MB in 1,400 files, is first listed in 1 second on a 2018
x86_64 MacBook Pro and 3.3 seconds on a Raspberry Pi 4. After that it is
15ms on an M4 and 0.1 seconds on the Pi, the tree of hashes is 6ms and
0.06 seconds more, and a sync with nothing to send is 0.1 to 0.2 seconds
from start to end. What is said between the two is then a hash each way.
It was the whole list each time, 130KB. The service holds the hashes from
one asking to the next, the file is what it starts again from.

A file goes over 128KB at a time, each part waited for before the next is
sent, and the service refuses a part that is not the next. So the parts
arrive in order however many routes there are between the two. 37MB goes to
the Pi at 6MB a second, and to the x86_64 Mac, which is on Wi-Fi, at 1.5MB.
Plain `ssh` does 8.7MB and 1.9MB over the same two, so it is the network
that sets the pace and not the waiting. It was slower than that, 49 seconds
to the Mac, till two faults in how mail goes over a TCP link were mended,
see `STATUS.md`.

## The parts

`lib/sync/sync.inc`

* `(sync-rules text) -> rules`, the rules of a `.gitignore`.
* `(sync-walk root rules) -> paths`, the files of a tree.
* `(sync-list root rules [kept live]) -> ((path size hash mode) ...)`.
* `(sync-tree root rules [kept no_modes live]) -> tree`, the tree of hashes.
* `(sync-root svc rules_text [no_modes wait]) -> :nil | :old | hash`, the
  top of the tree a service has.
* `(sync-services [name]) -> ((mbox system_id machine root) ...)`, who will
  take a sync.
* `(sync-push svc root rules_text [check remove kept no_modes])
  -> :nil | :old | (sent bytes removed failed send gone remoded)`.

`lib/hash/tree.inc`

* `(hash-tree things) -> tree`, of things each `(path hash [meta])`.
* `(hash-tree-root tree) -> hash`, the top.
* `(hash-tree-kids tree folder) -> kids`, what is in a folder, each
  `(name kind hash meta)`.
* `(hash-tree-text kids) -> str` and `(hash-tree-read text) -> kids`, a
  folder as it goes in a message.
* `(hash-tree-compare mine theirs)
  -> (differ meta gone into only_mine only_theirs)`, one folder against
  the same folder of another tree.
* `(hash-tree-under tree folder) -> paths`, all that is below a folder.

`service/sync/app_impl.lisp` is the service, `cmd/sync.lisp` the command.

## Tests

`tests/system/test_sync.lisp`. The rules, the paths that are refused, what
differs between two lists, and a push from one folder to another through a
service of its own, a file changed, a file in folders that are not there, a
file of more than one part, a file of nothing, what the rules leave out,
and a remove. It has been run between an M4, an x86_64 Mac and a Raspberry
Pi 4, and `rsync` then found no file different.

## The mode of a file

Who may read, write and run a file, the low 9 bits of its mode, is in the
list with its hash. A file sent is given the mode it has where it came
from, so a script is one that can be run. A file that is the same but for
its mode is not sent again, its mode is set, and `sync -c` counts those
apart. Windows has no such thing, a push from one or to one leaves modes
alone.

## Not here yet

* Pull. A machine that sees a newer tree on a neighbour and fetches it, so
  an update spreads from machine to machine and nobody holds the list of
  who is to have it. This is the aim, push is the first step.
* Which of two trees is the newer. The top of a tree says if two are the
  same, not which came first. And a restart when a newer one has arrived.
* A key, so that only a machine that has it can write.
* A folder that is empty, and a folder that is left empty by a remove.
* An older sync. One from before the tree of hashes is told apart, and
  nothing is sent to it. It has to be updated another way, once.

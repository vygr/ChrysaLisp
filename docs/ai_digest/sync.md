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
C96A1B10 x86_64/Darwin .
D649CA0E arm64/Linux .
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
other has not got. Each side lists its files, a name, a size and a SHA-256
of each. The lists are compared, and only a file that is not there, or is
not the same, goes over. It is the model the Net services make their mesh
with, `service/net/mesh.inc`, and the one a storage service would use.

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
There is no key and no check of who is asking. That is right for machines
that are all yours, and wrong for any that are not.

## What is sent

Every file under the root, but what the tree's own `.gitignore` leaves out,
so `obj/`, the `cpu`, `abi` and `os` files, `.system_id`, and the like stay
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
15ms on an M4 and 0.1 seconds on the Pi, and a sync with nothing to send is
0.2 seconds from start to end.

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
* `(sync-list root rules [kept]) -> ((path size hash) ...)`.
* `(sync-diff mine theirs) -> (send remove)`.
* `(sync-services [name]) -> ((mbox system_id machine root) ...)`, who will
  take a sync.
* `(sync-push svc root rules_text [check remove kept])
  -> :nil | (sent bytes removed failed send gone)`.

`service/sync/app_impl.lisp` is the service, `cmd/sync.lisp` the command.

## Tests

`tests/system/test_sync.lisp`. The rules, the paths that are refused, what
differs between two lists, and a push from one folder to another through a
service of its own, a file changed, a file in folders that are not there, a
file of more than one part, a file of nothing, what the rules leave out,
and a remove. It has been run between an M4, an x86_64 Mac and a Raspberry
Pi 4, and `rsync` then found no file different.

## Not here yet

* Pull. A machine that sees a newer tree on a neighbour and fetches it, so
  an update spreads from machine to machine and nobody holds the list of
  who is to have it. This is the aim, push is the first step.
* A version, to know a newer tree by, and a restart when one has arrived.
* A key, so that only a machine that has it can write.
* The modes of files. A new file is made as the host makes one, a new
  script is not marked as one to run.
* A folder that is empty, and a folder that is left empty by a remove.

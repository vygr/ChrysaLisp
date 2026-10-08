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

The service writes only under the root it was started with, the system's
own tree unless `-r` says another. A path that is not under it, one with a
`..`, or that starts with a `/`, or into `.git`, is refused.

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
x86_64 MacBook Pro and 3.3 seconds on a Raspberry Pi 4, and in 0.2 seconds
after that.

A file goes over 128KB at a time, each part waited for before the next is
sent, and the service refuses a part that is not the next. So the parts
arrive in order however many routes there are between the two. It is not
quick for a big file, 37MB took 7 seconds to the Pi and 49 to the x86_64
Mac, 5MB and 0.8MB a second. Source files are small and it does not show.
A window of parts in flight, or the `:in` and `:out` streams, is what would
mend it.

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

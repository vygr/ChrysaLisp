# Rack

One command, on every machine there is. `rack "tests -a"` makes every other
machine's tree the same as this one's and has each of them, and this one,
run every test, each on itself, and say how it went. It is how a change is
tested on a Mac, another Mac and a Raspberry Pi at once, in a second when
there is nothing to send, and it was how this system was built from the day
it worked.

There is nothing new in it. The machines find each other and link, the
mesh. A tree is made the same as another, `sync`. A node starts a node, the
host call the launch scripts use. A task is sent to a node of another
machine. `rack` is those four, in a row.

```image
docs/diagrams/rack_run.cwb
```

## A member

A machine is in the mesh while it has a node that is. `./rack.sh up` starts
one that stays, a member, a single `main_tui` with no terminal, running
`lib/rack/member.lisp`. It starts the Net service, listens for and looks
for the other machines, `link -l 3333 -a` and `link -a`, and takes a sync,
`sync -a`. Then it waits.

```code
./rack.sh up        start this machine's member
./rack.sh status    is it up
./rack.sh down      stop it, and only it
./rack.sh run 'rack "tests -a"'
```

`run` is the way in from a shell. The command line is left in a file, the
member runs it as a terminal would, and what it said comes back. Any
command, `./rack.sh run sync` lists who takes a sync.

`./stop.sh` stops every node of a machine, its member too. `./rack.sh down`
stops the member and nothing else.

A member builds nothing and tests nothing. It runs the boot image and the
Lisp it was started on, and goes on doing so while the tree changes under
it. After a change to the Net service, the mesh, sync or `lib/rack` itself,
stop it and start it.

## What it lets in

A member takes a sync, so any machine that can link to it can write to its
tree. Not outside it, a sync is fenced to the tree, [`docs/ai_digest/sync.md`](sync.md).
With a `mesh_key` file only a machine with the same key can link at all,
[`docs/intro/intro.md`](../intro/intro.md), "A Key", and a member should have one. It is not
started unless asked for, and nothing starts it at boot unless you have it
do so.

## The command

```code
rack [-b] [-l machines] [-d paths] "command line"
```

1. Each other machine that takes a sync is made the same as this one, what
   differs is sent. Files are not removed there, but those named by `-d`,
   with a `:` between.

2. Each machine that came level, and this one, is sent a task. The task
   starts a session, `lib/rack/session.lisp`: a new node, started by the
   host, so on the boot image as it is on disk now. It sizes itself to its
   machine, runs the command line, leaves what it said, stops the nodes it
   started and goes.

3. With `-b` a session before that one runs `make` and `make all boot`, so
   the one that follows is on the new boot image. For a change to VP code.

4. A line for each machine comes back.

```code
SYNC arm64/Linux 3 sent 4182 bytes 0 failed 244ms
SYNC x86_64/Darwin 3 sent 4182 bytes 0 failed 136ms
RAN arm64/Darwin Modules: 98, 2 run, 96 as they were Passed: 4644 Failed: 0 RESULT: SUCCESS [1s]
RAN x86_64/Darwin Modules: 98, 2 run, 96 as they were Passed: 4644 Failed: 0 RESULT: SUCCESS [1s]
RAN arm64/Linux Modules: 98, 2 run, 96 as they were Passed: 4644 Failed: 0 RESULT: SUCCESS [3s]
DONE 3711ms
```

`-u name` makes the sessions that user's, a folder of `usr/`, and not
whoever last signed on to each machine, so what a run writes of its own,
an app's settings say, goes to that user's folder.

`-l` names machines to bring level and not run on, as `sync` lists them,
`arm64/Linux`, with a `,` between. For a machine someone is sat at.

The sessions are apart from the member and from each other's machines. Each
machine builds and tests in its own tree, on its own file system, nothing
is run across machines but the task that starts the session. The fence
round a `run` task stands.

## The parts

* `lib/rack/rack.inc`: `(rack-run cmdline [make_first leave_out gone user
  tree]) -> (line ...)`, and `(rack-fresh phases [user]) -> str`, the
  sessions of one machine. Each session has a pair of files of its own in
  `/tmp`, so a rack run can be going inside a session of another, which is
  how `tests/solo/test_rack.lisp` tests it: a node with a system id of
  its own is the other machine, and a small tree is what is sent.
* `lib/rack/session.lisp`, a session. `lib/rack/member.lisp`, a member.
* `cmd/rack.lisp`, the command. `rack.sh`, the member from a shell.

## Not here yet

* Windows. `rack.sh` is a shell script, and the files a session and a
  member are given their work in are under `/tmp`.
* One rack run on a machine at a time, a session's files are the machine's.
* A machine that does not come level is not run on, and says so. One that
  does not answer in 6 minutes is `no reply`.
* A test of it. It needs more than one machine.

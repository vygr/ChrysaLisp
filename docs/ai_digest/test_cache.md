# The Tests Are A Cache Of Results

The unit tests, `tests/<category>/test_<name>.lisp`, are run with the `tests`
command, and their results are kept. A module is run again only when
something it stands on has changed. In Chris's words, "view the tests as a
cache of results, when those results go invalid we rerun that level of the
cache and its effects ripple out".

```code
tests
Modules: 85, 2 run, 83 as they were
Passed: 4112
Failed: 0
```

So the test of a change is as big as the change. An edit to a library runs
the modules that use it. An edit to what is under everything, the boot
image, runs them all. An edit to a doc runs none.

## What A Module Stands On

* The module itself.
* Every file it imports or includes, and every file they do.
* Every file any of those names, a shader, a data file, an app or a service
  it starts, wherever a line with a string on it has a word that is the path
  of a file.
* Every command any of them runs, `cmd/<name>.lisp`, where a string starts
  with the name of a command, or has one after a `|`.
* And under every module, the boot image, the host programs, the Lisp every
  task starts with, `class/lisp/root.inc` and `class/lisp/task.inc`, and the
  suite itself, `tests/suite.inc` and `cmd/tests.lisp`, with all they
  import.

The native code of a library, a `lisp.vp`, is in the boot image, so a change
to VP is a change to what is under everything, once it is built.

What a file names is found by reading it, not by running it, and it can name
too much, a word in a string that happens to be a path. That costs a run
that was not needed, which is the safe way to be wrong. A module that
stands on a thing in a way that can not be seen in its text, a path put
together as it runs, has to name it in a comment, a line with a string on
it.

## The Cache

`obj/<cpu>/<abi>/tests/cache`, with the objects of the CPU and ABI that is
running. So each machine has its own, and so has the VP64 emulator. It is
not in git.

A line for each file that was looked at, when it was changed, a hash of it,
SHA-256, and what it names. A file is read again only when it has changed.
And a line for each module, a hash of the hashes of all it stands on, and
its counts.

A module is run if it has no line, or the hash it would have now is not the
one on its line. A module that fails has no line kept, so it is run every
time till it passes. The counts of a module that is not run are added to
the summary as they were.

It is the content that is hashed, not the time. A boot image built again
from the same source is the same boot image, and a file changed and changed
back, with no run of the tests between, is as it was. Only the last result
of a module is kept, so if the tests were run on the change, they are run
again on the change back.

## The Options

* `tests`, what has to be run, of all the modules.
* `tests -m gpu`, the same, of the modules with `gpu` in their path.
* `tests -s`, list what would be run, and run none.
* `tests -a`, run every module whatever the cache has.

**A release is tested with `tests -a`**, on every machine, with the install
and the emulated platforms, whatever the cache has. The cache is for the
work between releases.

## What A Release Is Tested With

All of this, from cold, on each machine there is, each CPU and each OS.

* `make install`, from `snapshot.zip`, as a new user has it. If it fails,
  the snapshot is too old for the tree and a new one is wanted.
* `tests -a`, every module.
* `make it`, the boot images of all six CPUs, and the docs.
* `tests -a` on the VP64 emulator, the launch scripts take `-e`. On a
  machine of four cores or fewer, `tests -a -j 500`, a module at a time.
  With them all at once a Raspberry Pi 4 is overrun, and a different test
  with a wait in it fails each time. A module at a time they all pass, in
  six and a half minutes.
* `lint`, which must say `lint: clean`. It is `make vp`, `make apps
  debug`, then `files obj/vp/ | trace -i -l`, and then `make apps` and
  `make all boot` to leave the release build, all in the one session.
* RISC-V and LoongArch under QEMU. The host program is built there first,
  from the source as it is, one left from an older tree has a shorter table
  of calls than the boot image expects and the node dies of it. Then, from
  the cross built boot images, a self hosted `make all boot` must give the
  same image to the byte, and `tests -a` must pass.

A test that waits for another task asks `(task-timeout seconds)` how long,
which is ten times as long on the emulator. A wait of a set number of
microseconds is a test that fails on a slow enough machine.

## Three Machines

A change to Lisp is the one result on every machine. A change to VP or to a
host program is a result for each CPU. Each machine has its own cache and
works out for itself what to run, so the tests can be asked for on all of
them at once and each does only what it must.

## The Code

`tests/suite.inc`. `(test-file-scan file text)` is what a file names,
`(test-file-info file)` what is known of a file, `(test-stands-on files)`
all that files stand on, `(test-base)` what is under every module,
`(test-key module base)` the hash of a module, and `(run-suite)` does the
rest. `tests/system/test_suite_cache.lisp` tests them.

## The Lint

```code
lint
lint: clean, 834ms
```

The trace lint works out what each VP function really trashes, and says
where that is not what its header has written down. It is only right on a
debug build. `lint` makes one, runs the trace lint over every listing under
`obj/vp/`, puts the release build back, and prints what any of it had to
say, which is nothing when all is well. `-k` leaves the debug build, `-v`
says what each step took. `cmd/lint.lisp`.

It is run after any change to VP. It is under a second of work on an Apple
M4 Max, and 1.4 seconds from a shell with the session to start and stop. It
was 14 seconds as five commands, each a session of its own, and each of
those waited 2 seconds for nothing, see `STATUS.md` for 2026-10-08.

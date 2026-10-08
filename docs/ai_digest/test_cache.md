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

---
name: chrysalisp-tests
display-name: ChrysaLisp Tests
description: Use when running or writing ChrysaLisp tests — the tests/ suite and its `tests` command, the assert macros, error and table tests, and standalone test scripts.
---

# ChrysaLisp Tests Skill

ChrysaLisp has a comprehensive functional test suite in `tests/`. The
general ChrysaLisp disciplines (see the `chrysalisp` skill, `LLM.md`, and
`docs/ai_digest/`) apply to test code as well.

## Contents

This is a long skill. Find the task below and read that section in full before
acting, rather than skimming the whole file. Sections marked mandatory apply to
every task.

*	**[Running the Test Suite](#running-the-test-suite)**
	The `tests` command and its options, what the output looks like, and what
	differs between the native and emulator images. Which runs to make, how
	to watch one and clear up after it, looking at apps from inside. Also
	`lisp -r` snippets, the standard sanity tools, and how piped execution
	works.

*	**[Standard TUI Make Commands](#standard-tui-make-commands)**
	Every `make` option, which build each produces, and when a validate
	build is needed.

*	**[Suite Layout](#suite-layout)**
	Which file is the command, the framework, and each test module.

*	**[Writing Suite Modules](#writing-suite-modules)**
	Read before adding a test to the suite: the assert macros, error tests,
	table tests with `test-cases`, how a module is found and isolated, how a
	test passes anywhere, and not repeating checks.

*	**[Writing Standalone Test Scripts](#writing-standalone-test-scripts)**
	One-off scripts outside the suite, with error catching and host shutdown.

*	**[Lock Service & Lock History Inspection](#lock-service--lock-history-inspection)**
	The `@Lock` service, its history buffer, and how to inspect it from the
	TUI, by RPC, and in unit tests.

*	**[Pre-Public Release Tag Verification (Mandatory)](#pre-public-release-tag-verification-mandatory)**
	Mandatory before a release tag or a significant contribution. The full list
	of checks that must pass.

*	**[Multi-Instance & Network Link Testing](#multi-instance--network-link-testing)**
	Pointer to the `chrysalisp-net-testing` skill, with the key launch pattern.

## Running the Test Suite

Tests are run with the `tests` command (`cmd/tests.lisp`), inside the full
operating system environment, with the standard libraries, background nodes
and system services running. From the host shell, pipe the command to Node 0:

	echo "tests" | ./run_tui.sh -f

By default it prints only the failures and a summary, so the output needs no
filtering:

	=== Test Summary ===
	Modules: 67
	Passed: 3167
	Failed: 0
	Skipped: 0
	RESULT: SUCCESS

A failure names its module, the test, what was expected and what it got:

	[FAIL] math/test_integers: nlz 0 | Expected: 64 | Got: 0

Options:

*	`tests -m str` runs only the modules with `str` in their path, so
	`tests -m math` runs `tests/math/`, and `tests -m test_flow` one module.

*	`tests -l` lists the modules, without running them. Add `-m` to filter.

*	`tests -v` prints every test, pass or fail, with section headers and
	skipped tests. Do not use it from an agent for the whole suite, it is
	over 2,000 lines.

*	`tests -j num` sets the most modules to a batch, default 1. The modules
	run in parallel, a batch to a task, farmed over the nodes, and the
	results print in module order. If they all fit in one batch they run in
	the one task, one after another, so `tests -j 1000` is a serial run.
	Use that to tell a fault in a test from a fault of running at once.

*	`tests path ...` runs just the module files given.

*	The results are a cache: a module runs again only when something it
	stands on changed. `tests -a` runs every module, the release ones of
	`tests/release/` too, `tests -s` lists what is stale. Modules of
	`tests/solo/` change the network and run one at a time, after the rest.

*	A run always prints `=p=` lines as it goes: the plan, cached or dirty
	per module, start and done with its last time in ms, `at` with a
	module's last check, `late` for one well past its usual time, `lost` for
	a batch with no result, and end. Watch those, not the summary.

*	`tests -w` says the longest a task was kept from running on each node.
	A module that holds its node a second or more is why a test elsewhere
	fails now and then: its long loops need a `(task-slice)`.

*	Every module runs at the same time as the others, so a module must not
	rely on what another leaves behind, and must give any file or service
	it makes a name of its own. Tests that do depend on each other go in
	the one module. The lock service tests are one module for that reason,
	they read back the history of the one `@Lock` service.

*	`tests -f` records stack frames, with `lib/debug/frames.inc`, in every
	function a module defines or imports. An error then says what was
	running, where it normally says `Frame: :nil`:

		Frame: ("zz-outer -> tests/x/test_y.lisp(3)" "zz-inner -> tests/x/test_y.lisp(1)")

	Use it with `-m` to chase a module that fails to run to the end. It is
	slower, so is not the default.

How many nodes the suite runs on matters too. `./run_tui.sh -n 0 -f` sizes
the network to the machine, `-n 1` boots one node. Run both. Services and
child tasks land on other nodes on a multi node network, so message timing
and routing faults only show there.

Which boot image the suite runs on matters:

*	**Native, `./run_tui.sh -f`:** every test runs, the error tests too.

*	**Emulated VP64, `./run_tui.sh -e -f`:** the same image, with the same
	checks, run by the emulator. Run this when a change touches the VM or
	any VP level code.

*	**Full system, `./run.sh -f`:** the same suite with the GUI service up,
	and a TUI attached to the host.

Every build has its error checks. For the extra runtime validation as
well, build with `make it validate`. A validate build also fills every newly allocated heap
cell with a pattern, bytes of `0xA5`, so code that reads memory before
setting it fails the same way each time, rather than by luck. It fills each
freed cell with a second pattern, bytes of `0x5A`, and aborts on a double
free. It checks the count on every object ref and deref, and reports
`Dead object !` with a stack dump, and checks each `:sys_mem :free` is of
a block that was given out, `Bad free !`. It keeps a guard word after
every heap cell and tests it on free, `Heap overrun !`. The heap's checks,
the patterns and the guard word, are those of the boot image, so it must be
a validate one, `boot` with `validate`; the others are in each function. A fault address or pc of `0x5a5a5a5a5a5a5a5a` means use after free.
If a validate image will not boot, `make install` puts back the working
VP64 image from `snapshot.zip`, and `./run_tui.sh -e` can build from it.
Always finish with a plain `make it`, which puts back the normal images,
what goes into `snapshot.zip` and what `make install` runs on.

	echo "make it validate" | ./run_tui.sh -f
	echo "tests" | ./run_tui.sh -e -f
	echo "make it" | ./run_tui.sh -f

Inside an interactive TUI or Terminal session just type `tests`.

A build, a module, or a cached run takes seconds; `tests -a` takes minutes,
the release modules most of it. Guard a run with a limit a little over its
usual time, never far over, and treat hitting it as a hang or crash to look
into. See Watching A Run below.

### Which Runs To Make

Test runs must not eat the time of the work.

*	While working, run the modules the change touches and the reproduction
	itself, `tests path ...` or `tests -m str`. A change kept to one app or
	to a library function whose users are known is checked by its own
	module, `check`, and a look on a hidden desktop if it is visual, then
	committed.

*	The whole suite is for kernel, VP, translator and host changes, for a
	tag, overnight, or when asked for. Regenerate docs and write notes
	first, so it runs once, when nothing more is to change.

*	One run at a time on a machine: two suites share fixed ports and the
	rack's one job file, and each spoils the other. Check none is going
	before starting one.

*	When a change is decided, stop the run that tests the old code at
	once, make the change, run the touched modules, then one full run.
	Never wait a run out that will have to be made again.

*	After a batch that adds tests or tasks, soak it: twelve full runs or
	more, the slowest machine's results read first. Races show there, and
	not in single runs. Count only the runs after a fix.

### Watching A Run

*	Send a long run's output to a file and watch the file: `=p= done`
	against the plan, `[FAIL]`, `late`, and nothing new for a minute. A
	`grep` on `tail -f` needs `--line-buffered`, and nothing after it in
	the pipe that buffers, `tr` or `cut`; see one line come through before
	trusting the watch. Never put a run behind `| tail` or `| head` alone,
	nothing shows till it ends.

*	Know each run's usual time. A run a minute or two past it is already
	news: look at which module, which processes, what is stuck. A node at
	no CPU with no new output for a minute is dead, stop it then.

*	A run cut off at a limit: read what it was doing when cut, its last
	`=p=` lines, before touching the limit. A run known to be stuck is
	stopped at once, not run out.

*	A time limit wrapper round `run_tui.sh` kills only the script. Its
	nodes live on, keep its stdout open, so a `| grep` after it never ends,
	and join the next run's network, where they take batches and never
	answer. After a run cut short, find the nodes it left by start time and
	parent (an orphan is parent 1) and kill them by pid. Never by a pattern
	that may match another run's ssh or a standing rack member, never
	`./stop.sh`, never a session that is not of this run. In `pgrep -f`, a
	spawned node's `-run` arguments are joined by a vertical tab, not
	spaces, and a wait loop on `pgrep -f name` matches its own command line.

*	`./rack.sh run 'cmd'` runs `cmd` in the member's own one node, one job
	at a time from one job file. Short commands and modules are fine there;
	a full suite there is serial and slow, so give it a session,
	`./rack.sh run 'rack "tests -a"'`. `lisp file.lisp` never ends as a
	rack job, it REPLs on a stdin that never closes: use `lisp -r`. Errors
	of a desktop a rack job started are in `/tmp/chrysalisp_rack.log`.

*	A file made in the tree for a moment and deleted here is still on the
	other machines, no sync deletes: remove it there too, or `check` fails
	on it.

### Looking From Inside

Monitor and test with the system's own tools, not host scripts. Look at
an app with `widgets` (text, `-f`, `-d`, `-k` to keep it short) and
`locate`, drive it with `input`, which finds a view by name, never by a
guessed x y, and `grab` only when the look itself is the question, the
smallest area that answers it. An image costs far more than the same
answer as text. Read a tree of views with `(ui-save)` and `(ui-find)` of
`gui/lisp.inc`. Check files with `canvas-load`, `crc32`, `adler32`,
`inflate-decompress`. Host tools only for a reference that must come from
outside, and say so.

### Direct REPL Access (`lisp -r`)

To try a snippet of raw ChrysaLisp code, pass it to the `lisp` command's
`-r` / `--repl` option. The remainder of the command line is read into the
REPL:

```sh
echo "lisp -r (print (* 123 456))" | ./run_tui.sh -n 1 -f
```

*	`./run_tui.sh -n 1 -f` gives a single TUI only node.

*	`./run.sh -n 1 -f` gives a single GUI node with a TUI attached to the
	host, so GUI dependent code and libs can be run with the GUI boot image.

*	Use `{}` for strings: `lisp -r (print {hello world})`. The command line
	parser strips double quotes. `(read)` does escape processing in both
	string forms, so use `\q` inside `{}` for a double quote character, as
	well as `\n`, `\t` and `\\`.

*	To run a scratch script use `lisp -r (import {tests/scratch/probe.lisp})`
	so the command exits when done. A bare `lisp file.lisp` imports the file
	and then waits in the stdin REPL.

*	Start with one simple expression and build up. Do not batch many untested
	snippets into one invocation.

*	On a multi-node start the node list and routing take a little while to
	settle. `(lisp-nodes)` and `(mail-enquire ...)` can be incomplete in the
	first second or so; wait with `(net-quiet)` or `(task-sleep 1000000)`.

*	Keep `(env-push)` / `(env-pop)` balanced in a snippet. An unbalanced push
	leaves the wrong environment current when the command's `main` returns.

### Leaks

Ownership is a tree: what an object holds is below it, a link back to an
owner is a weak ref claimed where it is used, and letting go of the top
frees all of it. Three checks, all from inside:

*	`tests -e`: each module alone, twice, failing one that leaves its node
	holding more after the second run.
*	`tests -a -m app_leaks`, a release test: every app opened and closed
	on a hidden desktop five times, failing one that leaves the desktop's
	node holding more by over 4KB on each of the four after the first. It
	waits for the tasks an app started there to end before it measures, and
	asks one probe task it keeps on the node, not a new task each time.
*	`(loops obj)` and `(view-loops view)`, `lib/debug/loops.inc`: each
	place under obj that holds obj or what is between, a counted link back.

`(kernel-stats :t)` first lets go of what a task's end does, numbers,
symbols, free blocks, and the words the fonts keep, so what is left is
what is held: the sixth of what it gives, `(elem-get s 5)`, the cells in
use. Never the total less what is free, `(- (elem-get s 1) (elem-get s 2))`:
that has in it the end of each block too short for a cell, and moves by
about 6KB as a heap takes a block or gives one back, so an app that holds
nothing reads as more one time and less the next. To find a leak,
run the app off the desktop's node too: one that leaks only on the GUI
node is touching something in its shared root. Then cut the app down a
call at a time, and measure one call in a loop in a single task.

### Standard Sanity Tools

The standard sanity test tools from the TUI are `includes`, `forward`,
`brackets`, `check` and `trace`:

```sh
echo "files | includes" | ./run_tui.sh -f
echo "files | forward" | ./run_tui.sh -f
echo "files | brackets -q" | ./run_tui.sh -f
echo "files | check" | ./run_tui.sh -f
```

`check` reads each file as a tree, `lib/text/tree.inc`, and says what can be
seen to be wrong from the source alone, a line each, the file, the line, the
check and what it found. It must say nothing. `-c` names the checks to run:

*	`progn`, a `(progn)` round the else forms of an `(if)` or an `(ifn)`, or
	all the body of a `(when)`, `(unless)`, `(while)` or `(until)`: each of
	those takes as many forms as it is given.
*	`class`, a string of characters not in order given where a class of
	them is looked up, `(bskip)`, `(bfind)`, `(trim)`, `(split)`: only some
	of it is found. `(char-class)` puts one in order.
*	`zero`, a number written with a point that is too small for a fixed,
	`0.000001`, and so is 0.
*	`places`, in a `.shader`, a number written with more than 4 places
	after its point: a shader's number is rounded to 4, one that needs more
	is a str. It and `bracket` are all that is asked of a `.shader`.
*	`shadow`, a variable with the name of a function or macro of the system
	as a test of an `(and)`, an `(or)` or a clause of a `(cond)`: there the
	function is what is taken, and it is always true. `(and path x)`.
*	`const`, a `+` constant given a list of strings or numbers quoted once.
*	`style`, `(if tst (progn ...))` with no else, a `(when)`; `(if (not tst)
	form else ...)`, an `(ifn)`, and with no else an `(unless)`, not an
	`(ifn)`: `(ifn 7 1)` is 7 where `(if (not 7) 1)` is `:nil`; the same of
	an `(ifn)`, the other way about; a `(cond)` or a `(condn)` of one
	clause; and `(when (and a b) form)`, or an `(if)` of one with no else,
	which is `(and a b form)`.
*	`doc`, a `; (name args) -> result` comment under a `(defun)` or
	`(defmacro)` that is of another name, or of more or fewer things than
	it takes.

*	`enum`, a field of an `(enums)` named `size`: `+name_size` is what the
	`(enums)` makes for how many fields there are.
*	`capture`, a `(lambda)` or `(#)` written where it is given to another
	function of the same file, that uses a name the other has as a
	parameter. A name is found where a function is called from, so it has
	the other's: `(walk name (lambda (x) (visit x)))` when `(walk)` takes
	`visit`.

*	`held`, a `(create-stdio)` that is not kept in a name by a `(defq)`. It
	is let go of as soon as it has been looked at, its streams say they are
	stopping, and what is written after may not be read.

A test of the very thing has a comment that begins `;check off` on its line,
or on the line before, and nothing is said of it.

`trace` runs on the normal VP objects, `obj/vp/`: `make vp` for the
system, and `make apps` for the `apps/` VP functions. `lint` does both and
runs it.

### How Piped Execution Works

Node 0 reads host stdin. When input is piped (e.g. `echo "tests" | ...`), host
`read()` returns EOF (`-1`) once the input stream finishes. The TUI child task
detects EOF and signals the main TUI loop (`*eof* :t`). Once the running command
pipeline finishes, TUI detects `*eof*` and executes `(pii-exit)`, cleanly
shutting down Node 0. Node 0's termination then triggers the launch script's
`./stop.sh` cleanup, cleanly stopping all background worker nodes.

A clean run shows no `[FAIL]` lines and ends with `RESULT: SUCCESS`. Optional
features that are not defined print `[SKIP] name not defined` instead of failing
(see `tests/system/test_system.lisp`).

## Standard TUI Make Commands

Automated tools and developers now have direct access to the standard TUI
`make` command (`cmd/make.lisp`) and all its options. Piped commands work
identically for builds as they do for tests:

*	**Incremental Host Build:**

	```sh
	echo "make" | ./run_tui.sh -f
	```

*	**Host Boot Image Rebuild:**

	```sh
	echo "make all boot | time -s" | ./run_tui.sh -f
	```

	Recompiles all host `.vp` files and regenerates the platform native boot
	image (`obj/<arch>/<OS>/sys/boot_image`).

*	**Canonical Multi-Platform Rebuild (`make it`):**

	```sh
	echo "make it | time -s" | ./run_tui.sh -f
	```

	Recompiles all supported target platforms (`AMD64`, `WIN64`, `ARM64`,
	`RISCV64`, `LA64`, `VP64`), all in the normal mode (`*build_mode* = 1`),
	and regenerates the Markdown documentation in `docs/reference/`.

*	**Register Analysis:**

	```sh
	echo "lint" | ./run_tui.sh -f
	```

	Builds the system and apps and runs `trace` (`cmd/trace.lisp`) over
	their VP for register usage and clobber analysis, which by hand is:

	```sh
	echo "make vp" | ./run_tui.sh -f
	echo "make apps" | ./run_tui.sh -f
	echo "files obj/vp/ | trace -i -l" | ./run_tui.sh -f
	```

	Every function is linted, no filtering is needed. The generated
	`class/x/create` and `class/x/type` functions are documented by the
	header comment under their `(gen-create :x)` and `(gen-type :x)` calls.
	`trace -i -l -w` writes the calculated trashes back to the source on a
	mismatch.

	*Important:* a validate image must never be packaged into
	`snapshot.zip`. Run a plain `make it` before `make snapshot`.

*	**App Rebuild:**

	```sh
	echo "make apps" | ./run_tui.sh -f
	```

*	**Documentation Generation:**

	```sh
	echo "make docs" | ./run_tui.sh -f
	```

*	**Build Modes & Verbosity:**

	`make` accepts `validate`, `test`, `platforms`, and `-v <num>`
	(verbosity). Without `validate` a build is normal. For example: `make -v 1 it`.

## Suite Layout

*	`cmd/tests.lisp` — the `tests` command. Parses the options and calls
	`(run-suite)`.

*	`tests/suite.inc` — the framework: the counters, the assert macros,
	module discovery, and `(run-suite modules [verbose frames jobs child])`.

*	Category folders — `core/`, `math/`, `collections/`, `sequences/`, `text/`,
	`streams/`, `system/`, `net/`. Each module is a `test_<topic>.lisp` file of
	top-level assertions. Any file named `tests/<category>/test_*.lisp` is
	found and run automatically, in path order, there is no list to add it
	to. A script named `test_` that is not a suite module must be listed in
	`+test_excludes` in `tests/suite.inc`.

Keep any test scripts and associated files in the `tests/` folder. Never place
temporary test scripts, scratch directories, or test output in the project root;
use `tests/scratch/` and always clean it up after being done with it.

## Writing Suite Modules

A suite module is top-level code, not a function. It starts with a
section header and then interleaves setup with assertions:

	(report-header "My Feature")

	(defq x (my-function 10))
	(assert-eq "basic" 20 x)

No imports are needed for the framework, and the module needs no
registration, the file name is enough. Each module runs in an environment of
its own, so what it defines is gone when it ends. A module can not rely on a
variable or function left behind by another, and has no need to `undef` its
own.

Assertions, each takes a short name first:

*	`(assert-eq name expected form)` — strict `eql` equality.

*	`(assert-true name form)` — the form gives anything but `:nil`. Use it
	for predicates, many give a non `:nil` value that is not `:t`.

*	`(assert-list-eq name expected form)` — the two print the same, so a
	list matches a `nums` vector with the same elements.

*	`(assert-error name form)` — the form must throw an error. This is how
	the argument checks at the VP to Lisp boundary are tested.

*	`(test-skip name why)` — count a test that can not run here.

*	`(test-output code)` — what a snippet of code prints, as a string. It
	is run with `lisp -r` in a task of its own, so nothing reaches the
	terminal. Use it to test anything that prints, a test must not print
	to the terminal itself, the suite is quiet. The code is a command
	line, so use `{}` for its strings, and it can not hold a `|` or a `!`,
	which split a pipe, so no `lines!` or `each!` in it:

		(assert-eq "print" "a5\n" (test-output "(print {a} 5)"))

For many small cases use a table. Each form is followed by the result it
must give, and is named by its own text. Lists are compared by content, to
any depth, everything else by `eql`. A list also matches a `nums` or other
vector with the same elements, which is how a nested vector result is
written:

	(test-cases
		(slice "hello" 0 0) ""
		(slice "hello" 3 1) "le"
		(slice (list 1 2 3) -1 0) '(3 2 1)
		(first (list)) :nil)

Edge case modules, `test_*_edges.lisp`, are written this way. When adding
cases, test what the language defines: empty sequences, the first and last
index, negative indices, zero, the largest and smallest integer, and so on.
Test with `assert-error` that the build catches wrong code.

A string in a test file can not hold an escaped `"`. Use a `{}` string when
the text has a double quote in it, `{a "quoted" word}`.

Conventions:

*	**NEVER Run GUI Code from Tests or TUI:** Never attempt to instantiate or
	execute GUI code (such as `View`, `Window`, `Vdu`, `Flow`, `Button`, `Label`,
	`Md`, or GUI applications in `apps/`) from test suite modules, standalone
	test scripts, or the TUI boot image (`./run_tui.sh`). The headless test
	environment and TUI boot image do NOT initialize the GUI subsystem,
	window manager, or compositor. Attempting to reference or instantiate GUI
	classes in these environments immediately triggers `symbol_not_bound` errors
	(e.g. `Obj: View`). Test non-GUI logic, algorithms, and CLI commands only.

*	**No Root Scratch Folders:** Never create or use a `scratch/` directory in
	the project root. Any scratch scripts, test logs, or temporary directories
	must be placed in `tests/scratch/` (or directly within `tests/`), and must
	always be cleaned up and removed after testing is completed.

*	**Temp files:** Use `tmp_*.txt` names inside `tests/`, and clean up with
	`(pii-remove file)`. Null out the stream variable first — `(defq fs :nil)` —
	to release the OS file descriptor before removal.

*	**catch/throw are for faults:** They work the same in every build. To
	throw an error use `(throw "Description !" obj)`, or `:nil` if there is
	no object of interest. Do not use them to steer normal code.

*	**Child tasks may be on another node:** A `Pipe` child can be placed on
	any node, so `(mail-validate id)` cannot be used to check it is alive.
	Have the child report to a reply mailbox instead, as `test_pipe.lisp`
	does.

*	**Optional features:** If a feature may not be defined, check `(def? 'name)`
	and call `(test-skip name "not defined")` rather than failing the suite.

*	**Nothing under `obj/` but code:** `obj/` holds code generation,
	loading and binding only, VP, natives, boot images, the caches of
	those. A test's own files and every probe or scratch file go in
	`tests/scratch/`.

### A Test Passes Anywhere

A test fails only for a real fault of what it tests, whatever machine,
load or network it runs in.

*	**Wait for the event, never for a time.** A test waits for the thing
	it tests: an answer, a grant, a scene's end, a stream stopping. No
	check of an upper bound on elapsed time; a lower bound, not before, is
	fine. A wait that could be for ever is fine too: a true hang comes back
	as a lost batch, `+test_retry` in `tests/suite.inc`. Library calls take
	`:t` for till it comes where a test needs it. A time limit found in
	library code is a design question, not a test fix.

*	**Touch only what it made.** Its own nodes, counted by `(lisp-nodes
	:t)` or by the ids it got, the services and desktop of the node it
	started, files and names of its own. Never the first of a system wide
	name, never another machine, session, sound or display device.

*	**Give way.** Scheduling is cooperative: a long loop of Lisp in a test
	or a farmed child, formatting whole files, reading thousands of shapes,
	needs a `(task-slice)` every so many items, or tests beside it with a
	deadline fail now and then.

*	**Let go of what is shared.** A `(canvas-shared)` segment goes only when
	the canvas is freed; in a test `(. canvas :sub)` and `(setq var :nil)`.
	A count of the shared memory notes before and after a run must not grow.

*	**Ports:** a test that listens on a fixed port takes one below 32768,
	under what Linux and macOS give outgoing connections, and of its own.

*	**Solo tests leave the network as they found it, as seen:** a killed
	node is listed for a beat after its process has gone, so a solo test
	ends by waiting till the node count is back, as `tests/solo/test_gone.lisp`
	does.

### No Repeated Checks

Every check is time on every run of every machine. Before adding one, grep
`tests/` for the function and read what is there; add only what no check
covers, an edge, or a path with code of its own (a service reply, two
members, after a restart). The kept check lives in the facility's home
module. When a facility gets a home and a test of its own, take its old
checks out of the other modules in the same batch. A function used as a
tool inside another test is not a repeat.

What the runner itself stands on needs no plain check, if it breaks no
module runs. What the runner judges by does, with a negative case, as a
broken one makes the run lie: `eql` for `assert-eq`, `str` for
`assert-list-eq`, `catch` giving `:t` for `assert-error`, `str-as-num` for
the counts.

## Writing Standalone Test Scripts

For one-off scripts (specialized system checks, custom cmd app verification),
keep the script in `tests/`, make it self-contained, wrap the work in a `catch`
block for robust error reporting, and end with the host shutdown call:

	;use of (pipe-run command_line)
	(import "lib/task/pipe.inc")

	(defun my-test ()
		(defq test_string "string with new line\n")
		...
		(pipe-run appname)
		...)

	(catch
		(my-test)
		(progn
			;report error
			(print "Test failed with error" _)
			;signal to abort the catch
			:t))

	;clean shutdown of the VP node
	(pii-exit)

To run a `cmd/` app from a raw script, wrap it in `(pipe-run command_line)` from
the `(import "lib/task/pipe.inc")` library.

**Strictly Non-GUI:** Standalone test scripts execute in a headless environment
under the TUI boot image. They must never import GUI modules (`apps/desktop/`,
`gui.inc`, etc.) or attempt to construct UI components (`View`, `Vdu`, etc.).

## Lock Service & Lock History Inspection

ChrysaLisp includes a distributed lock service (`@Lock`, defined in
`service/lock/app.inc` and implemented in `service/lock/app_impl.lisp`).
The lock service maintains a circular history buffer of the last 128
lock and unlock actions (`+lock_max_history 128`), recording each operation
in `"key (action mode)"` format (e.g. `"tmp.txt (lock write)"` or
`"src.vp (unlock read)"`).

### Inspecting via TUI `locks` Command

In any interactive TUI or Terminal session, inspect recent lock activity with:

	locks

From the host shell:

```sh
echo "locks" | ./run_tui.sh -f
```

Options:
*	`locks -h` / `locks --help`: display command usage.

### Inspecting via RPC in Tests and Code

To query lock history programmatically:

```lisp
(import "service/lock/app.inc")

; Retrieve history as a list of strings
(defq history (lock-history-rpc))
(each (const print) history)
```

### Verifying Lock Behavior in Unit Tests

`tests/system/test_lock.lisp` verifies lock acquisition, lock modes, and lock
releases across CLI commands and libraries:

1.	**Command Verification Pattern:**
	Execute the command via `pipe-run` and verify that the expected lock and
	unlock events are recorded in `(lock-history-rpc)`:

	```lisp
	(pipe-run "echo data | save tmp_test.txt" (lambda (&ignore) :nil))
	(pipe-run "cat tmp_test.txt" (lambda (&ignore) :nil))
	(pipe-run "rm tmp_test.txt" (lambda (&ignore) :nil))

	(defq hist (lock-history-rpc))
	(assert-true "save write lock"
		(nempty? (some (# (if (eql %0 "tmp_test.txt (lock write)") %0)) hist)))
	(assert-true "save write unlock"
		(nempty? (some (# (if (eql %0 "tmp_test.txt (unlock write)") %0)) hist)))
	(assert-true "cat read lock"
		(nempty? (some (# (if (eql %0 "tmp_test.txt (lock read)") %0)) hist)))
	(assert-true "cat read unlock"
		(nempty? (some (# (if (eql %0 "tmp_test.txt (unlock read)") %0)) hist)))
	```

2.	**Library Verification Pattern:**
	Call scanning or dependency functions (e.g. `files-depends`, `files-scan`)
	and check history for read lock claims and releases:

	```lisp
	(files-depends "tmp_test.lisp")
	(defq hist (lock-history-rpc))
	(assert-true "files-depends read lock"
		(nempty? (some (# (if (eql %0 "tmp_test.lisp (lock read)") %0)) hist)))
	(assert-true "files-depends read unlock"
		(nempty? (some (# (if (eql %0 "tmp_test.lisp (unlock read)") %0)) hist)))
	```

3.	**Stream Lifetime & Unlock Invariant:**
	Before releasing any lock (`lock-release-rpc`), always ensure that any open
	file stream is flushed (if writable via `stream-flush`) and cleared to
	`:nil` (e.g. `(setq stream :nil)` or `(setq res (tree-load stream) stream :nil)`).
	Setting the stream to `:nil` invokes its destructor and closes host OS file
	descriptors before the lock is relinquished.

## Pre-Public Release Tag Verification (Mandatory)

Per `CONTRIBUTIONS.md`, all of the following verification checks must be run and
pass before tagging a public release or submitting significant contributions.
All checks should be performed using standard TUI pipeline commands:

1.	**Clean Includes:**
	Ensure all `.vp` files have clean, optimal include blocks:

	```sh
	echo "files | includes" | ./run_tui.sh -f
	```

	Must output nothing (zero mismatches).

2.	**Clean Imports:**
	Ensure all `.vp`, `.inc`, and `.lisp` files use optimal relative paths:

	```sh
	echo "files | imports" | ./run_tui.sh -f
	```

	Must output nothing (zero non-optimal paths).

3.	**Forward References Check:**
	Ensure no forward references to functions or macros exist:

	```sh
	echo "files | forward" | ./run_tui.sh -f
	```

	Must output nothing (zero forward references).

4.	**Bracket Matching Check:**
	Ensure all parentheses, square brackets, and braces are matched across all
	source files using syntax-aware scanning (`cmd/brackets.lisp`):

	```sh
	echo "files | brackets -q" | ./run_tui.sh -f
	```

	Must output nothing (zero unmatched or unclosed brackets). For detailed
	metrics, run with `-v` (counts and depth) or `-v 2` (type breakdowns).

5.	**Register Clobber & Lint Analysis (`trace`):**
	Verify register clobber integrity:

	```sh
	echo "lint" | ./run_tui.sh -f
	```

	Must say `lint: clean` (zero mismatches between documented and calculated
	transitive register trashes, for every function, apps and generated
	create and type functions included).

6.	**Full Canonical Multi-Platform Rebuild (`make it`):**
	Recompile all platforms in the normal mode and regenerate reference
	documentation:

	```sh
	echo "make it | time -s" | ./run_tui.sh -f
	echo "make apps" | ./run_tui.sh -f
	```

	*Important:* `make snapshot` must only be done after a plain `make it`,
	never a validate one.

7.	**Emulator (-e) Deterministic Binary Diff Verification:**
	Verify that the emulated VP64 build produces bit-for-bit identical binaries
	to the native host build across all target platforms:

	```sh
	rsync -av --delete obj/ /tmp/chrysalisp_obj_ref/
	echo "make it | time -s" | ./run_tui.sh -e -f
	diff -r obj/ /tmp/chrysalisp_obj_ref/
	```

	Must produce zero diff output (exit code 0).

8.	**Full Functional Test Suite (Both Native and Emulator Modes):**
	Run the complete test suite in both environments:

	*	Native host (under live GUI or TUI):

		```sh
		echo "tests" | ./run.sh -f
		```

		Must report `Failed: 0`, `Skipped: 0` and `RESULT: SUCCESS`.

	*	VP64 emulator:

		```sh
		echo "tests" | ./run_tui.sh -e -f
		```

		Must report `Failed: 0`, `Skipped: 0` and `RESULT: SUCCESS`. Run it
		again after `make it validate`, then restore with `make it`.

9.	**Multi-Instance Network Link & Cluster Tests (Both Native and -e Modes):**
	Verify distributed node discovery, connection, remote task dispatch, auto-discovery,
	and cluster diagnostics under both native execution and VP64 emulation (`-e`):

	*	**TCP Loopback Test:**
		```sh
		./tests/net/test_loopback.sh
		```
		Must complete with `=== LOOPBACK TEST RESULT: SUCCESS ===`.

	*	**Cluster Diagnostic Test (Native & Emulator):**
		```sh
		# Native host:
		./run_tui.sh -f -s tests/net/test_cluster.lisp

		# VP64 emulator (-e):
		./run_tui.sh -e -f -s tests/net/test_cluster.lisp
		```
		Discovers physical LAN peers via UDP broadcast (`link -a`), dynamically
		stabilizes topology, probes all nodes across all cluster machines with 0 bad
		task counts, and reports `=== CLUSTER QUERY: SUCCESS ===`.

10.	**Host C++ Cross-Platform Compilation Check:**
	If C++ PII or driver code was modified, verify compilation across platforms.
	On macOS, use `Makefile.mingw` to verify Windows host builds:

	```sh
	make -f Makefile.mingw
	```

	Must compile the Windows host, `chrysalisp.exe`, with zero errors.

11.	**Generate Release Snapshot (`make snapshot`):**
	Only after ALL preceding pre-release tag tests have completed and are
	verified 100% clean, generate the host distribution snapshot:

	```sh
	make snapshot
	```

	*Vital Requirement:* it MUST be the normal VP64 boot image of a plain
	`make it`, **NEVER** a validate one. `obj/vp64/VP64/sys/boot_image` in
	`snapshot.zip` is run by the installation script (`make install` ->
	`./run_tui.sh -i -e -f`) to translate the platform-native boot image and
	classes for the host. The zip must stay under 192 KB, `test_snapshot`
	fails over it. `snapshot.zip` is the official distribution artifact for
	new releases and the master branch.

12.	**Verify Clean Host Installation (`make install`):**
	After `snapshot.zip` is generated, verify that a clean host `make install`
	succeeds and that the test suite passes on the freshly installed system:

	```sh
	make install
	echo "tests" | ./run_tui.sh -f
	```

	This verifies the full end-to-end user onboarding flow: cleaning the host
	objects, unzipping `snapshot.zip`, compiling host C++ executables, running
	the installer under VP64 emulation to cross-compile the host native boot
	image, and confirming that the installed environment
	passes all functional tests (`RESULT: SUCCESS`).

---

## Multi-Instance & Network Link Testing

To test network links (`service/net/link`) or multi-instance server/client
setups on a single machine, see the `chrysalisp-net-testing` skill. Key pattern:

*	Launch the server in the background without `-b`:

	```sh
	./run_tui.sh -n 1 -s path/to/server.lisp > server.log 2>&1 &
	```

*	Launch client sessions with `-b 10`:

	```sh
	./run_tui.sh -b 10 -n 1 -s path/to/client.lisp
	```

	(which skips `./stop.sh`, keeping the background server alive).

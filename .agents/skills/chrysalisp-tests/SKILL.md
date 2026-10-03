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
	differs between the native and emulator images. Also `lisp -r` snippets,
	the standard sanity tools, and how piped execution works.

*	**[Standard TUI Make Commands](#standard-tui-make-commands)**
	Every `make` option, which build each produces, and when a debug or
	validate build is needed.

*	**[Suite Layout](#suite-layout)**
	Which file is the command, the framework, and each test module.

*	**[Writing Suite Modules](#writing-suite-modules)**
	Read before adding a test to the suite: the assert macros, error tests,
	table tests with `test-cases`, and how a module is found and isolated.

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

*	The modules under `tests/system/` share the one lock service and wait on
	timers, so they always run one at a time, after the rest. A module
	anywhere else must not rely on what another leaves behind, and must
	give any file or service it makes a name of its own.

*	`tests -f` records stack frames, with `lib/debug/frames.inc`, in every
	function a module defines or imports. An error then says what was
	running, where it normally says `Frame: :nil`:

		Frame: ("zz-outer -> tests/x/test_y.lisp(3)" "zz-inner -> tests/x/test_y.lisp(1)")

	Use it with `-m` to chase a module that fails to run to the end. It is
	slower, so is not the default.

How many nodes the suite runs on matters too. `./run_tui.sh -f` boots the
default network of several nodes, `-n 1` just one. Run both. Services and
child tasks land on other nodes on a multi node network, so message timing
and routing faults only show there. A test that waits for something that
should happen must give it plenty of time, it returns as soon as it does,
and use a short wait only for what should not happen.

Which boot image the suite runs on matters:

*	**Native, `./run_tui.sh -f`:** the native image has the error checks
	built in, so every test runs, including the error tests.

*	**Emulated VP64, `./run_tui.sh -e -f`:** `make it` builds the VP64 image
	in release mode, with the error checks stripped out. The error tests
	can not run there, so are counted as skipped, and the summary says so.
	Run this when a change touches the VM or any VP level code.

*	**Full system, `./run.sh -f`:** the same suite with the GUI service up,
	and a TUI attached to the host.

To run every test on the emulator too, build all the boot images with the
checks in, `make it debug`, or with the extra runtime validation as well,
`make it validate`. A validate build also fills every newly allocated heap
cell with a pattern, bytes of `0xA5`, so code that reads memory before
setting it fails the same way each time, rather than by luck. It fills each
freed cell with a second pattern, bytes of `0x5A`, and aborts on a double
free. It checks the count on every object ref and deref, and reports
`Dead object !` with a stack dump, and checks each `:sys_mem :free` is of
a block that was given out, `Bad free !`. It keeps a guard word after
every heap cell and tests it on free, `Heap overrun !`. A fault address or pc of `0x5a5a5a5a5a5a5a5a` means use after free.
If a validate image will not boot, `make install` puts back the working
VP64 image from `snapshot.zip`, and `./run_tui.sh -e` can build from it.
The emulator
run then reports `Skipped: 0`. Always finish
with a plain `make it`. That puts back the release VP64 image, which is what
goes into `snapshot.zip` and what `make install` runs on. It is release on
purpose: installed code should have no errors to check for, and it installs
about 20% faster.

	echo "make it debug" | ./run_tui.sh -f
	echo "tests" | ./run_tui.sh -e -f
	echo "make it" | ./run_tui.sh -f

Inside an interactive TUI or Terminal session just type `tests`.

A full run takes a couple of seconds on native, and about 15 on the emulator.
Guard a run with a time limit of seconds, not minutes, and treat hitting it
as a hang or crash to investigate.

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

### Standard Sanity Tools

The standard sanity test tools from the TUI are `includes`, `forward`,
`brackets` and `trace`:

```sh
echo "files | includes" | ./run_tui.sh -f
echo "files | forward" | ./run_tui.sh -f
echo "files | brackets -q" | ./run_tui.sh -f
```

`trace` must only be run on debug VP objects: `make vp` for the system, and
`make apps debug` for the `apps/` VP functions, which `make vp` does not
build. Running it against release objects reports spurious mismatches, and
`trace -w` would then write wrong trashes into the source. Always follow
with `make it` and `make apps` to restore the release images (see the
pre-release steps below).

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

	Recompiles all supported target platforms: native platforms (`AMD64`,
	`WIN64`, `ARM64`, `RISCV64`, `LA64`) are built in debug mode (`*build_mode* =
	1`), while `VP64` is specifically built in release mode (`*build_mode* = 0`).
	Also regenerates Markdown documentation in `docs/reference/`.

*	**Debug VP64 Build for Register Analysis:**

	```sh
	echo "make vp" | ./run_tui.sh -f
	```

	Builds the VP64 image in debug mode (`*build_mode* = 1`). Used with `trace`
	(`cmd/trace.lisp`) to perform register usage and clobber analysis, along
	with `make apps debug` for the `apps/` VP functions:

	```sh
	echo "make apps debug" | ./run_tui.sh -f
	echo "files obj/vp/ | trace -i -l" | ./run_tui.sh -f
	```

	Every function is linted, no filtering is needed. The generated
	`class/x/create` and `class/x/type` functions are documented by the
	header comment under their `(gen-create :x)` and `(gen-type :x)` calls.
	`trace -i -l -w` writes the calculated trashes back to the source on a
	mismatch.

	*Important:* The debug VP64 boot image produced by `make vp` must NEVER be
	packaged into `snapshot.zip`. The VP64 image in `snapshot.zip` is executed
	by the installer (`make install`) to cross-compile the host boot image, so it
	requires the full speed of a release build. Always run `make it` afterwards
	to restore the canonical release VP64 boot image (`*build_mode* = 0`).

*	**App Rebuild:**

	```sh
	echo "make apps" | ./run_tui.sh -f
	```

*	**Documentation Generation:**

	```sh
	echo "make docs" | ./run_tui.sh -f
	```

*	**Build Modes & Verbosity:**

	`make` accepts `release`, `debug`, `validate`, `test`, `platforms`, and
	`-v <num>` (verbosity). For example: `make -v 1 it`.

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
	the argument checks at the VP to Lisp boundary are tested. It can only
	be judged on an error checked build. On a release build the form is not
	run, as it would be undefined behaviour, and the test is counted as
	skipped.

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
Do not test what wrong code does on a release build, that is undefined, test
with `assert-error` that the checked build catches it.

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

*	**catch/throw are debug-only:** They are compiled out in `release` mode and
	exist for testing. To throw an error use `(throw "Description !" obj)`, or
	`:nil` if there is no object of interest. Do NOT use `catch` or `throw` in
	runtime ChrysaLisp code.

*	**Signals and error checks are debug-only:** The abort signal
	(`(. pipe :abort)`, Ctrl-C) only wakes a blocked task in a debug build,
	and the reader's "missing )" / "unexpected )" checks are compiled out
	of a release build, where an unbalanced form is undefined behaviour.
	`tests/system/test_pipe.lisp` detects a release image and counts both
	as skipped with `(test-skip)`.

*	**Child tasks may be on another node:** A `Pipe` child can be placed on
	any node, so `(mail-validate id)` cannot be used to check it is alive.
	Have the child report to a reply mailbox instead, as `test_pipe.lisp`
	does.

*	**Optional features:** If a feature may not be defined, check `(def? 'name)`
	and call `(test-skip name "not defined")` rather than failing the suite.

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
	(pipe-run "echo data | save tmp_test.txt" (lambda (_) :nil))
	(pipe-run "cat tmp_test.txt" (lambda (_) :nil))
	(pipe-run "rm tmp_test.txt" (lambda (_) :nil))

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
	Build the debug VP64 emulator image and verify register clobber integrity:

	```sh
	echo "make vp" | ./run_tui.sh -f
	echo "make apps debug" | ./run_tui.sh -f
	echo "files obj/vp/ | trace -i -l" | ./run_tui.sh -f
	```

	Must output nothing (zero mismatches between documented and calculated
	transitive register trashes, for every function, apps and generated
	create and type functions included).

6.	**Full Canonical Multi-Platform Rebuild (`make it`):**
	Recompile all platforms (native platforms in debug mode, VP64 in release
	mode `*build_mode* = 0`) and regenerate reference documentation:

	```sh
	echo "make it | time -s" | ./run_tui.sh -f
	echo "make apps" | ./run_tui.sh -f
	```

	*Important:* `make snapshot` must only be done after `make it` (never after
	`make vp`), so `snapshot.zip` contains the release VP64 boot image.

7.	**Emulator (-e) Deterministic Binary Diff Verification:**
	Verify that the emulated VP64 build produces bit-for-bit identical binaries
	to the native host build across all target platforms:

	```sh
	rsync -av --delete obj/ ../ChrysaLisp_copy/obj/
	echo "make it | time -s" | ./run_tui.sh -e -f
	diff -r obj/ ../ChrysaLisp_copy/obj/
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

		Must report `Failed: 0` and `RESULT: SUCCESS`. The emulator runs the
		release VP64 image, which has no error checks or signals, so the
		error tests are counted as `Skipped`, and the summary says why. To
		run them under the emulator build every image with the checks in,
		`make it debug`, and again with `make it validate`. Both must give
		`Skipped: 0`. Then restore the release image with `make it`.

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

	Must compile Windows `main_gui.exe` and `main_tui.exe` with zero errors.

11.	**Generate Release Snapshot (`make snapshot`):**
	Only after ALL preceding pre-release tag tests have completed and are
	verified 100% clean, generate the host distribution snapshot:

	```sh
	make snapshot
	```

	*Vital Requirement:* It MUST be the canonical release version of the VP64
	boot image produced by `make it` (`*build_mode* = 0`) that goes into
	`snapshot.zip`, **NEVER** the debug version produced by `make vp`
	(`*build_mode* = 1`). The reason is that `obj/vp64/VP64/sys/boot_image` in
	`snapshot.zip` is executed by the installation script (`make install` ->
	`./run_tui.sh -i -e -f`) to cross-compile the platform-native boot image and
	classes for the host. Having the release build in `snapshot.zip` ensures the
	installation process runs at maximum release speed without debug checking
	overhead. Because `make vp` was run in Step 4 for trace linting, `make it`
	in Step 5 restored the release image (`obj/vp64/VP64/sys/boot_image`,
	~150 KB). Always confirm the VP64 image is the release build before running
	`make snapshot`. `snapshot.zip` is the official distribution artifact for
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
	image at release speed, and confirming that the installed environment
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

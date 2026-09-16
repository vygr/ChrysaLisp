---
name: chrysalisp-tests
display-name: ChrysaLisp Tests
description: Use when running or writing ChrysaLisp tests — the tests/ suite, run_all.lisp harness, assert macros, and standalone test scripts.
---

# ChrysaLisp Tests Skill

ChrysaLisp has a comprehensive functional test suite in `tests/`. The
general ChrysaLisp disciplines (see the `chrysalisp` skill, `LLM.md`, and
`docs/ai_digest/`) apply to test code as well.

## Running the Test Suite

The standard, preferred way to run tests is through the TUI shell via the
`tests` command (`cmd/tests.lisp`). This executes the suite inside the full
operating system environment, with all standard libraries, background nodes,
and system services initialized.

From the host shell or automated tool invocations, feed the `tests` command to
Node 0 via piped stdin:

*	**Full System Mode (with GUI services):**

	```sh
	echo "tests" | ./run.sh -f
	```

	Launches Node 0 in the foreground with the `"Gui"` service active and a live
	TUI shell running "under the hood" of the GUI window.

*	**Headless TUI Mode:**

	```sh
	echo "tests" | ./run_tui.sh -f
	```

	Launches pure TUI on Node 0 with background VP nodes, without opening a GUI
	window. (Alternatively: `echo "tests" | ./run.sh -g 0 -f`).

*	**Interactive TUI Session:**

	Inside any running TUI or Terminal session, simply run:

	```lisp
	tests
	```

*	**Filtering Output for Quick Diagnosis:**

	To focus directly on failures, skips, or summary statistics:

	```sh
	echo "tests" | ./run.sh -f 2>&1 \
		| grep -E "\[FAIL\]|\[SKIP\]|Passed:|Failed:|RESULT"
	```

*	**Emulated VP64 Mode (`-e`):**

	When verifying VM or VP-level changes across architectures:

	```sh
	echo "tests" | ./run_tui.sh -e -f
	```

*	**Legacy Raw Script Fallback:**

	If diagnosing low-level boot or startup issues before the TUI initializes:

	```sh
	./run_tui.sh -n 1 -f -s tests/run_all.lisp
	```

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

*	**Canonical Multi-Platform Release Rebuild (`make it`):**

	```sh
	echo "make it | time -s" | ./run_tui.sh -f
	```

	Recompiles all supported target platforms (`AMD64`, `WIN64`, `ARM64`,
	`RISCV64`, `LA64`, `VP64`) in release mode (`*build_mode* = 0`) and
	regenerates Markdown documentation in `docs/reference/`.

*	**Debug VP64 Build for Register Analysis:**

	```sh
	echo "make vp" | ./run_tui.sh -f
	```

	Builds the VP64 image in debug mode (`*build_mode* = 1`). Used with `trace`
	(`cmd/trace.lisp`) to perform register usage and clobber analysis:

	```sh
	echo "files obj/vp/ | grep -v apps/ | grep -v /create | grep -v /type | trace -i -l" | ./run_tui.sh -f
	```

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

*	`cmd/tests.lisp` — standard TUI command wrapper. Defines options, sets up
	stdio streams, and invokes `(run-suite)`.

*	`tests/run_all.lisp` — the test harness entry point. It imports
	`./utils.inc`, defines `(run-suite)` (which resets `*test_passes*` and
	`*test_failures*` to 0), imports every test module organized by category,
	and calls `(print-summary)`. When executed directly without `options`
	defined, an outer safety block wraps the run in a `catch` and shuts down
	Node 0 via `(pii-exit)`.

*	`tests/utils.inc` — the shared harness: global counters `*test_passes*` /
	`*test_failures*`, `(report-header section_name)`, the assert macros, and
	`(print-summary)`.

*	Category folders — `core/`, `math/`, `collections/`, `sequences/`, `text/`,
	`streams/`, `system/`, `net/`. Each module is a `test_<topic>.lisp` file of
	top-level assertions executed when imported by `run_all.lisp`.

Keep any test scripts and associated files in the `tests/` folder. Never place
temporary test scripts, scratch directories, or test output in the project root;
use `tests/scratch/` and always clean it up after being done with it.

## Writing Suite Modules

A suite module is top-level code, not a function. It starts with a
section header and then interleaves setup with assertions:

	(report-header "My Feature")

	(defq x (my-function 10))
	(assert-eq "basic" 20 x)

No imports are needed — `utils.inc` is already loaded by `run_all.lisp`.
Register a new module in `tests/run_all.lisp` under the matching category
comment.

Assert macros (all take a short human-readable name first):

*	`(assert-eq name expected form)` — strict `eql` equality.

*	`(assert-true name form)` — truthiness check.

*	`(assert-list-eq name expected form)` — deep equality via string
	comparison (`equal?`), for nested structures.

Conventions:

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

*	**Optional features:** If a feature may not be defined, check `(def? 'name)`
	and print `[SKIP] name not defined` rather than failing the suite.

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

## Verification Requirements

Per `CONTRIBUTIONS.md`, changes must be verified before submission:

*	The full suite passes with `RESULT: SUCCESS` and zero failures:

	```sh
	echo "tests" | ./run.sh -f
	```

*	All existing functionality, particularly GUI desktop and demo applications,
	continues to run correctly after your changes.

*	Changes to the C++ Platform Implementation Interface (PII) or other
	platform-specific code must be tested on all supported platforms. On macOS,
	`Makefile.mingw` cross-compiles to verify Windows host changes.

*	Build reproducibility: `make it` is the gold standard for build integrity,
	and repeated builds must produce bit-for-bit identical files in `obj/`,
	including in emulated VP64 mode (`-e`).

## Binary-to-Binary Diff Verification

Once a known good build is working, ensure all platforms are compiled in the
canonical release mode produced by `make it` (matching `snapshot.zip`, not the
debug mode produced by `make vp`), then sync `obj/` to `../ChrysaLisp_copy/obj/`:

	rsync -av --delete obj/ ../ChrysaLisp_copy/obj/

Later, after making source changes and rebuilding with `make it`, use host
recursive binary comparison to inspect exactly which object binaries changed:

	diff -r obj/ ../ChrysaLisp_copy/obj/

This avoids spurious diffs from debug builds and provides an exact,
authoritative picture of legitimate binary modifications across platforms.

### Emulator (-e) Consistency Check

This binary comparison is also vital for verifying that the emulated VP64 build
(`-e`) produces bit-for-bit identical binaries to the native build:

1.	Build all platforms natively with `make it`:

	```sh
	echo "make it" | ./run_tui.sh -f
	```

2.	Sync `obj/` to `../ChrysaLisp_copy/obj/`:

	```sh
	rsync -av --delete obj/ ../ChrysaLisp_copy/obj/
	```

3.	Rebuild all platforms under the VP64 emulator:

	```sh
	echo "make it" | ./run_tui.sh -e -f
	```

4.	Run host binary diff:

	```sh
	diff -r obj/ ../ChrysaLisp_copy/obj/
	```

A clean diff (zero output / exit code 0) proves deterministic compilation and
guarantees that the VP64 emulator environment produces identical machine code
and data structures to the native host.

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

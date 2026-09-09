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

The standard way to run tests is always the full suite:

	./run_tui.sh -n 1 -f -s tests/run_all.lisp

This launches the system on a single VP node, in the foreground, and runs
the raw `tests/run_all.lisp` script. From inside a running TUI/Terminal
app, the `tests` command (`cmd/tests.lisp`) does the same thing.

If you are not changing the base VM or any VP level system files — which
most app coding should NOT be doing — there is no need to rebuild the
system from scratch. Running tests via this launcher is what you should be
doing.

Useful `run_tui.sh` flags: `-n cnt` number of nodes, `-f` foreground mode,
`-s script_name` run a raw script, `-e` emulated VP64 mode.

When you change the base VM or any VP level system files, verify the suite
in BOTH modes — `-e` mode must behave identically to native, only slower:

	./run_tui.sh -e -n 1 -f -s tests/run_all.lisp

Note on `-e` mode and build modes:

*	By default, the VP64 image run by `-e` is built in release mode
	(`*build_mode* = 0`). Debug error checking and safety blocks
	(`(errorcase ...)`, `errorif-lisp-args-len`) are stripped out for
	performance, so tests should not rely on safety throws on invalid calls.

*	Running `make vp` from within ChrysaLisp builds a debug version of the
	VP64 image (`*build_mode* = 1`). This debug image can be used with the
	`trace` command (`cmd/trace.lisp`) to perform register usage analysis!

The build toolchain itself is VP code, so every platform verifies and adds
support to all the other platforms: any running system — or the VP64 EMU on
any host — can cross-build boot images for other targets. If you break the
native build, `-e` remains a working fallback to run and diagnose from.
Rebuild the native boot image after VP changes with
`tests/build/test_build.lisp` (it uses the EMU build to generate the native
image, so it is unaffected by a broken native version).

Each assertion prints one line: `[PASS] name` or
`[FAIL] name | Expected: ... | Got: ...`. The run ends with a summary
block. To focus on problems, filter the output:

	./run_tui.sh -n 1 -f -s tests/run_all.lisp 2>&1 \
		| grep -E "\[FAIL\]|\[SKIP\]|Passed:|Failed:|RESULT"

A clean run shows no `[FAIL]` lines and ends with `RESULT: SUCCESS`.
Optional features that are not defined print `[SKIP] name not defined`
instead of failing (see `tests/system/test_system.lisp`).

## Suite Layout

*	`tests/run_all.lisp` — the entry point. It imports `./utils.inc`,
	defines `(run-suite)` which imports every test module organized by
	category, then calls `(print-summary)`. An outer safety block wraps
	the run in a `catch` and shuts down the VP node with
	`(ffi "service/gui/lisp_deinit")`; it is skipped when launched from
	the Terminal app, where `options` is already defined.

*	`tests/utils.inc` — the shared harness: global counters
	`*test_passes*` / `*test_failures*`, `(report-header section_name)`,
	the assert macros, and `(print-summary)`.

*	Category folders — `core/`, `math/`, `collections/`, `sequences/`,
	`text/`, `streams/`, `system/`. Each module is a `test_<topic>.lisp`
	file of top-level code that runs when imported by `run_all.lisp`.

*	Standalone scripts — e.g. `tests/build/test_build.lisp` for native
	boot image builds (`make all boot`), `tests/build/test_it.lisp` for
	full rebuilds (`make it`), and `tests/build/test_trace.lisp` for
	`make vp` and register clobber tracing (`trace -i -l`). These are
	self-contained: they import what they need, do their work, and shut
	down the node at the end.

Keep any test scripts and associated files in the `tests/` folder.

## Writing Suite Modules

A suite module is top-level code, not a function. It starts with a
section header and then interleaves setup with assertions:

	(report-header "My Feature")

	(defq x (my-function 10))
	(assert-eq "basic" 20 x)

No imports are needed — `utils.inc` is already loaded by
`run_all.lisp`. Register a new module in `tests/run_all.lisp` under the
matching category comment.

Assert macros (all take a short human-readable name first):

*	`(assert-eq name expected form)` — strict `eql` equality.

*	`(assert-true name form)` — truthiness check.

*	`(assert-list-eq name expected form)` — deep equality via string
	comparison (`equal?`), for nested structures.

Conventions:

*	**Temp files:** Use `tmp_*.txt` names, and clean up with
	`(pii-remove file)`. Null out the stream variable first —
	`(defq fs :nil)` — to release the OS file descriptor before removal.

*	**catch/throw are debug-only:** They are compiled out in `release`
	mode and exist for testing. To throw an error use
	`(throw "Description !" obj)`, or `:nil` if there is no object of
	interest. Do NOT use `catch` or `throw` in runtime ChrysaLisp code.

*	**Optional features:** If a feature may not be defined, check
	`(def? 'name)` and print `[SKIP] name not defined` rather than
	failing the suite.

## Writing Standalone Test Scripts

For one-off scripts (build checks, cmd app verification), keep the script
in `tests/`, make it self-contained, wrap the work in a `catch` block for
robust error reporting, and end with the host shutdown call:

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
	((ffi "service/gui/lisp_deinit"))

To run a `cmd/` app from a raw script, wrap it in
`(pipe-run command_line)` from the `(import "lib/task/pipe.inc")`
library.

## Verification Requirements

Per `CONTRIBUTIONS.md`, changes must be verified before submission:

*	The full suite passes with `RESULT: SUCCESS` and zero failures.

*	All existing functionality, particularly the GUI demo applications,
	continues to run correctly after your changes.

*	Changes to the C++ Platform Implementation Interface (PII) or other
	platform-specific code must be tested on all supported platforms. On
	macOS, `Makefile.mingw` cross-compiles to verify Windows host changes.

*	Build reproducibility: `make it` is the gold standard for build
	integrity, and repeated builds must produce bit-for-bit identical
	files in `obj/`, including in emulated VP64 mode (`-e`).

## Binary-to-Binary Diff Verification

Once a known good build is working, ensure all platforms are compiled in the
canonical release mode produced by `make it` (matching `snapshot.zip`, not the
debug mode produced by `make vp`), then snapshot `obj/` to
`../ChrysaLisp_copy/`:

	cp -r obj ../ChrysaLisp_copy/

Later, after making source changes and rebuilding with `make it`, use recursive
binary comparison to inspect exactly which object binaries changed:

	diff -rq obj ../ChrysaLisp_copy/obj

This avoids spurious diffs from debug builds and provides an exact,
authoritative picture of legitimate binary modifications across platforms.

### Emulator (-e) Consistency Check

This binary comparison is also vital for verifying that the emulated VP64 build
(`-e`) produces bit-for-bit identical binaries to the native build:

1.	Build all platforms natively with `make it` (`tests/build/test_it.lisp`).

2.	Snapshot `obj/` to `../ChrysaLisp_copy/`: `cp -r obj ../ChrysaLisp_copy/`.

3.	Rebuild all platforms under the VP64 emulator:
	`./run_tui.sh -e -f -s tests/build/test_it.lisp`.

4.	Run `diff -rq obj ../ChrysaLisp_copy/obj`.

A clean diff (zero differences) proves deterministic compilation and guarantees
that the VP64 emulator environment produces identical machine code and data
structures to the native host.

---

## Multi-Instance & Network Link Testing

To test network links (`service/net/link`) or multi-instance server/client setups on a single machine, see the `chrysalisp-net-testing` skill.
Key pattern:
- Launch the server in the background without `-b`: `./run_tui.sh -n 1 -s path/to/server.lisp > server.log 2>&1 &`
- Launch client sessions with `-b 10`: `./run_tui.sh -b 10 -n 1 -s path/to/client.lisp` (which skips `./stop.sh`, keeping the background server alive).


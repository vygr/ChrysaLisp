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
	(`cmd/trace.lisp`) to perform register usage and clobber analysis:

	```sh
	echo "files obj/vp/ | grep -v apps/ | grep -v /create | grep -v /type | trace -i -l" | ./run_tui.sh -f
	```

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

4.	**Register Clobber & Lint Analysis (`trace`):**
	Build the debug VP64 emulator image and verify register clobber integrity:

	```sh
	echo "make vp" | ./run_tui.sh -f
	echo "files obj/vp/ | grep -v apps/ | grep -v /create | grep -v /type | trace -i -l" | ./run_tui.sh -f
	```

	Must output nothing (zero mismatches between documented and calculated
	transitive register trashes).

5.	**Full Canonical Multi-Platform Rebuild (`make it`):**
	Recompile all platforms (native platforms in debug mode, VP64 in release
	mode `*build_mode* = 0`) and regenerate reference documentation:

	```sh
	echo "make it | time -s" | ./run_tui.sh -f
	```

	*Important:* `make snapshot` must only be done after `make it` (never after
	`make vp`), so `snapshot.zip` contains the release VP64 boot image.

6.	**Emulator (-e) Deterministic Binary Diff Verification:**
	Verify that the emulated VP64 build produces bit-for-bit identical binaries
	to the native host build across all target platforms:

	```sh
	rsync -av --delete obj/ ../ChrysaLisp_copy/obj/
	echo "make it | time -s" | ./run_tui.sh -e -f
	diff -r obj/ ../ChrysaLisp_copy/obj/
	```

	Must produce zero diff output (exit code 0).

7.	**Full Functional Test Suite (Both Native and Emulator Modes):**
	Run the complete test suite in both environments:

	*	Native host (under live GUI or TUI):

		```sh
		echo "tests" | ./run.sh -f
		```

		Must report `Passed: 1544, Failed: 0, RESULT: SUCCESS`.

	*	VP64 emulator:

		```sh
		echo "tests" | ./run_tui.sh -e -f
		```

		Must report `Passed: 1544, Failed: 0, RESULT: SUCCESS`.

8.	**Multi-Instance Network Link & Cluster Tests (Both Native and -e Modes):**
	Verify distributed node discovery, connection, remote task dispatch, auto-discovery,
	and cluster diagnostics under both native execution and VP64 emulation (`-e`):

	*	**TCP Loopback Test:**
		```sh
		./tests/net/test_loopback.sh
		```
		Must complete with `=== LOOPBACK TEST RESULT: SUCCESS ===`.

	*	**Auto-Discovery Test:**
		```sh
		./tests/net/test_disco.sh
		```
		Must complete with `=== AUTO-DISCOVERY TEST RESULT: SUCCESS ===`.

	*	**Cluster Diagnostic Test (Native & Emulator):**
		```sh
		# Native host:
		./run_tui.sh -f -s tests/net/test_cluster.lisp

		# VP64 emulator (-e):
		./run_tui.sh -e -f -s tests/net/test_cluster.lisp
		```
		Must probe all nodes across all cluster machines with 0 bad task counts and report `=== CLUSTER QUERY: SUCCESS ===`.

9.	**Host C++ Cross-Platform Compilation Check:**
	If C++ PII or driver code was modified, verify compilation across platforms.
	On macOS, use `Makefile.mingw` to verify Windows host builds:

	```sh
	make -f Makefile.mingw
	```

	Must compile Windows `main_gui.exe` and `main_tui.exe` with zero errors.

10.	**Generate Release Snapshot (`make snapshot`):**
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

11.	**Verify Clean Host Installation (`make install`):**
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

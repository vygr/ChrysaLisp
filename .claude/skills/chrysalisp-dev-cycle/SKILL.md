---
name: chrysalisp-dev-cycle
display-name: ChrysaLisp Dev Cycle
description: Use at the start of a working session on ChrysaLisp, and whenever a change has to be built, tested on more than one machine, shown to the user on a desktop, or committed — bringing the network of machines up, sync, the test harness, the lint, a GUI session, and taking it all down.
---

# ChrysaLisp Dev Cycle Skill

How a change gets from an edit to a commit: the machines are brought up as
one mesh, made the same, each runs the tests its own cache says are stale,
the user looks at what is for the eye, and it is committed. All of it is
done by ChrysaLisp, with commands in this repo. The other skills say how to
write code and tests, this one says how to run the loop.

## Contents

Find the task below and read that section in full before acting. Sections
marked mandatory apply to every session.

*	**[The Parts, And Where They Are](#the-parts-and-where-they-are)**
	The repo commands the loop is made of, and the docs for each.

*	**[Work As The Test User (Mandatory)](#work-as-the-test-user-mandatory)**
	Every session of an agent's is started as Test, not as the user.

*	**[Start Of A Session (Mandatory)](#start-of-a-session-mandatory)**
	What to read and what to bring up before any work.

*	**[Bring The Network Up And Down](#bring-the-network-up-and-down)**
	A member on each machine, `./rack.sh`, and how to check the mesh.

*	**[Who Takes A Sync (Mandatory)](#who-takes-a-sync-mandatory)**
	Others' machines in the mesh, the rig's `sync_from`, the herd, and
	`rack -o`. Read before any `rack` run when the mesh is not all the
	user's.

*	**[Sync And Test](#sync-and-test)**
	`rack`, the test cache, and what to run after which kind of change.

*	**[After A VP Change](#after-a-vp-change)**
	The build and the lint.

*	**[A Node That Dies With No Word](#a-node-that-dies-with-no-word)**
	A validate build first, the emulator overnight, and a fault handler in
	it to name the Lisp that was running.

*	**[A Desktop For The User](#a-desktop-for-the-user)**
	Bringing a GUI up in the mesh, and what not to do while it is up.

*	**[Commit, Push And Level](#commit-push-and-level)**
	Who commits, who pushes, and bringing the machines level after.

*	**[A Session Of Your Own That Hangs](#a-session-of-your-own-that-hangs)**
	Running a throwaway session safely, and clearing up one that sticks.

*	**[Rules That Were Learned The Hard Way (Mandatory)](#rules-that-were-learned-the-hard-way-mandatory)**
	Short, each one cost something.

*	**[Helper Scripts Outside The Repo](#helper-scripts-outside-the-repo)**
	What a set of wrapper scripts for several machines does, to make again.

## The Parts, And Where They Are

*	`./rack.sh up|down|status|run 'cmd'`: this machine's member of the mesh,
	one node that stays up. `docs/ai_digest/rack.md`.

*	`rack [-b] [-o ids] [-x machines] [-l machines] [-d paths] "cmd"`,
	`cmd/rack.lisp`: make every other machine's tree the same as this one's,
	then have each machine run a command line in a fresh session of its
	own, sized to itself. `-o` names the only machines to touch, by the
	start of each system id: your rig, when others' machines are in the
	mesh. `-x` names platforms never to touch.

*	`tests`, `cmd/tests.lisp`: the unit tests, as a cache of results, only
	what a change made stale is run. `tests -a` runs all, `tests -s` lists
	what is stale, `tests -m str` the modules with `str` in their path.
	`docs/ai_digest/test_cache.md`, and the `chrysalisp-tests` skill.

*	`lint`, `cmd/lint.lisp`: `make` and `make apps`, the `trace` lint of
	`obj/vp/`, then `make trace validate`, the VP of a validate build
	alone, and the same lint of `obj/vp_validate/`: a `(validatecase)`
	must use no more registers. Then `relies`. It must say `lint: clean`.

*	`relies`, `locals`, `cmd/`: what VP code leans on and what its locals
	cost. `relies -s` finds a register saved round a call that leaves it.

*	`bench`, `cmd/bench.lisp`: the benchmarks, `tests/bench/bench_*.lisp`.
	`bench -s file` before a change, `bench -c file` after. Not tests. On
	a quiet machine, one node: a few percent of a few ns is as likely the
	machine as the code, and one size of data is not enough.

*	`mesh`, `cmd/mesh.lisp`: who is in the mesh, and why a machine is not.
	`mesh -j` joins, `mesh -k` makes a key. `docs/intro/mesh.md`.

*	`sync`, `cmd/sync.lisp`: is another machine's tree the same as this
	one's, and send what differs. A machine takes a sync only from itself,
	from those named in its `sync_from`, and by the herd at waypoints, see
	Who Takes A Sync below. `docs/ai_digest/sync.md`.

*	`waypoint`, `cmd/waypoint.lisp`: after a `git pull`, note the commit the
	tree is at, so it takes part in the herd. `waypoint -c` says where it is.

*	`nodes`, `cmd/nodes.lisp`: add nodes, a desktop, or a network of a shape
	to a running one, and stop a network by name.

*	One host program, `obj/<cpu>/<abi>/<os>/chrysalisp`, `chrysalisp.exe`
	on Windows: find nodes by `pgrep -x chrysalisp`. SDL3 is loaded as it
	starts, not linked: with none it is a terminal's node, `(gui-info)` is
	`(0 0 0 0)`, and `CL_SDL3=/nonexistent` tries that path. `GUI=none`
	builds a host with no drivers at all.

*	`docs/releases/vX.Y.md`, the notes of the tag to come: each change
	worth a note goes in as it lands, under its heading, a line or a few,
	written for the people who read the release. Not a copy of the
	commits: what was run and what was not seen go in the commit message
	and in what is said to the user. They open with what forces an upgrade
	or an install and end with what is not ready, figures only measured.
	Only a GitHub release gets a name, the user's to give: before offering
	one, `gh release list --limit 100`, names are not used twice.

*	Two documents are kept as work lands, in the same commit:
	`docs/ai_digest/till_the_pips_squeak.md` for a change to the Lisp
	engine with a measured effect, its tables, and what was tried and did
	not work with the number that killed it, and `keeping_it_hot.md` for
	the call path; `docs/ai_digest/evidence_not_faith.md` for any measured
	figure that moves, build times, image sizes, counts. Its shape: each
	test's figures of today on each machine and a line of evidence, one
	progression table, a row a date, and a paragraph for each record.
	Every figure in it is today's, an old one only in the table, so grep
	the whole file for old counts after an update. The README's The
	Numbers has the headline figures and changes when one moves. Count
	functions from the bundles under `obj/vp/`, lines starting `("`.

## Work As The Test User (Mandatory)

An agent's sessions are not the user's, and should not write into the
user's folder, `usr/Guest/`, their app settings and the like.

*	Start every session of your own with `-u Test -a`: `echo "cmd" |
	./run_tui.sh -n 1 -u Test -a -f`, and `./run.sh -n 1 -u Test -a -f`.
	`-a` is alone: the session does not join the mesh by itself, as a
	session does. Without it, on a machine with no member up, a run of
	the tests is the machine's member, links to the other machines, and
	two machines testing at once see each other's `*` services.

*	Run the harness as `rack -u Test "tests"`.

*	A desktop brought up for the user to look at is theirs, start it with
	no `-u`, `./rack.sh run 'nodes -g 1'` from a member that has none.

*	A session started as a user's goes straight to the desktop, the login
	window is not shown, and `usr/current` is left alone. Never write
	`usr/current` to try something as another user, that switches the
	user's own desktop.

*	What a script types into a TUI is kept out of the history by itself,
	the launch script tells the terminal when its input is not a keyboard.

## Start Of A Session (Mandatory)

1.	Read the skills in `.claude/skills/`, the notes of the tag to come,
	`docs/releases/`, the newest, and `docs/ai_digest/ai_thoughts.md`.

2.	`git status` and `git log`. A clean tree at the head of `master` is the
	usual start.

3.	Look for the user's own nodes before starting or stopping anything:
	`pgrep -x chrysalisp | wc -l`. A member is one `chrysalisp`, its pid
	is in `/tmp/chrysalisp_rack.pid`. Anything more
	is the user's, or a run of yours that did not end.

4.	Bring the members up, next section.

## Bring The Network Up And Down

A machine is in the mesh while it has a member. On each machine, in the
ChrysaLisp folder:

```code
./rack.sh up
./rack.sh status
./rack.sh run mesh
./rack.sh down
```

*	`./rack.sh run mesh` lists the machines, how many nodes each has and
	what it is. Give it up to half a minute after a start, a machine that
	has just lost another does not try again at once.

*	Every machine needs the same `mesh_key` file at the root of its tree.
	It is in `.gitignore`, git and sync leave it alone.

*	A member is one node, on purpose. It builds and tests nothing. A test
	session is started fresh by `rack`, sizes itself to the machine with
	`(node-auto)`, runs, stops its own nodes and goes. So a machine showing
	"1 nodes" between runs is right.

*	A member runs the boot image and the Lisp it was started on. After a
	change to the Net service, the mesh, sync, `lib/rack/`, or a boot image
	they stand on, `./rack.sh down` and `up` on every machine.

*	`./rack.sh down` stops the member and what it started. Never
	`./stop.sh`, that is every node of the machine, the user's desktop too.

## Who Takes A Sync (Mandatory)

A shared `mesh_key` trusts a machine with everything, and the mesh may hold
machines that are not the user's: a friend's server dialled over the
internet, `mesh -t host`, with the same key. Every member runs `sync -a`,
so what decides is the Sync service, `lib/sync/sync.inc`, "Who takes a
sync". A machine takes a sync:

*	from itself;

*	from the machines named in the file `sync_from` at the root of its
	tree, a system id a line, the 32 hex digits `mesh` and `sync` show the
	start of. This is the rig: the machines the tests are run on name the
	machine the work is on. The machine being worked on names none;

*	by the herd, only when its tree is exactly a waypoint, noted by
	`waypoint` after a pull, and has no `.git`, from a machine at a newer
	one, or when it is already exactly the tree offered. A tree changed
	since is at no waypoint and takes no part, pushed to or from. A
	waypoint goes round by itself: each sync service offers its own to the
	others, `service/sync/spread.lisp`, see `docs/ai_digest/sync.md`, "The
	herd by itself". In our rig only the M4 has a `.git`; `waypoint` there,
	once Chris has pushed and the tree is exactly the commit, and the x64
	and the Pi take it;

*	from nobody else: the push is refused, `sync -t` and `rack` say so.

So:

*	Before a `rack` run, look at `./rack.sh run mesh`. If any machine is not
	the user's, name the rig with `-o`, the start of each system id with a
	`,` between, `rack -o C96A1B10,D649CA0E "tests"`. Never run `rack`
	without it then: a guest whose Sync service is older than this takes
	anything, and your tree would land on their machine.

*	A new rig machine needs a `sync_from` naming the machine the work is on.
	`sync_from`, `waypoint` and `mesh_key` are in `.gitignore` and are never
	sent.

*	After a change to the Sync service, restart it on each machine with
	`./rack.sh run 'sync -x'` then `./rack.sh run 'sync -a'`. That leaves
	every node, the user's desktop too, as it is.

*	A guest can still start a task on our nodes by mail, `open-task`. Only
	sync is guarded so far.

## Sync And Test

From the machine the work is on:

```code
./rack.sh run 'rack "tests"'
./rack.sh run 'rack -b "tests"'
./rack.sh run 'rack "tests -a"'
```

*	The first is the usual one, after any change. Each machine is made the
	same as this one, then runs what its own cache says is stale. A line a
	machine comes back, the counts and how long.

*	`-b` builds first on each, `make` then `make all boot`, in a session
	before the one that tests. Use it after a change to any `.vp` file.

*	`tests -a` ignores the cache. For a tag, and when a result is doubted.

*	`-l arm64/Darwin,` leaves a machine out, brought level and not run on.
	For a machine the user is sat at.

*	`-o ids` touches only those machines, every other is left alone, not
	brought level and not run on. The machine it is run from still runs
	the command, `-l` leaves that out. Use it whenever the mesh has a machine
	that is not the user's, Who Takes A Sync above.

*	A file deleted here is not deleted on the others unless named with
	`-d a/path:another/path`.

*	What to expect: every module on three machines in about 15 seconds, a
	couple of seconds when little is stale. Anything much longer is a
	fault, in the system or in a test that waits on a fixed time, find it.

*	On this machine alone, with no mesh: `echo "tests" | ./run_tui.sh -n 0
	-f`. `-n 0` sizes the network to the machine, `-f` ends the session
	when the command does.

*	A test that passes from `run_tui.sh` can fail from `rack`, and has. A
	rack session runs the tests on its first node, which has started nodes
	before. Run the harness before a commit, not only the one module by
	hand.

## After A VP Change

1.	`echo "make all boot" | ./run_tui.sh -n 19 -f`, it must build.

2.	`echo "lint" | ./run_tui.sh -n 0 -f`, it must say `lint: clean`. A
	mismatch in a function you changed on purpose is written back with
	`files obj/vp/ | trace -i -l -w`, and `lint` again.

3.	`rack -b "tests"` on every machine, the native code is not the same on
	each CPU. `-b` for any change under `sys/`, `class/`, `gui/`,
	`service/` or to a `lisp.vp`: without it the others test new source on
	their old boot image. A machine given VP source by hand runs `make`
	before its tests, and before the user's desktop starts there:
	`cl_sync.sh` and a rack's sync copy sources, they build nothing, and an
	`ffi` of a native not built yet throws as the app that wants it opens,
	`bn-montmul` on the x64 on 2026-11-21.

4.	A change under everything, the boot image, `class/lisp/root.inc`, the
	launch path of a command, is tried on the emulator as well, after a
	`make it`: `echo "echo hi" | ./run_tui.sh -e -n 1 -f`.

5.	A change to a translator, `lib/trans/`, or to what the VP means, runs
	`tests/system/test_vp_spec.lisp` natively, on `-e` and on `-V`, and on
	every other CPU before it is done: it holds each target to
	`docs/vm/vp_vm.md`, the sets, the float branches, constants of any size,
	the long branches.

6.	`./run_tui.sh -V` is the emulator in validation mode: each load or store
	not at its natural alignment is said on stderr, `VP64 UNALIGNED`, the
	function and where in it, once a place. It costs nothing when not
	asked for, the interpreter is built twice. Only the session's first
	node validates: a node it starts is given `-e`, never `-V`,
	`pii_spawn` of `src/host/`, so with `-n 4` three of the four check
	nothing. And `tests` with no `-a` runs nothing the cache has, a module
	that passed natively is not run again. So a category at a time on one
	node, every module: `echo "tests -a -m tests/media/" | ./run_tui.sh -V
	-n 1 -f -u Test -a`, its output to a file, grep it for UNALIGNED. All
	but `solo`, `net`, `release` and `gpu` take 35 minutes on the M4, 23 of
	them `text/test_tree`, whose own commands run out of time there and
	fail 11 checks: not a fault. A change is proved both ways, the code as
	it was says the place, the code as it is says nothing: 2026-11-22 it
	was thought clean when the module had not run at all.

*	The boot id is of every file the boot image's VP source reads, the
	list is `obj/<cpu>/<abi>/sys/boot_files`, and some of them are Lisp,
	`sys/lisp.inc` for one. A change to one of those with no `make all
	boot` after leaves this machine's id that of the old source, though
	its image is byte for byte what a new one would be. Nothing shows
	while every machine is as stale; the day one installs, by a waypoint,
	`mesh` says the rest are "on a different build" and a `rack` run
	reaches only this machine. `(boot-id)` against `(boot-id-tree)`, of
	`lib/boot/id.inc`, says if an image is behind its source. `make all
	boot` here, then the members started again together.

*	`files | includes` and `files | imports` say what they would change,
	`-w` writes it: read it without `-w` first. `make it` prints it at its
	end. If `-w` takes out what the build needs, the tool is wrong, fix
	`cmd/includes.lisp`.

*	There is a normal build and a validate build, nothing else. Never a
	bare `make validate`: it makes this platform's natives validate under a
	normal boot image and no node boots, `make install` mends it. `make
	trace validate` makes the validate VP alone, to look at.

*	A class renamed or removed leaves its outputs in `obj/vp/`,
	`obj/vp_validate/` and `obj/<cpu>/<abi>/` on every machine, and lint
	and `trace` trip on them there only: "No documentation found", or a
	`wrong_types` in `cmd/trace.lisp`. Remove the outputs whose source is
	gone.

*	A new `(ffi ...)` in `class/lisp/root.inc` stops every other machine:
	its old boot image has no such function and a session can not start
	to build. Build the native and boot first, then add the ffi. A new
	native is bound in its library's `.inc`; into `root.inc` only at a
	release, with a new snapshot. A library a node reads as it boots, the
	GUI's `gui/*/lisp.inc` say, is the same: its `(ffi ...)` of a new native
	at load stops `make all boot` on this machine too. Look it up when it
	is called, `((ffi "gui/pixmap/lisp_clear") pixmap ...)` in a `defun`,
	as `(pixmap-clear)` and the canvas's `:clear` do (2026-11-21). If it has been done, the others' sessions
	end at their start, `(ffi path [sym flags]) wrong_types` in
	`/tmp/chrysalisp_rack.log`, and `rack -b` can not help, its build is a
	session too. `make it` on the machine the work is on makes the natives
	and boot image of every platform: send `obj/<cpu>/<abi>/` to each by
	rsync, leaving out the host program's folder, `Darwin/` or `Linux/`,
	and `cache/`, `source/`, `sync_hashes` and `tests/`, then kill the
	sessions left at their start, the member's children, by pid. A boot-time ffi of a function the image
	does not hold says "Bind error" during an install: add it to
	`+boot_extras`, `lib/boot/image.inc`.

*	The translator loops till its output is the same and its passes are
	never counted: a length that depends on where a label is may only
	grow from pass to pass, or be fixed. A build that hangs in the
	assembler has a length that shrinks. After a kernel or emitter change
	`make all platforms`, a native build hides LoongArch and RISC-V
	faults. A function is at most 64KB.

*	Native code made from Lisp keeps the Lisp beside it, and a test feeds
	both the same random input and compares every number. A hot function
	moved to VP gains little if the Lisp round it does as much for each
	item: move the whole loop over the items.

*	A change on the build's own path, the assembler's farm say, must not
	make `make all`, `make all boot` or `make test` slower: time them
	before and after, several runs, on a quiet machine, and go back if it
	is.

The six CPU build, `make it`, the whole suite on the emulator, `make
install` and the QEMU platforms are for a tagged release, not for each
commit. The `chrysalisp-tests` skill has the full list for a tag.

## A Node That Dies With No Word

A node that dies with no message, a batch "no result", has run into
something the checks do not cover, a fault in VP code or memory used
after it was let go of.

1.	On the fast machine first, never a long emulator run in the day:
	`make all boot validate` on the M4 adds the heap, dead object and
	stack checks, then `make all boot` puts the normal image back.
	Emulator runs are for overnight.
2.	If it is only on the emulator, it reproduces in seconds when it is a
	crash. A SIGSEGV handler put into `vp64()` in src/host/vp64.cpp for
	the hunt, not committed, says the last pcs and every list form the VP
	stack points at: the forms name the Lisp that was running. An offset
	of memory is one of the image file plus one shift, as sys/load moves
	the image's tail; a function's name is before its code. Find the shift
	from where a name is in the image file and where it shows in a dump.
	An opcode is the low byte, the enum order of vp64.cpp, `dr` bits 8 to
	11, `sr` 12 to 15; at a call `[r15]` is the pushed pc. A virtual call
	one slot past a short vtable is a primitive given the wrong type: the
	fault is in Lisp, not vp64.cpp. Take the handler out after.
3.	A native node that dies with no word on Linux: `sysctl -w
	kernel.print-fatal-signals=1` puts the signal and registers in dmesg,
	`ulimit -c unlimited` and a `kernel.core_pattern` give a core. In gdb
	a frame of `??` is VP code: dump the memory round pc and the link
	register, the nearest `path/name` string before each names the
	function. Put both settings back after.
4.	A guarded run that times out leaves its nodes up, and they take the
	next run's batches. `pgrep -fl boot_image`, kill the VP64 and solo
	ones, keep the standing member, `chrysalisp -shm_sweep`.

## A Desktop For The User

Much of the work is for the eye, and an agent has none. The user looks.
But what a window holds, and where, an agent can read for itself, from
inside the system, a desktop on a test machine's screen that goes when it
is done:

```code
./rack.sh run 'rack -g -v -u Test -l arm64/Darwin,x86_64/Darwin "launch desktop/calculator ; widgets -w 10 -t Calculator -d 3 -k text"'
```

*	**The slow machine test.** `rack -e -n 4 -g ...` is the same hidden
	desktop run by the emulator, on the VP64 boot image, with 4 nodes: a
	fast machine about as slow as a Raspberry Pi 4, a plain loop some 19
	times slower on an M4. It is the common test of what a slow machine
	shows, before a small machine is asked: an app that holds the GUI's
	node, a picture shown part made, a wait that is seconds there. It
	does not make the GPU slow; a slow GPU's faults need the Pi's own
	desktop. `-e` and `-n` are rack's, for any session.

*	`widgets` is a view a line, type, bounds, properties; `-f type=Button`
	or `-f text=Save` for one part, `-d` and `-k` to keep it short. `grab
	-t title file` for the pixels, a picture costs far more to look at,
	small and seldom. Not ffmpeg, ssh scripts or a FIFO: rack, launch,
	widgets and grab do it.

```code
./rack.sh run 'nodes -g 1'
./rack.sh run 'nodes -a 14'
./rack.sh run mesh
```

*	The first adds a desktop to this machine's member, so it is in the
	mesh. The second adds plain nodes, to fill the machine. For the user to
	watch a small network, in the Network Map say, leave the second out.

*	Say what to open and what to look for, and say in the commit what the
	user saw, a thing is "not seen" until then.

*	**Do not change the tree under a desktop that is up.** It loaded the
	GUI and `class/lisp/root.inc` once, at its start. An app opened later
	reads its own source, `usr/<user>/env.inc` and `lib/task/pipe.inc`
	fresh from disk. A function renamed between the two is unbound, and a
	form a new pipe sends is unknown to old nodes. If a change has to be
	made, say so first, and say the desktop must be started again.

*	**What a desktop that is up will pick up, and what it will not.** An
	app opened again reads its own files under `apps/`, and what only it
	imports. It does not read again what the desktop had at its start,
	which is more than it looks: everything under `gui/` and
	`service/gui/`, `lib/consts/symbols.inc`, the fonts, the host
	program, and whatever the GUI itself imports, which includes
	`lib/cwb/doc.inc` by way of `lib/image/cwb.inc`. A function added to
	one of those is `symbol_not_bound` in an app opened on the old
	desktop. Before saying "open the app again", list what changed since
	the desktop came up, `git diff --stat` and what is not committed, and
	if any of it is on that list start a new desktop and say so. This was
	got wrong three times in one day.

*	A member that is started again is a new node to the other machines,
	and one that has been up a long time may not show its sync service
	to it: the mesh then runs on this machine alone and the test loop
	falls back to `ssh`. Start all the members again together, not one.

*	Fingers without a touch screen: `CL_TOUCH_TRACKPAD=1 ./rack.sh up`
	before the desktop is added, and each finger on the trackpad is a
	contact, the pad is the window. The pad then moves no mouse in the
	window. On a Mac the pad must not be set to be ignored while a mouse
	is there, System Settings, Accessibility, Pointer Control. It is a
	stand in: the Mac makes a wheel of two fingers and cancels a finger
	that rests, the driver undoes both, a panel does neither.

*	To see a thing yourself, draw it to a file and look: `cwb file.cwb
	-s script.lisp -o out.tga`, `sips -s format png out.tga --out
	out.png`, and read the picture. A script, not `-e`, for more than a
	line.

*	A recorder put in to watch the user's session is code like any other:
	run the path it is on with no desktop before the user is asked to,
	keep it out of every commit, and take it out when the watching is
	done. Two of them stopped the app under the user in one day.

*	The GUI service loads the GUI a file at a time in the order of what
	each file imports, not top to bottom as `(import "gui/lisp.inc")` does.
	A GUI file that needs a name as it loads must import the file that has
	it. `tests/system/test_gui_load.lisp` loads it that way and fails if a
	file does not. After a change under `gui/` that is more than that,
	start a real one before a commit: `echo "echo up" | ./run.sh -n 1 -f`,
	it must print no error. A window shows on the user's screen for a few
	seconds, tell them.

*	Nodes added to a member by hand are in the way of the test loop on
	that machine till they are taken down, `./rack.sh down` then `up`.

## Commit, Push And Level

*	Commit to `master` in batches as work is verified. The user pushes.

*	Before a commit: the harness passes on every machine, the lint is
	clean if VP changed, the release notes have the change, and any doc
	that describes what changed is brought up to it. `make docs` if a
	function's signature or a command's help changed.

*	The commit message carries what was run, on what, and what was not
	seen. It says what changed and why: no conversation, no quotes, no
	story of the session, nothing relayed from a private talk, and no file
	names of the user's own media, "a film of 1.7 GB". Docs and comments
	the same.

*	A new `snapshot.zip` comes with every change under `src/host/`, as
	Windows gets its host program only from it, and when a system can not
	make itself from the old one. Not for a boot image a few bytes
	different. `make -f Makefile.mingw windows_all`, `make vp`, `make
	snapshot`, which writes `snapshot_host`;
	`tests/system/test_snapshot.lisp` fails while `src/host/` differs from
	it. Compare `unzip -l snapshot.zip` with the last, `git show
	HEAD:snapshot.zip`, a growth is to be explained, and the Windows exe
	must have no `terminate called` in its strings. The boot image holds
	what an install needs, no more, no less.

*	A herd machine, with no `.git`, installs itself: given a newer
	waypoint its member ends and `rack.sh` runs `make install` and starts
	it again. Do not also `make install` there, or `rack.sh down` during
	it, two installs race.

*	A herd machine takes a waypoint only if its tree is exactly one. A
	`rack` run made while there was work not committed, a look at a
	hidden desktop say, has sent it that work, so after the push it is
	at no waypoint and does not take the new one: `./rack.sh run sync`
	says it differs. Bring it level with `~/.ssh/cl_sync.sh`, or a
	`rack` run, and it then installs by itself. Its member ends for the
	install, so a `rack` job that was waiting on its session never
	hears, and the member here stays at that job: end the `rack.sh run`
	by pid and restart this machine's member, then ask `sync` again.

*	After a push the other machines are brought level with
	`origin/master` by git, on each: fetch, remove untracked files that
	the push now tracks, they were put there by a sync, and check out.
	Then the harness once more.

## One Machine While Working, All At A Milestone

The user, 2026-10-13: "We can proably test this only on M4 for a lot of the
changes ? ... We can intermitently run the full set of machines, so we don't
run too far without a good known milestone."

*	While working a change, build and test on the machine the work is on.
	A change that is the same code on every machine, Lisp, shaders, data,
	docs, is tested there and synced. All the machines are for what can
	differ by machine: VP and the code generators, the boot image, the
	host, timing, speed or a GPU. A change that could stop the others
	building or booting is tried on them at once.

*	A crash is chased natively on the fast machine first, a validate build
	if it needs the checks; long emulator runs go overnight. Runs on the
	other machines go in the background, work goes on once the main one
	passes, and what they find is fixed as its own change.

*	Visible work: get it running, let the user look, then the long gate.

*	All the machines, and a soak, at a milestone: before a commit that
	touches the kernel, the emitters, the translators or the host program,
	and after a batch of commits. Say which machines have seen a thing
	when it is reported.

*	Overnight, a validate run on the fastest machine: `make all boot
	validate`, `tests -a`, then `make all boot` to put the usual image
	back. It fills a reused heap cell with a pattern, so a field never set
	shows every time, not only when a block happens to be reused: a
	message's src and offset went on the wire unset for years, seen only
	when a full run gave a test's mail a reused block. Heap misuse is the
	same on every CPU, one machine finds it. Not in the day.

*	After any change under `src/host/`: `make` here, the host built on the
	other machines too, and `make -f Makefile.mingw`, which builds the
	Windows host here with mingw. Where Wine is installed it can be run
	there too, `make install` and `tests -a` have been. It is the one host
	with no GUI functions, so it alone shows that kind of fault: it goes in
	the overnight set. A Wine build for macOS is at
	`github.com/Gcenx/macOS_Wine_builds`, untarred anywhere, `xattr -dr
	com.apple.quarantine` on it, a scratch `WINEPREFIX`, `WINEDEBUG=-all`.
	There is no PowerShell in it: start `chrysalisp.exe` directly, the
	install as `chrysalisp.exe obj/vp64/VP64/sys/boot_image -e -run
	"(progn (node-net :full 0) (import {apps/tui/install.lisp}))"`, a run
	as a `-run` file that `pipe-run`s the command and `(pii-exit 0)`s.
	Log to a file, never a pipe, the other nodes hold it, and kill them
	after. A pass under Wine is not a pass on Windows.
	A new host function is put at the end of the table, the host says how
	new it is in the fourth word of `(pii-host)`, and the kernel asks
	before it calls one an older host has not.

*	A change to what is sent between nodes must still be read by a kernel
	of before, or nothing can be built with it: the build starts its own
	tasks. Add to the end of a reply, do not move what is there.

## A Soak

One run that passes says little about a thing that goes wrong one time in
ten. After a batch that adds tests or tasks, or changes the host program,
the pointer handling, pipes or the mesh, run the whole suite on every
machine a dozen times or more, with no desktop up, and read every line:

```code
for i in $(seq 1 20); do <the test wrapper> -a | tail -4; done
```

*	Look at the slow machine's line first, it is the one that shows a
	race.

*	Count the notes of shared pixels before and after, `ls
	/tmp/chrysalisp_shm_* | wc -l` on each machine, they must not grow.
	`./obj/*/*/*/chrysalisp -shm_sweep` lets go of what a process that has
	gone left. A test, or an app, that stops part way leaves one.

*	A failure that comes once and not again is a fault to be found, not a
	loaded machine. Run the module alone twenty times: if it passes
	alone, something beside it holds its node, a task that does a second
	of work with nothing waited for. `(task-slice)` in it.

*	For "none since the fix", count the runs after the fix.

## Before A First Push To A Public Place

A new clone of what is to be pushed, somewhere outside the tree, `make
install` in it from the snapshot it carries, and the whole suite there. It
is what a stranger will do first. `git clone --local . <dir>`. `make
install` there stops no nodes of the machine.

## A Session Of Your Own That Hangs

*	A session ends by itself when its input does: `echo "cmd" |
	./run_tui.sh -n 1 -f`. Never follow it with a fixed sleep.

*	One command line to a session. A second line sent while the first
	command runs goes to that command's stdin, not to the terminal.

*	For a thing that may hang or take a node down, start it in the
	background with its output in a file, wait a few seconds, read the
	file, and kill what is left by its own pid:

	```code
	( echo "cmd" | ./run_tui.sh -n 1 -f > out.txt 2>&1 ) &
	```

	Read the output before saying what happened. A time limit that was hit
	is not a reproduction of anything.

*	To find a session of yours: `ps -axo pid,ppid,lstart,command | grep
	chrysalisp`. Yours has the start time of your run, and its nodes have its
	first node as their parent. Kill those pids. Do not kill by name.

*	`-s script` does not end the session when the script does, its nodes
	stay: run a script as a command of an `echo "..." | ./run_tui.sh -f`
	session. `rack ... "lisp file"` hangs at the end, `lisp` waits on its
	stdin: give rack the steps as commands.

*	A command that takes file names from stdin when given none, `tests`,
	`check`, `forward`, reads the lines after it as names and they never
	run. Give it its own session, or its paths.

*	One suite on a machine at a time, they share `tests/scratch/` and
	services. Test output can hold raw bytes, `grep` then prints nothing:
	`LC_ALL=C grep -a`.

*	Its note, `/tmp/chrysalisp_<pid>.session`, is left behind by a kill,
	remove it. A note with a `net` line in it shows as a network in
	`nodes`.

*	Scratch files go in `tests/scratch/` or outside the repo, never in the
	root, and are removed after. A command file made to test with goes in
	`cmd/` with a `tmp_` name and is removed in the same step.

## Rules That Were Learned The Hard Way (Mandatory)

*	Ask what is already there before adding a part. Most of what was built
	in this cycle is a few lines calling things that were there for years.

*	Say which of the things you report you saw run, and which you only
	reasoned about or drew. They read the same otherwise.

*	A check that compares two things shows they are the same, not that
	either is right. Compare with what was meant.

*	When a thing can be reached two ways, a test of one has to shut the
	other.

*	Test on more than one node, in more than one shape, and on the slow
	machine. One node hides routing and timing, a fast machine hides waits.

*	A node that has just gone is known of for a while. A task left to find
	a node can be sent to it and is lost. Wait for the count of nodes to
	settle before relying on it.

*	`(find str str)` finds the first character, it does not search. Use
	`(substr text pattern)` and test for empty.

*	Timeouts are in seconds. A build is a tenth of a second and the suite
	a few. Waiting minutes is waiting on a fault.

*	A GPU answer wrong now and then, in a soak and never alone, is found
	with the GPU shared: each node has a device of its own, so the test's
	body as four module files in `tests/scratch/`, looped, and `tests -a`
	of the four at once, shows in a minute what a soak shows in an hour.
	Have it print what the buffers hold a moment later: a write that
	landed after its kernel ran is a fault of order. On the Pi
	`V3D_DEBUG=sync` has the driver make every job wait for all before
	it; a fault that goes with it is the driver's order, see the GPU
	driver in `docs/ai_digest/host_interface.md`.

*	What is done by hand more than twice becomes a command or a script,
	and is then timed like one.

*	Before saying a thing can not be done here, look for the tool. The
	Windows host was called "never compiled" three times with the
	compiler installed.

*	Measure where the time is before making a part faster. Three guesses
	in one day were wrong: the flow analysis that was two sorts, the
	member that was a label, the clock that was the call.

*	A file a test names is a thing the test cache reads. A very big one,
	a store, is taken as its time and length, `tests/suite.inc`.

*	A session of yours that ends with "Segmentation fault" and nothing
	said has given a function for sequences a thing that is not one. What
	it printed before is lost. Find it with a script a call.

*	A command started in the background must not ask anything. A shell
	that has `cp` ask before it writes over a file sat at that question
	for half an hour.

*	A thing the user reports as odd, once, is to be looked at as a fault
	before it is called the machine, the load or the network. Each one
	that was looked at on 2026-10-10 was a fault.

*	A shortcut that draws less has to know everything that changes what is
	drawn. One that moved a picture in place of drawing it knew of where
	a thing was and not of its path, nor of which layer it was on, and
	each was found by the user.

## Helper Scripts Outside The Repo

One person's set of machines needs their addresses and logins, which do
not belong in the repo. Wrappers for that are kept outside it, and are thin:
each only runs the repo commands above on each machine by `ssh`. If they
are not there, make them again from this.

*	**mesh**, `up|down|restart|status [machines]`: `./rack.sh <what>` in the
	tree of each machine, by `ssh` for those that are not this one.
	Restart all the machines together, never one: a member runs the boot
	image it was started on, and one restarted after a rebuild is on a
	different build from those left up, which the mesh will not work
	with. `./rack.sh run mesh` says so, "Heard, and not right: ... is on
	a different build", and is the first place to look when a job is not
	run on every machine.

*	**test**, `[-b] [tests args]`: works out which machines have nodes up
	that are not the member, those are left out with `-l`, lists files
	deleted here for `-d`, then `./rack.sh run 'rack ... "tests args"'` on
	this machine, and prints a line a machine. If the mesh does not answer
	for every machine it starts the members once and tries again.

*	**level**: after a push, the git steps of the section above on each
	machine at once, a line a machine with its head and what is changed.

*	**sync**, `machine`: `rsync` of the working tree to one machine, for
	when the change under test is to sync, the mesh or the Net service
	itself, which the members can not be trusted to carry.

*	**release**: all a tag is tested with, on every machine from cold,
	`make install`, `tests -a`, `make it`, the emulator, the lint. Not run
	unless the user asks for a release.

An `ssh` control socket to each machine keeps these quick. `ssh` is for
starting a member, building a host program, and recovery. Moving files and
running tests is the mesh's job.

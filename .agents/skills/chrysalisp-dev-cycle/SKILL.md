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

*	**[Sync And Test](#sync-and-test)**
	`rack`, the test cache, and what to run after which kind of change.

*	**[After A VP Change](#after-a-vp-change)**
	The build, the lint, and putting the release images back.

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

*	`rack [-b] [-l machines] [-d paths] "cmd"`, `cmd/rack.lisp`: make every
	other machine's tree the same as this one's, then have each machine run
	a command line in a fresh session of its own, sized to itself.

*	`tests`, `cmd/tests.lisp`: the unit tests, as a cache of results, only
	what a change made stale is run. `tests -a` runs all, `tests -s` lists
	what is stale, `tests -m str` the modules with `str` in their path.
	`docs/ai_digest/test_cache.md`, and the `chrysalisp-tests` skill.

*	`lint`, `cmd/lint.lisp`: debug build, the `trace` lint of `obj/vp/`, the
	native release build back. It must say `lint: clean`.

*	`mesh`, `cmd/mesh.lisp`: who is in the mesh, and why a machine is not.
	`mesh -j` joins, `mesh -k` makes a key. `docs/intro/mesh.md`.

*	`sync`, `cmd/sync.lisp`: is another machine's tree the same as this
	one's, and send what differs. `docs/ai_digest/sync.md`.

*	`nodes`, `cmd/nodes.lisp`: add nodes, a desktop, or a network of a shape
	to a running one, and stop a network by name.

*	`STATUS.md`: the record of every change, newest first. Each entry says
	what was run and on what, and what was "not seen".

## Work As The Test User (Mandatory)

An agent's sessions are not the user's, and should not write into the
user's folder, `usr/Guest/`, their app settings and the like.

*	Start every session of your own with `-u Test`: `echo "cmd" |
	./run_tui.sh -n 1 -u Test -f`, and `./run.sh -n 1 -u Test -f`.

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

1.	Read the skills in `.agents/skills/`, the top of `STATUS.md` back to
	where you recognise it, and `docs/ai_digest/ai_thoughts.md`.

2.	`git status` and `git log`. A clean tree at the head of `master` is the
	usual start.

3.	Look for the user's own nodes before starting or stopping anything:
	`pgrep -x main_gui | wc -l` and `pgrep -x main_tui | wc -l`. A member is
	one `main_tui`, its pid is in `/tmp/chrysalisp_rack.pid`. Anything more
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
	`make vp`, `make apps debug`, then `files obj/vp/ | trace -i -l -w`,
	and `lint` again.

3.	`rack -b "tests"` on every machine, the native code is not the same on
	each CPU.

4.	The lint leaves the VP64 emulator image a debug build. Before anything
	is run on the emulator, and before a snapshot, `make it` puts the
	release one back, then `make apps`. A debug VP64 image is about 200K,
	the release one about 170K.

5.	A change under everything, the boot image, `class/lisp/root.inc`, the
	launch path of a command, is tried on the release emulator as well:
	`echo "echo hi" | ./run_tui.sh -e -n 1 -f`. A release build has no
	error checks, no `catch` and no `throw`, tests that need them skip.

The six CPU build, `make it`, the whole suite on the emulator, `make
install` and the QEMU platforms are for a tagged release, not for each
commit. The `chrysalisp-tests` skill has the full list for a tag.

## A Desktop For The User

Much of the work is for the eye, and an agent has none. The user looks.

```code
./rack.sh run 'nodes -g 1'
./rack.sh run 'nodes -a 14'
./rack.sh run mesh
```

*	The first adds a desktop to this machine's member, so it is in the
	mesh. The second adds plain nodes, to fill the machine. For the user to
	watch a small network, in the Network Map say, leave the second out.

*	Say what to open and what to look for. Record in `STATUS.md` what the
	user saw and said, a thing is "not seen" until then.

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
	clean if VP changed, `STATUS.md` has its entry at the top, and any doc
	that describes what changed is brought up to it. `make docs` if a
	function's signature or a command's help changed.

*	An entry in `STATUS.md` says what the user asked for in their words,
	what was done, what was run and on what, and what is not done or not
	seen. A correction goes in as a correction, the entry it corrects is
	left.

*	After a push the other machines are brought level with
	`origin/master` by git, on each: fetch, remove untracked files that
	the push now tracks, they were put there by a sync, and check out.
	Then the harness once more.

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
	`./obj/*/*/*/main_tui -shm_sweep` lets go of what a process that has
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
	main_tui`. Yours has the start time of your run, and its nodes have its
	first node as their parent. Kill those pids. Do not kill by name.

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

*	What is done by hand more than twice becomes a command or a script,
	and is then timed like one.

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

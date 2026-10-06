# ChrysaLisp

![](./screen_shot_5.png)

------

`docs/ai_digest/evidence_not_faith.md` has new figures, measured on 2026-10-06
on three machines, each with one node for each processor. `make test`, mean,
is 0.053 seconds on 16 nodes of an M4 Max, it was 0.070 on 20, 0.17 on the 12
nodes of an i9-8950HK, and 1.3 to 1.45 on the 4 of a Pi 4. All six targets
cross compile in a third of a second on the M4. The bootstrap install takes
1.2 seconds on the M4, 3.5 on the Intel, and 24 on the Pi 4, the 10 seconds
the doc gave for the Pi was old and had not been measured again. A build is
28 worker tasks on 16 nodes, not the 40 on 20 the doc said.

------

All four GUI drivers now keep to one rule for color. A blit in a color tints
a glyph texture or a greyscale texture, the single channel ones, and draws a
normal texture as it is. The raw and frame buffer drivers always did. The SDL2
and SDL3 drivers tinted anything, which is how a wrongly uploaded font went
unseen. Every blit in the tree that is given a color, the text and VDU
glyphs, the atoms of the molecule demo and the menu of Onslaught, is of a
glyph or greyscale texture, so nothing changes on screen.

------

A desktop is a node, to be added and taken away, and a session lives while it
has a way in.

`(pii-spawn)` can start either host program, a GUI node from a TUI node and
the other way round, and `(node-spawn num kind script)` gives the new node a
script to run. `nodes -g 1` adds a desktop, a GUI node that runs the GUI
service, to a TUI network as well. `nodes -t 1` adds a node on the TUI host,
which is the lighter. Quit closes that desktop, its node exits, and the rest
of the network stays up.

A session no longer ends with its first node. It ends when its last front has
gone, a front is a terminal or a desktop, and the launch script, or the watch
it leaves, then stops the rest of its nodes. So leave the TUI of a network
that has a desktop and it lives on, close the desktop and it is gone. New
Shutdown button beside Quit, it stops every node of the network that is on
this machine.

`make gui GUI=sdl3` on a machine with no `pkg-config` is given the folder
SDL3 is in, once, `SDL3_PREFIX=`, and keeps it in the file `sdl3_prefix`.

The host programs must be rebuilt, `make`, and `snapshot.zip` has new Windows
programs.

------

A session stops only itself, on macOS and Linux. A session is the nodes one
launch script started, and the nodes they started in turn.

It was that every launch began by stopping every node on the machine, and a
launch in the foreground ended the same way, so two sessions could not be up
at once, and closing one took the other with it. Now the links of a launch
have names of its own, from a random number, the script keeps the pid of each
node it starts, and `(node-spawn)` leaves the pids and link names of what it
starts in `/tmp/chrysalisp_<pid>.session`. When the first node of a session
exits, the script stops the rest and clears up their files. A launch that is
not in the foreground leaves a watch to do it.

A node now exits when its GUI quits, it used to carry on with no window till
the next launch stopped it. `-b`, the base offset for link names, is gone,
there is nothing left for it to do. `./stop.sh` is as it was, it stops
every node on the machine, for when a session has not stopped itself. The
Windows scripts are not changed.

------

A swap can go the other way. `(. canvas :swap +swap_read)`, any negative
number, reads the texture of a canvas back into its pixmap. Zero and up are
the upload modes, as they always were. The canvas is given a new pixmap, so
one that was freed is made again and can be drawn on, and one that is shared
is left alone. After a `(. canvas :shade)` it is how an app gets at the pixels
the GPU drew.

It is one more call at the end of the host GUI table, `read_texture`, in all
four drivers, so `make` after this pull, and `snapshot.zip` has a new Windows
`main_gui.exe`. On the SDL drivers the texture is drawn to one that can be
read. On the raw and frame buffer drivers a texture is memory, and a glyph
texture comes back as white with its alpha.

Checked on the SDL3, raw and SDL2 drivers, paint, upload, free, read back,
every pixel the same. And the raymarch shader drawn by the GPU and read back
is within 1 in 255 of the native code for it, on every pixel.

------

The menu text of Onslaught came out white on a raw GUI build. Its font is a
white image that is drawn in a color, and it was uploaded as a normal texture.
The rule is that a glyph or a greyscale texture is tinted and a normal one is
not, the raw driver keeps to it, the SDL drivers tint anything. The font is
white on solid black, its shape is in its brightness and not its alpha, so it
is now uploaded as a greyscale texture, `+pixmap_mode_greyscale`, as the
molecule demo does for its atoms.

------

A canvas can free its pixmap. Many a canvas only ever has a pixmap as the step
to get its texture made, an image that is loaded and then shown, and the
pixels then sat in memory for nothing.

`(. canvas :swap +swap_flag_free)` uploads the pixmap and then lets go of it,
and `(. canvas :free)` lets go of it with the texture left as it is.
`(canvas-load file +load_flag_free)` does it for an image as it is loaded. The
canvas still draws, and still knows its size, from the texture. It can not be
drawn on, every draw call is clipped away, at no cost to a draw call on a
canvas that has its pixmap. A pixmap that is shared lives on in the cache,
the canvas only lets go of its hold on it.

Onslaught frees the pixmaps of all its images, and makes each flipped image
straight after the one it is flipped from. The memory its images hold, at
zoom 1, 2 and 3, was 4.7MB, 18.4MB and 36.7MB, and is now 7KB, 27KB and 40KB.

------

The raw GUI driver can be built on SDL3, `make gui GUI=raw3`. It does all its
own drawing into a buffer, SDL only puts the buffer in a window and gives the
events, so the move is small. `make gui GUI=raw` is the same driver on SDL2,
as before. The raw3 build has the sdl3 AUDIO driver.

------

A GUI driver now gives the GUI service an event of our own, not a copy of an
SDL2 event. `src/host/gui_event.h` has the one record every driver fills,
type, position, buttons, clicks, key and wheel direction, and
`sys/pii/lisp.inc` has the same for the GUI service, `+gui_event_...` and
`+gui_ev_...`. The SDL2, SDL3, raw and frame buffer drivers all fill it, and
`src/host/sdl_dummy.h`, the fake SDL header the frame buffer driver needed, is
gone. A window that is resized and a window that is shown are now two events.

The host programs and the GUI service must match, so `make` after this pull,
and `snapshot.zip` has a new Windows `main_gui.exe`.

------

The sdl3 GUI driver has sound. New AUDIO driver, `src/host/audio_sdl3.cpp`,
built with `make gui GUI=sdl3`. SDL3 opens the device and reads a wav file,
the driver does the mixing itself, 32 voices each with its pan, so there is
no SDL_mixer to install. Pause, resume and stop now act on every voice that
is playing the sound.

------

A shader on the GPU, in the GUI. New GUI driver, `src/host/gui_sdl3.cpp`,
`make gui GUI=sdl3`, the GUI on SDL3 with its GPU renderer. The host GUI table
has five new calls to draw a shader into a texture, and every driver has them,
the SDL2, raw and frame buffer drivers answer that they can not. So the host
programs must be rebuilt with `make`, and `snapshot.zip` has a new Windows
`main_gui.exe`.

`(. canvas :shade shader block)` draws a shader over a canvas on the GPU, and
`(shader-gui program)` in `lib/gpu/gui.inc` makes the shader from whichever
back end the driver takes. The surface demo has a CPU and a GPU button. On an
M4 Max the GPU side runs at the 60 frames a second of its timer, the CPU side
on 16 nodes at 10.

SDL2 and SDL3 can not be linked into one program, their calls have the same
names. Each GUI driver now has its own object
folder, so `make gui GUI=...` is a build of that driver, not a mix.

------

A fourth shader back end, `lib/gpu/msl.inc`, gives Metal Shading Language
text, for the SDL3 GPU interface on a Mac. The raymarch shader was run that
way, offscreen, SDL 3.4.16 on the Metal driver of an M4 Max, with the inputs
block from `(shader-pack)` as the uniform, and it agrees with OpenGL and with
the CPU back end. A 1024 by 768 frame with every pixel read back takes 1.1 to
1.3ms. The route for the host is SDL3, with raylib as the fall back, and
graphics belongs to the GUI host, not to a service.

The launcher's default lists now have Onslaught in Games and the surface
demo in Demos.

------

The first step to GPU support, a shader language of our own. New `lib/gpu/`.
A shader is written as s-expressions, `lib/gpu/shader.inc` reads and type
checks it, and a back end takes the typed tree. `lib/gpu/glsl.inc` gives GLSL
fragment shader text. `lib/gpu/cpu.inc` gives a Lisp lambda that shades a tile
of pixels with no GPU, a float is a `real` and a vector a `reals`.

The surface raymarch demo, https://vygr.github.io/JS-Raymarch, is ported as
`lib/gpu/shaders/raymarch.shader`. Its GLSL text was run on the GPU of an M4
Max and read back, and the CPU back end agrees with it, pixel for pixel, over
all the controls bar the bump map, whose noise hangs on the last bits of a
32 bit float.

The inputs of a shader, the values of an app's controls, travel as one std140
block, `(shader-layout)`, `(shader-pack)` and `(shader-unpack)`.

A third back end, `lib/gpu/vp.inc`, turns a shader into VP source for a native
function, assembled once for the CPU of the node and kept under `obj/`. It
gives the same pixels as the CPU back end and is 37 times as fast on the Macs,
84 times on the Pi 4. The raymarch shader at 320 by 240 takes 219ms on one
core of an M4, 377ms on the x64, 913ms on the Pi 4. Tested on ARM64 macOS and
Linux, x86_64 macOS, RISC-V 64 and LoongArch 64 under QEMU, and the VP64
emulator.

New demo, `apps/demos/surface`, the raymarch shader with no GPU. It makes a
slider for each input the shader declares, packs the inputs block each frame,
and farms the tiles over the nodes as native code. A 640 by 480 frame takes
76 to 96ms on 16 nodes of an M4 Max. It is not in the launcher's list yet.

There is no host GPU interface and no `@Gpu` service yet. New doc,
`docs/ai_digest/shader_language.md`. New tests, `tests/gpu/test_shader.lisp`.

------

First run of this work on Windows, by Martyn Blyss. Install, the TUI and GUI,
`make it` and `make test`, 0.19 seconds, all worked. The test suite hung, and
`nodes -a 1` took down the node it was typed on.

The cause was `(pii-spawn)`. A host call runs on the stack of the task that
makes it, a few KB of heap, and `CreateProcess` on Windows needs far more. A
heavy host call belongs on the kernel task's stack, by `:sys_task :callback`,
as the net, audio and gui calls are made, and `:host_os :lisp_spawn` now does
that, on every host. `nodes -a 1` on Windows now starts the node and it joins.

Also found by that run. The Windows host read its `.system_id` file in text
mode, 16 random bytes can hold one that text mode takes as the end of the
file, it is now binary. `(net-quiet)` could wait for ever on a network that
never settled, it now gives up. The tree test now skips a saved game with no
battle recorded, and the format test takes the CRs out of a Windows checkout
before it gives source to the formatter.

New `docs/history/`. `docs/history/history.md`, "How We Got Here", is the
record of where ChrysaLisp came from, the Spectrum games at Mikro-Gen, the
macro set under the ST and Amiga games that became the Virtual Processor,
Taos, Tao Group and intent, and on to this repository, with who did what.
`docs/history/press/` holds reduced scans of what the press wrote about Taos
from 1990 to 1996, each credited to its author and publication.

`docs/ai_digest/udat_ai_shares.md` is now `docs/ai_digest/ai_thoughts.md`. It
holds the views of more than one AI, Udat's from reading the design, and
Claude's from working in the code, with a section for each day.

Release notes are now kept, one file for each release, in `docs/releases/`,
from `docs/releases/v7.0.md` on. They are the short form of this file, what a
release means to someone who has not followed every change.

The rest of the `:lisp` class is written with registers, not script vars,
`(catch)`, `(ffi)`, the repl, `:lisp :run`, the printer, the reader, and the
expand and bind passes. `(catch)` held 48 bytes of stack while its form ran,
it now holds 8. A full build is 1% fewer instructions. Only `:lisp :init` and
`:lisp :deinit` are as they were.

Bug fix in the generic VP optimiser. It turns a read of a stack slot into a
register copy if the same offset from `:rsp` was read or written just before.
A `(vp-push)` or `(vp-pop)` between the two moves `:rsp`, so it is not the
same slot, but that did not end the search, only an alloc or a free did. Code
that pushes and then reads a slot could get the wrong value. Nothing in the
tree did, till now.

`(node-auto)`, and so `-n 0`, now starts 1 node for each processor, it was 2.
A full build is quickest at about that on an M4, an Intel MacBook and a Pi4,
twice as many was 8% to 20% slower. A machine with 16 processors now gets 16
nodes, a Pi4 gets 4.

The Lisp engine has been squeezed. Over the test suite it runs 40% fewer
instructions, and the recursive core holds far less on the stack, a special
form and the body of a lambda hold nothing at all. The args list and the
environment of a call are reused, not made and freed, a prebound function is
not evaluated, and the Lisp object is fetched from the task control block, not
kept in a stack slot at every level. `make test` on one node is 0.33 seconds,
from 0.54. New doc `docs/ai_digest/till_the_pips_squeak.md` has each step, what
it saved, and what was tried that did not pay.

The boot environment is spread over 509 buckets once `class/lisp/root.inc` has
loaded. It was one bucket, and a symbol not bound anywhere, as the first
element of most lists the reader expands is not, was a scan of all 828 of its
symbols. A full build is 4.4% fewer instructions for it.

A node whose process has gone is forgotten by its neighbours at once, a shared
memory link asks the host if its peer still runs. It was left to run out its
time, a few seconds, and a task started on it in that time got no answer.

Added "-r, --repl" command line option to the `lisp` command. This allows for
the passing of code on the command line to be executed in the REPL.

Bug fix in `:lisp :read`. Its "missing )" and "unexpected )" errors were
calling `:lisp :repl_error` with the arguments in the wrong order, so an
unbalanced form, eg. `lisp -r (print (+ 1 2)`, crashed the node rather than
reporting the error.

`brackets` now skips quoted strings inside `{}` blocks, so
`{this, "missing )"}` no longer reports a false mismatch.

Onslaught can now be played remotely. A running game answers requests on its
`@Onslaught` service, so any task on any node can set the control keys and
read back the game state. The new `onslaught` command shows the state, sets
keys, and has a bot, `onslaught -b 60`, that plays the game for you, path
finding its way over the battle map to capture the enemy banner.
See `apps/games/onslaught/app.inc` for the RPC calls.

`trace -i -l` now lints every function, with no `grep -v` filters. The
generated `class/x/create` and `class/x/type` functions are documented by a
header comment under their `(gen-create)` and `(gen-type)` calls, which the
doc scanner, `make docs` and `trace -w` all use. The `apps/` VP functions are
scanned too, build them with `make apps debug` before linting.

Riscv64 native is running again. `emit-call-abi` in `lib/trans/riscv64.inc`
had picked up a nested copy of its own `(defun)`, so every host ABI call
emitted no code and the boot image crashed at startup. Tested on Linux
riscv64 under QEMU, the test suite passes and a self hosted `make all boot`
gives a byte identical boot image to the one cross built on the Mac.

LA64 native is running, tested the same way on Linux loong64 under QEMU. The
`Makefile` now knows a `loongarch64` host, and these faults in
`lib/trans/la64.inc` are fixed. `emit-lea-p` split its offset unsigned, but
`addi.d` sign extends. `emit-div-rrr` and `emit-div-rrr-u` returned quotient
and remainder swapped. `emit-cvt-rf` had the wrong `ffint.d.l` opcode, and
`emit-cvt-fr` trashed `:f12`. Constants over 32 bits could also skip a needed
`lu32i.d` or `lu52i.d`. All the other opcodes were checked against `as`.

Call fusion for the link register targets, ARM64, RISCV64 and LA64. A call
saves the link register on the stack around itself, so when one call follows
another in straight line code the restore and the save between them cancel.
The translator prepass now leaves the link on the stack between such calls
and adjusts any `:rsp` relative access there by the extra slot. The shared
code is in `lib/trans/vp.inc`, and on ARM64 it is folded into the existing
prepass as one sweep with one map lookup per instruction. Boot images are
2.4% smaller on ARM64 and 4.2% smaller on RISCV64 and LA64, for no build time
cost that can be told from the noise. `docs/ai_digest/evidence_not_faith.md`
has the re-measured sizes and timings.

The unit tests have a new framework, `tests/suite.inc`, in place of
`tests/run_all.lisp` and `tests/utils.inc`. The `tests` command now prints
only the failures and a summary, so needs no `grep`, with `-v` to show every
test, `-m str` to run just the matching modules, and `-l` to list them. Test
modules are found by name, `tests/<category>/test_*.lisp`, and each runs in an
environment of its own. New `(assert-error name form)` tests that a form
throws, on an error checked build, and is counted as skipped on a release
build. New `(test-cases form expected ...)` gives a table of edge cases. The
raw script mode has gone.

Four new edge case modules, the suite goes from 1629 to 2032 tests. They
found these, now fixed. `(nlz 0)` and `(nlo -1)` gave 0, not 64. `(trim)` of
a string that was all trim characters gave it back unchanged. `(join)` of an
empty list threw. `(swap list i i)` gave `:nil`, not the list. An index out
of range to `(slice)`, and any bad argument to `(<<)`, `(>>)` or `(>>>)`,
crashed an error checked build rather than throw, as their error paths were
passing the wrong register to `:lisp :repl_error`.

Five more edge case modules, for iteration, flow and binding, collections,
vectors with fixeds and reals, and streams, take the suite to 2457 tests.
`(nums-sum)` of an empty vector crashed an error checked build, it now
throws, as the other vector functions do. `(sort)`, `(usort)` and `(shuffle)`
use positional `%0` argument names. Note the default compare for `(sort)` is
`cmp`, so is for strings, numbers need a compare function given.

Five more edge case modules, for regexp and search, JSON and URL, tree save
and load, structures, and mail, take the suite to 2676 tests.
`(json-stringify)` gave `:nil` for any string with the letter q in it, a
quote included, now fixed. In a `(test-cases)` table a list now also matches
a `nums` vector with the same elements, so a nested vector result can be
written.

Bug fix in `(map!)` and `(filter!)`. When the function they call threw an
error they did not put back the loop index of the loop around them, so
catching that error and carrying on gave the outer loop a wrong `(!)`, or
crashed. `(each!)`, `(some!)` and `(reduce!)` were correct.

`(pivot)`, so `(sort)`, now passes on an error thrown by the compare function,
on an error checked build. It was taken as an equal compare, so sorting
numbers with the default string compare gave an unsorted list and no error.

`(replace-regex)` and `(replace-regex-edits)` now give the empty string for a
capture group that took no part in the match, `(x)?b` say. It gave text from
a reversed slice.

`(tree-load)` gives `:nil` for a stream with nothing in it to read, an empty
file say, as it does for no stream. It threw.

`(n2i)` is noted as having no rounding mode. It is the fastest conversion for
each type, so a negative fixed with a fraction goes down, and a negative real
goes toward zero. Use `(floor)` or `(ceil)` first when the direction matters.

Regexp can now match at the very end of the text. A pattern that needs no
character, `$` or `\s*$` say, was only tried at positions before the end, so
`(matches "abc" "$")` found nothing and `(replace-regex "abc" "$" "<")` did
nothing. It is now tried once more at the end, unless the last match already
took the text up to there.

Whole word searches with `(query)` now work for a regexp with alternatives.
The pattern was wrapped as `!cat|dog!`, which is `!cat` or `dog!`, so found
`cat` in `cats`. `(. regexp :compile pattern :t)` now puts the word break
tests outside the pattern's group, as `!(cat|dog)!`, with that group as
group 0, so capture group numbers do not change. An empty pattern with whole
words on is left empty, it was `!!`, which matched at every word break.

Editor replace, `(edit-replace)`, fixes. Replacing with nothing, to delete the
matches, did nothing, as `(. buffer :paste)` splits its text on form feeds
and that drops empty parts. Replace now works on a list, with the new
`(. buffer :copy_parts)` and `(. buffer :paste_parts parts)`, which keep an
empty text for a cursor in its place. Replace with ignore case on now finds
its matches, it searched the text without lowering it. `(replace-matches)`
gives the text back unchanged when nothing matched, so `(replace-str)` and
`(replace-regex)` no longer throw on an empty text, and a substring search
for an empty pattern finds nothing rather than throw. New edit edge tests
cover these, and editing with several cursors.

`tests -f` records stack frames, using `lib/debug/frames.inc`, in the
functions each test module defines or imports, so an error in a module says
what was running rather than `Frame: :nil`.

The lock service history was never trimmed. It is meant to hold the last
`+lock_max_history` entries, but the result of the trim was thrown away, so
the list grew for as long as the service ran. It is now trimmed in place.
New edge tests for the lock service, contention, shared reads, the key
hierarchy, waiting claims and the `(with-lock)` macros, and for the Buffer
class, the empty buffer, the ends, line joins, selections, several cursors,
undo and redo, load and save, and find.

New edge tests for the Document class, the select methods, break, tabs, case,
sort, invert, unique, comment, trim, reflow and split, and that each is one
undo step.

Document class changes. `:select_line` on a selection over several lines now
takes all of them whole, it took just the top one. `:break` gives the new
line the whole indent when the break is at the line start or inside the
indent, it gave only the part before the cursor. `:right_tab` leaves blank
lines alone, so they gain no trailing spaces.

Lock service fixes, found by running the tests on more than one node. A
claim that timed out could still be granted, if the lock was released just
as the caller gave up, and was then held by nobody until its lease ran out.
`(lock-claim-rpc)` now sends a cancel when it gives up, and the service
forgets the claim, or releases it if it had just been granted. A waiting
claim, or a held write lock, from a node the service had not been routed to
yet was treated as from a dead node and dropped. This happens for the first
few seconds after a network boots. A node is now only dead if it was known
and has gone.

The tests no longer print to the terminal. The few that test `(print)`,
`(prin)` and `(edit-print)` now capture the output with the new
`(test-output code)`, which runs the code in a task of its own, and check
what was printed, not just the return value.

The `onslaught` command can play a game on another machine. `-i` prints the
mailbox id of a game's service, and `-m id` plays the game with that id. The
`@Onslaught` name is system wide, so only finds a game on the same machine,
but its mailbox is good from anywhere. `tests/net/remote_onslaught.lisp` is
an example, it opens the game on the GUI node of another machine on the LAN,
has its mailbox id sent back, and runs the bot here to play it over the link.

A validate build, `make it validate`, now fills every cell `:sys_heap :alloc`
gives out with a pattern, `+hp_cell_fill`, bytes of `0xA5`. It is not zero,
and is no good as a pointer or a count, so code that uses memory before
setting it fails, and fails the same way each time. A new block from the
host is all zero, which hid such a fault until the cell came to be reused.
This is the kind of check validate mode is for, one a debug build can not
spare the time for. The test suite passes on it, native and emulated.

A validate build also fills each cell as it is freed, past its free list
link, with a second pattern, `+hp_cell_free_fill`, bytes of `0x5A`, so use
after a free fails too. A cell that already holds that pattern when freed
is a double free, and aborts with `Double free !`. No test reaches that
abort, there is no route to a double free from Lisp. The fill found two
faults at once, both harmless in a normal build as nothing reused the cell
in time, and both are fixed for every build:

* `:sys_mail :free_mbox` freed the mailbox node and then read its mail
	list. It now splices the mail off first.

* `:sys_task :stop` frees the task control block that holds the stack it
	is running on, with a call, so the return address sat in the freed cell.
	It now moves onto the kernel task's stack, below its saved state, for
	that call. ARM64 did not show it, the return address is in the link
	register. The emulator did.

Two more validate checks. Every `:obj :ref` and `:obj :deref` tests the
count, 0 or above `+obj_count_max` is not a live object, a freed one holds
the fill, and jumps to `:obj :dead`, which prints `Dead object !` and the
stack dump. `(obj-ref)` on the stale address of a freed list trips it, as
it should. `:sys_mem :free` tests that the heap in the block header is one
of the `:sys_mem` heaps, else `Bad free !` and the dump. No test reaches
that one. The validate ARM64 image is now 251,308 bytes, most of the growth
is the count test at each inline ref.

A validate build keeps a guard word, `+hp_cell_guard`, after every heap
cell. It sits outside the cell size, in the stride between cells, so
`+hp_heap_cellsize` is the same in every build, and `:array`, `:str` and
the rest use the whole cell as before. The guard is written on alloc and
tested on free, a changed one is a write past the end of the cell, `Heap
overrun !` and the stack dump. The dump shows who freed the cell, not who
wrote on it. A double free now reports the same way, `Double free !` and
the dump. Writing a wrong guard on purpose trips the check on every free.

The routing ping no longer carries a node's services. It holds a hash of
them, the sum of the hash each entry string already keeps, so the order
they are held in does not matter, with 0 for none. A node that gets a ping
adds up the entries it holds for that origin, and if the two differ it asks
the origin, with the new kernel call `+kn_call_want`. The origin answers
with a full ping to all, services and hash, and no more than one a second
however many ask. `declare` and `forget` still send a full ping at once, so
a change spreads without anyone having to ask. A ping is now a fixed size,
whatever services a node has, and a service list crosses the network once,
when it changes.

On the networks to hand it saves nothing, there are too few `*` services
for the lists to have cost much. A full mesh of 20 forwards about 730 pings
a node every 5 seconds, 92KB, before and after. That cost is the flood
itself, and is the next thing to go at. The slots of a node's entry in the
node map now have names, in `sys/mail/class.inc`. New test,
`tests/system/test_services.lisp`. Checked on a full mesh, a cube of 27, a
ring of 12 and a tree of 15. The ping message has changed, so every machine
on a network must have this build.

The ping now backs off. Each ping doubles the time to the next, from 1
second to 64, and says in the message how long that is, so a settled
network goes quiet. A node is purged if not heard from in twice the time
its last ping gave, plus 2 seconds.

What wakes the network is a kick. A shared memory out link now sends its
status ping each second as a heartbeat, and closes when the peer has taken
nothing for 10 seconds, before it only closed if the buffer was full. The
kernel ping task sums the peers of its links 10 times a second, and when
the sum changes, a link has come up or gone down, its next ping is a kick.
Every node that gets a kick starts its back off again, pings at a random
time within the spread, so not all at once, and purges any node it has not
heard from in the spread plus 3 seconds. The spread is a twentieth of a
second for each node it knows of, and no less than 1 second, so the rate of
the answers does not grow with the network. So only the nodes at the edge of a change have
to notice it. Killing the node that joins the two halves of a quiet tree of
15, the 8 nodes left drop the other 7 in one go, 4 seconds after the links
notice, 1 of spread and 3 of window. `declare`, `forget`, and the answer to a `+kn_call_want`, now ask
the ping task for a full ping, which goes within a tenth of a second.

A link, and the ping task, now notice when they have not run for a while,
this node was busy or the machine was asleep, and do not count that time
against the peer, or the nodes. Suspending all 4 nodes of a network for 13
seconds, the links used to close as it woke, now it carries on. Under the
emulator a busy node can leave a link untouched for 8 seconds, natively the
most seen was 0.4.

A shared memory link that times out is no longer torn down. The out link
marks it down, forgets the peer, so nothing is routed that way and the ping
task kicks the network, and carries on watching. A peer that was only busy,
or stopped, takes from the buffer again, the in link hears from it, and the
link is up again. So a wrong guess costs a moment, not a node, and the
timeout is now 3 seconds, not 10. It also learns. When a peer comes back
after leaving the link for a time, it is allowed twice that from then on,
up to 60 seconds, and that fades by a sixteenth each second, so a peer that
later dies is not waited on for long. One node of 4 stopped for 12 seconds
is dropped after 7, and is back a third of a second after it is resumed.
Stopped again for 6 seconds it is not dropped. Killed, it is gone 12
seconds later, the time is still fading from the first stop.

A shared memory link now knows the process at the other end, each side
sends its process id in the link's status ping, and the out link asks the
host once a second if that process is still running. So a dead node is known
in a second, for certain, and a busy one is never taken for dead. Scheduling
is cooperative, a node that computes for a long time without a yield runs
no link task and sends no ping, to its neighbours it looked dead, and a task
farm then restarted its job. Only a process that is there but has taken
nothing from the link for 60 seconds is now dropped, it is stuck. The 3
second timeout, and what it learns, is kept for a peer whose process is not
known. And a node is not purged from the node map, however long since its
ping, while it is the peer of one of this node's links. One node of 4
stopped for 12 seconds is now not dropped, killed it is gone 5 seconds
later, a second for the host to say so, and the spread and window of the
kick. All the nodes of a machine must have this build, the link's status
has changed, the ping between machines has not.

An idle network burns far less. A link's tasks polled every 0.8ms when
there was nothing to do, each of them, so the cost of doing nothing grew
with the links a node has. A fully connected 27 took 3.76 percent of a core
for each node, a 5 by 5 mesh of 4 links 1.09, a cube of 27 of 6 links 1.29.
A link still starts at 0.1ms and doubles to 0.8ms, so mail cuts through
while there is some, but one that has had no mail for `lk_quiet` sleeps,
some 50ms, now goes on doubling to `lk_sleep_idle`, 8ms. Any mail puts it
back. Those 3 networks are now 0.85, 0.22 and 0.27 percent, and `make test`
is as it was on each, 0.078 seconds, the cube still level with the fully
connected. Just raising the limit to 8ms made `make test` a quarter slower,
the links eased off between jobs, and holding the fast stage for a second
only halved the burn, each routing ping held every link fast. What it does
cost is that the first mail onto a link gone quiet can wait up to 8ms at
each end, a task started now and then on another node answers in 8ms where
it took 1. Measure it with `ps`, the cpu time of each node over 20 seconds.

The generic VP optimiser, `lib/asm/vpopt.inc`, is now one sweep. It looks
each instruction up once, to see which pass it is for, it was up to 4 maps
in turn, and each instruction a scan back goes over once, not twice. The
second sweep, that ran read after read/write again, has gone, it took a
quarter of the time to save 4 instructions in the whole system. The
optimiser's time over a full build is 55ms, it was 79, `make test` on 20
nodes 0.0745 seconds, it was 0.077, and the arm64 boot image is 16 bytes
bigger. The boot images of all 6 targets are byte for byte those of the old
code with only the second sweep taken out. The maps are all made over
`+vp_emit_ops`, in its order, so a symbol's cached slot is right for each.

A look at what the optimiser leaves, in the 29,530 instructions of the 925
functions, found little, so no new passes. 82 branches or jumps to a label
that is itself a jump or a return, 11 branches over a jump, and 23 dead
writes and copies that would need liveness, which a JIT can not afford. A
bound on how far back a scan looks gains nothing, a scan stops at a label
or a call soon enough. A test for the nop before a lookup was slower than
letting the lookup miss. Better than either, `emit-vp-nop`, the nop the
optimiser puts in place of what it removes, is now in `+vp_emit_ops`, so it
has its slot, with nothing in it, in every map of the ops, and a lookup of
it is a hit on its cached slot, not a search of the whole map and a miss.

A node now tells its neighbours its load, not just how many tasks it has,
and a new task goes to the neighbour with the least. A count of tasks is not
how busy a node is, most tasks are asleep, and one that computes without a
yield holds up all the rest. The load is the task count, plus how late a
task that is ready to run has lately been in getting to, one more task for
each `lag_per_task`, 32ms. The ping task measures the lateness, it asks to
sleep a tick and sees when it wakes, it rises at once and falls away by a
quarter a tick. A node that is busy can not say so, so the out link counts
the time a peer has left the link untaken the same way, till the peer next
gives its load. With one node of 4 computing for 200ms at a time, and with
the fewest tasks, all 40 of 40 new tasks went to it, and took 143ms each to
answer, now none do, and they take 1ms. `make test` is as it was, 0.091
seconds on 10 nodes, 0.076 on 19. A first go, with the count in the load
only brought up to date each tick, sent the workers of a farm all to the
same nodes, and a build took 6 times as long, the count has to be live. And
the lateness must not swamp the count. At 1ms a task a build on 10 nodes of
the MacBook was a tenth slower, at 8ms it was level there, but 6 to 9
percent slower on a Pi4, where each compile makes a node late, at 32ms it
is level on both. All the nodes of a machine must have this build.

The task farms no longer lose a job on a slow machine. A farm worker,
`lib/asm/asm.lisp` and `lib/task/cmd.lisp`, ended if it had no job for 2
seconds, so as not to be left behind if its farm went away. But scheduling
is cooperative, and on a Pi4 under the emulator the farm's own node is busy
for longer than that, it cannot hand out the next job in time, the worker
ends, and the job is then sent to nobody. The assembler's farm started a
job again after 2 seconds, 20 emulated, which hid it, 3 jobs were started
again in a build on the Pi4, and each one cost the time. A worker now waits
60 seconds, the farm ends its workers itself when it is done, and the farm
starts a job again after 60 seconds, not 2, that is only for a worker that
dies and its node does not. The emulated build on the Pi4, 8 nodes, went
from 53 seconds to 37, with no job started again.

Mail waiting to go out was freed after 10 seconds, as undeliverable. A busy
node does not take from its links for longer than that, so mail for a node
that was there could be lost. It is now freed only if the node it is for is
no longer known, and a parcel not yet whole only if the node it is from is
not.

An emulated node runs some 10 times slower, so the VP64 build gives a link
30 seconds, and the ping window and slack are 10 times longer too. With 3
seconds the suite on 4 emulated nodes would sometimes lose a node and hang.

The host can now be asked about the machine, and to start another node. New
host functions, `pii_spawn`, `pii_pid`, `pii_alive`, `pii_cpus` and
`pii_memory`, on Darwin, Linux and Windows, and from Lisp `(pii-spawn args)`
`(pii-pid)` `(pii-alive pid)` `(pii-cpus)` and `(pii-memory)`. `pii_spawn`
runs the same host program and boot image as the node that asks, with the
arguments given, and `-e` if that node is emulated. `(node-spawn [num])`
uses it to start more nodes on this machine, each joined by a shared memory
link to this node and to each other, and `(node-link name)` starts one end
of such a link on a running node. `nodes -a 3` adds 3 nodes, `nodes -i`
shows the process id, processors and memory. A node started this way is
seen on the network a tenth of a second later. The Windows side builds,
with `make -f Makefile.mingw`, but has not been run. The host programs must
be rebuilt, `make`, a new boot image on an old host will crash if it calls
these. And an install from before this cannot build its way forward, the
old boot image does not have the functions the Lisp library now binds, so
update with `make install`, which starts from the new `snapshot.zip`.

A network can size itself to the machine. `./run_tui.sh -n 0`, and the
same for `run.sh` and the topology scripts, starts one node, which runs
`(node-auto)` before the script it was given. That starts nodes till there
are 2 for each processor, and no more than 32, fully connected, and waits
till they are seen, so the services started next spread over them, they do
not all end up on the first node. 32 nodes on a 16 processor MacBook are up
in a third of a second. `make install` now uses it, the installer ran on 10
nodes whatever the machine, too many for a Pi4, too few for the MacBook. The
PowerShell scripts take `-n 0` too, that has not been run, the batch files
do not.

Every node now knows which machine every other node is on. The routing ping
carries the system id of its origin, and the node map keeps it. `(lisp-nodes
:t)` gives the nodes on this machine, those that share its file system,
`(lisp-nodes system)` those of some other machine, and `(lisp-systems)` the
machines known. So to find, say, the node of another machine that has the
GUI, ask the nodes of that machine, not every node there is. `(mail-nodes)`
now gives a string for each node, its node id then its system id. The ping
message has changed, so every machine must have this build.

The `Local` task farm uses it. It was written before there was a system id,
so it started with this node alone, launched a few workers, and launched
more each time a reply showed it a node it had not seen. Now it takes the
nodes of this machine when it is made, starts the same herd it would have
grown to, a worker to each node in turn, at once, and starts more if nodes
join later. `:add_node` has gone. On a fully connected network here it is
no faster, `make all` on 10 nodes takes a tenth of a second either way, and
1.4 seconds emulated, the old way found the nodes soon enough. It is there
for the machines and the networks where it did not.

A test that changes the network, adds a node say, upsets the tests running
beside it. Such a module goes in `tests/solo/`, the runner runs those one at
a time after all the others. `tests/solo/test_spawn.lisp` starts a node,
runs a task on it and ends it. `tests/system/test_host.lisp` checks the
rest.

The lock test that failed now and then is explained. Two messages sent one
after the other from one task to another can arrive the other way round,
when there is more than one route between the nodes. On a cube of 27, 200
numbered messages to each other node, 17 of the 26 saw some out of order,
on a full mesh none do. The test sent a write claim then a read claim, and
expected the write to be queued first. It failed 2 runs in 8 on the cube,
now it waits to see the write queued before it sends the read, and passed
14 of 14. The mail system itself is as it was, it does not keep the order.

New launch scripts, `run_tui_ring.sh`, `run_tui_cube.sh`, `run_tui_tree.sh`,
`run_tui_mesh.sh` and `run_tui_star.sh`, the topologies with no GUI, so
`echo "tests" | ./run_tui_cube.sh -n 3 -f` runs the suite on a cube of 27.
The ping message has changed again, so every machine must have this build.

The lock service gives a node that drops out of the routes 5 seconds to
come back before it is taken to have died. A lock test failed once, a claim
on a free key was not granted within its 2 seconds, and then passed in the
next 90 runs. The cause was not proved. The likely one is that, now the
tests run in parallel, the lock tests start while a network is still
settling, the routes shift, and a node missed for a moment had its waiting
claim dropped without a word.

`:fstream` no longer keeps a spare byte in front of its buffer, it was only
there for the reader to step back to.

The Lisp reader no longer pushes a char back into its stream. `:lisp
:read_num`, on a minus not followed by a digit, used to step the stream
back one char and jump to `:lisp :read_sym`, which takes the symbol from
the stream buffer. That is only safe if the char is still in the buffer,
and no stream type made sure of it. It now reads the rest of the symbol,
if there is any, and puts the minus in front. It was found by the `fmt`
work. One formatted file, the same token for token, read `(- ax 24)` as
`( ax 24)`, every time in a task that had read other files first, and never
on its own. With the change it reads right every time. No small test was
found that trips the old reader, so `tests/core/test_reader_minus.lisp`
checks what a minus reads as, but would not have caught this. The boot
images change, ARM64 222,124 and VP64 release 152,244.

The `fmt` command has a new engine, `lib/text/format.inc`. It is an
experiment, work in progress, and is parked. Do not use it on the tree. How
source should be laid out is a matter of taste, and the rules are not
settled.

Where it has got to. A form is laid out afresh, the line breaks inside it
are not kept, and only white space between tokens is changed. What a form
wants is a table of rules at the top of the library, a weight for the gap
before each argument, and flags, and the engine only applies them. A line
that carries a form on is a tab in from the line the form opened on, and a
tab more for each form that opened on that line before it. The limit is
pressure to break, not an order to. `fmt -l 0` keeps the line breaks of the
source and only indents.

What is settled is what it must not do. The source scanners and the doc
builder stay as they are, reading a line at a time. A form they look for,
`defun` `dec-method` `import` `ffi` `call` a VP instruction and the like,
starts a line if and only if it did in the source, and is never broken.
Comments right under such a line stay there, and key map lines are kept.
`fmt` reads its own result back and leaves the file alone if the forms
differ.

On a formatted copy of the whole tree, 708 files, the Lisp reader gives the
same forms, all six boot images build byte for byte the same, `make docs`
gives the same docs, the lint and the `includes`, `forward` and `brackets`
checks give the same output, the suite passes, and a second pass changes
nothing. `tests/text/test_format.lisp` holds the rules as they stand as
tests.

The `tests` command now runs the modules in parallel. It follows the `-j`
habit of the other commands, `-j --jobs num` is the most modules to a
batch, default 1, each batch a task farmed over the nodes with
`pipe-farm`, and a set that fits in one batch runs in the one task, so a
large `-j` is a serial run. A batch task ends with a line of counts, `-c`,
that the runner adds up, and results print in module order. Module paths
can be given as arguments. No module is held back to run alone. The rule
is that tests which depend on each other go in the one module, so the two
lock service modules, which both read its history, are now the one,
`tests/system/test_lock.lisp`, 66 modules in all. Native, 10 nodes, 1.1s
against 1.6s for the serial run, `-j 1000`. Emulator, 11.1s against 19.1s.
The emulator is no faster on 8 nodes than on 1, the waits on timers in the
system modules set the time, not the work.

These two change the boot images by a few bytes, ARM64 221,924 and VP64
release 152,092, so `snapshot.zip` is behind until it is next rebuilt.

`mail-read-timeout` now cancels its timer when the mail arrives first. Before,
the timer stayed on the kernel timeout list until it ran out, so a fast run of
RPC calls built a long list, and each new timer is placed by a walk of that
list. 20,000 timed reads with a 5 second timeout took 0.65s and 40,000 took
5.4s. They now take 8ms and 15ms.

Fixed a memory leak in `:lisp :read`. The "missing )" and "unexpected )"
error paths never freed the part read form.

Sound effects can now be panned. `(audio-play-rpc handle [pan])` takes an
optional pan, -255 full left to 255 full right, default 0 centre. The host
`host_audio_play_sfx` and the `:host_audio :play` binding take the pan as a
second argument, so the host binaries need rebuilding with `make`.

Onslaught is now feature complete against the C++ version. Its sound effects
pan to where the sprite is on the screen. LOAD GAME restores the saved campaign, which is
saved on every return to the map and on exit. Power and strength now carry
over between battles. Talismans used in battle destroy all missiles and
mines. The hall of glory asks for your initials. The SOUND menu option, which
was for music, is removed.

Onslaught now has the campaign map, ported from the C++ version. START GAME
goes to the map, where you move between locations, attack enemy lands, and
watch plagues, crusades and rebellions spread. Battles won and lost now win
and lose territory. The mind combat against the wizardlord is ported too,
and temples give up their talismans to those that win it. Field battles now
pick one of the three field maps at random. Sprite deaths now work as the
C++ engine did, a kill marks the sprite and it dies on its own turn in the
update loop, so chained mine, monk and bomb explosions no longer nest on the
stack, and blast kills now score. The mind duel background parallax scrolls.

A command's stdin stream now has its own mailbox, `:stdio :init` no longer
uses the task mailbox for it. This leaves `(task-mbox)` free in command apps,
so code run via `lisp -r` under the GUI boot image can open a window and
receive GUI events.

Added `(ui-save stream view)` to `gui/lisp.inc`. Saves a View tree, types,
properties and children, in `tree-save` format for inspection.

`(env-push [env])` now pushes the given environment, or a new one if none.
`(env-pop)` no longer takes an environment, and returns the popped environment.

Bug fix in the GUI key up event handling. Was missing sending and up to the
owner if the target View went away.

Added large string chunking to `tree-save` and `tree-load`
(`lib/collections/tree.inc`), splitting strings > 512 characters into `(cat
...)` slices and reassembling them on load.

New `brackets` CLI command (`cmd/brackets.lisp`) for parallel, syntax-aware
bracket matching (`()`, `[]`, `{}`) with verbosity levels (`-v`).

New View `:add_before` and `:add_after` methods.

Catch load time errors as well as running 'main' in `:lisp :run`.

Added Window property `:resizable`, defaults to `:t`.

`(ctx_blit)` can now take optional sx, sy arguments. If not provided, it will
use the default 0,0.

Made `tree-load` and `tree-save` nil-safe, simplifying config file reads and
writes across apps.

Added `opt-toggle` to `lib/options/options.inc` for boolean options enabled by default.
Simplified `cmd/dump.lisp` options to `-w` / `--width`.

Added distributed lock server integration (`service/lock/app.inc`) across
Viewer, Hexviewer, and Edit applications for document file loading, saving, and
scanning, using `with-read-lock` and `with-write-lock` with clean stream
flushing and closure.

Fixed parenthesis mismatch in `cmd/includes.lisp` and `cmd/toflm.lisp` where
unclosed functions caused worker nodes to segfault during `make docs`.

Updated bash launch scripts (`funcs.sh`, `stop.sh`) to automatically preserve
and restore terminal state (`restore_tty`) on script exit, interruption, or
segfault (`EXIT INT TERM HUP`), preventing the TTY from being left in raw mode.
Foreground runs (`-f`) also ensure `stop.sh` cleans up background worker nodes
on abnormal exits, and `stop.sh` restores terminal settings to sane mode.

New `with-lock`, `with-read-lock`, and `with-write-lock` scoped lock macros in
`service/lock/app.inc`. Migrated CLI file commands, libraries, and all 19 GUI
application `.tre` config load/save routines to use the new macros, ensuring
locks are reliably released and file streams are flushed and cleared before
unlocking.

Updated `ensure-lock-service` and `ensure-net-service` RPC helpers to wait for
services without launching them with `open-remote`.

Lock service (@Lock): added `+lock_max_history 128` setting, tracking the last
128 lock and unlock actions in `"key (action type)"` format (e.g. `"file (lock write)"`).
Added `(lock-history-rpc [timeout])` and new `locks` CLI command (`cmd/locks.lisp`)
to inspect the recent lock history.

New `lib/streams/hex.inc` stream library for bidirectional hex stream encoding
and decoding (`hex-encode-stream`, `hex-decode-stream`), supporting optional byte
offset and character columns (`+hex_stream_flag_offset`, `+hex_stream_flag_chars`).
Updated `cmd/dump.lisp` to use `lib/streams/hex.inc` with `-o` and `-c` toggles.

Updated `lib/text/buffer.inc` `:stream_load_hex` to stream data asynchronously
via an async child pipeline and `lib/streams/hex.inc`.

Added read/write file locking across CLI commands (`cat`, `save`, `cp`, `mv`,
`rm`, `dump`, `edit`, `forward`, `grep`, `rle`, `unrle`, `lz4`, `unlz4`,
`tocpm`, `toflm`, `hbook`, `huff`, `unhuff`, `imports`, `includes`, `diff`,
`patch`, `sed`, `trace`, `head`, `tail`, and `ctf`), libraries (`lib/files/files.inc`,
`lib/files/info.inc`), and `docs` app, properly clearing and closing streams before releasing locks.

Added `tests/streams/test_hex.lisp` and `tests/system/test_lock.lisp` unit test
suites, verifying lock acquisition, release, and history logging.

Added `ui-tool-tips` hover hints to the six desktop apps that had the tip
mailbox wired but no tip strings: `crypto`, `weather`, `news`, `lexicon`,
`rosetta`, and `sunclock`. Anonymous control flows were named (`*ctrl_bar*`,
`*time_bar*`) where needed so `ui-tool-tips` could reference them.

All GUI app `config-save` and `config-load` functions now wrap file access with
`with-read-lock` / `with-write-lock` (key = config file path) via
`service/lock/app.inc`, serialising concurrent `.tre` config reads and writes
across all 19 apps. State files that build their path at load-time use a
`(const (cat *env_home* +state_filename))` compile-time key to avoid per-call
allocation.

All zoomable apps now save zoom and font size to config files. Viewer and
HexView now also save search and replace parameters.

Render an "_" if glyph not found in font, just so there is a visual clue of a
missing char.

`lock-claim-rpc` and `lock-release-rpc` now explicitly return `:t` on success
and `:nil` on timeout or failure.

`code` handler in Docs app updated to auto embed into a horizontal scroll if
required.

Lock Service 0.4 replaces the trie architecture with flat sequence primitives,
introducing shared-read (+lock_mode_read) and exclusive-write (+lock_mode_write,
default) hierarchical path locking via (every (const eql) path1 path2). The
release features reader compaction on identical keys, starvation-free path-wise
strict FIFO queue draining, and autonomous fault tolerance that reclaims
dead-node locks via (lisp-nodes), prunes abandoned requests to prevent ghost
locks, and revokes orphaned active locks after a 60-second lease TTL.

New `docs/ai_digest/lock_service.md` document.

New `fmt` command for formatting ChrysaLisp source ! This is a work in progress,
and the rules will be added to as they become more certain. For now it should
NOT break your program even if it might arrange things oddly !

Fixed `(exit :stream :lisp_iostream '(:r7 :r1))`, and `(entry :mstream :itop
`(,this ,pos))` entry/exit bugs.

------

New `curl` command (`cmd/curl.lisp`) to fetch and display content from HTTP
URLs, supporting `-h`, `-i`, `-I`, `-s`, `-X`, `-H`, and `-d`.

New `Weather` desktop GUI application (`apps/desktop/weather/app.lisp`) showcasing
the HTTP/1.1 client (`lib/net/http.inc`) and streaming JSON parser (`lib/net/json.inc`).
Features non-blocking background fetching, procedural 2D vector weather icons,
metric tiles, 3-day forecast cards, and quick city pickers.

New `Crypto` desktop GUI application (`apps/desktop/crypto/app.lisp`) providing a live
market ticker with real-time pricing from Coinranking (`http://api.coinranking.com/v2/coins`).
Features procedural 24-hour vector sparkline trend curves, asset selection grid,
24h range calculations, and persistent user configuration in `crypto.tre`.

New `World Sun Clock` desktop GUI application (`apps/desktop/sunclock/app.lisp`) showcasing
spherical solar astronomy and procedural vector world cartography. Features real-time
subsolar point positioning, day/night solar terminator shading on equirectangular projection,
sunrise/sunset and daylight duration predictions for major world cities, interactive time travel
scrubbing (`-1h`, `+1h`, `-1d`, `+1d`, `Now`), animated daylight sweep mode, and user
configuration persistence in `sunclock.tre`.

New `News` desktop GUI application (`apps/desktop/news/app.lisp`) providing a live Hacker News
reader and feed ticker. Features category filtering (Top, Newest, Show HN, Ask HN, Jobs),
master-detail story feed navigation, rich formatted article summaries and threaded discussion
viewing powered by ChrysaLisp's native `Md` widget (`gui/md/lisp.inc`), non-blocking background
fetching via `node-hnapi.herokuapp.com`, periodic auto-refresh, and configuration persistence
in `news.tre`.

Updated all `*-rpc` service client functions across the system to use
`mail-read-timeout` to prevent indefinite blocking on unresponsive services.

New `(mail-read-timeout mbox [timeout_us]) -> msg | :nil` function in
`sys/lisp.inc` to read from a reply mailbox with a default 1s architecture-scaled
timeout (`mail-timeout-read` alias).

Wrapped all standalone network test and server scripts in `catch` blocks for
clean exception handling and graceful termination.

Removed obsolete machine-specific cluster test scripts.

New `(net-quiet [delay_us] [stable_count] [last_cnt]) -> (node_id ...)` function
in `sys/lisp.inc` to wait until the network node count stabilizes (default: 8
consecutive checks of 100ms without node changes), returning the active list of
nodes.

New `cluster` command (`cmd/cluster.lisp`) to inspect cluster health, host
architectures (`cpu`/`os`/`abi`), memory, and registered services across all
known nodes in the network. Supports `-v` (`--verbose`) for per-node progress
and `-t` (`--timeout <ms>`) for custom probe timeouts.

New `link -a` (`--auto`) mode for zero-configuration network link discovery.
Servers started with `link -l [port] -a` broadcast periodic UDP beacons on port
3334 to announce their TCP port. Clients started with `link -a` listen for
beacons and automatically discover, connect, and join LAN peers to the cluster.
Servers without `-a` remain in manual mode and do not broadcast.

Host network layer updated with non-blocking UDP socket bindings
(`host_net_udp_bind`, `host_net_udp_send`, `host_net_udp_recv`) supporting
broadcast and port reuse, with corresponding `:host_net` VP methods. All host
network callbacks are encapsulated within the `@Net` service task on Node 0,
accessed by clients via `(net-beacon-rpc)` and `(net-discover-rpc)`.

Foreground GUI mode (`./run.sh -f`) now attaches a live TUI terminal on Node 0
under the hood of the GUI compositor, with native EOF stream handling in PII
and TUI layers for automated command piping (e.g. `echo "tests" | ./run.sh -f`).

`(lisp-node?)` and `(cpp-node?)` functions removed now we have self hosted
system to system bridging.

`vp-rdef` and `vp-fdef` macros now simply expand to a `(bind '(... &ignore)
...)` statement. So now uses the standard bind operators to skip args etc.

New `(system-id) -> nodeid` kernel function to retrieve host system identifier.

Services update. We now have "*" Global, crosses network bridges, "@" System
wide, ie on a single machine/laptop/filesystem, and "" no prefix which is local
to that specific VP node.

New `:sys_link :in_frag` function for common link fragment processing.

New `-g` launch scripts option for number of GUI sessions to run. Defaults to 1.
On multi node local nets this lets you run multi screen sessions.

Introduce the `*build_verb*` setting to control the amount of printed output
from the build system. Defaults to 0, ie. minimal. `make -v 1` to list as
before.

Upgrade the CPM async pipeline with a more orthogonal approach. Much simpler but
same benefits.

`:host_net :in` method now shares the `:sys_mail :in` parcel handling method.

Added clean teardown of the GUI from `Logout` app `quit` RPC. Clean up
everything and leave the VP node pristine like it was never there.

Added `(pii-exit)` function to shutdown the VP node.

Implicit `progn` for else on `(if tst form [else_form ...]) -> 'form` and `(ifn
tst form [else_form ...]) -> 'form`. `(when tst ...)` and `(unless tst ...)`
macros updated to use this and now both return `:nil` if the body did not
execute.

Review and update to the `lib/date/date.inc` library and the GUI `Clock` app.

Updated `Canvas` demo, showcasing a few more features. Added new `Opcode`
canvas demo.

------

Native TCP network linking (`service/net`) links ChrysaLisp instances directly
across physical machines over IP or DNS/mDNS hostnames. The server listener
(`link -l [port]`) is persistent and decoupled from per-connection worker tasks
(`:host_net :conn`), remaining listening indefinitely to accept reconnects and
multiple peers concurrently. Network I/O callbacks run on the host thread stack
via `sys_task :callback`.

Legacy ChrysaLib / `hub_node` shared-memory bridging has been removed from
`cmd/link.lisp` and documentation, fully replaced by the self-hosted network
linking service.

Shared memory links (`sys/link`) now automatically time out and close down if
transmission is blocked indefinitely (`lk_timeout`). When `(:sys_link :out)`
cannot obtain buffer space for the timeout duration, it marks the link node
terminated, triggering coordinated shutdown of `(:sys_link :in)` and the parent
`(:sys_link :link)` task. The link node is removed from the kernel link table
(`statics_sys_mail_links_array`), shared memory is unmapped and closed, and the
task count bias is restored, halting CPU polling and freeing OS resources when
peers disconnect or hang.

`pinsert` and `perase` now support variadic arguments, mirroring `def` and
`undef`. `(pinsert pset key ...)` and `(pinsert pmap [key val] ...)` allow
inserting multiple elements or key/value pairs in a single call. Likewise,
`(perase props key [key] ...)` can erase multiple keys from either a `pset` or
`pmap`.

Allow both positive and negative kerning pair adjustments. Use a full triplet
adjustment check instead of the simple cavity threshold test. Plus calculate the
best deadzone default kerning position on a per font basis. This has improved
the aparence of italic text and and saves up to 12% on `.ctf` file sizes. This
only requires a change to the `ctf` command, no changes to the VP rendering
code.

Adjusted the Textfield widget cursor positioning to better fit this new
optical kerning !

`emit-native-reg?` is now a macro that just expands to a `(pfind
+emit_native_regs r)` with no requirement to type check the argument for being a
symbol.

`:lisp :env_args_sig` can now take uppercase class types and will encode them
for exact vtable match rather than any subtype match. eg. `(signature '(:NUMS
:nums))` will require the exact `:nums` class for the first arg and any subclass
for the second arg.

Updated `:dim` class binding to require exact `:nums` type for the dimension
arg. Done a source sweep to use the exact type match signature option where
appropriate in other classes.

`:lisp :env_args_xxx` methods recoded to do there own subclass check, faster
with no stack usage.

Apply 2D conical dilation to the glyph outlines before envelope distance
calculations. Helps situations like "Fa" "<<<" ">>>" etc. Without breaking
italic fonts.

Added column layout support to the Docs app. New `docs/test.md` document with
all the supported formatting and layouts tested. Added column alignment support.

Move MD text rendering to a `gui/md/lisp.inc` widget. Docs app `text` handler
now gathers the text lines and embed an `Md` widget into the document.

Updated other text zoomable apps to use the new `:zoom` propertiy on the main
window widget.

Fix bug in `:reals :sum` where it was using an `(vp-xor-rr :f1 :f1)` which is
not a legal instruction.

Added `-i, --integrity` option to the `cmd/trace.lisp` command. This option
performs a full instruction field type check on loading the VP emit code from
disc.

Add "\q" to the `(escape)` functions list of escape chars. Use this in the VP
`emit-string` when outputting the VP `progn` files.

New native VP version of `(escape)` added as `:str :escape` method.

New doc `docs/ai_digest/keeping_it_hot.md`.

`:str :print` method now write an escaped, with `:str :escape` version of the
string to the output stream !

Switched to `-x` and `:x` throughout for selecting regexp mode for consistency.

Added `+char_class_regexp`, and removed legacy call to `(unescape)` as this is
now done by `(read)`.

Added `"-n, --line-number"` to `grep` command.

Added `"-i, --ignore-case"` to `edit`, `grep` and `sed` commands.

Added `-i` support to all the GUI apps find toolbars.

Lower `to-upper` and `to-lower` to VP code. As we are now using them for case
insensitive searching !

Converted some old raw VP register methods to `vp-rdef` style.

New `x86_64` translator, with much cleaner code. The old version was the first
ever written without an instruction manual, so was all reverse engineared !

New LoongArch64 `la64.inc` translator.

Further optimizations to the `arm64` translator, more 2 -> 3 address peehole
tests and extended preepass checks for instruction fusing opportunities. Saved
around 6KB of boot image.

Optimizations to the generic `vpopts` optimizer pass. Use of padded `pmap`
instruction lists and combined several barrier lists into single pfind calls.

Added `emit-prepass` to `x64` translator and some additional instruction fusing
opportunities.

Eliminate the `emit-tlabel` VP sudo instruction.

New `service/net` `@Net` service ! TCP/IP sockets service. Simple test cmd app
`nettest` to cover a round trip read test. More layers to come, but this gets
the host API working.

Start of network libs `lib/net/url.inc`, `lib/net/http.inc`, `lib/net/json.inc`.
`nettest` command updated to use these and the url testing moved to the unit
test suite.

New `real-to-str` function in `root.inc`.

Added `\b` escape support to the `:str :escape` and `:str :unescape` methods,
and fixed the JSON parser to correctly handle `\b` escapes in strings.

New `docs/ai_digest/net_stack.md` document covering the `lib/net/` libraries
and the `nettest` command, and added link in `LLM.md`.

Optimized `:str :unescape` by refactoring `read-hex-nibble` from an inlined
macro to a local subroutine `(call 'read_hex_nibble)`. Updated `(call)` in
`lib/asm/class.inc` to support parameterless local subroutine calls.

Added support for launching Lisp tasks directly from inline source code strings
(starting with `(`) across the cluster, removing the need for intermediate
disk files. In the kernel, `opt_run` routes inline code strings directly to
`class/lisp/run`, and `import` in `class/lisp/class.vp` wraps the source string
in an `sstream` fed directly to the REPL.

Refactored `CPM-load` and `CPM-save` in `lib/image/cpm.inc` to use asynchronous
local task pipelines (`+kn_call_pin`) with symmetrical `cpm-load-stage-xxx` and
`cpm-save-stage-xxx` pipeline stages for image and `.FLM` film compression and
decompression, replacing sequential intermediate `(memory-stream)` buffers with
zero-buffering streaming pipelines wired back-to-front via IPC stream mailboxes.
Removed top-level `rle.inc` and `lz4.inc` imports so compression libraries are
only imported dynamically inside child tasks when needed.

New `docs/ai_digest/async_pipelines.md` document covering inline raw Lisp tasks,
node pinning, shared-memory concurrency, and the `CPM-load` and `CPM-save` async
pipelines, and added link in `LLM.md`.

`(lines!)` now supports early breakout if the callback function returns a truthy
(non-nil) value. Iteration halts immediately and `(lines!)` returns that value,
or `:nil` if iteration completes to the end of the stream or bounded range.

`(print)` and `(prin)` now always return `:nil` instead of the last printed argument.
This allows passing `print` directly as a first-class function to iterators like
`(lines! print stream)` without triggering an early breakout, and eliminates the need
for wrapper closures returning `:nil`.

------

`:pmap :find` and `:pmap :insert` now use the `+str_hashslot` cache for both
`:sym` and `:str` keys.

Do the `:font :flush` check along with the other caches in `:lisp :run`, do this
check first before flushing the symbols, do not cross reference the font symbols
and the global symbols !

Fixed `Substr` class search of a `:sym` as the source text.

`-j, --jobs` option added to `ctf` command, defaults to 1. Each font file is
distributed to an individual node.

Rename `:plist` class to `:pmap`. Added `:pset` class.

Updated `Lset`, `Fset` and `Tree` collections to support `:pset`.

Updated unit tests with `:pset` tests.

Updated documentation for collections and new `:pset` class.

Switched `+str_hashslot` field to being a positive slot index.

Correct `(atom? obj)` function to handle new `:pset` and `:pmap` classes.

Added `:pset` and `:pmap` support to, `cmd/includes`.

`:dim` class now restricted to only `:nums` arrays as backing storage.

Corrected a double release bug in the lock service.

`:hmap` and `:hset` classes now inherit from `:list` and take advantage of this
to avoid extra memory allocation for bucket management. As `:hmap` is used as
the basis for Lisp level lexical scoping, this saves multiple alloc/free calls
for each lambda invocation ! On the M4 seeing full build times drop from 0.074s
to 0.068s, which is a good result for this change.

------

New `:pmap` VP class ! `(pinsert pmap key val) -> pmap)`, `(pfind pmap key)
-> :nil | val)`, `(perase pmap key) -> pmap` and `(pfindi pmap key) -> :nil
| idx)` functions.

`case` macro now builds a `:pmap` map for the situations where it can !
Fallback is the linear `find` as before.

`cmd/trace.lisp` now uses the new features of the `case` macro to elimenate
several structures and functions.

Fixed a bug in the `:list :find` when the find start pos was the end of the
list.

`Lmap`, `Fmap`, `Lset`, `Fset`, `lib/asm/scopes.inc`, `emit-native-reg?`
upgraded to use the new `pmap` functions.

Added `pcase` base macro for `case` which allows accses to the initial symbols
list used for the switch. With this you can preload/sort/pack a batch of symbols
for many `pcase` uses to line up the `str_hashslot` fields ! Look at
`(assign-asm-to-asm)` for a good example.

New `docs/ai_digest/case_for_pmap.md` document.

New "lib/asm/regs.inc" file for VP register lists, maps and utilities.

Rework of the `cmd/trace.lisp` command to use a unified state vector, and to
process traces in LIFO order. This catches some edge cases and leads to a faster
trace. Current full system trace on the M4 is 0.30s.

Docs app `vdu` sections now strip the common margin in the same manner to `file`
sections.

Fix to Regexp `:match?` and `:search` methods, to correctly handle empty string
input.

Added `:pmap` to the stats command types that get tracked.

Added extra sanity checks to the `(list-bind-args)` function. Now supports both
`&&` ignore source and destination and `&` skip source argument.

`(.)` method call primitive now does not mutate the args list passed to it ! It
now passes the exact same args list to the callee, and the `defclass` macro
insert a `&` skip arg binding for the method symbol. This also means that a
method implementation can now, if desired, reflect on its dispatch symbol.

Swap the order of the args to `:lisp :repl_error`, this avoids extra copies in
most use cases. Saved about 1KB of boot_image.

Fixed bug in `:host_os :pii_write_num`, was using incorrect quotant.

Added `+zero_clobber_funcs` list to `cmd/trace.lisp` command to avoid special
case code just to keep the tracer happy.

New `cmd/ctf.lisp` command app for upgrading `.ctf`, or converting `.otf/.ttf`
files to `.ctf` format. `-v` option for verbose levels of inspection, `-c` to
convert to `.ctf` and `-r` to add/specify code ranges. This replaces the old C++
QtCTF project with ChrysaLisp native support.

Added support for QuadTo command in `.ctf` format fonts.

New `(opt-nums cnt 'opt_var) -> args` options lambda, to grab several numeric
args and push them into an option list. The `cmd/ctf.lisp` command is a good
example of using this to collect char code ranges.

Bug fix to `file-stream` relative seek. Must adjust for the unread portion of
the stream buffer.

`cmd/ctf.lisp` command and VP `:font` methods updated to calculate optical based
kerning tables, and render using the info calculated.

New `ctf_command.md` document, detailing the CTF format and tooling.

Updated the `.ctf` font format to use a unified 3.13 fixed point format
co-ordinate system, and reduced other fields to `ushort` and `short`. The halves
the size of the `.ctf` files.

------

New `benchmark` GUI app for a visual full build benchmark result. The mean time
is smoothed and displayed in the title bar as well as running values for mean,
best and worst times. This is in effect a GUI version of the `make test`
command.

Fix `Buffer :paste` issue with empty text.

Created a VSCode language syntax highlighting set to match the Edit app scheme.
`vscode_chrysalisp_syntax.zip` contains the extension and example settings.

Switch to a unified globally tracked undo/redo system in the Edit app. No need
to have separate global undo/redo actions anymore. And it's logically more
consistent for the user.

Added syntax aware tab compression/expantion to the Syntax class. Buffer
`:stream_load` and `:stream_save` now use these methods rather than the raw
string functions.

`(compress str tab_width idx) -> str` and `(expand str tab_width idx) -> str`
functions now take the logical char index.

Edit app undo/redo now jump to the known group transaction markers. Plus only
perform tab expand/compress on syntax aware buffers.

Docs app now uses a Document buffer and memory-stream to collect and display
`file`, `vdu` and `code` markup handler content.

Vdu widget now shows up any embedded control chars, not LF, present. And cleaned
up the Usage info sections of commands apps to not use embedded tabs.

`cmd/tests.lisp` command for running the unit test suite from the command line.

Updated implementation studies for the Editor, Viewer and Whiteboard
applications.

Fixed `:set_focus` call args bug in action-macro-global.

New `(. buffer :eof [csr]) -> count` and `(. buffer :sof [csr]) -> count`
methods to return the number of chars till the end of the file or the start of
file. Edit app `action-macro-to-eof` now uses this to track if the macro should
stop playback.

`(. buffer :set_focus [csr])` now takes an optional cursor for the bound, else
defaults to the primary cursor.

`XML-parse` switched to using the `(callback f e [...])` macro to hide internal
environment from the user functions and now supports Single-Quoted Attribute
Values.

New `(slices lst) -> ((s0 e0) (s1 e1) ...)` that takes a list of element
indexes, numbers, and returns a compact list of slices that cover all the
elements. The list passed in is sorted, so if you don't want it mutated, then
pass in `(cat lst)`.

`(. set :tolist) -> list` and `(. map :tolist) -> list` methods added to Set and
Map collections classes.

`(scatter map|set ([key]|[key val] ...)) -> map|set` can now be used to populate
`Set`s as well as `Map`s.

`(scatter)` and `(gather)` can now be given a list of args, as well as take
`&rest` args.

New `cmd/trace.lisp` command for static analysis of the VP output code. Has
the ability to lint against the `docs/reference/vp_classes/` documents.

New `(tsort roots dep_fnc) -> order` topological sort function. Takes the roots
and the callback function that given a node will return that nodes dependant
nodes.

Scanned the entire code base with the new `trace` data flow analysis tool,
corrected various bugs found and updated all the source trashes documentation.

Added `tab` expand processing to the GUI Terminal app.

Added `lib/files/info.inc` library. `(files-classes-info) -> classes_db` and
`(files-function-info &optional classes_db) -> func_db` for source file scanning
and caching !

`trace` command updated to use the new `lib/files/info.inc` library instead of
doing its own documentation scanning.

`cmd/vpgraph.lisp` command has been deleted, `cmd/trace.lisp` does everything
and more than this old source based version.

The instructions, `vp-call-r`, `vp-call-i`, `vp-jmp-r` and `vp-jmp-i`, now take
optional arguments for the `:class` and `:method` info. This info is used by the
static trashes tools to calculate the union'd set of trashes registers for the
methods that could be called.

`trace` command now uses the instrumented virtual calls in the VP code to track
the union of all class/subclasses for virtual call sites.

New documents for the `docs/ai_digest/source_database.md` and
`docs/ai_digest/trashes_command.md` library and data flow analysis tool.

Added `-w` writeback mode support to the docs database scanners and the
`trace` command. Use carefully, with great power comes great responsibility !

`cmd/grep.lisp` command now support `-v` inverse, option. Defaults to `:nil`.

New `(transfer src_map dst_map [key val] ...) -> dst_map` function in
collections.

New `(bitcnt n) -> num bits` added to `root.inc`.

VP `call` and `jump` macros now allow optional args for dispatch and object
register. Defaults to `:r14` and `:r0`.

Added a `make vp` option to the `make` command, for just rebuilding all the VP64
and VP output files.

Fix bug in `split` `-s` option. Should have been parsing a string option.

New pseudocode VP ops, `(vp-trash reg_list)` and `(emit-trash reg_list)` to
indicate to the tracer that these registers would be invalidated at this point
in the code.

All source, and resulting reference documentation, for the VP function trashes
info, is now generated automatically from the `trace` command.

------

Added cross-compilation support for Windows host executables on MacOS/Linux
using `mingw-w64` and a new `Makefile.mingw`. Includes automated dependency
download for Windows SDL2 libraries.

Refactored PII directory listing and removal functions to be iterative and use
off-stack static buffers for all OS platforms, ensuring low machine stack
usage for ChrysaLisp tasks.

Updated link drivers with variable poling, with exponential backoff, increased
the link buffer size and one extra buffer per direction. Prioritises the hot
channels across the network to lower latency.

Fix for Windows, timer sleep function in the `pii_windows.cpp`. Needed to use
higher resolution timer and yield code.

Reduce stack usage on GUI service startup by loading in dependacy order. Add the
`*module_class/lisp/root.inc* :t` definition to show that `root.inc` has been
loaded by the system.

Add optional `end` value to `(files-all-depends paths [imps end]) -> paths` and
`(files-depends path [end]) -> paths` functions.

Tuning of `tk_stack_state` settings for each CPU, don't save any registers but
those required on a context switch.

New `(emit-prepass)` function added for the translators, the ARM64 now uses this
to implement LDP/STP instruction fusing. Good results, for a small impact of
system build speed, we reduced the boot_image by 6KB.

Restructured the translator instruction maps to avoid ANY linear scans. Align
all the instruction symbol `str_hashslot` values across all the maps, and fill
any blanks with `:nil`. Now achieving the same speed for a full build as before
the arm64 LDP/STP pass, 0.074s, and showing a 5 platform full build time, on the
M4 MacBook, of 0.38s.

New `docs/ai_digest/turbo_charging.md` doc, covering the Translator instruction
maps.

Refactored the host C/C++ files to remove dependacies on extranious libraries.
As a result the `snapshot.zip` asset, that includes the VP64 `boot_image` and
the 2 TUI/GUI Windows .exe's, has dropped to 100KB.

The `forward` command now auto filters to just `".vp" ".inc" ".lisp"` file
extensions.

Fix to `:sys_mail :alloc` where it was not restoring the message fragment
length, on rare occasions this could result in an incorrect message buffer size
allocation.

Review and update `vp-sync` memory barrier usage. Added missing acquire barriers
to `sys/link/class.vp` to ensure correct synchronization on weakly ordered
architectures like ARM64.

Switched to a new byte ring buffer, rather than slots, based SH_MEM link
protocol. This should allow tighter packing of small messages into the available
link buffers memory.

Added `:sys_task :wake_links` method and use it to wake any sleeping link tasks
when a message gets posted to the off chip list.

Adjust `lk_data_size` (the message fragment data packet), to best fit into a
string object that is >= `lk_page_size` and fits exactly in the memory cell that
best fits that spec. 6104 bytes, but this is worked out dynamically, if the cell
sizes change then this will compile to a better value.

Validate build mode checks that all translator instruction maps match slots.

`:in` links now get task priority 2, `:out` links get priority 1, posting to the
outbound mail list wakes all priority 1 tasks.

------

Added comprehensive unit tests for lazy quantifiers `*?`, `+?` and `??` in
`tests/text/test_lazy.lisp`, including mixed lazy and greedy scenarios.

Fix to the `+?` lazy quantifier in `lib/text/regexp.inc` to correctly prioritize
the loop exit over the repetition.

New `docs/ai_digest/regexp_system.md` document detailing the NFA-based engine.

`:size` method added to Set classes.

Added '-s, --stdout' option to 'time' command to pass through to stdout,
defaults to :nil.

Mandelbrot now demonstrates a boundary scan algorithm.

`(canvas-tile canvas data x1 y1 x2 y2) -> area)` function promoted to `:canvas
:tile` method.

Parameter switch for the `map!` and `filter!` optionals. `(filter! lambda seq
[start end out])`, `(map! lambda seqs [start end out])`.

Optimizations for the optional args processing for the `!` iterators.

New LZ4 library and command apps. `lib/streams/lz4.inc`, `cmd/lz4.lisp` and
`cmd/unlz4.lisp`.

Rename `tee` command to `save` and added `-s, --stdout` flag to pass through to
stdout, defaults to :nil.

Fix to `set-str` macro to avoid potential double eval of the `val` argument.

New `:sys_mem :copy_to_ring` and `:sys_mem :copy_from_ring` methods for ring
buffer copy operations.

`(reflow)` function, now take optional indent string and tab_width arguments.

New `docs/ai_digest/type_system.md` document.

New `:sys_mem :copy_in_ring` method for ring to ring none overlapping copies.

New `docs/ai_digest/exeptions.md` document.

New `(open-pipe tasks [modes])`, optional modes list. `Pipe` class now uses this
to implement the new `|` (distribution) and `!` (pinning) operators.

New `docs/ai_digest/task_pipelines.md` document.

New `docs/ai_digest/type_philosophy.md` document.

Rename `(num-intern)` funtion to `(num)`, to match `(sym)`.

New `docs/ai_digest/flow_through.md` document.

Added lz4 layer support for CPM image format. Both RLE and LZ4 are now optional
compression layers ! RAW -> RLE -> LZ4

`canvas-save` now take any number of optionals and passes them through to the
file format save function if present.

Fixed Mandelbrot demo to not use the `(vp-sub-ff :f0 :f0)` trick ! Dosn't work
on x64 MacBooks.

Validate clears the float registers to `NaN` on each task restore ! Caught all
cases of `(vp-sub-ff zero zero)` cheat code !

Swap order of optionals on `(CPM-save canvas stream type [rle lz4 ident])`.

New `docs/ai_digest/cscript_compiler.md` document.

Fix `cmd/slice.lisp` to clamp the slice to the line length.

New `docs/ai_digest/cscript_skills.md` document.

Added support for Entypo bullet symbol `(0xe979)` for `*` bullets in the Docs
app.

Added support for `strikethrough`, `highlight` and `bold-italic` to the Docs app
text handler.

New `docs/ai_digest/docs_rendering.md` document.

Docs app `parse-line` function upgraded to use the new `(splice)` functionality.

Rename the `*env_toolbar_font*` etc to `*env_symbol_font*`.

------

Added a few more `&optional` outputs to the vector library functions.

Optimised the `Mesh` and `Surface` classes.

Allow an empty list of sequences to the `!` iterator functions ! As a result you
can do raw numeric iteration. ie. `(map! ! '() (list) 0 10) -> (0 1 2 3 4 5 6 7
8 9)`, `(map! ! '() (list) 10 0) -> (9 8 7 6 5 4 3 2 1 0)`.

Added basic `(for start end [body])` numeric loops using the empty sequence
iterators.

New `imports` command for optimizing import paths.

New `vpgraph` command for creating/listing VP function/s call graph.

Chess demo is now playable ! Reorganised to be an action based app,
undo/redo/reset, and config files etc.

The `Tree` widget has completely merged into the `Files` widget.

Addition of optional start and end line index to lines! function. `(lines!
lambda stream [start end]) -> :nil`.

Add a smart context window of words around the cursor to the Editor word
completion, Dictionary class now take these words as an optional context.

Rename of Dictionary class `:find_matches_case` to `:find_various`.

`:fstream :read_next` now allows space for a single char to be pushed back into
the stream.

Added Castling to the Chess app.

New `toflm.lisp` command. This will take a series of frames and encode a `.flm`
format movie file.

Reserve legacy `.cpm` header fields.

Move the transient `:tcursor` cursor API to the Edit class, and avoid extra
cursor list copying. Added Buffer methods `:get_cursors_sorted` and
`:get_cursors_extent`.

Removed the `anaphoric.inc` library. Not used, and don't want to encourage used
of this.

Better tab handling for the TUI print code.

Fix issue in `Local` farm class which was causing 1 extra task to be launched.

`:size` method added to Map classes.

`(lisp-nodes)` function now maps to `str` and merges the local VP node by
default.

Editor now does multi-cursor auto indent after line-break action. Document class
supports this via the new `:break` method.

Redo of the Raymarch demo to create the frames of a full looped `.flm` file.

Fixed an edge encodeing condition in the `toflm.lisp` command.

Molecule demo now dynamically renders the atoms and caches the images as .cpm
files. Updated to use a dynamic, none blocking, distributed render farm.

Add GUI node protection to the swap call.

Reworked the static symbol system to remove `:get_static_sym` and
`:ref_static_sym` methods.

`diff` and `patch` commands and library updated to use output/input in standard
"Normal diff" format.

Fix for Terminal app pipe finish and update prompt race condition.

Don't require the SDL libs in GUI=fb builds.

Added texture mode 2 for the `:swap` call. We now have mode 0, for normal, 1 for
glyph mode, and 2 for greyscale mode.

`canvas-load` takes optional for pixmap swap mode.

New `(str-to-real str) -> real` function that handles scientific notation like
1.5e-3 or 9.97231e-09.

New `Mesh-obj` class for loading `.obj` files. Mesh demo now loads a test
teapot.obj.

Micro optimization work on `:hmap :pfind` and `:hset :find` VP methods.

Added more unit tests and reoganized the `tests/` folder into better more
granular files and folders.

Update Film player app to have file selector and actions style app organisation.

New Window `:dispatch` method to handle common actions for apps.

------

Native VP support for `mat4x4-mul`, `mat4x4-inv`, `mat4x4-vec4-mul` and
`mat4x4-vec3-mul`.

Support added for `real` and `fixed` types in CScript variables.

`(push array ...)` will now push object references for `:list` and flattened
object data for any other type ! You can even push data from a `:str` into a
`:reals` for example, which is great for message data.

`:reals :mat4x4_v4_mul` and `:reals :mat4x4_v3_mul` now support long vectors.

Added `move` mode to Whiteboard app. Snap to grid. Left button draw in front,
right button draw in back.

Added `:set_snap x y` methods to Strokes widget.

Added `(vector-bounds-2d paths) -> ((min_x min_y) (max_x max_y))` and
`(vector-point-in-polygon p paths winding_mode) -> :t | :nil` functions to
vector lib.

New `lib/image/` folder for image format handlers. Start with new `.cwb` format
loader for Whiteboard application format files. `(CWB-info stream) -> (width
height type) | (-1 -1 -1)` and `(CWB-load stream [scale]) -> :nil | canvas`
functions from the `lib/image/cwb.inc` library. Can now load and view `.cwb`
files from the Images app and any apps/commands that use the `canvas-load`
functions.

Fix to the `:pixmap :as_argb` method premul alpha format type test. Plus a fix
to the pixel conversion cache in the `:pixmap :as_premul` method.

TGA loader moved to pure Lisp library, this is not time critical. As we gather
the image import tools into this library we will eventually add a VP helper
function for faster Canvas writes. Plus a tidy up of the Canvas loader helper
functions.

New `(canvas-tile canvas data x1 y1 x2 y2) -> area)` function. This is the
Raymarch app tile helper promoted to a Canvas function for others to use.

String stream class now supports the `:seek` method. And therefore the
`(stream-seek)` Lisp level function.

TGA and CPM import libraries updated to use `canvas-tile` and `string-stream`
line buffer.

Mandelbrot demo now uses the generic `canvas-tile` function.

New `(CPM-save pixmap stream format) -> pixmap` function in CPM image library.
This replaces the VP version.

`CPM-load` and `CPM-save` switched to use the RLE library and added improvements
to the RLE library for a sliding window algorithm.

Added quiet mode the `cmd/edit.lisp`, `-q` option.

Renamed `pixmap-save` and `pixmap-info` functions to `canvas-save` and
`canvas-info`.

Updated Buffer, Regexp classes, and the Editor app, to handle blank line `$`
matches correctly.

Fix typo bug in the Regexp `:search` method to `(const bind)` -> `(const bfind)`
to enable the fast path to work correctly.

VP versions of `pixmap-read` and `pixmap-write` functions.

Regexp class updated with support for lazy quantifiers `*?`, `+?` and `??`.

Fix to Edit class to highlight zero length matches, which are now possible.

Fix to Buffer class `:next_found_cursor` method to correctly move the cursor to
the start of the next line if the match is at the end of the line, ie includes
the "\n".

New stream `(fill-bits stream (array bit_pool bit_pool_size) data num_bits cnt)
-> stream` and `(copy-bits wstream rstream (array bit_pool bit_pool_size) (array
bit_pool bit_pool_size) num_bits cnt) -> wstream` VP functions.

Updated all Windows `.ps1` launch scripts to latest options, bringing them
inline with the `.sh` scripts.

------

Refactored GUI `Edit` widget to delegate all focus region management to the
underlying `Buffer`/`Document` object. This involved removing local focus
properties and adding a comprehensive set of proxy methods for search,
navigation, and focus operations.

Unified toolbar update logic across the Editor and Viewer applications. The
`update-find-toolbar` helper now correctly handles different toolbar layouts
and provides consistent visual feedback for focus and search states.

Simplified search action logic in tool applications by leveraging the buffer's
native support for focus-bounded match navigation.

Updated Editor, Viewer, and Hexview applications to use the new cursor-based
`:set_focus` API and the updated `Edit` widget proxies.

Added safety checks and improved error handling in `edit-replace` and
`find-count` to handle cases with empty results or inactive focus regions
gracefully.

Enabled and verified all comprehensive unit tests for the text editing library,
ensuring 100% pass rate for focus and search refinements.

------

Added `:focus` field to Buffer class to support restricted editing and searching
regions. New `:get_focus`, `:set_focus` and `:filter_cursors` methods added
to Buffer class.

Refined search navigation logic in Buffer class to align with the programmable
editor model. `:next_found_cursor`, `:prev_found_cursor`, `:find_next`,
`:find_prev` and `:find_add_next` now provide intuitive match boundary
behavior and respect the focus region.

New `edit-get-focus`, `edit-set-focus`, `edit-filter-cursors` and `edit-focus-cursors`
proxy commands added to `edit.inc` and exported.

Added safety check to `edit-replace` to handle cases with no matches gracefully.

Comprehensive unit tests for the new focus and search functionality added to
`tests/test_buffer_new.lisp` and `tests/test_edit.lisp`.

Updated `edit` command and documentation to include focus and search navigation
features.

------

Extra information provided by `stats` command. `:list`, `:str` and `:nums`
objects traced. This is used by the author to run single node TUI and GUI in
order to see what objects get allocated statically into the root environment,
and what calls to `num` could be added etc.

Added enhanced stack trace information from `:sys_task :dump`, will now give
the stack dump and the Lisp launch script name, with the current repl stream
name and line number.

Renamed the `*debug_mode*` settings to `*build_mode*`.

New `*build_mode* 2`, `validate` mode that builds in runtime validation checks.
The VP64 build will max out at `*build_mode* 1`, `debug` mode.

Added runtime stack validation in validate build mode. Every function outside of
the `sys/` classes gets stack validated at it's entry point. If the stack is out
of bounds it will stack dump and report what script was running and any repl
info available. To turn on this build mode just run "make it validate".

`obj-get` and `obj-set` functions now range and align check the field access in
debug build mode.

Lisp `error` objects now carry the initial Lisp script name along with the repl
info at the point of the error throw.

Tab completion added to TUI.

New VP `:str :unescape` method, and Lisp level `(unescape str) -> str` function
in `root.inc`.

Lisp `read` function now does basic string escape processing.
"\r\n\f\v\t\q\x\"".

Fixed bug in `condn` where is was not detecting error values correctly.

`(mail-send mbox str)` now only takes a `:str` object as the payload. `gui-rpc`
functions, Whiteboard and Files apps, have been changed to use a `:str`.

Fix drag on Boing/Freeball `action-mouse-motion` problem.

`forward` and `grep` commands now accept the `opt_j` batch size option, defaults
to 1.

TUI uses user login name for prompt.

New `(task-flags) -> flags` function.

Separate the VP class information from the VP object information. We now have
the `class.inc` containing the method related code and `struct.inc` containing
the object structure code, `lisp.inc` contains Lisp level functions and bindings
to the VP layers. The `struct.inc` files are shared with the Lisp system for
`getf` and `setf` interaction. This avoids replication of structure information.

Aggressive refactoring of the VP64 EMU, in `src/host/vp64.cpp`.

Unification of the VP level structure defining macros with the
`lib/class/struct.inc` way of doing things. `def-struct`, `def-enum` and
`def-bit` have been removed and replaced by use of `structure`, `enums` and
`bits`. The standard macros have been enhanced to make them much stricter on
field redefinition testing.

Docs app now overrides the `enum` macros for embedded `widget` content.

Fix error in `:str :slice` for reversed strings.

Added a bunch of simple unit tests, run with `./run_tui.sh -n 1 -f -s
tests/run_all.lisp`.

Added missing `:empty?` method in Emap class.

Filled in the `struct.inc` import chain. Always import the base `struct.inc`
file dependencies.

Buffer load/save methods have switched to a streams based API.

New `edit` command which is `The ChrysaLisp Parallel Programmable Editor`. See
the new document for details. This comes with a new `lib/text/edit.inc` library
available to all applications.

Implemented Kernel signals and the ability for the Terminal and TUI to force
close pipelines of tasks. cntrl-D will abort the pipeline, ctnrl-d will send
and EOF into the pipeline and wait for orderly shutdown, if the pipeline does
not close within 2 seconds it will be aborted.

Added `:left_white_space_select`, `:right_white_space_select`,
`:left_bracket_select`, `:right_bracket_select`, `:primary_cursor`,
`:next_found_cursor`, `:prev_found_cursor`, `:find_next`, `:find_prev` and
`:find_add_next` methods to Buffer class. `edit-select-ws-left`,
`edit-select-ws-right`, `edit-select-bracket-left`, `edit-select-bracket-right`,
`edit-primary`, `edit-find-next`, `edit-find-prev` and `edit-find-add-next`
proxy commands added to `edit.inc` and exported.

Exported missing `edit-replace` and `edit-select-form` symbols from `edit.inc`.

Updated `edit` command help information to include the new selection commands.

------

Tidy up to Document class, ensuring multi cursor merging on floored operations.

`:select_form` method added to Document class. `action-select-form`,
`action-copy-form` and `action-cut-form` added to Editor and Viewer apps.

`:left_white_space` and `:right_white_space` methods added to Buffer class.
`action-left-white-space` and `action-right-white-space` added to Editor and
Viewer apps.

New `(splice seq1 seq2 idxs) -> seq` primitive. The `idxs` are provided in a
`:nums` vector, each slice is taken from the source sequences in turn, you can
give the same source sequence for both. The sources must be the same type and
the returned sequence will be that type.

New `(msafe? %0) -> :t | :nil)` predicate to ask if a parameter is macro safe.
ie. will not lead to double evaluation.

`erase`, `insert`, `replace` and `rotate` are all now macros over `splice`.

New text replace compiler functions that create a splice command for you that
does entire text replacement in a single call to splice. `replace-compile`,
`replace-matches`, `replace-edits`, `replace-regex-edits`, `replace-str-edits`,
`replace-regex` and `replace-str`.

Removed `:rslice` method. `:slice` method understand the slice reversal if
required. `:slice` methods now only return the new slice object.

New `:list :ref_all` method, and several list methods moved to be inline
`s-call's` with a jump to this method at the end.

Removed `:str :append`, `:str :init1` and `:str :init3` methods while rewriting
`:str :cat`.

Install now uses 10 VP nodes. Things have moved on since this limit for the Pi3.

Signatures for Search classes, `:search` and `:match?` now take the pattern or
the compiled meta data as a single non optional argument.

Editor app, `sed` and `grep` commands now uses the new `replace` compiler.

TUI updated to share command history with the GUI Terminal app, and handle line
editing.

`lisp-elem-index` fubnction renamed to `seq-elem-index`.

New `(compress str tab_width) -> str` function to match `(expand str tab_width)
-> str`.

Moved `:reflow` method out of Syntax class and into `root.inc` utils. Now
`(reflow words line_width) -> lines)`.

------

`lib/asm/scopes.inc` now uses an environment to hold each scopes symbol map.

New `Mask` widget for drawing the opaque region of a view. Edit class now uses
these to render the selection layers.

Fixed bug in right button mouse handler.

Changed `defgetmethod` and `defsetmethod` to use `:` key symbols.

Added new `(defproxymethod :<name> ([arg ...]) :<field>)` to pass through to
another object stored in field `:<field>`

Complete rewrite of the text `Buffer` class. It now supports multi cursor
editing. The GUI `Edit` class has likewise been updated to use this feature
along with the `Editor` and `Viewer` apps.

`Edit` widget now supports adding cursors with right button or setting cursor
with left button.

New Editor `action-set-cursors`, bound to `cntrl-g`, to set cursors for all of
the search matches.

New Editor `action-add-cursors`, bound to `cntrl-G`, to add cursors for all of
the search matches. You can keep changing the search and add more.

Text `Buffer` class now take `flags` rather than `mode`, current flags are
`+buffer_flag_syntax` and `+buffer_flag_undo` to turn on those features. Apps
updated to use these flags.

Auto generate the necessary code to transfer the local array elements to
external storage in `:set_cap` method.

Auto generate the `str_gap` size from the `:sys_mem` cell information.

Addition of `(time-it heading body)` macro to `root.inc`. Used for simple timing
of code without needing to set up for profiling.

`:set_found_cursors` and `:add_found_cursors` methods moved to Buffer class.

Minimum allocated cell size has changed to 24 bytes, the size of a `:num`
object, as a result `:array` now get 8 local slots available for element storage
before going to an external block.

Fix `make-info` function to cope with relative function paths in file products
creation.

Moved `:select_word`, `:select_line` and `:select_paragraph` methods into the
Buffer class.

VP version of `csr-sort`, `csr-cmp`, `csr-map-delete` and `csr-map-insert`.

Renamed Edit class `:get_find` and `:set_find` methods to `:get_focus` and
`:set_focus` in order to better reflect what they do.

Editor actions `action-set-cursors` and `action-set-cursors` now respect the
focus region.

New Editor action, `action-add-next`, bound to cntrl-F, which will add the next
found selection to the cursor list.

New Document class, inherits from Buffer but supplies the higher level word,
line, paragraph and such mutation and selection methods. Buffer is for characters
and cursors, Document for structure etc

`:region :paste_rect` method now checks for area merges. Cut down on `:draw` ctx
calls etc.

Document `:select_block` now handles multiple cursors.

The official way to ignore a single argument in a bind or function signature is
now a single `&` symbol. This truly ignores the bind site and skips to the next
binding. This also now means that the return value from `bind` is the last value
you bound ! not the last value you ignored !

Added horizontal scroll bar to Files widgets, set to 75% `:min_width`.

New Scroll widget `:visible` method now takes optional flags to control
`+scroll_flag_vertical` and `+scroll_flag_horizontal`, defaults to
`+scroll_flag_both`.

------

Fix Editor `load-depends` bug when on the scratchpad buffer !.

Swapped over the `copy`/`paste` icons.

Font app now copies the lowercase version of the char code.

New `(fn-consts c ...)` for creating a list of constant offsets.

Rework `num` `fixed` and `real` classes to reduce stack and register use.

Rework `nums` `fixeds` and `reals` classes to use improved vector inline helpers
to reduce stack and register use.

New support for `(vp-cpy-df)` and `(vp-cpy-fd)` instructions in the VM.

Reworked `lib/asm/class.inc` to use environments for the vtable storage. Faster
method lookups.

Added `make apps` option to build just the application level Jit native code.

VP class vtable and super info now kept in compiler var environments `*vtables*`
and `*supers*`.

Long awaited change for VP class names to be key symbols. So `list` -> `:list`
etc.

Make command `release` and `debug` options to set build mode for `it` and `apps`
platform build actions. Default is to build native platforms in debug mode and
VP64 in release mode. These settings will override everything to the given mode.

Structure type `real` now supported. This replaces the `ulong` type which was
actually redundant. Mandelbrot and Raymarch demos updated to use this for
passing `real` fields in messages.

Structure type `fixed` now supported. This comes with a rework of the field type
codes to be a simple enum.

Lowered `starts-with` and `ends-with` to VP functions.

Expose `(env-copy env num_buckets) -> env` to application code and
simplification of VP and Lisp class construction using `env-copy`.

New `apps/slider/app.lisp` simple sliding puzzle app.

New `apps/pairs/app.lisp` simple matching pairs puzzle app.

New `apps/solitaire/app.lisp` simple peg solitaire puzzle app.

GUI apps arranged into subfolders. Launcher and various path code updated to use
relative paths.

New `(path-to-file) -> path` function that returns the path your file is within.
This information comes from a `(first (repl-info))` call.

Moved all user environment and app state files to new `usr/` folder.

------

New `(nql obj1 obj2) -> :t | :nil` built in function.

New `'obj :eql` virtual method.

Fixed bug in optional start index value for `(find elm seq [idx])` and `(rfind
elm seq [idx])` functions.

Optimized `'list :lisp_merge` to be faster and use no stack variables.

Optimized `'list :lisp_match` to use no stack variables.

Editor now supports `sticky` cursor x position.

Removed the use of task level storage.

Big changes to `reals` ! A `real` is now a 64bit IEEE float, with hardware
support. Approx 1000x faster.

New `(ceil fixed)` and new `(vec-ceil ...)` support methods.

Raymarch and Mandelbrot demos recoded to demo IEEE real support.

`vp-def` renamed to `vp-rdef`. Float version is `vp-fdef`.

Fix bug in `'canvas :ftri` shown up by denser Meshes.

`(fn-const c) -> offset` function and `'fn_consts` label added for easy VP
function constant pool creation.

Consildated all `sin` operations to use `r_sin`.

Consildated all `sqrt` operations to use `vp-sqrt-ff`.

`list-bind-args` now take optional destination reg list. To better fit with
extracting `real` register values from objects.

Renamed application level `vec-xxx` functions to `vector-xxx` to avoid clashing
with the `sys_math` class.

`:quant` method added to real and reals classes. `(quant real tol) -> real` and
`(reals-quant reals tol [reals]) -> reals)` added to `root.inc`.

------

New `class/mstream/class.inc` VP class for in memory stream buffers. A seekable
list of string objects. Lisp binding `(memory-stream) -> stream`.

New `-c codebook` option for `huff` and `unhuff` commands to select static
codebook mode.

New simple Todo app.

Stack widget now uses Radiobar widget for the tabs.

`ui` macros now have the ability to erase a property from a widget by using the
`:erase` value for the property.

Editor app now has service RPC actions. First one is the ability for the Debug
app to request loading of a file and showing the current breakpoint.

Editor now supports some redumentry auto debug breakpoint helpers. Launch debug,
toggle breakpoint, remove, enable all, disable all, remove all. It will NOT
remove any user named breakpoints, only auto named.

Upgraded the Images app to have a file selection.

New Files widget.

Smarter string encoding for the `.tre` files.

Lowering of `hex-encode` and `hex-decode` to VP functions. These now replace all
uses of `id-encode` and `id-decode`.

New `(read-blk stream bytes) -> :nil | str` and
`(write-blk stream str) -> bytes` builtin VP function.

Moved `vp-min, vp-max, vp-abs` into the VP VM proper. ARM64 and x64 have native
operations `cmov` and `csel` that could implement these.

Upgrade to `(obj-get obj offset type|0 size|0) -> num|str` and `(obj-set obj
offset type|0 size|0 num|str) -> obj` to treat sub `struct` members as strings.
`getf` and `setf` macros updated to correctly set the arguments to this new API.

New `-s script_name` option for the batch/shell files, to ease LLM tests. eg.
`./run_tui.sh -n 1 -f -s script_name`.

Added `(type-of obj)` support to the all `class/` classes.

Fleshed out the `(set-xxx str idx val)` matching macros to `(get-xxx str idx)`.

Fleshed out the `(read-xxx stream) ->val` and `(write-xxx stream val) -> stream`
macros.

New `includes` command for auto creation of `.vp` file header includes, great
time saving and checking tool.

New `(getf-> obj field|(field offset) ...) -> (val ...)` macro, the counterpart
to `setf->`.

Added `(path-to-relative target [current]) -> path` function to compliment the
`(path-to-absolute target [current]) -> path`. If the current is not provided it
will use `(first (repl-info))`.

New simple `sed` command line app. In `-x` mode it will match and replace
submatches using the `$0-$9` parameter syntax.

New generic files line scanner, which calls back to a user handler. `(scan-files
files handler [split_class comment_char]) -> files`. The handler can choose to
be presented with split line tokens, provide a line splitter charclass, specify
a comment character if needed. The handler should return `:nil` or a list of new
files to add to the work list.

`(read-char stream [width])` now defaults to unsigned byte, and if a width is
specified positive widths mean signed values and negative width mean unsigned
value.

Audio service now shares and reference counts the resource handles.

Lowered the `(split str [cls])` function to VP code as it's forming the basis
for a lot of file scanning work now.

Fixed the off by one issue in the Edit buffer left bracket matching.

Fixed the Terminal apps one extra line in the history buffer issue.

------

New `opt-tail-call` VP optimization.

New `vpstats` command. After a `make it` you could run it on the `obj/vp/`
folder etc with `files obj/vp/ | vpstats`.

Used the `vpstats` information to frequency order the `vpopts.inc` tests.

None recursive `negamax` implementation for the Chess child task. Also took the
time to restructure the move generation to take better advantage of the
alpha/beta cutoff optimisation.

`options` functions now uses `callback` to invoke the user option function. Plus
added some basic option generators. `(opt-flag opt_var)`, `(opt-str opt_var)`
and `(opt-num opt_var)`.

Fix bug in the `(merge dlist slist) -> dlist` function when used with none
symbol lists.

Updated Calculator app with some basic programmer modes and operators.

Updated Launcher app with user configurable catorgories and ordering. State is
saved to `launcher.tre` in the users home folder.

New Eyes GUI application. Bit of silly fun with the AI.

Fixed a few Editor `(some (# (unless ...)))` undefined issues. Switched to using
the `bskip` functions where possible.

New `(read-bits stream (array bit_pool bit_pool_size) num_bits) -> (data|-1)`
and `(write-bits stream (array bit_pool bit_pool_size) data num_bits) -> stream`
Lisp bindings.

Rename of `(write-line stream str)` to `(write-line-lf stream str)` and
`(write stream str)` to `(write-line stream str)` in order to match the
`(read-line stream)` naming.

New `lib/streams/rle.inc` module.
`(rle-decompress in_stream out_stream token_bits run_bits)` and
`(rle-compress in_stream out_stream token_bits run_bits)` functions.

New `cmd/rle.lisp` and `cmd/unrle.lisp` command line apps for rle/unrle use.

Better `'num :hash` method. Does some bit mixing rather than just returning the
value unchanged.

New `lib/streams/huffman.inc` module. `huffman-compress` `huffman-decompress`
`huffman-build-freq-map` `huffman-write-codebook` `huffman-read-codebook`
`huffman-compress-static` and `huffman-decompress-static` functions.

New `cmd/huff.lisp` and `cmd/unhuff.lisp` command line apps for adaptive
huff/unhuff use.

New `cmd/hbook.lisp` command for scanning and optionally creating Huffman
static code books.

Editor app now saves and restores position and size to its state file.

New Hexview app, which uses the Viewer app as a base but just pipes the file
loading through the `dump -c 16` command.

------

`*Lock` service added. This still requires further work, but it removes the idea
of lock files within the file system during Jit worker compilation. It should be
one such service per file system plus it has other serialization issues ATM.

`(bskip cls str idx) -> idx` and `(bskipn cls str idx) -> idx` can now take the
input index as a negative value like the `(slice seq start end)` function.

New `docs` command app. Used to scan source files for documentation. This is now
used by the `make docs` command to scan documents in parallel.

`rfind` changed to use slice end index compatible style.

New `(rbskip cls str idx) -> idx` and `(rbskipn cls str idx) -> idx` functions,
uses slice end index compatible style.

Fix for the Editor open files tree layout problem.

Fix for the Editor `action-find-function` problem, due to the change in `ffi`
syntax.

New `'str :bfind` VP class method. All the binary search char functions now call
down to this low level search.

Clipboard service now integrated with the HOST text clipboard.

Generate improved VP classes reference documentation via scanning for the `ffi`
inline documentation.

------

Terminal app fix for user input.

New `(bit-mask mask ...) -> val` function and
`(bits? val mask ...) -> :t | :nil` macro predicate defined to go with the
`(bits)` bitmask definition function.

New `(eval-list list [env]) -> list` inplace list element evaluation primitive.
This is making the `:repl_eval_list` method available at the user level.

Improvements to the `case` macro. Lots of new features since this was first
written !

New `(macrobind)` macro.

New `(static-qqp)` macro that only does a prebind pass, no static macroexpand !
GUI UI macros now use this.

Majour tidy up of the `lib/asm/vp.inc` file. Use of `static-q*` where possible
and removeal of redundant type conversions.

Reference counted `netid` object for temp mailboxes. Create one via
`(mail-mbox)` and no need to explicitly free anymore. `(free-select)` call is
now gone, and `(alloc-select)` is replaced by the `(task-mboxes)` function which
creates the select list using `(task-mbox)`, for the first entry, and
`(mail-mbox)` for the rest.

Reworked the `lib/task/local.inc` class. Added a few more options as well as
tidying up the code. As a result the build times have improved again. Worth
spending some of this gain on new VP optimisation checks.

`opt-redundant-branch` test added to `vpopt.inc`. This test looks to see if a
constant based branch is being done when we already loaded a constant into that
register that we can static eliminate. This can happen with inline code
embedding, AND as branch instructions are a KILL op for other searches, it's
worth checking for.

Rework of the symbol interning system for the entire OS and Lisp engine. Removed
redundant methods and streamlined the base method used by `lisp :read_sym` to
operate directly from the stream buffers without intermediate copies or string
object churn.

------

New non recursive constraints system for GUI layouts. Significant reduction in
stack usage.

New `+view_flag_subtree` view flag. This limits the `view :flatten` method to
not descend into such marked views. The `Scroll` widget sets this flag
automatically on its chld widget, for example.

New `(repl-info) -> (name line)` function. Replaces `*stream_name*` and
`*stream_line*` variables.

Netmon now gathers stack space stats. The space reported, in debug mode, is the
maximum of all the current task stacks on that VP node and the current maximum
stack use in release mode.

Extensive rework on the Arm64 translator to NOT use 16 byte stack alignment !
The VP `:rsp` is now NOT mapped to the Arm64 `:r31` register. This significantly
reduces the stack space requirements. Still use LDP/STP but not via the `:r31`
register.

Riscv64 translator now uses 8 byte stack alignment.

------

New `(condn)` special form. With that comes a faster `(and)` macro that follows
the `(or)` macro in simplicity.

New `ifn` special form. `(ifn tst form [form])`. Comes with a removal of the
`opt` macro as it's now redundant, can just use `(setq s (ifn s v))`.
`(setd ...)` macro updated to reduce to this form.

New VP level `(until tst [body]) ->tst`.

Big tidy up of the `(ffi path [sym flags])` syntax and optional values.

New `(lines! lambda stream)` function than replaces `(each-line)`, accses the
line index with `(!)`.

New `(filter! lambda seq [start end out])` function than replaces
`(filter-array)` with a simple macro `(filter lambda seq)`.

New callback macro. `(callback lambda env arg ...) -> (eval `(apply ,lambda
'(,arg ...)) env)`.

New `(++ s [i])` and `(-- s [i])` macros.

------

Audio SFX service from Martyn Blyss. Many thanks ! Boing demo updated with a
little nostalgia.

Moved Clipboard service to new `service/` folder.

Moved GUI service to new `service/` folder.

`(vp-simd)` now does vector padding, rather than raise an error !

Rename all `(xxx-rev)` functions to `(rxxx)`.

New `'sys_task :count` method and `(task-count bias) -> cnt` Lisp level
function.

Updated `map` class `:update` method to return the update function value ! So
now `(. map :update key lambda) -> val`.

New `(. map :memoize key lambda) -> val` method.

Added support for nested bullet lists, italic and bold fonts, to Docs
application.

------

`(write-char)` and `(write)` now return the number of bytes written to the
stream. Translators updated to use this info to improve performance.

New `stats` command  for gathering basic runtime info.

New Set `(. set :inserted key) -> :nil | set` method, to avoid double search
test/insert operations.

New Map `(. map :update key lambda) -> map` method, to avoid double search
test/update operations.

------

Simplification of VP level Lisp type checking code.

`(each!)`, `(some!)`, `(map!)` and `(reduce!)` now 25% faster and use `(!)`
function to retrieve the current sequence index. They no longer create an extra
environment to hold the `_` binding.

------

Addition of `\q` wildcard, double quote, to regexp and charclass libs.

Editor and Viewer app now support `:top` and `:bottom` cursor actions.
cntl-home and cntl-end bindings.

Editor and Viewer app support for a cursor stack. cntl-d and cntl-D bindings
for `(action-push)` and `(action-pop)`. File will reload on pop, unless they
have been deleted.

Addition of `(files-all-depends)` and `(files-depends)` to
`lib/files/files.inc` library.

Editor now has key bindings for `(action-load-depends)` and
`(action-load-all-depends)` on cntl-e and cntl-E. Acts on the current file as
the source for the action.

`(split)`, `(trim)`, `(trim-start)` and `(trim-end)` functions now takes an
optional char-class as the split/trim string !

Editor `(action-find-function)` binding on cntl-j, with `(action-pop)` on
cntl-J for convenience.

Slowly but surely adding the usage lines for all the `ffi` bindings. This is
particularly important now we have the jump to function, push/pop actions.

Added `-d`, `-i` and `-a` options to `files` command.

New `(flatten list) -> list` function in `root.inc`.

Change the argument ordering for the `(sort list [cmp start end])` function and
made the comparison function optional and default to `cmp`.

New `(usort list [cmp start end])` utility function.

Iterator functions `(each!)`, `(some!)`, `(map!)` and `(reduce!)` now take
arguments other than the lambda and the list of sequences as optionals !

------

Fix `forward.lisp` defs regexp !

Introduce `(redefun)` and `(redefmacro)` ! If you are overriding an existing
function or macro you must now use this declaration.

`(char-to-num)` recoded to use ranges to specify the character regions.

Introduce the lambda shortcut macro early in the `root.inc` file.

`(elem-set)` and `(dim-set)` now return the target array not the value.

`(elem-get)`, `(elem-set)`, `(dim-get)` and `(dim-set)` now take the target
array as the first argument.

`(slice)` now takes the target sequence as the first argument.

------

New `map!`, `reduce!` VP primitives.

Faster and more generic `(filter-array)` function. Replaces `(filter)`

`(slice)` can now reverse any sequence.

Generic `(reverse)` macro that wraps `(slice)`.

New `'list :collect` and `'list :min_length` methods.

`each!`, `some!`, `map!` and `reduce!` recoded to use new list methods.

------

New Tab path completion lib `lib/files/urls.inc`.

Tab path completion added to Textfields.

File picker now uses a Tree widget.

New `repeat` command.

Reduction of translation passes by 25%.

New `charclass.md` document.

New `searching.md` document.

New `collections.md` document.

------

New `Radiobar` widget. Can be used in radio or toggles mode.

Basic search added to Docs application.

New `:visible` method added to Scroll widget.

Added `-m` option to `grep` command.

New Fset/Xset `:intern` method.

New Stroke widget. Whiteboard updated to use this for multi stroke capture.

New `(path-smooth tol src) -> dst` function.

`(load-tree)` and `(save-tree)` now handle `:path` data types.

Whiteboard now uses `.tre` format files for data storage.

`(zip)` and `(unzip)` now work on any sequence types.

New generic `(split seq sseq) -> seqs` function.

New `forward [path] ...` command app. This detects forward referenced function
calls, to help you optimize your source.

`(find)` and `(find-rev)` take an optional start index position.

Faster, approximately 20%, `Syntax :colorise` method.

New `bskip` and `bskipn` functions.

------

Region selection added to Viewer app.

Find count on status bar shows only count in selected region active.

New `&ignore` binding action and `(most)` function.

`(first)`, `(second)`, `(third)`, `(last)`, `(rest)`, `(most)` promoted to VP
built in functions.

Reference free `(progn)` implementation, `(cond)`, `(while)` and `(lambda)`
bodies, now used the exact same code for implicit and explicit progn.

Change to the task distribution system to randomly pick from the group of best
neighbor with the lowest loading.

Faster `(reverse-list)` functions, and new `(reverse seq)`.

`Buffer :paste` now takes optional wrap_width.

`(#)` macro does NOT descend into quote or quasi-quote forms.

New `lisp` Docs app section handler ! `widgets.md` now shows live Lisp code
and the widget created by it. These section handlers run in an auto module and
anything they export persists within a `*handler_env*` and is visible to the
next `lisp` section ! The repl return value is auto exported as `*result*`,
this return value if not `:nil` is embedded in the document.

Docs `file` section handler now takes optional start and end markers. Docs can
now `snip` a small section of a file for inclusion.

------

Remove the double macroexpand calls that `defun` and `defmethod` have been
doing ! Fairly embarrassing this one, as it's been like that for years now.

New Editor collection actions. `action-collect` and `action-collect-global`.

New `(escape string)` charclass lib function. Editor now uses this to auto
escape the pattern if in regexp mode when using `action-set-find-text`.

New Docs application section handler for widget embedding. The `editor.md` and
`viewer.md` docs show the idea.

New API for the `(matches)` and `(substr)` functions. Simplified to just return
a list of slices for each match. Just the slice ranges, and no substrings
stored.

------

Profile app updated to use a syntax highlighted buffer, and to follow the Debug
app style.

New `(import-from lib ['(sym1 ...) '(class1 ...)])` function. As a result of
this work the `export` functions now take the list of symbols as an explicit
list rather than rest arguments.

New `class/lisp/task.inc` file. This file is imported by the `lisp :init`
method for ALL Lisp tasks. So all tasks started by `class/lisp/run.vp`. While
the `class/lisp/root.inc` environment is shared by all tasks, the new
`class/lisp/task.inc` is a per task environment.

Editor and Viewer apps now have a status bar.

Addition of `-f -c -r -w` options to `grep` command app.

Addition of the `lib/task/cmd.inc` `(pipe-farm)` library. Editor GUI
application and `make docs` now use this library to do multiple command app
farms.

New LPS table search algorithm in `Kmplps` class.

------

If you want to have a play with the Debug stepper service app, then just run
the Template app and then bring up the Debug app. You'll notice that until you
bring up the Debug service, everything seams like normal, then soon as the
service appears, the Template app will break. You can then hit fast forward and
each of the toolbar actions and the min/max/close actions will breakpoint. If
you hit play mode, you can see the 1 second timer demo on Template firing.

Template app also now has a call to `(profile-report)` after the close action.
So you can switch the debug mode to 1, at the front of the Template app source
and check out profiling info on closing the app. Make sure you run the Profile
service app if you want to see the profile results !

------

Improvements to Debug stepper app. Now shows the current executed form, its
return value and a syntax highlighted local environment list.

Addition of `(debug-brk name exp)` conditional breakpoints. Set `exp` to `:t`
for unconditional.

New `rm`, `cp` and `mv` commands apps.

------

Font app now shows tooltips for the character codes. A click on the character
button copies the tip text to the clipboard.

Terminal app saves common history to `terminal.tre`.

Terminal supports dynamic page scaling.

New `lib/text/charclass.inc` module. Parsers and Regexp changed to use this as
standard. A new `(bfind char string) -> :nil | index` function is provided to
perform a binary search find in any sorted char string, which all `(char-class
class_key) -> str` generated strings now are !

New `diff` and `patch` commands. Plus associated `lib/text/diff.inc` library.

------

Editor now has global macro playback to EOF ! Really powerful stuff.

Editor block invert action.

New `memoize` macro. Easy creation of Fmap or Lmap caches.

Editor now has search/replace within selected region option.

Editor can create 10 macro's. The recorder always records into slot "0". You
can save slot "0" to any other slot with shift-cntrl-[1-9]. Playback any slot
with cntrl-[1-9]. Macros can record/playback other macros... Need to add some
protection to that !

Editor saves/loads the 10 macros to the users Editor state file.

Collections now has a generic `tree-save` and `tree-load` system. Lists, Maps
and Sets can now be elements of the tree.

Buffer class now hold a relative bracket nesting change cache per line. Much
faster bracket matching.

New Node widget, a clickable Text widget.

------

Reminder of how to bridge Lisp subnets over ChrysaLib hubs !

Remote machine, say my Raspberry Pi4 at 192.168.1.94, over in the living room:

```code
../../C++/ChrysaLib/hub_node -shm&
./run.sh or ./run_tui.sh
link CLB-L1
```

Local machine, say my MacBook:

```code
../../C++/ChrysaLib/hub_node -shm 192.168.1.94&
./run.sh or ./run_tui.sh
link CLB-L1
```

The link command is typed in at your ChrysaLisp TUI or Terminal. Notice that we
run the hub_node's in the background.

------

Editor global undo and redo ! Use with caution.

Fixed bug in Editor macro recording.

Addition of `tree-load` and ` tree-save` to collection functions.

Regexp now supports allmost all Vim shortcuts:

```code
^  start of line
$  end of line
{  start of word
}  end of word
.  any char
+  one or more
*  zero or more
?  zero or one
|  or
[] class, [0-9], [abc123]
() group
\  esc
\r return
\f form feed
\v vertical tab
\n line feed
\t tab
\s [ \t]
\S [^ \r\f\v\n\t]
\d [0-9]
\D [^0-9]
\l [a-z]
\u [A-Z]
\a [A-Za-z]
\p [A-Za-z0-9]
\w [A-Za-z0-9_]
\W [^A-Za-z0-9_]
\x [A-Fa-f0-9]
```

------

New `(replace seq s e seq)` function in `root.inc`.

Textfield widgets offset text to show cursor.

Editor app tree global expand and collapse buttons. New `:expand` and
`:collapse` methods on Tree widget.

Added replace across all buffers to Editor app.

`defmethod` auto declares the `this` parameter.

Editor search and replace across all buffers.

Textfield `:set_text` method.

Textfield click to focus for key event dispatch.

Editor supports global find across all files. Matching files are inserted into
scratch buffer.

Editor support for block loading of files.

New `(query)` function to build a variable engine instance and pattern based on
whole word and regexp flags provided.

New auto generated Lisp class reference documentation.

New `scatter` map class function to go with `gather`.

New format Editor state file. Nested Fmaps.

Complete refresh of the Viewer application.

------

New `Substr` string search class. Editor updated to use this new class.

New `Regexp` string search class. Supports `\s \f \q \t \r \n \w \b \ . + * ? |
( ) ^ $ []`.

New `grep` command, just `-e regexp_pattern` for now.

New built in search functions, `substr`, `match?` and `matches`. For language
level access to Substr and Regexp `:search` and `:match?`.

New built in iterator macros, `each-found` and `each-match`. For language level
access to Substr and Regexp `:search`.

eg.

(each-found print "abcdefxyz" "abc")
(0 3)

(each-match print "abcdefxyz" "abc|xyz")
(0 3)
(6 9)

Argument quoting added to options processing lib.

Added `static-qq` and `static-q` macros to `root.inc`, these perform static
quasi-quotation and quotation.

Regexp search/replace added to Editor app.

Substr and Regexp `:search` now returns submatches. So `(matches submatches)`.
Submatches are your `$0 $1 $2` etc.

```lisp
(. +*regexp* :search "abc67894nndndfnd890" "([0-9]+)")
(((3 8) (16 19)) (("67894") ("890")))

(. +*regexp* :search "abc67894nndndfnd890" "([0-9]+|[a-z]+)")
(((0 3) (3 8) (8 16) (16 19)) (("abc") ("67894") ("nndndfnd") ("890")))
```

------

Rename `pupa.inc` files to `env.inc`.

New `lib/files/files.inc` library for easy folder and file enumeration. Changed
all apps and commands to use it.

New Tree widget `:populate` method.

Docs app redesign by Bannanearwig, to use a Tree widget. Plus reorg of docs
folder.

New Tree class `:select` method.

Addition of key events to Docs app.

`make install` now exits and auto calls `./stop.sh`. No need to CNTRL-C
anymore.

Added dynamic font scaling, `cntrl-[ ]`, to Docs app.

------

ChrysaLisp and ChrysaLib now work together, using the same communication
protocols. As a result the ChrysaLisp `usb-links` branch has been deleted as it
is now redundant.

ChrysaLib now provides IP, USB and SHMEM link connectivity, you can setup a
backbone network with ChrysaLib's `hub_node` and launch ChrysaLisp subnets that
come and go freely.

ChrysaLisp subnets can share work and services with themselves and ChrysaLib
services provided in C++ can also be seen and used.

------

Pixmap pixel type import and export conversions now auto generated.

Pixmap type code used to cache premul/argb status. This fixes a problem with
image file conversions where switching through premul format can loose
information.

Simplify Service naming and implement `"*"` prefix to allow global discovery.

Simplify RPC calls. And rename of `(task-mailbox)` to `(task-mbox)`.

`sdir` command now defaults to prefix `'*'`.

New `Local` task class in `lib/tasks/local.inc`, for assigning a local
dynamically growing Farm of worker tasks.

------

Host GUI compositors can now reduce memory usage for glyph textures by 75% and
improve performance on glyph blits.

`gui_raw.cpp` driver switched to save 75% glyph memory and faster red/blue
channel calculations.

`gui_fb.c` driver updated to use same 8bit glyph textures.

New `errors.md` doc.

New `cscript.md` doc.

Separate stack scoped variables into its own module `lib/asm/scopes.inc`.

------

Renamed the `vp-vec` op to `vp-simd` as this carries better info at the source
level.

Recoded the `'region` class methods and the `'host_gui :composite` method to clarify
what happens and use `vp-simd`.

Added strength reduction optimization to the `emit-mul-cr` operation.

Addition of basic `text` command support for the SVG import library. Along with
this added a new `clock.svg` test image and the `OpenSans-Bold.ctf` font.

More optimization for the SVG parser, and changed the `(path-transform m3x2 src
dst)` function to follow the SVG m3x2 transform format.

Simplification of the `*compile_env*` and `*func_env*` variable bindings and
cycle breaking on errors.

Bug fix to `'nums :dot` and `'fixeds :dot`. It should have been using the first
vector input to dictate the length of the dot product. This was found while
optimizing the SVG `(mat3x2-mul-f)` function.

------

New keyboard cooking system. ChrysaLisp now takes on the work to map raw scan
codes to modifier key states and country code cooking of keycaps.

Supported so far:

`lib/keys/macbook_uk.inc`
`lib/keys/microsoft_uk.inc`
`lib/keys/macbook_us.inc`
`lib/keys/microsoft_us.inc`

The `microsoft_us` module is just a copy of the UK file for now until some kind
sole in the USA edits it and pushes a PR.

To switch keyboard modules, change the value of `*env_keyboard_map*` in the
`usr/Guest/pupa.inc` file.

Test user account added to check test cycle of Login/Logout.

------

New GUI Logout app available from the Launcher and from the SDL Window close
button. This will run the `apps/logout/app.lisp` application and if the user
confirms the wish to exit the system then an RPC call to the GUI service will
be made to quit the GUI.

Currently when the `host_gui_deinit` call is made this will also call `exit(0)`
! This is a temporary stop gap while things catch up. This is however VERY
convenient for those running full screen and in FRAMEBUFFER mode.

Along with the exit app, the bash scripts will auto call `./stop.sh` if you are
running the system with the `-f` option ! Another convenience.

------

First stage in new portable compositor API.

`gui_sdl.cpp` file is now where the new host compositor via SDL lives.

`gui_fb.cpp` file is where the new host compositor via Linux FB will live !

Extra make options, just add `GUI=sdl` or `GUI=fb` on the Linux `make install`
line. It will default to `GUI=sdl` if nothing specified or invalid.

We have had a fantastic contribution of the Linux FB compositor driver from
Greg Haerr ! Many thanks Greg for doing this and for showing folks that a
Raspberry Pi3 is actually a seriously fast device ! Folks seams to forget this
as the world moved on with layers and layers of software (pun definitely
intended).

`-f` option added to the run scripts to launch GUI task in the foreground. This
is for Frame buffer mode so that the TTY driver goes to the GUI event
processing. So for Pi4 Frame buffer mode I'd recommend `./run.sh -f`. For now
the ESC key will exit back to the shell immediately ! This will be improved
shortly.

------

Fix to make VP and C-Script optimizers immune to prebinder policy with regards
to quasi-quotation. This removes the ordering issues with regards to the vp and
cscript include files, but open a wider debate on the prebinder quasi-quotation
policy in general. I will add a TODO item to review the current policy.

prebind no longer steps into quasi-quotation ! See above.

prebinding of quote, quasi-quote, lambda and macro symbols !!!

`root.inc` predicates have been changed to functions rather than macros.

------

Addition of `test` option for the `make` command. If you wish to run a quick
make test for your platform type `make test` in the Terminal app or TUI prompt.
This test will do several full cleaned builds for your current platform and
give you the stats.

New NetSpeed app ! Measures currently available VP register, memory and reals
ops/s performance for each node on the network. This is a dynamic measurement,
if you are running other processes then you will see the values react
accordingly.

New Hchart UI widget. Netmon and NetSpeed apps changed to use this and
restructured the source while doing so.

Introduction of new `errorif` macros to ease Lisp function argument error
checking.

------

Update for the Netmon app.

New `time` terminal command to allow simple timing of the duration of `stdin`.
Use like `make it | time` etc.

Removed `(assign)` register key symbol equate creation. This was hiding a huge
number of bugs in the source. It instead now allows normal register equates,
and all the extra `(get r)` calls have been removed from the assembler `vp.inc`
file.

Introduce the `*func_env*` environment. Changed `code.inc` to use this, and VP
register equates. Auto cleared by destruction of the environment at
`(func-end)`, so no need for the `*func_syms*` list or `undef` of that lists
contents.

------

Prep for RISCV64 port. :) The `aarch64` cpu folder has moved to `arm64` to
follow Apple M1 standard. The RISCV64 outputs VP64 binaries for now, just to
exercise the framework. The `make install` should now work and a session should
run provided you specify the `-e` option. ie `./run_tui.sh -e` or `./run.sh -e`

Riscv64 work is progressing, still a few issues but going well so far.

Fix to parsing of 64 bit `0b` numbers.

Riscv64 port work is done. Further work can be done on the target platform if
needed etc. TUI runs in native on Linux Riscv64. :)

The Riscv64 emit file is a really good example of how you should go about doing
a port. Due to bug tracking and experience over several ports. This is the
cleanest emit file yet. The mask creation and bit-field composition macros
allow you to follow the manual and get it correct. I should have done this
sooner. Will retrofit this idea on the other ports over time.

Many thanks to Martin Wendt for his tireless remote test cycle to allow me to
get this done with no hardware. So, now we have 5 platforms building in under 5
seconds (on my old MacBook). A good thing (tm).

Install build on VisionFive2:

https://www.youtube.com/watch?v=xZGjFP0gNBY

Then native build:

https://www.youtube.com/shorts/DhOC7wWRcnk

------

Enhancements to `(env-push [env])` and `(env-pop [env])` to take an optional
environment to act on. Defaults to the current environment as before.

Changed `(env-resize num [env])` to take optional environment, default to
current, to match other env functions.

Removed `*debug_mode* 2` as the guard page system won't work under the new
MacOS execution restrictions and since the move to Lisp source for most day to
day coding, this hasn't been used for years now.

------

Remove unsupported source. Available in the repo history if folks want to dig
around.

Added new `(vp-sync c)` instruction for vp level memory barrier operations.
Currently will only support operation 0 ie. full shared memory cores sync.

Fix for Arm platforms task exit code ! After a `git pull` do a `make install`
to rebuild your native image.

Support for `+...` symbols as keys in `(case)` statements.

New `(font-info font) -> (name size)` function. Returns the name and size in a
list.

------

I'm currently dealing with an ongoing medical emergency, not myself, but family
member. So things may be a bit quiet for a while. But the project is not dead,
just in slow mode until life gets back to normality.

Update: My partner and love of my life died suddenly after a fight with Acute
Leukemia, I will forever miss her. I will get back to the project, but can't
give a date yet. Please bare with me till normal life can resume. Thanks.

Update: Life just won't stop kicking at the moment. A few weeks after my
partner died, my sisters husband has died in circumstances that have lead to a
lengthy inquest. My focus is my sister at this time, thank you.

------

Rename `(elem)` to `(elem-get)`, will now be inline with new `(dim-get)` and
`(dim-set)` multi dimensional element get and set functions.

New `(dim (nums x y z ...) array)`, `(dim-get dim (nums x y z ...))` and
`(dim-set dim (nums x y z ...) obj)` built in functions.

VP pseudo instructions `(vp-abs) (vp-min) (vp-max) (vp-vec)` added and the
`sys/math/class.inc` vector DSL now uses this to perform operations.

Separated out the source and destination vectors on the `(vp-vec)` instruction.

New value Spinner widget. Added parameter spinners to the Pcb app.

Auto sizing Grid widgets, set either `:grid_width` or `:grid_height` to 0.

------

Start of XML, SVG import libs. Plenty more to do yet.

New '(Lmap)' linear map class.

Moved all the Canvas loading and saving to Lisp.

New 'sstream :claim_string method.

New `(vp-cstr)` VP pseudo op.

Register key symbols for call to call anonymous register passing.

VP VM register symbols renamed to :r0 to :r14 and :rsp keywords.

Assembler and Translator stages separated out into their own libs.

Join us at #ChrysaLisp-OS:matrix.org

------

New `(mat4x4-invert)` function and Scene graph class updated to use this to
perform lighting and back face culling in object space.

New `(Iso-capsule)` surface class.

`(time-in-seconds time)` promoted to `root.inc`

Mesh demo now streams mesh data for the scene graph nodes via a Mesh loader
Farm.

Multi register ops promoted to `vp.inc` file.

Polymorphic number conversions. `(n2i)` `(n2f)` and `(n2r)`. These replace the
old functions.

Simplify Edit, Viewer and Terminal apps by having a main vdu subclass. The
event loop code can then be shared, plus better partitioning of the
application.

New `(export-symbols)` and `(export-classes)` macros in `root.inc`.

New GUI Edit widget. Editor, Viewer and Terminal apps changed to use this.

New `(env-resize env num_buckets)` function. Resizes in place the `hmap`
buckets. This works on subclasses of `hmap` as well ! ie Widgets.

------

Start of new Cubes demo. Along with a new `lib/math/matrix.inc` library.
Renamed the `lib/math/math.inc` library to `lib/math/vector.inc`.

New Molecule demo. Renders standard SDF Mol files. Some SDF files have an issue
with no space between the number of atoms and the number of bonds, so add a
space if required.

New Lisp level co-op `(task-slice)` function. Will do a deschedule if the
current thread has been running for over a millisecond. More flexible strategy
for a co-op system than just calling deschedule.

VP versions of dot product `(nums-dot)`.

Real format fixes to overflow conditions and some optimisations to multiply and
conversions to fixed and integer.

Addition of `(static-q form)` macro to `root.inc`. Performs a macro and prebind
expansion of the given form and then wraps it in a quote for runtime.

`(eql)` and `(find)` can now be used on vectors. As a result a new `(Fmap
[num_buckets])` class is available as standard from `root.inc`. This map type
uses the `(find)` call to search the buckets.

`(find)` now follows strict behavior of `(eql)` for element tests.

New `(Fset [num_buckets])` class.

New `:find_matches_case` method on Dictionary class. Editor now uses this for
word matching.

New Canvas class `:tri` method.

------

Optimized versions of the bracket matching methods on the Buffer class. 25%
faster, but these functions are a good candidate for a VP string method !

Updated Pcb viewer with tooltips.

Updated remaining GUI apps with tooltips.

Mouse/shift selection and cut/paste/copy added to Textfield widgets.

New macros.md document.

Correct the quote skipping in the `(split)` function.

New `classes.md` document.

Addition of `(let* ...)` macro to `root.inc`.

View class :ctx_panel method moved to Lisp code.

(canvas-lighter) and (canvas-darker) functions moved to Lisp.

Removal of redundant VP class methods. A lot of code has moved over to Lisp
recently and so these methods are no longer referenced.

New `event_loops.md` doc.

New `event_dispatch.md` doc.

New `widgets.md` doc.

New application Template. Copy to new folder to get the framework for your new
app.

------

Editor now has line number display.

New `(mail-validate netid)` function. And with this, the GUI now validates the
owner of each top level GUI view and if they fail to validate they are removed.
This means that any process that opens a Window and then throws an uncaught
exception will have that Window removed and cleaned up.

Much faster search index creation on Buffer class.

Added Macro playback till EOF action to Editor. Demo screen recording in
`Macro_Playback_Till_EOF.mp4`

:find method on Buffer class now returns the buffer index list directly. Much
faster.

More robust exit conditions on `(action-macro-to-eof)` function.

GUI now runs its own native mouse cursor.

More flexible `(case)`. No longer restricted to symbol only keys.

`(case)` now checks if all the clauses are atoms and if so does not do an
`(eval)` of the case clause. Which makes the code produced a tight static flat
map in that situation.

`(raise)` and `(lower)` macros added to `lib/class/class.inc`. Adjusted the
macro to allow concatenation of user values.

Extend `(kernel-stats)` to return the amount of memory available on the free
lists. Add an extra column to Netmon to show the amount of allocated vs used
memory.

------

Make system now uses `(abs-path)`.

Editor pulls in `root.inc` to the matches dictionary on startup.

Viewer application now shares most of the Editor engine source. Can copy from
Viewer to clipboard now.

`lib/text/english.txt` database file, 84000 words, available to have in your
working file set if you wish to extend the word matching to include these
words. Folks can add other databases for other languages if they wish to follow
this idea.

Editor now a single instance service. Eventually we could have the ability to
ask an Editor service to perform actions for us remotely, but for now this just
ensures we have a single instance per user.

TUI moved to `apps/tui/`. New version 2.0 of the GUI Terminal application. Can
now copy and paste to and from the Terminal.

Basic event roll up added to Editor, Terminal and Viewer applications.

New `(expand string tab_width)` native function to speed tab expansion for
Editor and Viewer applications.

GUI enter/exit events and general tooltips system.

Whiteboard application upgraded to latest framework.

New `(task-mboxes)` and `(free-select)` functions. To standardise allocation
and freeing of mailbox selection lists. The first element will allways be the
main `(task-mbox)`.

Truncate error report `Obj:` field to 256 characters.

Enhanced error reports, including Lisp stack frame, files and line numbers. To
opt in to this extra tracking add `(import "lib/debug/frames.inc")` to the top
of your application.

------

Find and replace added to the Editor app. Multiple buffers, save all buffers,
rewind all, paragraph reflow, tab in/out block, jump to left/right matched
brackets, select block and live matched bracket hinting.

cntrl-v paste to Textfields.

Viewer app updated to new Edit event dispatch system.

Edit app saves/restores users open file list. Added action-close-buffer.

New `(some-rev)` added to `root.inc`.

Editor saves file meta info.

Tool tips experiment in the Editor.

Mouse wheel support added across the GUI. Apps and Scroll widget.

New `(.?)` macro. Returns `:nil` if not callable else the bound method lambda.

GUI event loop moved out to Lisp. And move the SDL event queue handling out to
Lisp !

GUI_SERVICE event->action map.

Add action-comment-block and action-uncomment-block actions to the Editor.

Lazy colouring of buffers on state restore.

Whole word search toggle in Editor search/replace.

First pass at Intelisense completion in the Editor.

Split the Editor actions out into separate files.

Editor does `(action-save-all)` on app close.

Added relative path accsess to `(import)`.

------

VP and CScript optimizers improved for clarity and extra cases.

Edit app re-write started. Basic editing first stage.

Key modifiers passed in GUI key events.

New text Buffer class. `lib/text/buffer.inc`

New :find_node method on GUI Tree class.

Editor now has cut/paste/copy/undo/redo and has start of multi buffers and
project file trees.

------

Rename 'local-align' just 'align' as it's no longer a function but a simple
symbol.

New Element room for Chrysalisp OS, #ChrysaLisp-OS@matrix.org, unfortunately
Gary Boyd admin of the old room has gone dark, I sincerely hope he is OK. But
we must move to another room where there is admin access.

New `(setf-> msg field ...)` macro and extended `(obj-set) (obj-get)`
functions to ease message creation.

New `(export env sym ...)` macro to go along with `(env-push) ... (env-pop)`. A
new Module technique !

Docs app rewrite to use dynamically loaded section handler modules. Added new
'image' module for embed images and 'file' section to embed source code. Also
rewrote the :text section handler to allow heading underlines and text flow.

Pipe functions re-implemented as a Pipe class `lib/task/pipe.inc`. Terminals
switched to use this new class. Stdio class message structure and pipe startup
sequence simplified.

------

New launch scripts for Windows powershell, that implement the -n -e -b -h
options, care of Martyn Blyss.

Removal of 'task :open_child as now redundant.

VP version of 'task :callback.

New `(env-push)` and `(env-pop)` functions for manual environment handling.

VP assembler functions now use a transient environment between the
`(def-method)` and `(def-func-end)`.

`(vp-rdef)` macro now checks to ensure no symbols are redefined from outside the
function.

------

Added nesting to the `(#)` macro ! Plus arbitrary % parameters not just
starting from %0.

Promotion of `(obj-get)` `(obj-set)` `(obj-ref)` and `(weak-ref)` to
`root.inc`.

`(get-xxx)` macros now uses `(obj-get)` and addition of `(get-nodeid)` and
`(get-netid)` macros.

Addition of +net_id_size+ and +node_id_size+ symbols.

`(structure)` macro promoted to `root.inc` with new `(getf)` macro. Structure
not only creates constant symbols ie `name_field` for the field offsets but
also type symbols `name_field_t` to allow the `(getf)` macro to create the
correct accessor.

No longer enforce constant format on structure member symbols. Standardize on
trailing "_t" for type symbols.

`(def-struct)`, `(def-enum)` and `(def-bit)` now implemented as macros. Deleted
`(def-struct-end)`, `(def-enum-end)` and `(def-bit-end)`.

Introduction of `(enums)` at Lisp level in `root.inc`. Enums fields are not
typed, they have no auto `xxx_t` symbol created.

Introduction of `(bits)` at Lisp level in `root.inc`. Bits fields also are not
typed, they have no auto `xxx_t` symbol created.

------

Host main.cpp pii_sleep function now standardized on usec for time interval
like other the other time functions. As we are no longer using SDL sleep call.

Changed the install network to a 3x3 mesh to not overload the Raspberry PI.

Textfield widget now has :clear_text property. This is mapped to :text property
depending on the value of a :mode (:nil | :t) property.

Implemented `(if ...)` in VP code ! Nice performance boost across the system.

Implemented `(or ...)` as a single `(cond ..)` statement, no more uses
`(gensysm)` symbols for each clause !

Always build the EMU vp64 boot_image in release mode ! This takes 20% off the
install time and shrinks the snapshot.zip.

Improvements to the launch scripts to allow base cpu offset... optional -n, -e
and -b parameters. If the base offset is other than 0, the default, then the
`./stop.sh` script will not be called before launching the new network !

[-n cnt] number of nodes
[-b base] base offset
[-e] emulator mode
[-h] help

Added `link` command to allow bringing up a SHMEM link driver from the TUI or
GUI command line.

------

New `(mail-timeout)` function for building timeout select operations.

New `usb-links` branch with USB transfer cable link support for bridging host
systems.

New `lib/task/global.inc` class for managing a dynamic set of tasks, one per
network node. Netmon app now uses this lib to demo.

New `lib/task/farm.inc` class for managing a dynamic set of tasks. Raymarch and
Mandelbrot apps now uses this lib to demo.

Chess app demo now uses a single child Farm worker for each move calculation.
It is now fault tolerant and will restart the move calculation if it times out
or the node the worker is on dies.

Dynamic bind the `'lisp :run` `'sys_kernel :ping` `'sys_link :link` and
`'sys_link :usb_link` tasks. This drops the smallest boot image by 25KB.

------

New `(type-of)` implementation. Now returns a list of keyword symbols
reflecting the entire class inheritance of the object.

Faster more memory efficient version of the the Lisp class/method system.

Change all mbox structures at the Lisp interface to be net_id_size strings !
This is the first stage of changing all network node id's to be multi-byte
identifiers.

Dynamic assignment of tx, rx link channels and routing/service pings moved to
kernel task.

Random node id's for all network cpu's.

New vp64.inc byte code target ready for dynamic translator and emulator.

New split, slice and gui command apps !

Can now launch Emulated vp64 CPU node networks with `-e` launch script option.
Enjoy.

Total rewrite of the boot system and the way it organizes mmap regions.

MacOS M1 silicon version now running TUI (GUI needs SDL crew update for M1...)
and is showing the fastest benchmark times of any current platform. Under 0.2
second full build time on the MacBook Air !

------

`(stream-seek)` and `(pii-fstat)` support. `(age)` now just a wrapper to
`(pii-fstat)`

Most of the view class methods have now moved out to Lisp ! Huge saving in boot
image footprint ! From 172KB down to 158KB with very little performance impact
while at the same time opening up the entire GUI widget system to Lisp level
coding.

New (obj-ref) and (weak-ref) functions for weak reference support.

New source viewer app and associated Tree widget ! Tree widget can be used by
other applications and is not limited to just directory structure use.

All directory builder functions converted to be none recursive.

New `lib/collections/xnode.inc` library for easy tree structures.

New `pixmap` class that separates the pixel array and GPU upload concept from
the ability to draw on and view a canvas.

`:darker` and `:brighter` methods moved over to the `pixmap` class.

`(find)` and `(find-rev)` now cope with list sequence string and number
lookups.

------

Converged csv, yaml and data exchange libraries to `lib/xchange`. Main includes

* lib/xchange/csv-data.inc  - Reading and writing CSV files
* lib/xchange/msg-data.inc  - Serializing and deserializing data structures
* lib/xchange/yaml-data.inc - Reading and writing YAML files

Replaced `properties` from `xtras.inc` with `Emaps`

Beginning to move the widget code out to Lisp level. Eventually only the GUI
compositor system will be in VP code.

(defun) and (defmacro) now prebind by default. New (defun-unbound) macro for
any situation where this is not desired.

Start of the move of GUI Widget classes over to Lisp classes. Only the time
critical compositor methods will remain in VP code.

Rename (class) and (method) to (defclass) and (defmethod) !

New `(num)` for manual internment of number objects. `(read)` now
interns number objects.

------

New (class), (method), (method) and (.) macro in `class/lisp/root.inc` to
allow OOPS style libraries and classes.

New `lib/hmap/xmap.inc` and `lib/hmap/xset.inc` classes for generic maps and
sets.

New `lib/consts/colors.inc` and `lib/consts/chars.inc` for ARGB and CHAR
constants.

New `lib/text/syntax.inc` class for syntax colouring support for editors and
VDU widgets users.

Docs viewer app now uses syntax colored embedded VDU widgets for `vdu` sections
in the documentation files.

Added (def?) built in function.

------

New !!! hot off the press ChrysaLisp IDE for Windows to start with, but coming
to Mac and Linux soon... https://github.com/PaulBlythe/Chrysalisp-IDE

Promoted (odd?) (even?) (pow) (neg?) (pos?) to root.inc.

(mail-declare) and (mail-forget) now return and take the service entry key.

Start of the Chat app showing use of transient services. Got to correct the
textfield spaces char issue now.

VDU widget now supports full unicode range glyphs and ink/paper attributes !
Just use (array) instead of (str) to create your line entries in the line
buffer you pass to (vdu-load). Lower 31 bits is the unicode char code, bit 31
is an inverse video bit. Top 32 bits is the paper/ink attributes in 1+15 ARGB
color format.

New profiling lib ! `lib/debug/profile.inc`. Whiteboard app nows runs
profiling to demo the output.

```code
Whiteboard App
Fun:		   redraw Cnt:	261 Total ns:	 1481
Fun:		  flatten Cnt:	 13 Total ns:	 1278
Fun:		   commit Cnt:	 13 Total ns:	 1374
Fun:		 snapshot Cnt:	 13 Total ns:	   77
Fun:	 radio-select Cnt:	  8 Total ns:	  132
Fun:			trans Cnt:	  8 Total ns:	   13
Fun:			 main Cnt:	  1 Total ns:		0

Whiteboard Child
Fun:		   redraw Cnt:	820 Total ns:   758926
Fun:			fpoly Cnt:	297 Total ns:   116142
Fun:		  flatten Cnt:	213 Total ns:	21736
Fun:			 main Cnt:	  1 Total ns:		0
```

New PROFILE_SERVICE app to allow multiple profile report viewing.

------

Reminder that #ChrysaLisp@matrix.org IRC chat room is available for all. Highly
recommend the Element open source app for use with this. More often than not
things get discussed there before they make it into the repo and this status
doc. !

New anaphoric lib `(aeach seq body)` macro from Nuclearfall.

Rework of the `(make)` system to remove all the `make.inc` files ! C++ version
to follow.

Wonderful new game demo `Minefield` from Nuclearfall. I am so bad at this
thing....

C++ version now up to date with new (make) system.

(file-stream path file_open_append) mode now available. Make sure to 'make' the
main.c for the host support.

New snapshot.zip bringing the Windows version up to date and providing a new
prebuilt main.exe for folks that want to try the Windows version. Many thanks
to Martyn Blyss aka BananaEarwig for the new build. This should fix the GUI
issue on Windows as well as bring the new (file-stream) open options.

Removed (def:) macro as this is now redundant since the keyword symbols
addition.

Corrected (#) macro after testing with pre-binding turned off.

Enabled (pii-remove) now we have Windows support care of Martyn Blyss.

Removed (defcfun) (defcfun-bind) (defcmacro) (defcmacro-bind) from the compiler
environment. (include) now imports into the `*compile_env*` directly.

Fix for Windows main.c gettimeofday() EPOC calculation. GUI clock app now
displays correctly on Windows.

Fix for render to texture mode GUI on Windows, nice bit of sleuthing with
Martyn Blyss to get that sorted. Folks should join the IRC group to join in :)
A much better fix will come along but this temp fix gets us running for now.

------

(file-stream path [mode]) now reads a file from the filesystem in buffers of
4KB, and has no file size limit as a result. Old behavior is retained in the
new (load-stream path) function which will gulp the entire file into a string
and return a string stream. Lisp IO stream access is moved to the new
(io-stream iopath) function.

(file-stream path [mode]) optional mode now supported (file_open_read,
file_open_write) ! Writable file streams now available.

(intern) and (intern-seq) functions available in root.inc.

(str-as-num) can now parse negative number ! -10, -0xfe, -56.7 etc.

Added `lib/hmap/hmap.inc` for generic Lisp level hash map support.

------

(prebind) will now pre-bind symbols that begin with a '+' character. Lisp
constants that follow the conventional +xyz+ standard will now be bound to the
hard value within (defun) functions.

`yaml-data` now supports reading and writing of fundamental YAML data
constructs. This update also introduced various additions of general functions
to the `xtras` library.

Removed (tuple-get) and (tuple-set) in favour of new constant bindings.

Fixed multi GUI instance launching.

Corrected (seq?) macro multi parameter (eval) issue and converted (first)
(second) (last) and (rest) to functions.

Fix Whiteboard demo after removal of (tuple-set/get).

------

Added (gui-info) to return current mouse position and gui screen dimensions,
and (view-locate) to allow apps to calculate a window launch position. Used
this to generalize Nulearfall's Launcher positioning code for all apps. Apps
now open windows centered on the mouse location while fitting within the GUI
screen, but this can be adjusted if required with an optional positioning flag.

New Iteration doc. Frank got me beating up grass... :) join
#ChrysaLisp@matrix.org if you want to join in the banter. Install the Element
IRC app and join us, don't take yourself seriously but do take coding seriously
!

YAML Serialize/Deserialize library added `yaml-data`. Currently only supports
deserialization of Lists and Properties (dictionaries) as strings. Future
changes will include serializing, native type conversions (numbers, etc.),
as well as support for Anchors, Aliases, etc. to inch closer
to YAML 1.2 compliance.

------

Lots of rework of the service system ! Service (declare) and (enquire) calls
now have no race condition. Also took the opportunity to completely rework the
messages routing system structures and remove the need for (kernel-total).

Most of the (open-xxx) calls have been converted to Lisp rather than VP. No
advantage now and we can easily add more task distribution calls at the Lisp
level. Saved nearly 2KB of boot image.

New (mail-nodes) call to return the current known list of network CPU id's.

Frank has continued to update the new xtras.inc library with various flavours
of tree walkers and converted the argparse.inc lib over to use the latest
properties APIs.

------

Closure style shortcut lambda syntax available as a macro. This may eventually
get promoted to part of the (read) function. Thanks to FrancC01 for inspiring
this addition.

eg.
```code
(map (# (< %0 0)) '(1 2 3 4 5 6 -6 -7 -8 0 7))
(:nil :nil :nil :nil :nil :nil :t :t :t :nil :nil)
```

Anaphoric macros have moved over to the lib/ folder.

New (tolist env) function to convert an environment into a list of list of
pairs.

New (env?) macro available.

------

Keyword symbols now available, any symbol beginning with a : will always
evaluate to itself. This will now start to be used throughout the system.

VP Method call type specifiers are now keywords.

VP Method names are now keywords.

New wc and head commands from FrankC01. Examples of using the new argparse.inc
functionality.

Unordered list support in the docs viewer added by Nuclearfall.

Added `diary.md` doc to show the creation of a new feature as it happens !

New autogen of `commands.md` via 'make docs' for all the commands in 'cmd/'
folder.

Restructure of library files into own directory. Updated all command files
to reflect relocation of included library functions.

Added `csv-data` library to support fundamental reading and writing
csv files.

Moved apps/math.inc lib over to lib/math/math.inc

GUI component properties are now keywords.

------

Created standard File picker, added demo Files browser and used picker to
implement save/load of Whiteboard document files. Picker is now available for
use by all applications.

Added (first), (second), (last) and (rest) macros to the sequence section of
the root.inc file.

------

Added (def:) macro for easy definition of self evaluating symbols.

'find and 'rfind methods promoted to the 'seq interface. (find) and (find-rev)
can now search all seq subclasses.

Promoted the (get) macro from `gui/lisp.inc` to `class/lisp/root.inc`.

Added `cmd/files.lisp` to list files that match a given directory postfix and
file prefix.

------

Renamed class/slave to class/stdio and taken the opportunity to rename
class/vector to class/list.

Switched to static lists and string buffers for the emit buffer variables and
parser lists. Shaved a few milliseconds off the build time.

Add (make-tree) function to the 'make doc' command to include all Lisp binding
files. Filled in all the missing syntax comments so they appear in the
`syntax.md` file.

Added a Lisp.tmLanguage file to the project for those that use VSCode editor.
This is a drop in replacement for Mattn's Lisp syntax colouring extension to
give you ChrysaLisp specific keywords. It really makes a difference to see good
syntax highlighting.

Fixed point equates defined as fixed types and removed the (fixed) macro.

(nums-div) and (nums-mod) now check for div/mod by 0 in debug builds.

Now have (find) and (find-rev) for searching Lists and Strings.

(min) and (max) now just returns a reference to the min/max number, no need to
create a new number as numbers are immutable.

------

Nuclearfall has started a repo of a whole pile of CTF fonts:
https://github.com/nuclearfall/CTFonts go pay it a visit and drop off a star.

As a result I have added sys_pii::dirlist host call. As he deserves to be able
to have the font app find all his fonts easily ! Plus we now have a simple host
dir listing call.

Added basic context aware tab completion to the GUI Terminal app. In command
positions will only look for '.lisp' files within the 'cmd/' folder otherwise
will do system wide maximum extension.

Optimized (case) macro to (prebind) clauses and not wrap clauses within a
(progn) statement if only a single expression.

Lots of extra code added recently, up to 170KB boot image now, on a 500 builds
(make-test) run the mean is now 0.412 seconds on my MacBook Pro. So still under
1/2 second for a full system build.

------

(read) now parses fixed point format numbers directly to fixed number type.

New (cap) function for setting capacity of array types, so array, list, nums,
fixeds, reals, path.

Finished off the Bubbles demo ! Nice use of the new long vectors, showing how
to switch between fixed and real number formats and vectors. Plus it looks cute
:)

Better sys_math::i_random function as that was showing bad repeat patterns.

------

Big changes are afoot with numeric types. They are going polymorphic with
respect to the Lisp interface functions. This will allow apps to switch from
fixed, to reals (and floats and doubles, when they come along), with very
little effort.

Various platforms will have limited capabilities for floats, so reals are a
fast software implemented compromise for IEEE float/doubles on such platforms.

Long vectors of numeric types are also going polymorphic. You will be able to
create numpy style arrays of numeric types and manipulate them with generic
long vector operations at the Lisp level. For example look at how the
`apps/math.inc` file is getting more and more generic.

------

Considerable improvements to the (ui-xxx) macros with standard defaults from
the user environment pupa.inc file.

Whiteboard now has a display list thread that takes care of canvas redraw and
path flattening at whatever the selected display rate is.

Fixed a serious issue with macro expansion. I was expanding into quasi quoted
lists and this was causing all sorts of strangeness. Corrected this issue and
then improved several old macros that had been showing issues with this.

------

Nuclearfall has set up an IRC channel on irc.freenode.net at #ChrysaLisp. The
room should be available 24-7 and I'll keep this open all the time I'm awake.
Users of Riot.im can also access the channel on matrix.org at
#Chrysalisp:matrix.org

New Whiteboard app, with shamed face to Neauoire as it took so long to get
round to doing this. Despite it being my day job, or maybe because it's my day
job !

------

Window component is now just a resizable panel. Soon there will be some extra
ui tree macros to build standard window types with titles and close buttons
with less verbose construction.

Defined some useful standard flow combinations.

------

Added new font::glyph_ranges method and (font-glyph-ranges) function. Changed
Entypo demo to be able to view all font ranges and rename to Fonts.

A few tidy up to Nuclearfalls Edit app now it's merged into master branch.

Added textfield basic editing and cursor.

------

Switched on the task priorities system with Kernel, Link, GUI and Apps priority
bands. Things look good so far so we will run with this setup for a while and
see how things work out.

Added (mail-alloc-mbox) and (mail-free-mbox) Lisp bindings to allow Lisp code
to directly create and use new mailboxes. This tidies up abuse of temp
in-stream objects ! Changed various demos to use them.

Added (read-long), (read-int), (read-short), (write-long), (write-int),
(write-short) macros to simplify stream code and string-stream use for building
messages. Changed various demos to use them.

Reworked the Raymarch demo to use a job que concept and simpler farm code, no
need to use the array of in-stream idea, although that was an interesting
experiment, it's way too heavyweight here.

------

Fixed several instabilities shown up by working on the Raspberry PI4 for a few
days !

Main fix is to make sure that on dynamically loading a new function to call the
new sys_pii::clear_icache method ! ARM does not cope with snooping code loading
like the x86 does and I was assuming that mmap with the PROT_EXEC flag would be
enough, but this is not the case when that buffer is loaded later on with more
functions ! Silly me.

Fixed a misalignment of stat buffer structure within the relocation buffer.

Fixed a missing save of the this pointer in the canvas::lisp_glyph_paths
method.

Fixed a bad bug in sys_mail::mbox_free ! Not even using the correct statics !

Fixes to deinit of mem and heap classes.

More robust startup with shared memory initialization. Removed the race
condition where the link buffer could be cleared while containing live data,
plus no need to clear the TX buffer from the VP side anymore.

Overall, a lot of fixes thanks to the Raspberry PI ! Certainly worth keeping
that platform up to date.

Plus, ground work for a priority based scheduling system !

------

GUI terminal app now supports scroll back line history and arbitrary resizing.
User setting for line history size is `*env_terminal_lines*`. Defaults to 10 *
40 lines.

------

Added a simple Mandelbrot demo, fixed point math for now, in order to test the
32:32 real number format in a more formal setting. Plus Mandelbrots are great
:)

It's a nice demo of a self decomposing multi-child process app too.

------

https://github.com/vygr/QtCTF

Published the QtCTF conversion app for ChrysaLisp font creation. This
eventually needs to be able to have several char ranges supported, but
currently only has one. The CTF format supports many, just this app generates
one for now.

------

Implement ChrysaLisp font rendering, decoded TTF/OTF files into new CTF
ChrysaLisp format glyph data and render using ChrysaLisp's points and canvas
classes.

I'll publish the TTF/OTF format convertor as a separate Github repo once I tidy
up the code. I've done that as a C++14, Qt app.

So there is no dependency on the SDL_ttf or libttf or libfree_type libs
anymore.

Further work needed to lower some of this code to VP, currently this takes an
extra 3KB of boot_image and that could come down a little.

As this if very fresh code, baked over a weekend hack, let me know if any odd
things show up.

------

Added a software float number class `class/real/class.*` and low level support
in `sys/math/class.vp` that supports a 32:32 mantisa:exp format, a good
compromise between IEEE 32 and 64 bit formats but simple enough to be quick to
do in software.

As you can see my approach to this was to create a test app `cmd/real.lisp` to
prove out the idea, then I created the same basic code in VP, followed by the
Lisp level functionality and Lisp bindings in the class library.

There are a few basic extras to add yet, and I'd like to do a demo of this by
providing an option for the Raymarch demo, or perhaps do something like a
Mandelbrot demo, to use fixed or real math. But I've got some text area support
to add to the VDU class first for Nuclearfalls's text editor. ;)

------

Implemented a more general system for repairing damaged regions over any number
of frames. This can now cope with triple buffering etc, but it still relies on
the previously rendered frames being available uncorrupted as they come back to
being the new back buffer !

The setting is now in service/gui/class.inc, `(defcvar 'num_old_regions 1)`,
defaults to 1 for double buffered preserved previous frame rendering.

You can set the value to 0, and rebuild the system, if you can't rely on any
previous frames being preserved, and this will render to a full screen texture
as an internal back buffer and always draw this to the entire screen area each
frame.

------

Local mailbox ids are now never reused ! Plus mailbox destinations are
validated against the list of currently allocated mailboxes. Any mail sent to a
freed mailbox is passed onto the postman task to deal with. Currently this mail
will just be freed, but eventually it may be logged and available as debugging
aids. But the most important thing is that the system will not crash as a
result of old messages still to be delivered after a mailbox dies being sent to
the wrong mailbox !

------

GUI process now sends out a +ev_type_gui event to all top level components on a
GUI resize event. This allows apps to resize themselves or in the case of the
new wallpaper app demo, maybe switch to better fitting assets etc.

Canvas now defaults to centring the texture in the view. Soon I'll add the
flags to align left/right/top/bottom and stretch.

------

Add support for 24bit TGA loading. Support 8bit greyscale .cpm saving.

Support Host window resize and restore. Can now run fullscreen and the screen
rebuilds correctly after a restore from min-sizing.

GUI Terminal has support for line editing including left/right keys and
character insertion etc.

------

Added the canvas::save_cpm function and various support systems. New tocpm
command line app for converting and saving to cpm format. Need to optimise this
over time, but just cranked out a version in the C-Script format for now to get
things going.

Thanks again to Nuclearfall for pushing me into this in the nicest possible way
by working on a ChrysaLisp logo :)

Next stop will be direct svg import for the vector rendering...

------

Added a quick and dirty 32bit uncompressed .tga file importer so we can all
admire Nuclearfall's logo designs :)

I will as a result of this I plan to get the .cpm save routine done and create
a command line image conversion tool to compress these to .cpm format. But I
will keep the .tga import tool around.

------

Fixed an issue with slave class not calling deinit in the pipe abort case. This
only effected stats gathering builds, but it always was incorrectly not calling
deinit.

Nuclearfall contributed a fix to the prompt erasing issue with backspace in the
GUI terminal app. Thank you.

------

Moved the object tracking node into the mem block header. This means that there
is no requirement to use cross compilation to switch build types. You do need
to restart after you switch the *debug_mode* setting as that's in the cached
and shared boot environment ! So remember to restart before doing the 'make
boot'.

As a result there is 8 bytes extra space for use by array classes so they can
now have an extra short form element !

------

Add a profiling build option, *debug_mode* 2, that tracks object creation and
destruction and gathers usage data on object counts.

This could also be used in future for GC by adding a virtual mark method ! I
will think on this as I'm not keen on GC, but maybe a build option for GC vs
ref counting might be an idea for the future.

Currently the standard Github build will have *debug_mode* 2 by default. It
does use slightly more memory and slightly slower though. Swapping between
builds also needs the C++ ChrysaLisp, to cross compile, because the object
sizes change as profiling is turned on/off. This needs some further thought as
I don't like being dependant on the C++ version of the Lisp.

You can view the object stats using the new gui stats app :) Nice to watch the
build benchmarks running and see no objects leaking :)

------

Remove the pipe class and some associated methods ! Now all in Lisp. :) Minor
slow down on system builds but this saves 3KB of boot_image, so going to run
with it.

(pipe-close) waits with a select for all stderr and stdout inputs to stop.

msg_in class can now take an existing mailbox id. This allows the slave class
to reuse the standard process mailbox for its stdin stream rather than create
yet another.

Created msg_out::wait_ack in readiness for a non blocking mode. This function
will allow a stream to catch up with any acks it ignored while in none blocking
mode and deinit uses it to clear up all outstanding ack messages.

------

Rework the assembler to not use (pipe) and open a farm of children that send
results back via (stream-msg-out) streams. Significantly faster !

(catch) now sets the _ symbol to the string form of the thrown error for use by
the eform.

------

Added a Chess font and made use of it in the Chess demo app. Some tidy ups to
the code. Not finished yet, but going to move onto other issues for a while,
come back to this later to add the fully playable game.

Added (obj-set) and removed several field setting and getting native
functions with simple Lisp bindings.

------

Sequenced message streams now available from Lisp apps ! Chess demo shows how
you can use them to simplify process to process commms.

New (obj-get field size|0) function. (obj-set field obj size|0) will come
at some point...

Tidy up the lisp.inc files into 3 separate main areas, sys/lisp.inc,
class/lisp.inc and gui/lisp.inc.

Along with the new (mail-select) and (mail-poll) functions this allows a much
more Lisp centric way of building apps. I'll convert several of the other demos
over to use this idea as I go forward.

------

Chess GUI app now runs the search engine remotely and messages back to the GUI
parent app via a simple sequence message model. Soon this will start to use the
msg_in/msg_out classes direct from Lisp and do stream based (read) (write)
style data exchange !

Fixed the Windows double enter key and esc key problems in the TUI. Plus
compiled the Windows main.exe bringing it in line with the new recursive folder
creation on file_open_write case.

As a result of the above, removed most of the empty folders from the
snapshot.zip file.

------

Made the host main.c file_open_write case attempt to create folders for any
path that fails to open. This will allow the snapshot.zip file to not have to
carry empty folders as the host will create them as required.

Need to test on Windows once I get hold of my Windows laptop again... then I'll
trim the snapshot.zip file of the empty folders.

------

Removed sys_mail:trymail and sys_mail:tryread. Broke out sys_mail::poll from
sys_mail::select and standardised on this way of polling an array of mailboxes.
Made the API directly compatible with Lisp apps....

Tidy up of the msg_in and msg_out stream classes in readiness for the Lisp API
for sequenced streams between Lisp processes. All this is heading in the
direction of a higher level API for Lisp process message data handling and to
allow Lisp apps to drive new process communication models.

Eventually I want to remove the pipe and slave classes entirely, they will just
be a model that the terminal and cmd style Lisp apps employ, but it's driven
from Lisp and not a native feature in the class library.

First demo of this will be the improved Chess demo, told you there was method
in porting that over ! :)

Removed the app specific native code from the boot image and made the relevent
apps Jit compile their native code ! Not quite a virtual binary yet, but not
bad performance using brute force runtime use of (make) !

------

Ported over the simple Chess. This is a bit of trivial fun, but I'll use it to
optimise some of the Lisp and eventually do a proper GUI front end for it, plus
run the child process remotely and that will force me to sort out some better
standard for that.

------

Removed task::yield and made task::sleep 0 do the same things. Reduce footprint
slightly. Plus removed the call to yield from the Lisp while function. This was
being called far too often, so it's now up to the programmer to sprinkle
(task_sleep 0) where appropriate.

Add component::ref which lets Lisp code directly reference object fields.. I've
deliberated allowing this for a while and despite the bad taste it leaves I
can't shake the fact that it makes the Lisp bindings so much easier and faster.
This will allow me to push more and more performance insensitive code out into
the Lisp bindings and reduce the boot_image footprint. Eventually the goal is
for the GUI to only have the time critical View object core compositing in VP
code.

------

Implemented a more generic component connection idea. This gets rid of lots of
specific UI component code, around 2KB of boot_image ! Its also only allocates
the target id array if required so saving a small amount of RAM.

Fixed a silly memory leak in the Windows main.c myunmap function.

Finally implemented the view::hide and view:to_back methods with a minimal
redraw, title drag with the right button now does a to_back and drag.

Added a Freeball demo to thrash the sprite compositing. This shows that you
don't and never did have to have a Window in order to have content on the
screen. Any GUI component can be composited directly, nothing about a Window is
special.

Share GUI textures between canvas's loaded with the load_shared flag. Obvious
but I wasn't doing it before. Clearly saves a lot of GPU memory.

------

Implemented (assign-asm-asm) auto copy type for field access. Now when you
don't put a type qualifier (i ui b ub s us) as an optional third parameter,
assign will attempt to lookup the type of the symbol and use the correct VP cpy
instruction.

This makes things a lot more robust as you don't need to remember what your
type was, it all comes from the field type in your (def-struct). Plus this
removes the need for field access macros, something that was annoying me
somewhat.

So a large amount of source got modified and as I visit more files I will
convert over to the new way of doing things.

This will make the build time a fraction slower, but it's worth it.

------

Implemented nested (quasi-quote), made (list) not copy it's args and (some!)
and (each!) no longer reuse the parameter list within the loop.

Canvas now uses a custom GPU blend mode for the pre-multiplied alpha format so
that saves an entire buffer copy and format conversion per texture upload !
Canvas init and init_shared optimised and also made the edge array a shared
array.

VP instruction are now mostly macro generated.

------

Control statements can now take compounds expressions !

Swap to using standard Lisp syntax for <, >, >=, <=, =, /=, +, -, *, / and %.

Purchased a Raspberry PI4 and checked to make sure everything runs fine on it.
Running a CloudKernels 64bit 18.04 Ubuntu image on it everything worked and it
turns in a full build time of 2.1 seconds ! Not bad at all PI folks. PI3 is
currently managing about 5.2 seconds.

------

Lower all the hmap Lisp bindings to VP, so (def) (defq) (set) (setq) (undef)
(env) are all a little faster, plus this took the boot image size down over 1KB
! Really must get around to using the (env) functions more exotic uses to do
some OOPS stuff at the Lisp level.

------

Revisit the seq Lisp bindings, lowered to VP and removed the second function
param to (each!).

------

Big push on the consistency and ease of doing Lisp bindings to native VP code.
Anything that helps avoid finger trouble and produces tighter code.

Changed the env_arg_type checker function to not trash the Lisp object and args
regs. This allows the release build code to not have to do extra copies just to
allow the debug version to work, plus it gives better code even in debug builds
!.

Ongoing drive to avoid recursive functions. (prebind) and (macroexpand) now
avoid this, but (copy) and (quasiquote) still do so. They will be recode soon
to not do so. And then I will lower the default task stack size.

MacBook full build now at 0.35s, Raspberry PI3 6.0s. :)

Debug boot image sizes:

AMD64 158508 bytes
WIN64 158860 bytes
ARM64 189244 bytes

------

Added a better way of binding parameters from array values to function call
parameters. Worked through most of the Lisp bindings and used them to make the
code much simpler and easier to follow.

Various optimisations as I went along and revisited the source for all these
functions.

------

Some more work on the docs browser to add margins and highlight code words
within paragraphs plus structure the code a bit better.

Added a set of colour themes and used them everywhere, currently a subtle grey
shades look.

Continued to lower functions to VP, and made pre-binding throughout root.inc
the standard and elsewhere a simple call profiler showed would benefit.

------

Implemented a simple docs browser, and that lead to a lot of rework on the GUI
compositor to better deal with idiotic amounts of components being placed in a
flow ! The eventual form of the docs browser needs a new GUI component to
handle blocks of text, but still it did end up with the compositor being better
so I can't complain too much.

Lowered and thought through the macro expansion code again. Can improve this
further eventually by not using recursion on the stack but this version is far
better than before.

------

Finally got round to implementing a heap collector ! You can clearly see the
effect by watching the Netmon app while running builds and so forth etc.

What I didn't expect was the performance gain from this. I suspect this is down
to the collector sorting the free lists into batches that map to each block, as
well as freeing up page table space on the host.

Seeing 0.55s builds now on the MacBook, and 7s builds on the Raspberry PI3. But
most importantly, memory is now freed back to the host OS during runtime !

------

Making some attempts to rename functions and macros to better fit with Common
Lisp. I'm not trying to duplicate the exact functionality, but at least make
things a little more familiar where it makes sense.

Added the (case) macro that helps with building a jump table dispatched case
clauses. Nice use of Lisp macros that one.

Updated the C++ ChrysaLisp to be able to build the latest OS image.

After all the lowering to VP work the MacBook build is now benchmarking at 0.7s
and the Raspberry PI3 build time has dropped from 12.5s to 9.2s, that's a great
result for the effort.

------

Worked through almost all the Lisp bindings to convert to VP. Saved several KB
on the boot image size as a result. Plus a small but worthwhile speed up.

------

Looks like SDL 2.0.9 fixes the 2.0.8 red screen on Mojave problem !!! So I'm
removing the temp fix for that problem. Thanks SDL crew for sorting this out in
this release.

------

A couple of improvements to the Kernel class code, specifically the opts
processing code that now gets used as part of the general process launch
handling as well as the -run boot option.

Will be out of action a few days due to suffering a Vertical Root Fracture and
emergency dental extraction ! Ouch. :(

------

Did a few more class lib tidy ups and VP lowering. Plus converted the TUI to
be written in Lisp ! That one has been on my mind for a while and it got back
around 1.5KB of boot image !

------

Moved the Lisp class bindings out to the classes that provide the functionality
being used. I still want to tidy up the Lisp root.inc file to just include a
set of finer grained lisp.inc files though.

Lowered the stream class Lisp bindings to VP. Now that the bindings are
separated out it's going to be easier to get round to doing this to the rest of
them.

------

Last of the GUI apps, the terminal, converted over to Lisp. I'm going to have
to sort out a better way to handle the mailbox select functionality for Lisp
eventually, the change to pipe::select to get the Terminal app over to Lisp is
a bit of a bodge.

On prompting from no-identd I took a look at Anaphoric macros and agree that
they can be useful but folks should be aware of what the issues are if you use
them. So I've added the obvious ones to root.inc. Maybe they should be going in
a separate class/lisp/anaphoric.inc file ?

------

Not had a huge amount of free time so did a few conversions to VP level code on
some critical Lisp functions. A little extra performance and helps to keep the
size of the boot image in check. Can always knock out a few VP conversions if
time won't allow anything more substantial.

I decide to change the 'sys_mem 'realloc to not trash :r6-:r7. Thoughts on this
are that if your going to end up doing the memory copy then the 2 registers
push/pop is no big deal, but the functions that use this can benefit from
having there iterators held in registers and not stack variables and that's a
far better situation.

------

Implemented a Lisp version of my PCB viewer app. Used it to thrash out some
issues with the circle drawing flatness tests. Plus it's a great demo of what
200 lines of Lisp can do. :)

------

Reorganised the obj/ folder to take advantage of the now common abi binaries !
Saved nearly 100KB on the snapshot.zip file as a result of the shared binaries
between Darwin and Linux x86_64 platforms.

------

Tidied up the source trying to keep to a consistent style for register equated
source with (list) format rather than quasi-quote format.

------

Added support for type 1 pixel types to the .CPM loader. This enabled me to
load the shadow file for the Boing demo. This also means that the stream class
now supports a read_bits method for variable length data reading. write_bits
method will come along soon as part of the .CPM saving routines.

------

Created a list of symbols, `*func_syms*`, that get undef'd at the close of each
function in order to avoid cross contamination between labels and symbols and
raise errors at compilation time when such happens.

------

Added the shared memory link driver code for Windows platform and created
run.bat, run_tui.bat and run_mesh.bat launch scripts. Windows seams a little
slow on starting up the 64 CPU mesh compared to MacOS or Linux, but it does run
just fine. Enjoy.

------

Implemented the ability to draw anti-aliased polygons directly without needing
to super sample the canvas buffer. Both options are now available, even in
combination ! The anti-aliased routine uses an 8x rooks pattern sampling, which
seams pretty good and has good performance. At some future date I may need to
revisit the simple x sort as it's only fast provided there are not loads of
active edges.

------

A fix for the none-blocking stdin on Windows is done. However this shows up
another bug in Windows that you have to press enter twice to get stdin from the
console ! And the ESC key doesn't get sent through to stdin. Seams these issues
are know issues with Windows. I'll keep a look out for any updates and fixes
for this. For now however the TUI situation is restored to normal on all other
platforms and Windows TUI is far more useable than it was.

------

Many thanks to Martyn Blyss for pushing the Windows port forward. We now have
support for running on Windows 64bit. A few things remain to be done to get the
Windows version running a multiple virtual CPU network, but the GUI is now
running and the TUI is able to be used to compile and build images.

Due to Windows not supporting none-blocking reads from STDIN I'm in the middle
of changing things around to deal with this issue, so temporally the TUI can't
run interactive commands. The GUI terminal can do that still, this only effects
the TUI. This is top of the list to fix !

------

Got another hospital visit for the eyes :( Lots of garbage in my vision still,
but getting some useful documentation done and a few things to help out on the
Windows port.

Tried to concentrate on documenting the aspects of the VP and C-Script coding
that most people will be wondering about when looking at the source files. I
know what it's like when you read a statement and a huge lightbulb goes on in
your head. It's so easy to just assume these things are obvious when you wrote
the code to start with !

------

I have a torn retina ! Not sure how this happened, but just had laser treatment
to weld things down. So not a lot of screen time at the moment !

------

Big drive to get the platform isolation interface (PII) as simple as possible.
Started a windows branch for the windows port, thanks to some prompting by
BannanaEarwig.

------

Happy now with the polygon and stroking APIs after playing around with the new
analogue clock face demo. Makes a real difference to the flow of the source
code after rearranging the parameter ordering.

------

Implemented a set of long vector methods on the points class. Thinking along
the lines of numpy. Even though there not specifically for short 2D and 3D
vectors they have helped the Raymarch demo to go lots faster as far less churn
of objects happens.

Got plans to implement a genetic algorithm trained neural network 'evolving
bugs' demo using the long vectors as a way to shake down the API and tune
performance.

------

Implementation of system services allowed me to implement a multi-thread
debugger and single stepping logger. The Boing demo and Global tasks test now
exercise the features. New Debug app is in apps/system/debug/app.lisp, note there is
only Lisp code involved in this app. :)

------

Took a detour to create a C++ version of ChrysaLisp to directly compare with my
hand rolled compiler and format. The Lisp side of that project is now done and
can build the full OS from the same source files.

Based on comparison builds of the ChrysaLisp OS source using its own
compiler/assembler and the C++ version, ChrysaLisp native is around 2.5x faster
than the Clang C++ version.

The C++ Lisp executable on its own is currently 279kb, while the entire
ChrysaLisp OS including its compiler and Lisp and libraries, GUI etc, is 165kb.

https://github.com/vygr/ChrysaLisp-

Regards all

Chris

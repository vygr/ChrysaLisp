# ChrysaLisp

![](./screen_shot_5.png)

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

New `(filter! lambda seq [out start end])` function than replaces
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

Terminal app saves common history to `terminal_state.tre`.

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
`apps/login/Guest/pupa.inc` file.

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

New `(setf-> msg field ...)` macro and extended `(set-field) (get-field)`
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

`(vp-def)` macro now checks to ensure no symbols are redefined from outside the
function.

------

Added nesting to the `(#)` macro ! Plus arbitrary % parameters not just
starting from %0.

Promotion of `(get-field)` `(set-field)` `(obj-ref)` and `(weak-ref)` to
`root.inc`.

`(get-xxx)` macros now uses `(get-field)` and addition of `(get-nodeid)` and
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

New `(num-intern)` for manual internment of number objects. `(read)` now
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
Fun:           redraw Cnt:    261 Total ns:     1481
Fun:          flatten Cnt:     13 Total ns:     1278
Fun:           commit Cnt:     13 Total ns:     1374
Fun:         snapshot Cnt:     13 Total ns:       77
Fun:     radio-select Cnt:      8 Total ns:      132
Fun:            trans Cnt:      8 Total ns:       13
Fun:             main Cnt:      1 Total ns:        0

Whiteboard Child
Fun:           redraw Cnt:    820 Total ns:   758926
Fun:            fpoly Cnt:    297 Total ns:   116142
Fun:          flatten Cnt:    213 Total ns:    21736
Fun:             main Cnt:      1 Total ns:        0
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
to Martyn Bliss aka BananaEarwig for the new build. This should fix the GUI
issue on Windows as well as bring the new (file-stream) open options.

Removed (def:) macro as this is now redundant since the keyword symbols
addition.

Corrected (#) macro after testing with pre-binding turned off.

Enabled (pii-remove) now we have Windows support care of Martyn Bliss.

Removed (defcfun) (defcfun-bind) (defcmacro) (defcmacro-bind) from the compiler
environment. (include) now imports into the `*compile_env*` directly.

Fix for Windows main.c gettimeofday() EPOC calculation. GUI clock app now
displays correctly on Windows.

Fix for render to texture mode GUI on Windows, nice bit of sleuthing with
Martyn Bliss to get that sorted. Folks should join the IRC group to join in :)
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

Renamed (merge) to (merge-obj) to avoid clashing with Common Lisp and to be
more descriptive of what the function actually does.

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

Added (set-field) and removed several field setting and getting native
functions with simple Lisp bindings.

------

Sequenced message streams now available from Lisp apps ! Chess demo shows how
you can use them to simplify process to process commms.

New (get-field field size|0) function. (set-field field obj size|0) will come
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

Many thanks to Martyn Bliss for pushing the Windows port forward. We now have
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
exercise the features. New Debug app is in apps/debug/app.lisp, note there is
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

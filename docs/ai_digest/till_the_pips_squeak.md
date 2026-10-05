# Till the Pips Squeak: Squeezing the Lisp Engine

The ChrysaLisp Lisp engine was already fast. [Keeping It Hot](keeping_it_hot.md)
describes why: a function call that lives in the L1 cache, single bucket
environments, a slot hint on every symbol, cells recycled from the top of a
free list.

This document is the story of one working day, the 5th of October 2026, in
which that engine was taken apart instruction by instruction and put back
together. At the end of the day, running the same test suite, it executes 40%
fewer instructions, and the recursive core of the interpreter holds a fraction
of what it did on the stack.

Nothing here is a new algorithm. It is a list of things the engine was doing
that it did not need to do, how each was found, and what it cost. Some of the
things tried did not work, and they are here too.

## 1. The Brief

The brief was short. Squeeze the `:lisp` class till the pips come out. Save
register use, save stack use, strip redundant code, and focus on the hot paths.
Work in from the leaf methods, and let the savings ripple inwards. Everything
must work in both a release and a debug build. And one priority above the
others:

> We are wanting to crunch the stack use with no speed loss as the priority. So
> any of the recursive core that can save a stack slot has a great effect on
> things.

A Lisp interpreter is a recursion. `:lisp :repl_eval` calls a function, that
evaluates its args with `:lisp :repl_eval`, that calls a function. Every slot
one level of that recursion holds on the stack is paid at every level, in every
task, on every node. A slot saved in the core is saved everywhere.

## 2. Evidence, Not Faith

Before any change, there has to be a number that the change can move. Three
were used, and each step was measured against all three.

*	**The instruction count.** The VP64 emulator runs the release boot image
	one VP instruction at a time, so a copy of it was given a counter for each
	instruction address. Run the test suite under it, add the counts up by
	function, and the result is an exact profile: how many instructions each
	function ran, and how many times it was called. It does not vary from run
	to run. A change that saves 0.3% shows as 0.3%.

*	**CPU time**, of the test suite run serially in one native node. This
	varies by a percent or two between runs, so it confirms a large gain and
	says nothing about a small one.

*	**`make test`**, the build benchmark, which assembles the whole system.
	Self build speed is what a developer feels.

And a gate. Every step had to pass the full test suite on a native build with
1 node and with 10, on the release emulator build, and on a debug build with 1
node and with 10, and leave the lint, `trace -i -l`, with no warnings. The lint
checks that the registers each function says it trashes are the ones it does,
which matters a great deal when the work is moving values out of stack slots
and into registers that have to survive a call.

Later the profiler learned one more trick, a count for every instruction
inside one chosen function. That is what found the last big gain, see section
8.

## 3. Where the Time Went

The first profile, of the test suite, said that the engine was not slow at any
one thing. It was doing a great many small things that added up:

| Function                | Share at the start |
|-------------------------|--------------------|
| `class/lisp/repl_eval`  | 15%                |
| `class/hmap/pfind`      | 10%                |
| `class/obj/deref`       | 8%                 |
| `class/list/clear`      | 6%                 |
| `class/lisp/repl_eval_list` | 5%             |
| `sys/mem/alloc`         | 5%                 |
| `class/lisp/env_bind`   | 5%                 |
| `class/array/slice`     | 5%                 |

Nearly all of that is the cost of *making a call*, not of what the call does.
Slicing the form, evaluating the slice, taking and dropping references, making
a list, making an environment, looking symbols up, and freeing it all again.

## 4. The Call Path, Step by Step

### 4.1 Args evaluated straight into a new list

A call used to slice its form, which copied each arg form and took a reference
to it, then evaluate that list in place, dropping each form's reference as its
value replaced it, and take and drop a reference of the last value on the way
out.

Now it takes an empty list and gives it each value as it is evaluated. The
list takes the value as it is given, with no extra reference.

This one change was 10.7% of all instructions, and `class/array/slice`,
`class/list/ref_all` and `class/lisp/repl_eval_list` fell out of the profile
altogether.

### 4.2 The symbol lookup, in line

A symbol lookup was `:hmap :get`, calling `:hmap :search`, calling `:hmap
:pfind` for the environment and then for each parent, each of those calling
`:hset :bucket`. Four levels of call, to compare a pointer.

The search is now one emitter, `(hmap-search)`, in `class/hmap/class.inc`. It
holds the loop over the parents, the slot test, and the scan, and only calls
`:hset :bucket` for a map of more than one bucket, which a call environment
never is. `:hmap :get` and `:hmap :search` use it, and so does `:lisp
:repl_eval` itself, so evaluating a symbol makes no call at all.

### 4.3 The prebound function is not evaluated

`:lisp :repl_bind` runs over each form as it is read, and replaces the symbol
in the function position with what it is bound to. So by the time a form is
evaluated, its first element is nearly always a `:func` object, or the lambda
list of a user function.

The engine evaluated that first element anyway, by calling `:lisp :repl_eval`,
to be told that a `:func` object evaluates to itself. For a lambda it was
worse, the lambda list was evaluated as a call of the `lambda` special form,
which went through `:lisp :repl_apply` to a function whose whole job is to
return its argument with a reference.

Now it looks at what the first element is. A `:func` object is used as it
stands. A list that starts with the prebound `lambda` is used as it stands.
Only something else, the rare general case, is evaluated.

And because the form holds its prebound function, the engine does not take a
reference to it, and does not need a stack slot for it. It finds it again from
the form when it wants it.

### 4.4 A special form is a jump

`(if)`, `(cond)`, `(while)`, `(defq)`, `(setq)`, `(quote)` and the rest are
special forms, they are given the form itself, not evaluated args.

They were called through `:lisp :repl_apply`, from a `:lisp :repl_eval` that
had built a 40 byte frame to hold things over the call. But there is nothing to
hold. The value of the special form *is* the value of the eval. So it is now a
register jump, with no frame at all, and the special form returns directly to
whoever called `:lisp :repl_eval`.

### 4.5 A built in function is called directly

A built in function was applied through `:lisp :repl_apply`, which looked at
the function to find out it was a `:func`, and jumped to it. `:lisp
:repl_eval` already knows it is a `:func`, so it calls the code pointer
itself.

### 4.6 The args list is reused

Every call of a built in or a lambda made a list for its arg values, and freed
it after. That is an allocation, an init, a deinit and a free, to hold two or
three pointers for the length of one call.

The Lisp object now keeps a chain of empty lists, `+lisp_args_pool`. `:lisp
:repl_eval` takes one from the chain, and when the call is done, if nobody else
has taken a reference to the list, and it has not grown past its own inline
storage, its values are dropped and it goes back on the chain. The link to the
next is kept in the first element of the empty list, so the chain costs one
pointer in the Lisp object.

Only the first call at each depth of nesting ever creates a list. This was the
biggest single speed gain of the day, 13% of all instructions, and it took
`sys/mem/alloc`, `class/list/create`, `class/list/clear` and `class/obj/deref`
down by half or more each.

### 4.7 The environment is reused

The same idea, for the `:hmap` a lambda call binds its parameters in. The Lisp
object keeps a second chain, `+lisp_env_pool`. `:lisp :env_push` takes an
environment from it, `:lisp :env_pop` puts it back, with its keys and values
dropped. The link is the environment's own parent field.

An environment that was captured, with `(env)`, or resized, is not put back,
it is given a `deref` like any object, and lives as long as its references do.

### 4.8 Binding with no search

The environment of a lambda call is empty when its args are bound. So there is
nothing to search for. For the usual call, a list of plain symbols bound to a
list of as many values, each pair is now written straight into the inline
storage of the environment, and the symbol is told its slot.

That slot does a second job. A symbol given twice in a parameter list, `(lambda
(_ _ x) ...)`, must not be written twice. But the only way a symbol's slot can
point at *itself*, in an environment this loop is filling, is if this loop put
it there. So one compare finds a duplicate, with no scan. That, a `&` symbol, a
list to destructure, or an environment that is not empty, goes to the general
code.

This is an emitter, `(lisp-bind-fresh)`, used by `:lisp :env_bind` and by
`:lisp :repl_eval`.

### 4.9 The values are moved, not copied

`:lisp :repl_eval` owns the args list of a lambda call. Nobody else can have a
reference to it. So when it binds the args it does not give the environment a
reference to each value and then drop the list's reference, it *moves* them.
The list is left empty, and goes straight back on its chain, before the body
of the lambda runs.

## 5. The Stack

### 5.1 `this` is not kept on the stack

By contract, a method of the `:lisp` class is given the Lisp object in `:r0`
and gives it back in `:r0`. Every method in the recursive core kept it in a
stack slot, to be able to do that.

But each `:lisp :repl_eval` gives it back, so across an eval it is simply
still in `:r0`. Across a call that keeps to the low registers, it can wait in
a high one. And when it really has been lost, after a call that trashes every
register, it can be fetched again, the task control block has held it all
along:

```vdu
(defun lisp-this (r)
	(fn-bind 'sys/statics/statics r)
	(assign `((,r statics_sys_task_current_tcb)) `(,r))
	(assign `((,r +tk_node_lisp)) `(,r)))
```

Three loads, on the paths that need them, in place of a slot at every level
of the recursion.

`:lisp :repl_error` takes it from there too. It is cold code, and now nothing
has to keep the Lisp object to hand just in case it has an error to report.
Its first input was removed, from the declaration and from every one of its
350 callers.

### 5.2 Drop a value without a call

`:obj :deref` is a call, and a call trashes registers, which is why so much
was kept on the stack. But most of the time the object being dropped has
another reference, the environment has it, or it is a symbol. So:

```vdu
(class/obj/deref-live value tmp 'last_ref)
```

emits the decrement in line, and only jumps to the label, where the real call
is made, if this is the last reference. The usual case costs no call and loses
no registers.

### 5.3 What each form now holds

All of that, together with the changes to the call path, gives this. The
figures are what one level of the recursion holds on the stack, as well as its
return address, while it calls down to the next.

| Form                            | Before | After |
|---------------------------------|--------|-------|
| a symbol, or a self evaluating form | 40 | no call that recurses |
| a special form                  | 40     | 0, it is a jump |
| a built in, while its args eval | 40     | 16    |
| a built in, while it runs       | 40     | 8     |
| a lambda, while its args eval   | 40, and 8 for `:repl_apply` | 16 |
| a lambda, while its body runs   | 40, and 8 for `:repl_apply` | 0  |
| `(progn)`, a body of one form   | 24     | 0, it is a jump |
| `(progn)`, a body of more       | 24     | 16    |
| `(if)`, over its test           | 24     | 8     |
| `(while)`                       | 16     | 8     |
| `(cond)`                        | 32     | 16    |

The 16 bytes a call holds while its args evaluate is the form and the args
list. There is no slot for where it has got to in the form, that is worked out
from the length of the args list so far.

Take a recursive function, `(defun f (n) (if (= n 0) 0 (+ 1 (f (- n 1)))))`.
Adding up the frames above, with a return address for each call, each level of
it held about 150 bytes of stack. It now holds 32: the return address of the
call of `f`, and the 16 bytes and return address of the `+` whose arg is the
next call. The `(if)` and the body of `f` hold nothing, they are jumps.

## 6. The Results

The test suite, run serially in one node, on an Apple M4:

| Measure                      | Start of day | End of day |
|------------------------------|--------------|------------|
| Instructions                 | 15.05 G      | 9.01 G, 40.1% fewer |
| CPU time                     | 1.021 s      | 0.63 s     |
| Stack depth of the suite's parked tasks | 3,288 bytes | 1,608 bytes |

The build benchmark, `make test`, mean time for a full build:

| Machine                      | Start of day | End of day |
|------------------------------|--------------|------------|
| Apple M4, 1 node             | 0.543 s      | 0.328 s    |
| Apple M4, 19 nodes           | 0.076 s      | 0.053 s    |
| Raspberry Pi 4, 8 nodes      | 2.3 s was the best ever seen | 1.45 s |
| Intel i9-8950HK MacBook, 10 nodes | about 0.23 s | 0.18 s to 0.20 s |

The M4 figures were measured at both ends of the day. The start figures for
the Pi 4 and the Intel MacBook are what had been seen on them before the work,
not runs made that morning, the end figures for both were measured, two runs
each.

How the instruction count fell, step by step:

| Step                                                   | Total, of the start |
|--------------------------------------------------------|---------------------|
| Start                                                  | 100%                |
| Leaf methods, frame only for a call                    | 99.3%               |
| Args evaluated straight into a new list                | 88.7%               |
| Symbol lookup in line                                  | 86.6%               |
| Prebound function seen without a call                  | 85.6%               |
| `:env_bind` fast path                                  | 83.7%               |
| `this` off the stack, `(progn)` and the flow forms     | 82.8%               |
| Special form a jump, built in called direct, lambda in line | 77.0%          |
| Args list reused                                       | 67.2%               |
| Environment reused                                     | 63.2%               |
| Bind with no search                                    | 61.8%               |
| Lookup in line in `:repl_eval`, args moved             | 60.6%               |
| General `:env_bind` in registers                       | 60.4%               |
| Boot environment in buckets                            | 59.9%               |

## 7. What Did Not Work

Not every idea paid, and the ones that did not are as much a part of the
method as the ones that did. Each of these was built, measured, and removed.

*	**Looking the function symbol up in line.** The first thought for the
	function position was a fast symbol lookup. It gained nothing, because
	the function position is not a symbol by the time it is evaluated, it is
	prebound. Seeing that led to 4.3, which did pay.

*	**A key mask on each environment.** A 64 bit mask on each call
	environment, a bit for each key, so that a search could pass over an
	environment that could not hold the key with one test. It was right, and
	it cost more than it saved. The search was not spending its time walking
	small environments.

*	**Growing a map when a bucket gets long.** The guess was that the
	environment of a task, 19 buckets, was filling up. It triggered 24 times
	in a full build and changed nothing.

*	**Rewriting the general bind.** The general path of `:lisp :env_bind`
	was rewritten from script variables to registers on the guess that the
	script variables were its cost. It is cleaner and a little shorter. It is
	0.2% faster. The guess was wrong.

The last three have one thing in common, they were guesses at where a cost
came from. What ended the guessing is the next section.

## 8. The Scan Nobody Saw

`:hmap :search` was averaging about 200 instructions a call, in a full build.
A search that finds its key at the hinted slot of the first environment is
about 20. So something was scanning, a lot, and three guesses at what had
missed.

So the profiler was made to count every instruction inside that one function.
The answer was in the counts of the scan loop:

*	1.8 million searches in a build.
*	45 million steps of the scan loop.
*	4 thousand of the scans found their key.
*	42 thousand searches ended with the key not found at all.

The boot environment, where every built in function and every function in
`class/lisp/root.inc` lives, is one bucket of 828 symbols. That is deliberate,
a symbol's slot hint finds it there with no hash and no divide.

But the boot environment is the last map of *every* search. And a search for a
symbol that is not bound anywhere has to scan all of it to know. The reader
does exactly that, all the time. When it expands and binds a form, it looks up
the first element of each list, to see if it is a macro or a function. The
first element of a parameter list, `(a b c)`, is `a`. The first element of
`(:r0 :r1)` is a keyword. Neither is bound. Each one was a walk over 828
entries. 42 thousand of them is most of those 45 million steps.

The fix is one line at the end of `class/lisp/root.inc`:

```vdu
(env-resize 509)
```

The boot environment still loads as one bucket, and is then spread over 509. A
symbol that is not there now costs a look at one short bucket. A full build
runs 4.4% fewer instructions, and `:hmap :search` went from 198 instructions a
call to 86.

## 9. What Was Learned

*	**Measure what cannot be argued with.** An exact instruction count turned
	every question into a number. Most of the steps above are worth one or two
	percent, far below what a stopwatch can show, and they add up to 40.

*	**The cost was in making the call.** Not one of these changes makes any
	Lisp function do its work faster. All of them remove work that was done to
	get to the function and back.

*	**Know what is already true.** The form holds the prebound function. The
	environment of a call is empty when it is bound. The engine owns the args
	list. The task control block has the Lisp object. Each of those was already
	a fact, and each removed code once it was used.

*	**Reuse beats allocate, however fast the allocator.** The cell allocator
	is a few instructions. Not calling it at all is fewer, and there is the
	init and the deinit that go with it.

*	**The common case in line, the rare case out of line.** A value nearly
	always has another reference. An environment is nearly always not
	captured. A call is nearly always plain symbols. Each has a short straight
	path, and a label to jump to when it is not so.

*	**A guess is a place to look, not an answer.** Three plausible guesses
	about the search were wrong. One afternoon of counting was right.

## 10. Care Points

These are the things the new engine relies on, that the old one did not.

*	**The form keeps its function alive.** `:lisp :repl_eval` takes no
	reference to a prebound function. It already relied on the form staying
	as it is while its args were evaluated, this leans on the same thing. Code
	that rewrites the first element of a form that is running, in place, would
	break it. A function that has to be evaluated still gets its reference.

*	**The two chains are not bounded.** A task keeps as many empty args lists
	and environments as its deepest nesting ever needed, till its Lisp object
	is freed. Each is one small cell. One that grew past its inline storage
	is freed as before, not kept.

*	**The stack figure is not a high water mark.** In a release build the
	stack figure from `(kernel-stats)` is the depth of the tasks that are
	parked, at the moment it is asked. It shows that the frames have shrunk.
	It does not show the deepest any task has been. A debug build scans each
	live task's stack for the canary, which is nearer, and a validate build
	checks the margin on every function entry.

*	**The floor of the task stack is the host.** A host call runs the host's
	C code on the stack of the task that made it. On the Mac, a task stack of
	3,072 bytes runs the test suite and a build, and 2,560 does not start. The
	Lisp engine is no longer the larger part of what a task needs.

## 11. Where to Look

| What                                         | Where                            |
|----------------------------------------------|----------------------------------|
| The evaluator, args, special forms, lambda   | `class/lisp/repl_eval.vp`        |
| `(progn)` and the flow forms                 | `class/lisp/lisp_progn.vp`       |
| `:env_push`, `:env_pop`, `:env_bind`         | `class/lisp/env_bind.vp`         |
| `(lisp-this)`, `(lisp-bind-fresh)`           | `class/lisp/class.inc`           |
| The two chains, `+lisp_args_pool`, `+lisp_env_pool` | `class/lisp/struct.inc`   |
| `(hmap-search)`                              | `class/hmap/class.inc`           |
| `(class/obj/deref-live)`                     | `class/obj/class.inc`            |
| The boot environment's buckets               | the end of `class/lisp/root.inc` |
| The call, phase by phase                     | [Keeping It Hot](keeping_it_hot.md) |

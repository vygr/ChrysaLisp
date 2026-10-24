# The Source Service

A Lisp task can be sent to run on any node of the mesh, on another machine,
and read its files from the machine that sent it. The node it runs on needs
none of the task's own files, Lisp or native. NetMon and NetSpeed send their
children to every node this way.

| File | What it is |
|---|---|
| `service/source/app.lisp` | starts the service, if this system has none |
| `service/source/app_impl.lisp` | the service: a file asked for, its text |
| `service/source/app.inc` | the calls of a task that sends, and of one that is sent |
| `tests/system/test_source.lisp` | the test |

## One to a system, said to nobody else

Each system has one `@Source`, started beside `@Lock` by the login and by the
Terminal. An `@` name is seen only by the tasks of its own system, so the
service is not said to the mesh. A machine that sends a task gives that task
the service's mailbox, and nobody else needs to find it.

## What is sent

`(source-task path)` makes what is opened in place of a path, three lines of
Lisp:

```vdu
(import {service/source/app.inc})
(source-import {8921786E6C1619AE9B9398F23F3CD8F6} {6300000000000000DA2C6CF9EBB80719DCBD53AE46FD80D1})
(import {apps/system/netmon/child.lisp})
```

The first is the sending system's id, the second the mailbox of its
`@Source`. NetMon makes this once and opens each node's child with it:

```vdu
(open-task (source-task (cat *app_root* "child.lisp")) node +kn_call_pin 0 mbox)
```

A system with no `@Source` gets the path back, and the task reads its files
where it runs, as any task does.

## Where it runs

The kernel starts a Lisp task for anything that begins with `(`, and runs it
as it would a file, a form at a time: each form is read after the one before
has run.

1. `service/source/app.inc` is imported, from this node's own tree.
2. `(source-import sid svc)` defines, in the task's own environment, the
   sending system's id and service, and an `import` of its own. It shadows
   root's `import` for everything read after it.
3. The file is imported by that `import`.

The task's `import` is root's but for where it reads from. A module is
imported once into an environment, `*module_<path>*`, and a relative path is
from the module that imports it, as before. It reads by
`(source-stream sid svc path)`:

* On the system that sent it, the ids are the same, and the file is read as a
  file, `file-stream`. Nothing is asked.
* On another, the `@Source` of the system it runs on is asked, `%`, once,
  for the file and all it imports, all the way down. Each comes back as `L`,
  the copy there is the same, read as a file, `T` and the text, read as a
  `string-stream`, or `n`. The task keeps what it is given, and its imports
  after are read from that with no asking. Errors name the file and line as
  ever.

## One machine asks once

The `@Source` of each machine is the broker for its tasks. Asked for a file
of another system, `%sid svc path`, it asks that system's `@Source` once,
`&`, for the file and all it imports, the hash of each, and compares them
with the copies it has. The text is asked for, `*`, only of those that are
not the same. What comes back is kept, and the tasks that asked meanwhile,
200 children of a farm say, are all answered as it comes; every one after
is answered from what was kept. The manifests of natives are kept the same
way. So the system that sent the tasks is asked once for each tree, however
many tasks it sent. What is kept is of that `@Source`: one started again has
another mailbox, and is asked anew; a file changed under one that runs on is
not seen till then.

The service keeps the hash of each file of its own till the file changes,
and of each function its hash and the functions it calls till its bundle is
built again, so a bundle is read and parsed once. `!` says what it has done:
the asks of tasks, those answered from what was kept, and the asks it made
of other systems, as `tests/system/test_source.lisp` counts them.

Once its files are read the task runs its `main`, as any task does.

## Natives

A task of another system has a function server of its own, named by that
system's id, in memory, see [function servers](../vm/vp_functions.md). Its
`ffi` makes the native ready first, `(source-native path)`, in one asking of
this system's own `@Source`, started if it has none:

1. That service asks the sending system's `@Source` for the manifest of the
   function, `#<mode> <path>`: its name and the hash of its VP, and the same
   of every function it calls, all the way, not the system's own, what a
   boot image has, which is each machine's own.
2. Each line is looked at here. A hash this node's own VP has is the node's,
   server zero's, the waypoint of this machine, nothing is fetched for it; if
   the node has not built it, it is translated from the node's own VP into
   the node's place. A hash the cache has, `obj/<cpu>/<abi>/cache/<key>`, is
   that native. The key is the hash of the VP and this translator's id, the hash
   of its files, so a translator changed is a miss. Not the build mode: VP
   that differs by mode has another hash, and what is the same in two modes,
   most below the Lisp bindings, is translated once.
3. The VP of those not here is asked for at once, `$<mode> <path> ...`, only
   those functions, and each translated and kept in the cache.
4. The natives not the node's own go back to the task, one after another,
   and it places them in its server, `(task-place)`, and binds.

The translator is the service's, made the first time one is wanted and kept:
the op and register lists of the VP, `lib/trans/vp.inc` and the CPU's
emitter, none of the assembler. One request at a time, so a native asked for
twice is made once. Only what the node has not the same of is placed: two
copies of a class on one node, two vtables, and an object of one is not an
instance of the other. And the node's own that call, all the way down, one
that is placed: they are given too, the node's translation, to be linked in
the task's server to the task's own. A name is a contract, which copy a link
gets is the source's. A class with a method of that system's own is then the
task's copy, vtable and all, as above.

A system that may not keep translations on its disk, for its security, or
that has no disk of its own, says so in its root environment,
`*env_source_cache*` `:nil`: nothing is written, each native is translated
each time it is wanted, and the node's own are placed as the rest are.

## What the service gives

Lisp that can be imported, `.lisp` and `.inc`, by a path from the root of
the tree, and the manifest and VP of functions. Not a path that starts with `/`
or has `..` in it, and nothing in a user's own folder, `usr/<name>/`:
`usr/env.inc` is the system's. The keys are files of the tree, `mesh_key` and
`keys/`, and are never given. A request of a task for its natives starts with
`+`, the sending system's id, its `@Source`, the build mode and the function.

A request is the mailbox to answer to and the path. The answer is `y` and the
file's text, or `n`. The asker waits 10 seconds, `+source_wait`, on a mailbox
of its own, so an answer that comes late goes to a mailbox nobody reads.

## What it does not do yet

* The node a task runs on needs `service/source/app.inc` itself, for the
  first line of what is sent.
* Only `import` reads from the sender. A `load` or a `file-stream` in the task
  reads where it runs, and so does an `import` inside a function of a library
  that was read before the task's `import` was made.
* An import made at run time, a path worked out as the task goes, is not
  in the tree its file names, and is asked for by itself.
* The translator is this system's own, not the sender's: the same on one
  waypoint.

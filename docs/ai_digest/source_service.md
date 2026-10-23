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
* On another, the text is asked of the service, `(source-ask svc path)`, and
  read as a `string-stream`. Each file the task imports is had this way, the
  ones its files import too, and errors name the file and line as ever.

Once its files are read the task runs its `main`, as any task does.

## Natives

A task of another system has a function server of its own, `source/<id>/`,
see [function servers](../vm/vp_functions.md). Its `ffi` makes the native
ready before it is bound, `(source-native path)`:

* A native this node has built is server zero's, what this machine's waypoint
  has, and is bound as ever. Nothing is asked.
* One it has not is asked of this system's own `@Source`, started if it has
  none. That service keeps a translator for this CPU, made the first time one
  is wanted: the op and register lists of the VP, `lib/trans/vp.inc` and the
  CPU's emitter, none of the assembler. It asks the sending system's
  `@Source` for the bundle the function is in, `obj/vp/<mode>/<path>` in this
  node's build mode, translates each function of it into the task's folder,
  and then each function they call that this node has not built, a name seen
  once so a ring ends. One request at a time, so a native asked for twice is
  made once.

Then the task's `ffi` binds it, from its server. Only what the node has not
built is fetched: two copies of a class on one node, two vtables, and an
object of one is not an instance of the other.

## What the service gives

Lisp that can be imported, `.lisp` and `.inc`, by a path from the root of
the tree, and the VP bundle a function is in. Not a path that starts with `/`
or has `..` in it, and nothing in a user's own folder, `usr/<name>/`:
`usr/env.inc` is the system's. The keys are files of the tree, `mesh_key` and
`keys/`, and are never given. A request for a translation starts with `+`,
the sending system's id, its `@Source`, the build mode and the function.

A request is the mailbox to answer to and the path. The answer is `y` and the
file's text, or `n`. The asker waits 10 seconds, `+source_wait`, on a mailbox
of its own, so an answer that comes late goes to a mailbox nobody reads.

## What it does not do yet

* The node a task runs on needs `service/source/app.inc` itself, for the
  first line of what is sent.
* Only `import` reads from the sender. A `load` or a `file-stream` in the task
  reads where it runs, and so does an `import` inside a function of a library
  that was read before the task's `import` was made.
* Every Lisp file comes from the sender, each time a task is sent, a whole
  file in one answer. There is no cache of them by their content yet, and a
  native once in a server's folder is not asked again if the sender's changes.
* The translator is this system's own, not the sender's: the same on one
  waypoint.

# Evidence, Not Faith: Benchmarking ChrysaLisp

ChrysaLisp makes a series of claims that can seem extraordinary to those
accustomed to traditional operating systems: sub-second full system rebuilds,
seamless cross-platform compilation, and extreme efficiency on hardware ranging
from high-end laptops to low-power single-board computers.

These are not aspirations; they are the measured results of a system designed
from first principles. This document presents the concrete evidence for these
claims, derived directly from the system's own build and diagnostic tools.

## The Anatomy of a 50ms Build: What Actually Happens

When a benchmark reports an entire operating system rebuild in **0.053 seconds
(53 ms)** on an Apple Silicon M4 Max processor booted with 16 VP nodes, one
for each of its processors, which is what `./run_tui.sh -n 0` gives, it is
natural to assume it is merely compiling a few differential modules using a
pre-warmed, monolithic compiler cached in memory.

In ChrysaLisp, that is not what happens. Every build cycle executes a complete,
cold, and hermetic lifecycle across a shared-nothing cluster:

```image
docs/diagrams/build_cycle.cwb
```

### 1. Phase 1: Synthesizing 16 Independent Toolchains Across 16 Nodes

Before the build command executes, **no assembler, compiler, or code generator
exists in memory on any node**. Because ChrysaLisp uses a strictly isolated,
task-centric memory model across its nodes:

* **16 Task-Isolated Syntheses:** The make pipeline (`lib/asm/asm.inc`) uses
  `lib/task/local.inc` to spawn a herd of worker tasks (`lib/asm/asm.lisp`)
  across the 16 VP nodes, one for each node of the machine. Each worker sets
  up its compiler before its first file, about 8 ms of CPU on the M4, so a
  worker more than there are nodes only adds that. Until 2026-10-22 the herd
  was a tenth of the files and one more for each other node, 28 for 16 nodes
  and 65 for 19, and as much CPU went on setting up as on compiling.

* **Biggest First:** The files are queued by size, the biggest given out
  first, so the small ones fill in at the end and the workers all finish
  within a couple of ms of each other.

* **Task-Local `within-compile-env` Environments:** While worker tasks share the
  root environment of their host node, the compilation environment is scoped per
  task. Each of the 28 worker tasks enters its own `(within-compile-env ...)`
  block, independently synthesizing its own private compiler environment from
  scratch in parallel. The 16 worker tasks each do this once.

* **Zero-State Bootstrapping:** In the first fraction of a millisecond, each
  worker task evaluates `lib/asm/`, macro generators (`def-class`, `def-method`,
  `assign`), register allocation tables, and vpx transpilers.

* **RAM-Native Toolchains:** Within milliseconds, 16 complete, fully
  functional native assembly engines are live in RAM across the 16-node
  cluster.

### 2. Phase 2: Distributed Parallel Execution Across 16 Worker Tasks

Once the 16 worker tasks across the 16 nodes have independently synthesized
their toolchains, the build workload is dynamically distributed:

* **16 Host OS Processes:** The host kernel (macOS) actively schedules and
  context-switches 16 separate host processes across its performance and
  efficiency cores, hosting the cooperative task scheduler within each VP node.

* **Dynamic Herd Dispatch:** The master build coordinator dispatches jobs to the
  worker task herd using mailbox messages, dynamically load-balancing work units
  as worker tasks finish chunks and report back.

* **Inter-Node Shared Memory Links (`sys_link`):** Dual-channel circular ring
  buffers (`lk_shmem`) coordinate communication, negotiate channel ownership,
  and synchronize status words (`lk_chan_status_frag`, `ping`, `skip`).

* **Decentralized Load Balancing (`+kn_call_run`):** Child task creation
  requests flow "downhill" across the network like water, seeking nodes with
  lower task counts to spawn new workers.

* **Zero GC Pauses & Deterministic Timing:** Memory allocations hit
  pre-allocated fixed-size heap cell buckets with immediate reference counting,
  completely eliminating tracing garbage collection stalls.

* **Linkerless Image Packaging:** Relative symbolic offsets are calculated and
  packaged into the final boot image without a traditional linker stage.

### 3. Phase 3: Total Teardown and Memory Reclamation

As soon as all compilation jobs finish and each worker task exits:

* **Complete Toolchain Destruction:** Each worker task exits its
  `(within-compile-env ...)` block and terminates, destroying all local symbols,
  macro tables, vpx variable scopes, and code-generation environments.

* **Immediate Heap Reclamation:** All ASTs, intermediate strings, and parser
  structures are dereferenced, and memory cells are returned to the allocator
  via `:sys_mem :collect`.

* **No Lingering State:** The 16 toolchain instances **do not remain cached in
  memory** between build cycles. The next run begins again from absolute zero.

## The Benchmarks: Multi-Platform Build Analysis

The following benchmarks were last re-measured on 2026-10-06, from the TUI,
on three machines, each with one node for each processor, `./run_tui.sh -n 0`.

* An Apple MacBook Pro with an Apple M4 Max processor, 16 nodes. A second
  session of 16 nodes, a GUI, was up and idle on it while these were taken.

* An Apple MacBook Pro of 2018 with an Intel i9-8950HK processor, 12 nodes.

* A Raspberry Pi 4, 4 nodes.

### Test 1: Native Compilation & Distributed Lifecycle (The Baseline)

This test measures the complete, cold lifecycle: synthesizing the independent
toolchains, compiling all source modules, linking the complete OS, and tearing
down all compiler environments.

* **Command:** `make test`

* **Action:** The Lisp application `cmd/make.lisp` executes repeated cold
  rebuild cycles, and reports the mean, the best and the worst.

| Machine | Nodes | Mean | Best | Worst | On one node, mean |
|---|---|---|---|---|---|
| Apple M4 Max | 16 | 0.052 to 0.055 | 0.048 | 0.057 to 0.073 | 0.340 |
| Intel i9-8950HK | 12 | 0.170 to 0.190 | 0.158 | 0.178 to 0.222 | 0.757 |
| Raspberry Pi 4 | 4 | 1.28 to 1.45 | 1.15 | 1.35 to 2.78 | 3.74 |

All times are seconds. The M4 figures are five runs of the benchmark, the
others three. On 20 nodes the M4 gave means of 0.052 and 0.056, so more nodes
than processors no longer helps.

* **Evidence:** On the M4 the best and worst cycle of a run are within 9 to
  25 ms of each other, with no GC pause spikes, allocator fragmentation, or
  JIT de-optimization penalties behind them. At ~53ms, the system can execute
  this complete birth-to-death compilation cycle **19 times per second**. On
  one node, one core, it does so 3 times a second.

These are quicker than the 0.070 seconds, on 20 nodes, that this document gave
until 2026-10-03. The difference is the work on the Lisp engine, see
[`docs/ai_digest/till_the_pips_squeak.md`](till_the_pips_squeak.md).

By 2026-10-22 the source had grown to 142 `.vp` files and the M4 mean on 16
nodes was 0.074 seconds, the worst run up to 0.090. Profiled serially, one
core took 0.40 seconds for the whole tree: VP to native translation 132 ms of
it, evaluating the function bodies 107 ms, the VP optimiser 37 ms, emitting
41 ms, writing the 1,032 function files 40 ms. Two changes brought it back:
the herd of one worker a node, biggest file first, above, and translation
that stops as soon as a pass leaves every label where it was, which saves
a pass of each function, 132 to 104 ms. Every product of all six platforms
was the same, byte for byte. Three interleaved runs each, 16 nodes:

| `make test` on the M4 | Mean | Best | Worst |
|---|---|---|---|
| Before, 2026-10-22 | 0.073 to 0.074 | 0.059 to 0.068 | 0.081 to 0.090 |
| After | 0.057 to 0.059 | 0.055 to 0.056 | 0.059 to 0.066 |

On 19 nodes the mean went from 0.074 to 0.056, and on one node from 0.405
to 0.375. Splitting files into parcels of several was tried and made no
difference: with the biggest first the herd was already even.

The same day, two more. Every function a build makes is a file, 1,032 of
them, written from every node at once, and `(save)` opened each with
`O_TRUNC`. A file cut to nothing and grown again has its blocks let go of
and new ones found, and on APFS the processes doing it wait on each other:
in C, 19 processes each rewriting 55 small files took 79 to 94 ms that way,
7 to 8 ms written over and cut to the length after. The host's `pii_save`
does the second. Same host, the old save against it, three interleaved
runs each:

| `make test` on the M4 | Old save | `pii_save` |
|---|---|---|
| 19 nodes | 0.056 to 0.057 | 0.048 to 0.052 |
| 16 nodes | 0.057 to 0.060 | 0.052 to 0.054 |
| 1 node | 0.378 to 0.382 | 0.362 to 0.373 |

And `make` with something to find: it merged the include list of each
`.vp` file with all it includes, and theirs, to see if any was newer than
its oldest product, 97 files a closure on average and 1.9 million string
compares for the system. Now each file has the newest age of it and all
under it, found pass after pass till a pass changes none, as the includes
have loops. Profiled on one node, the closure went from 6.0 ms to 0.65 ms;
a merge of symbols was 3.4 ms, a hash set beside each list 14.8 ms, and a
scan from the end of each list 4.3 million compares to the 1.9 million
from the front. Twelve files touched in turn, each built by the old and
the new, gave the same objects every time. On 19 nodes, interleaved:

| `make` on the M4 | Before | After |
|---|---|---|
| Nothing to do, 15 runs each | 0.039 to 0.044, mean 0.0416 | 0.033 to 0.037, mean 0.0337 |
| After a touch of `sys/pii/class.vp`, 6 runs each | mean 0.0524 | mean 0.0466 |

Of what was left of a `make` with nothing to do, 11 ms was setting up the
compile environment and finding the files: two walks of the tree, 4 to 5
ms each, one for the `.vp` files and one for the `class.inc` files, and
`lib/asm/func.inc`, the whole assembler and translator, 3.7 ms, where
reading the classes needs only `lib/asm/class.inc`, `sys/pii/abi.inc` and
`sys/statics/class.inc`, 1.0 ms. Now one walk gives both and that is
what is loaded; what `make-info` found of all 300 files, includes and
products, was the same either way, as were the boot images of all six
platforms and what twelve touched files rebuilt. On 19 nodes, interleaved:

| On the M4 | Before | After |
|---|---|---|
| `make`, nothing to do, 8 runs each | mean 0.0339 | mean 0.0258 |
| `make` after a touch of `sys/pii/class.vp`, 6 each | mean 0.0447 | mean 0.0372 |
| `make platforms`, nothing to do, 8 each | mean 0.157 | mean 0.143 |
| `make all boot`, 5 each | mean 0.110 | mean 0.100 |

A build of every platform walked the tree again in each of its six
`make`s; the walk is now made once and handed down. Interleaved, 6 runs
each: `make platforms` with nothing to do, mean 0.143 to 0.112;
`make all boot platforms`, 0.605 to 0.565. The same objects were rebuilt
on all six after six touched files.

Reading every file for its includes was 13.6 ms of what was left, 300
files read a line at a time, all of it in the reading. What make reads of
each file is now kept, by the file's age, in `obj/make_scans`, and a file
is read again only when it has changed: none of one changed in the second
the keep was written, and none at all if `lib/asm/asm.inc` is not the one
they were read with. The same objects were rebuilt after twelve touched
files, and after a `.vp` file was given a new include and that include
touched; the boot images of all six platforms were the same. Interleaved,
8 runs each:

| On the M4 | Before | After |
|---|---|---|
| `make`, nothing to do | mean 0.0261 | mean 0.0173 |
| `make platforms`, nothing to do | mean 0.110 | mean 0.058 |
| `make all boot` | mean 0.100 | mean 0.091 |

### Test 2: Multi-Platform Simultaneous Cross-Compilation (Throughput)

This test measures the time to simultaneously compile all system sources for six
different target architectures from scratch.

* **Command:** `make all platforms | time`

* **Action:** Invokes `make-all-platforms`, cross-compiling the operating system
  for `x86_64/AMD64`, `x86_64/WIN64`, `arm64/ARM64`, `riscv64/RISCV64`,
  `la64/LA64`, and `vp64/VP64`.

| Machine | Nodes | Result |
|---|---|---|
| Apple M4 Max | 16 | 0.32 to 0.37 seconds, five runs |
| Intel i9-8950HK | 12 | 1.18 to 1.22 seconds, three runs |
| Raspberry Pi 4 | 4 | 7.6 and 9.0 seconds, two runs |

* **Evidence:** The entire operating system is compiled from source six times
  over (once for each architecture) in about a third of a second on the M4,
  demonstrating the massive throughput of the lightweight JIT assembler.

### Test 3: The Bootstrap Install (The Portability Test)

This test measures the system's ability to bootstrap a native build from scratch
while running entirely inside the portable C++ software emulator.

* **Command:** `make install`

* **Action:** Launches the **emulated VP64** environment, on one emulated node
  for each processor, and invokes `make all boot` to construct a fully native
  boot image from source. The time is the one the installer reports.

| Machine | Nodes | Result |
|---|---|---|
| Apple M4 Max | 16 | 1.13 to 1.19 seconds, four runs |
| Intel i9-8950HK | 12 | 3.42 to 3.51 seconds, three runs |
| Raspberry Pi 4 | 4 | 23.8 and 24.0 seconds, two runs |

On the Pi 4 the same install on one emulated node took 74 seconds, and on two
40 seconds, so it is the four nodes that bring it to 24.

* **Evidence:** Even when executing inside a portable C++ software emulator,
  ChrysaLisp can compile and link its entire native environment in a little
  over a second on an M4 laptop, and in 24 seconds on a low-power Raspberry
  Pi 4.

The 10 seconds this document used to give for the Pi 4 was from an earlier and
smaller system, and had not been measured again till now.

## Compact Boot Images: L1 Cache Residence

ChrysaLisp's "linkerless" direct-offset architecture produces self-contained,
minimal `boot_image` binaries across all supported architectures:

* `obj/vp64/VP64/sys/boot_image`: **129,844 bytes**

* `obj/x86_64/AMD64/sys/boot_image`: **173,900 bytes**

* `obj/x86_64/WIN64/sys/boot_image`: **174,108 bytes**

* `obj/arm64/ARM64/sys/boot_image`: **187,940 bytes**

* `obj/riscv64/RISCV64/sys/boot_image`: **218,556 bytes**

* `obj/la64/LA64/sys/boot_image`: **218,052 bytes**

These are the sizes on 2026-10-21. Since that day an image holds what an
install needs and no more, 773 functions of the 1,027 there are; the rest
are bound from their files as they are asked for, see the next section.
With every function in them, as the images were till then, the same tree
gives 217,308, 303,908, 304,532, 312,732, 360,684 and 359,460 bytes.

They had grown. With every function in them they were 160,764, 218,524,
219,076, 233,164, 270,540 and 270,108 bytes on 2026-10-06, and 178,004,
242,380, 242,956, 255,868, 298,036 and 297,156 on 2026-10-11: the network,
the whiteboard's set logic of polygons and the crypto of the mesh went in.
In the ten days after, more of the native kernels that libraries reach by
`ffi` went in, for media, crypto, images, maths and streams, till they were
45 KB of the VP64 image, and only four of them, the crypto of a machine's
key, are needed to install, as the next section measured.

The three link register targets include call fusion, see `lib/trans/vp.inc`.
It was measured when it went in, on 2026-10-03. The images were then 221,900,
256,900 and 256,532 bytes, and without it 227,196, 267,900 and 267,516, so it
saves 2.4% on ARM64 and 4.2% on RISCV64 and LA64. What it costs was measured
too. On ARM64, as one sweep with the existing prepass, `make test` on 19
nodes went from a mean of 0.0714 to 0.0723 seconds over 8 interleaved runs
each, and the bootstrap install from 1.79 to 1.82 seconds, as they were that
day. Both differences are inside the run to run noise.

Because these system images are from 130 to 219 KB, they fit entirely
inside the L1/L2 instruction and data caches of modern CPU cores. The CPU rarely
stalls on main memory access during core execution, resulting in near-zero memory
bus latency.

## The Image Is A Choice: Any Function Can Be Bound When It Is First Needed

The boot image is not a linked program. It is a list of functions laid end
to end, each with the paths of the functions it calls, `sys/mem/alloc`,
`class/str/create`, and the loader, `sys/load/`, turns those paths into
addresses. A function that is in the image is found there. One that is not
is read from its own file, `obj/<cpu>/<abi>/<path>`, into the same blocks
of memory, the first time anything asks for it, and what it calls is bound
the same way in turn. The system has always been able to do this. It is how
the functions of an application under `apps/` arrive, they are never in the
image.

So what is in the image is what was chosen to be bound ahead of time, and
nothing more than that. **Till 2026-10-21 the images packed in everything
outside `apps/`, purely for convenience:** one file to ship, one file to
boot from, and no list to keep of what an install will ask for. It was not
a need of the system, and it was not for speed. Now they hold what an
install needs, see below. The loader's `init` does not even hold the kernel:
it binds `sys/kernel/kernel` by its name, as it would any function, once it
has its statics and its buffer for relocating paths.

**How small it can be.** `(boot-image)` given no list of functions makes an
image of the three the loader starts from, `sys/load/init`,
`sys/statics/statics` and `sys/kernel/kernel`, and what they call. Built on
2026-10-11, beside the full image of the same tree:

| Target | Every function | The least |
|---|---|---|
| VP64 | 178,004 | 27,132 |
| x86_64 AMD64 | 242,380 | 31,972 |
| x86_64 WIN64 | 242,956 | 32,084 |
| arm64 ARM64 | 255,868 | 35,036 |
| riscv64 RISCV64 | 298,036 | 40,828 |
| la64 LA64 | 297,156 | 40,764 |

About a seventh. Its author puts the floor lower still, about 4 KB, if a few
more of what `init` reaches were bound by name as the kernel is. That has
not been built or measured.

**It runs.** On a Raspberry Pi 4, arm64, with the least image in place of
the full one and the function files in `obj/` beside it:

* **The whole test suite passes:** 135 modules, 6,006 tests, none failed, on
  four nodes. Everything they use was bound as it was asked for.

* **It is no slower.** A one node session started, given a command and
  ended, timed from outside. The first run after an image is changed is
  left out, it is slow with either image, 340 ms and 2.6 seconds, a cache
  being filled:

| Command | Full image | The least image |
|---|---|---|
| `echo up`, five runs | 132 to 133 ms | 121 to 122 ms |
| `tests -m polygon`, three runs | 172 to 173 ms | 160 to 163 ms |
| `make`, nothing to do, four runs | 373 to 377 ms | 354 to 357 ms |

  The files are in the host's own cache after the first read, and a smaller
  image is less to map. The session that ran `tests -m polygon` on the least
  image made 1,428 calls to open a file, the host's count, the shell that
  starts it among them.

**What an install needs.** `snapshot.zip` holds the VP64 image and no
function files, so an install has nothing to bind from: its image has to
hold every function the install uses. Which those are was measured, not
reasoned out. An emulated install was run from the least VP64 image with
the VP64 function files beside it, and the host was asked which files it
opened: 527 of the 944. An image of those 527 alone is **112,436 bytes**,
and a clean tree with that image and nothing else under `obj/` installs:
complete in 37 seconds on the Pi 4, one emulated node, the native image it
builds the same 255,868 bytes.

| The VP64 image of the snapshot | Functions | Bytes | Zipped |
|---|---|---|---|
| Every function, as it is shipped | 944 | 178,004 | 76,485 |
| What an install uses | 527 | 112,436 | 46,459 |

What an install never touches is nearly all of the GUI, the services, and
the set logic of polygons. What it does touch that might not be guessed is
`random-bytes` and SHA-256, for the key a machine is given the day it is
installed.

**Why the full image was shipped till 2026-10-21: convenience.** The saving
was then 30 KB of download. Against it, the list of 527 was true of one
tree on one day, and an image that is one function short of it fails on a
clean machine, which is the worst place there is to fail. An image with
everything in it can not be short.

**Why it no longer is.** In the ten days after, the image with every
function in it grew by 39 KB, most of it native kernels that only a
library's `ffi` reaches, and `snapshot.zip` from 129 KB, the one of
2026-10-08, to 172 KB. So the
image is now what an install needs, by a rule and not by a list of 527:

* **Everything in `sys/` and `class/`,** the kernel and Lisp, and what
  their functions call.

* **And 29 others by name,** in `+boot_extras` of `lib/boot/image.inc`: the
  font, pixmap and path functions Lisp's boot asks for, the four crypto
  kernels a machine's key is made with, and the Net service's sockets.
  `(boot-funcs)` there is what `make-boot-all` asks.

The 29 were measured as the 527 were. An emulated install was run from an
image of the kernel alone, the VP64 function files beside it, the loader
of that tree made to say each file it read; of the files it read, those
outside `sys/` and `class/` are the 29. Then:

| The VP64 image of the snapshot | Functions | Bytes | Zipped |
|---|---|---|---|
| Every function, till 2026-10-21 | 1,027 | 217,308 | 91,783 |
| What an install read, 2026-10-21 | 736 | 121,932 | |
| `sys/`, `class/` and the 29, as shipped | 773 | 129,844 | 53,807 |

The rule costs 8 KB over the bare list. For that, a new function in
`sys/` or `class/` is in the image without anyone saying so, and only a
new one elsewhere that an install binds before it has built anything has
to be named. One that is not, stops the install with `Bind error:` and its
name. `snapshot.zip` went from 172 KB to 134 KB.

**What it was checked by, 2026-10-21:**

* **Installs from the image alone,** nothing else under `obj/`: on the M4
  from a copy of the tree, 16 emulated nodes; on the Pi 4 in a new folder,
  `make install`, 57 seconds with the host programs compiled; and under
  Wine from the new `snapshot.zip`. Each made its native image and its key.

* **`tests -a` on the M4 on the new images,** 16 nodes: 186 modules, 7,791
  tests, none failed. That includes the release modules, and the one that
  starts a hidden desktop and opens every application on it, so a desktop
  binding what it needs at its start was done too.

* **A one node session started and ended on the M4,** timed from outside:
  28 to 29 ms with the new arm64 image, 30 to 36 ms with the full one,
  three runs each.

**What was not measured.** Many desktops starting at once, each binding the
same files; and how near the deepest chain of functions binding one
another came to the end of the loader's buffer for relocating paths, which
is two pages and is used as a stack.

**What it leaves to know.** A tree that once built VP64 keeps those
function files through `make install`, whose `clean` removes only the host
programs. A function the image does not hold would be bound from such a
file, from whatever source it was built of. No install binds one today.

## The VP64 Target: A Blueprint for Silicon

`VP64` is not merely an emulation fallback; it is a fully specified, clean,
orthogonal 64-bit RISC instruction set with 16 general-purpose registers and 16
floating-point registers.

* **The Universal Installer:** The `vp64` `boot_image` serves as the golden
  master. Any platform capable of compiling a basic host C++ driver can
  immediately boot the VP64 image and bootstrap a native JIT environment.

* **Direct Hardware Viability:** The virtual processor architecture avoids
  complex microcode or CISC decoding stages. The translation from VP
  instructions to hardware ALU operations is near 1:1, making VP64 a direct
  blueprint for dedicated, hyper-efficient silicon hardware.

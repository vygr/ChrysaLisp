# Evidence, Not Faith: Benchmarking ChrysaLisp

ChrysaLisp makes a series of claims that can seem extraordinary to those
accustomed to traditional operating systems: sub-second full system rebuilds,
seamless cross-platform compilation, and extreme efficiency on hardware ranging
from high-end laptops to low-power single-board computers.

These are not aspirations; they are the measured results of a system designed
from first principles. This document presents the concrete evidence for these
claims, derived directly from the system's own build and diagnostic tools.

## The Anatomy of a 40ms Build: What Actually Happens

When a benchmark reports an entire operating system rebuild in **0.040 seconds
(40 ms)** on an Apple Silicon M4 Max processor booted with 16 VP nodes, one
for each of its processors, which is what `./run_tui.sh -n 0` gives, it is
natural to assume it is merely compiling a few differential modules using a
pre-warmed, monolithic compiler cached in memory. That is the figure of
2026-11-10, 39 ms on 2026-11-11; it was 53 ms on 2026-10-06, and how it
came down is below.

In ChrysaLisp, that is not what happens. Every build cycle executes a complete,
cold, and hermetic lifecycle across a shared-nothing cluster:

```image
docs/diagrams/build_cycle.cwb
```

### 1. Phase 1: Synthesizing 20 Independent Toolchains Across 16 Nodes

Before the build command executes, **no assembler, compiler, or code generator
exists in memory on any node**. Because ChrysaLisp uses a strictly isolated,
task-centric memory model across its nodes:

* **20 Task-Isolated Syntheses:** The make pipeline (`lib/asm/asm.inc`) uses
  `lib/task/local.inc` to spawn a herd of worker tasks (`lib/asm/asm.lisp`)
  across the 16 VP nodes: one worker, and three more for each node beyond
  the first, to no more than 20, so 10 on a Pi 4's 4 nodes and 20 here.
  Each worker sets up its compiler before its first file, about 8 ms of CPU
  on the M4. One worker a node left some nodes with none, a child asked of
  a node moves on if a neighbour has fewer tasks, and a worker waiting for
  its next job has nothing to do. On a few nodes the spare workers are
  worth a quarter of a build; on 16 they make no difference, and twice as
  many is slower.

* **Biggest First:** The files are queued by size, the biggest given out
  first, so the small ones fill in at the end and the workers all finish
  within a couple of ms of each other.

* **Task-Local `within-compile-env` Environments:** While worker tasks share the
  root environment of their host node, the compilation environment is scoped per
  task. Each of the 20 worker tasks enters its own `(within-compile-env ...)`
  block, independently synthesizing its own private compiler environment from
  scratch in parallel, once.

* **Zero-State Bootstrapping:** In the first fraction of a millisecond, each
  worker task evaluates `lib/asm/`, macro generators (`def-class`, `def-method`,
  `assign`), and register allocation tables.

* **RAM-Native Toolchains:** Within milliseconds, 20 complete, fully
  functional native assembly engines are live in RAM across the 16-node
  cluster.

### 2. Phase 2: Distributed Parallel Execution Across 20 Worker Tasks

Once the 20 worker tasks across the 16 nodes have independently synthesized
their toolchains, the build workload is dynamically distributed:

* **16 Host OS Processes:** The host kernel (macOS) actively schedules and
  context-switches 16 separate host processes across its performance and
  efficiency cores, hosting the cooperative task scheduler within each VP node.

* **Dynamic Herd Dispatch:** The master build coordinator dispatches jobs to the
  worker task herd using mailbox messages, dynamically load-balancing work units
  as worker tasks finish chunks and report back.

* **Giving Way:** A worker gives way after each function, so the links of
  its node and the coordinator, if it is there, are not kept waiting for
  the length of a file.

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
  macro tables and code-generation environments.

* **Immediate Heap Reclamation:** All ASTs, intermediate strings, and parser
  structures are dereferenced, and memory cells are returned to the allocator
  via `:sys_mem :collect`.

* **No Lingering State:** The 20 toolchain instances **do not remain cached in
  memory** between build cycles. The next run begins again from absolute zero.

## The Benchmarks: Multi-Platform Build Analysis

The following benchmarks were last re-measured on 2026-11-11, from the TUI,
on three machines, each with one node for each processor, `./run_tui.sh -n 0`,
three runs of each. Nothing else of ChrysaLisp was up: each machine's
standing member of the mesh was closed for the runs, its links poll. Till
2026-11-10 it was left up and idle beside them.

* An Apple MacBook Pro with an Apple M4 Max processor, 16 nodes.

* An Apple MacBook Pro of 2018 with an Intel i9-8950HK processor, 12 nodes.
  A second user was signed on to it.

* A Raspberry Pi 4, 4 nodes.

Under each is how it has come down.

### Test 1: Native Compilation & Distributed Lifecycle (The Baseline)

This test measures the complete, cold lifecycle: synthesizing the independent
toolchains, compiling all source modules, linking the complete OS, and tearing
down all compiler environments.

* **Command:** `make test`

* **Action:** The Lisp application `cmd/make.lisp` executes repeated cold
  rebuild cycles, and reports the mean, the best and the worst.

| Machine | Nodes | Mean | Best | Worst | On one node, mean |
|---|---|---|---|---|---|
| Apple M4 Max | 16 | 0.038 to 0.040 | 0.036 to 0.037 | 0.042 to 0.044 | 0.241 to 0.243 |
| Intel i9-8950HK | 12 | 0.143 to 0.147 | 0.126 to 0.134 | 0.147 to 0.158 | 0.519 to 0.523 |
| Raspberry Pi 4 | 4 | 0.93 to 0.94 | 0.83 to 0.87 | 1.00 to 1.06 | 2.51 to 2.80 |

All times are seconds. On 19 nodes the M4 gave means of 0.037. One of the
Pi's three runs on one node had a cycle of 5.1 seconds in it, its SD card,
and is the 2.80.

* **Evidence:** On the M4 the best and worst cycle of a run are within 6 to
  8 ms of each other, with no GC pause spikes, allocator fragmentation, or
  JIT de-optimization penalties behind them. At ~40ms, the system can execute
  this complete birth-to-death compilation cycle **25 times per second**. On
  one node, one core, it does so 4 times a second. A Raspberry Pi 4 builds
  the whole system from source once a second.

How the mean has come down, seconds:

| When | M4, 16 nodes | M4, one node | i9, 12 nodes | i9, one node | Pi 4, 4 nodes | Pi 4, one node | What had changed |
|---|---|---|---|---|---|---|---|
| Till 2026-10-03 | 0.070, on 20 | | | | | | |
| 2026-10-06 | 0.052 to 0.055 | 0.340 | 0.170 to 0.190 | 0.757 | 1.28 to 1.45 | 3.74 | The Lisp engine, see [`till_the_pips_squeak.md`](till_the_pips_squeak.md) |
| 2026-10-22 | 0.052 to 0.054 | 0.362 to 0.373 | | | | | More source, 142 files; a worker a node, a translation that stops early, a save that does not truncate |
| 2026-11-10 | 0.039 to 0.042 | 0.234 to 0.235 | 0.140 to 0.176 | 0.512 to 0.517 | 0.96 to 1.08 | 2.42 to 2.58 | vpx gone, no pass over a frame, functions written by hand |
| 2026-11-11 | 0.038 to 0.040 | 0.241 to 0.243 | 0.143 to 0.147 | 0.519 to 0.523 | 0.93 to 0.94 | 2.51 to 2.80 | The make by content and as one farm; a herd of more workers than nodes where they are few |

What changed for each record:

* **2026-10-06, the Lisp engine.** The evaluator, the binding of a lambda's
  args, the environments and the iterators, see
  [`till_the_pips_squeak.md`](till_the_pips_squeak.md). The assembler is a
  Lisp program, so a quicker Lisp is a quicker build.

* **2026-10-22, the build itself.** The source had grown and the build
  with it, to 0.074 seconds. One worker for each node with the biggest
  file given out first, where there had been 28 workers for 16 nodes; a
  translation that stops as soon as a pass leaves every label where it
  was; and a save that writes over a file and cuts it to its length,
  where truncating it first made the nodes wait on each other. The same
  day the VP of every function was made the same on every platform, so a
  source file is compiled to VP once, kept, and translated for each
  platform: that is the record of Test 2.

* **2026-11-10, the source and not a pass.** vpx, the expression
  compiler, had gone, and every function was written as plain VP. The
  pass that kept the slots of a frame in registers was measured and taken
  out: it was a sixth of a build, and of 1,074 functions it changed 42,
  for 217 stack loads and 74 stores less and 375 register copies more. 31
  of those were written by hand to hold their values in registers, and
  are 1,770 instructions with 155 stack loads, where the pass had them at
  2,020 and 198. Loads and stores were put in the order of the memory in
  the source where that makes pairs, 39 more on arm64. An ordering sort
  inside `(assign)` was measured too, 3 to 4% of a build, and not kept.
  `till_the_pips_squeak.md` has what was tried.

* **2026-11-11, the make, and not this figure.** `make test` is the cold
  cycle, every file assembled and nothing kept, so a make that keeps what
  it has made does not show in it: the work is what it was, and so is the
  time on 16 nodes and on 12. What the herd's spare workers are worth
  shows where the nodes are few. On the M4 held to 4 nodes the mean went
  from 0.119 to 0.089, on 8 from 0.070 to 0.055, on 12 from 0.051 to
  0.043, and on 19 it stayed at 0.037. The figures above were taken with
  the standing member closed, those of 2026-11-10 with it up.
  `till_the_pips_squeak.md` has what was tried.

On 2026-11-10 each machine ran `tests -a` on its own build before its
figures were taken: 187 modules and 8,255 tests on each, none failed, in
286 seconds on the M4, 289 on the i9 and 493 on the Pi.

### Test 2: Multi-Platform Simultaneous Cross-Compilation (Throughput)

This test measures the time to make the native code of the whole system for
six different target architectures at once.

* **Command:** `make all platforms | time`

* **Action:** Makes the code of every function, 1,067 of them, for
  `x86_64/AMD64`, `x86_64/WIN64`, `arm64/ARM64`, `riscv64/RISCV64`,
  `la64/LA64`, and `vp64/VP64`: 6,400 files. Since 2026-11-11 it is made
  from the VP that is kept. The VP of a function is the same for every
  platform, a source file is assembled to it only when the file or
  something it includes has changed, and `all` makes all the code again
  whether it has or not. Till then the command assembled the source first:
  that part is Test 1.

| Machine | Nodes | Result | With the six boot images, `make all platforms boot` |
|---|---|---|---|
| Apple M4 Max | 16 | 0.063 to 0.068 seconds | 0.081 to 0.083 |
| Intel i9-8950HK | 12 | 0.135 to 0.172 seconds | 0.182 to 0.191 |
| Raspberry Pi 4 | 4 | 0.40 to 0.51 seconds | 0.46 to 0.52 |

The M4's first run of the three, after it had stood idle, was 0.167.

* **Evidence:** The code of the entire operating system is made for six
  architectures in under 70 ms on the M4, and in half a second on a
  Raspberry Pi 4. Two thirds of that is the saving of the 6,400 files.

How it has come down, seconds:

| When | M4, 16 nodes | i9, 12 nodes | Pi 4, 4 nodes | What had changed |
|---|---|---|---|---|
| 2026-10-06 | 0.32 to 0.37 | 1.18 to 1.22 | 7.6 and 9.0 | Every platform compiled from source |
| 2026-10-22 | 0.21 to 0.25, on 19 | | | The source compiled to VP once and translated six times |
| 2026-11-10 | 0.17 to 0.21 | 0.43 to 0.51 | 1.48 to 1.57 | As Test 1 |
| 2026-11-11 | 0.063 to 0.068 | 0.135 to 0.172 | 0.40 to 0.51 | The VP is kept and not assembled again; one farm, where each platform had a herd of its own, one after another, and each read the file again |

### Test 2a: A Make With Little To Do

What a make costs when it is not forced: it keys every source file by its
content and all it includes, and makes only what is not as its key says.

* **Commands:** `make | time` and `make platforms boot | time`, with nothing
  changed, and after one function of `class/num/class.vp` was changed.

| Machine | Nodes | `make`, nothing to do | `make`, one function changed | `make platforms boot`, nothing to do | `make platforms boot`, one function changed |
|---|---|---|---|---|---|
| Apple M4 Max | 16 | 0.004 to 0.006 | 0.015 to 0.018 | 0.008 to 0.009 | 0.038 to 0.045 |
| Intel i9-8950HK | 12 | 0.011 to 0.017 | 0.031 to 0.036 | 0.016 to 0.023 | 0.076 to 0.078 |
| Raspberry Pi 4 | 4 | 0.032 to 0.034 | 0.102 to 0.110 | 0.052 to 0.056 | 0.205 to 0.225 |

All times are seconds, measured on 2026-11-11. The file that was changed
is one that every boot image holds, so the last column is its code for six
platforms and six boot images. On the morning of 2026-11-11, before the
make went by content, the M4 on 19 nodes took 0.015 to find it had nothing
to do, 0.204 for `make platforms boot` with nothing to do and 0.254 after
one file was touched.

### Test 3: The Bootstrap Install (The Portability Test)

This test measures the system's ability to bootstrap a native build from scratch
while running entirely inside the portable C++ software emulator.

* **Command:** `make install`

* **Action:** Launches the **emulated VP64** environment, on one emulated node
  for each processor, and invokes `make all boot` to construct a fully native
  boot image from source, then `make apps` for the apps that have code of
  their own. The installer reports the time of each.

| Machine | Nodes | `make all boot` | `make apps` |
|---|---|---|---|
| Apple M4 Max | 16 | 1.07 to 1.30 seconds | 0.61 to 0.65 |
| Intel i9-8950HK | 12 | 4.00 to 4.27 seconds | 1.50 to 1.64 |
| Raspberry Pi 4 | 4 | 25.0 to 29.5 seconds | 6.4 to 10.3 |

Each run was from a copy of the tree with only the host program and the
unpacked `snapshot.zip` under `obj/`. The native image each install built
was the size of its tree's own every time, 71,500 bytes on the M4 and the
Pi, 68,644 on the i9.

* **Evidence:** Even when executing inside a portable C++ software emulator,
  ChrysaLisp can compile and link its entire native environment in a little
  over a second on an M4 laptop, and in under 30 seconds on a low-power
  Raspberry Pi 4.

How it has gone, seconds:

| When | M4, 16 nodes | i9, 12 nodes | Pi 4, 4 nodes | What had changed |
|---|---|---|---|---|
| 2026-10-06 | 1.13 to 1.19 | 3.42 to 3.51 | 23.8 and 24.0 | |
| 2026-11-10 | 1.32 to 1.34 | 3.04 to 3.45 | 15.2 to 15.9 | The VP64 image is of the one normal build, with its checks, and carries every translator; the install makes the VP once and translates it |
| 2026-11-11, `make all boot` | 1.07 to 1.30 | 4.00 to 4.27 | 25.0 to 29.5 | See below |
| 2026-11-11, `make apps` | 0.61 to 0.65 | 1.50 to 1.64 | 6.4 to 10.3 | It makes the apps that have changed, where it made every one whatever |

**The rows till 2026-11-10 are of the second step, not the first.** The
installer prints two times, and the last was the one read: `make apps`,
which then made all the code again. It was taken for the install. Timed
again on the tree of 2026-11-10, the i9 was 4.14 to 4.40 seconds for
`make all boot` and 2.93 to 3.28 for `make apps`, the Pi 28.4 to 28.5 and
13.8 to 15.5. So the first step is about where it was, and the second has
halved. On the Pi the first step was 28.5 seconds or 37.5 by whether three
of its four nodes had a worker or two, and with a herd of ten is 25.0 to
29.5.

What changed for the record of 2026-11-10: two things, one each way. Since
2026-10-26 there is one build mode, the normal one with its checks, and
the VP64 image an install runs is of it, where it was a release build with
none. That costs an install, and was chosen: it is one base every machine
is tested on, and release mode was not winning enough on the native
systems to keep a second one. Against it, the install now makes the VP
once and translates it, with the translators the image carries. The Pi
comes out a third quicker and the i9 a little; the M4, which had the least
to gain from the second, is 0.15 to 0.2 seconds slower.

## Compact Boot Images: L1 Cache Residence

ChrysaLisp's "linkerless" direct-offset architecture produces self-contained,
minimal `boot_image` binaries across all supported architectures. Bytes,
on 2026-11-10 and before:

| Image | 2026-10-06 | 2026-10-11 | 2026-10-21 | 2026-10-21 | 2026-11-10 |
|---|---|---|---|---|---|
| What it holds | Every function | Every function | Every function | What an install needs | See below |
| `vp64/VP64` | 160,764 | 178,004 | 217,308 | 129,844 | **243,372** |
| `x86_64/AMD64` | 218,524 | 242,380 | 303,908 | 173,900 | **68,588** |
| `x86_64/WIN64` | 219,076 | 242,956 | 304,532 | 174,108 | **68,172** |
| `arm64/ARM64` | 233,164 | 255,868 | 312,732 | 187,940 | **71,444** |
| `riscv64/RISCV64` | 270,540 | 298,036 | 360,684 | 218,556 | **76,756** |
| `la64/LA64` | 270,108 | 297,156 | 359,460 | 218,052 | **76,300** |

With every function in them the images grew as the system did: the
network, the whiteboard's set logic of polygons and the crypto of the mesh
by 2026-10-11, then the native kernels that libraries reach by `ffi`, for
media, crypto, images, maths and streams. On 2026-10-21 they were cut to
what an install needs, 773 functions of the 1,027 there were, the rest
bound from their files as they are asked for, see the next section. Since
release 7.5 a native image holds the loader, the kernel and what they
call, and no more; the VP64 image holds what an install needs before it
has built anything, and carries the translators of every platform, so it
is the big one.

The three link register targets include call fusion, see `lib/trans/vp.inc`:
it makes their images 2.4% smaller on ARM64 and 4.2% on RISCV64 and LA64,
and costs a build and an install nothing that can be measured.

Because a native system image is from 68 to 77 KB, it fits entirely
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
the same way in turn. It is how the functions of an application under
`apps/` have always arrived, they are never in the image.

So what is in an image is what was chosen to be bound ahead of time, and
nothing more than that. On 2026-11-10, of the 1,061 functions there are
outside `apps/`:

| Image | Functions | Bytes | What it holds |
|---|---|---|---|
| A native one, `arm64/ARM64` | 237 | 71,444 | The loader, the kernel and what they call, the translator of its own CPU among them |
| The VP64 one, in `snapshot.zip` | 797 | 243,372 | What an install binds before it has built anything, and the translator of every CPU |

**A native image holds only what starts the system.** It is made by an
install, after the rest has been built, so the files are there to bind
from. The GUI, the services, the codecs and most of the class library are
not in it.

**It runs.** `tests -a` on 2026-11-10, on the native images of three
machines: 187 modules and 8,255 tests on each, none failed, everything
they use bound as it was asked for. That includes the module that starts
a hidden desktop and opens every application on it. A one node session on
the M4, started, given `echo up` and ended, timed from outside: 32 to 35
ms, five runs after the first.

**The VP64 image is what an install needs.** `snapshot.zip` holds it and
no function files, so an install has nothing to bind from till it has
built something: its image has to hold every function the install uses
before then. That is a rule, `(boot-funcs)` in `lib/boot/image.inc`, and
not a list of what one install was seen to read: everything in `sys/` and
`class/`, the kernel and Lisp, and 36 others by name, the font, pixmap and
path functions Lisp's boot asks for, the crypto a machine's key is made
with, the Net service's sockets, and the translators and the parser of VP
text. A new function in `sys/` or `class/` is in it without anyone saying
so. One elsewhere that an install binds too soon, and is not named, stops
the install with `Bind error:` and its name. `snapshot.zip` is 159,487
bytes, the VP64 image and the host program for Windows.

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

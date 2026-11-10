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
2026-11-10; it was 53 ms on 2026-10-06, and how it came down is below.

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
  worker more than there are nodes only adds that.

* **Biggest First:** The files are queued by size, the biggest given out
  first, so the small ones fill in at the end and the workers all finish
  within a couple of ms of each other.

* **Task-Local `within-compile-env` Environments:** While worker tasks share the
  root environment of their host node, the compilation environment is scoped per
  task. Each of the 16 worker tasks enters its own `(within-compile-env ...)`
  block, independently synthesizing its own private compiler environment from
  scratch in parallel, once.

* **Zero-State Bootstrapping:** In the first fraction of a millisecond, each
  worker task evaluates `lib/asm/`, macro generators (`def-class`, `def-method`,
  `assign`), and register allocation tables.

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
  macro tables and code-generation environments.

* **Immediate Heap Reclamation:** All ASTs, intermediate strings, and parser
  structures are dereferenced, and memory cells are returned to the allocator
  via `:sys_mem :collect`.

* **No Lingering State:** The 16 toolchain instances **do not remain cached in
  memory** between build cycles. The next run begins again from absolute zero.

## The Benchmarks: Multi-Platform Build Analysis

The following benchmarks were last re-measured on 2026-11-10, from the TUI,
on three machines, each with one node for each processor, `./run_tui.sh -n 0`,
three runs of each. Each machine had its standing member of the mesh up and
idle beside the runs, and nothing else of ChrysaLisp.

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
| Apple M4 Max | 16 | 0.039 to 0.042 | 0.036 to 0.040 | 0.043 to 0.052 | 0.234 to 0.235 |
| Intel i9-8950HK | 12 | 0.140 to 0.176 | 0.134 to 0.158 | 0.146 to 0.218 | 0.512 to 0.517 |
| Raspberry Pi 4 | 4 | 0.96 to 1.08 | 0.91 to 0.93 | 1.02 to 1.41 | 2.42 to 2.58 |

All times are seconds. On 19 nodes the M4 gave means of 0.0376 to 0.0379.
The i9's first two runs were 0.175 and 0.176 and its third 0.140.

* **Evidence:** On the M4 the best and worst cycle of a run are within 7 to
  12 ms of each other, with no GC pause spikes, allocator fragmentation, or
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

Each machine ran `tests -a` on its own build before its figures were
taken: 187 modules and 8,255 tests on each, none failed, in 286 seconds on
the M4, 289 on the i9 and 493 on the Pi.

### Test 2: Multi-Platform Simultaneous Cross-Compilation (Throughput)

This test measures the time to simultaneously compile all system sources for six
different target architectures from scratch.

* **Command:** `make all platforms | time`

* **Action:** Invokes `make-all-platforms`, cross-compiling the operating system
  for `x86_64/AMD64`, `x86_64/WIN64`, `arm64/ARM64`, `riscv64/RISCV64`,
  `la64/LA64`, and `vp64/VP64`.

| Machine | Nodes | Result |
|---|---|---|
| Apple M4 Max | 16 | 0.17 to 0.21 seconds |
| Intel i9-8950HK | 12 | 0.43 to 0.51 seconds |
| Raspberry Pi 4 | 4 | 1.48 to 1.57 seconds |

* **Evidence:** The entire operating system is built for six architectures
  in a fifth of a second on the M4, and in a second and a half on a
  Raspberry Pi 4.

How it has come down, seconds:

| When | M4, 16 nodes | i9, 12 nodes | Pi 4, 4 nodes | What had changed |
|---|---|---|---|---|
| 2026-10-06 | 0.32 to 0.37 | 1.18 to 1.22 | 7.6 and 9.0 | Every platform compiled from source |
| 2026-10-22 | 0.21 to 0.25, on 19 | | | The source compiled to VP once and translated six times |
| 2026-11-10 | 0.17 to 0.21 | 0.43 to 0.51 | 1.48 to 1.57 | As Test 1 |

### Test 3: The Bootstrap Install (The Portability Test)

This test measures the system's ability to bootstrap a native build from scratch
while running entirely inside the portable C++ software emulator.

* **Command:** `make install`

* **Action:** Launches the **emulated VP64** environment, on one emulated node
  for each processor, and invokes `make all boot` to construct a fully native
  boot image from source. The time is the one the installer reports.

| Machine | Nodes | Result |
|---|---|---|
| Apple M4 Max | 16 | 1.32 to 1.34 seconds |
| Intel i9-8950HK | 12 | 3.04 to 3.45 seconds |
| Raspberry Pi 4 | 4 | 15.2 to 15.9 seconds |

Each run was from a copy of the tree with only the host program and the
unpacked `snapshot.zip` under `obj/`. The native image each install built
was the size of its tree's own every time, 71,444 bytes on the M4 and the
Pi, 68,588 on the i9.

* **Evidence:** Even when executing inside a portable C++ software emulator,
  ChrysaLisp can compile and link its entire native environment in a little
  over a second on an M4 laptop, and in under 16 seconds on a low-power
  Raspberry Pi 4.

How it has gone, seconds:

| When | M4, 16 nodes | i9, 12 nodes | Pi 4, 4 nodes | What had changed |
|---|---|---|---|---|
| 2026-10-06 | 1.13 to 1.19 | 3.42 to 3.51 | 23.8 and 24.0 | |
| 2026-11-10 | 1.32 to 1.34 | 3.04 to 3.45 | 15.2 to 15.9 | The VP64 image is of the one normal build, with its checks, and carries every translator; the install makes the VP once and translates it |

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

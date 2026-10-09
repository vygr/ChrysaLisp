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

```
[Zero State: No Compiler/Build Tools in RAM]
       |
       V (Phase 1: Genesis in microseconds)
[Synthesize 28 Independent Toolchains Across 16 Nodes]
       |
       V (Phase 2: Parallel MIMD Compilation & Linking)
[Assemble OS, Route Packets, Arbitrate Locks Across Herd]
       |
       V (Phase 3: Total Teardown & Reclamation)
[Destroy Toolchains, Dereference ASTs, Reclaim All Heap Memory]
       |
       V
[Return to Zero State: Clean RAM]
```

### 1. Phase 1: Synthesizing 28 Independent Toolchains Across 16 Nodes

Before the build command executes, **no assembler, compiler, or code generator
exists in memory on any node**. Because ChrysaLisp uses a strictly isolated,
task-centric memory model across its nodes:

* **28 Task-Isolated Syntheses:** The make pipeline (`lib/asm/asm.inc`) uses
  `lib/task/local.inc` to spawn a herd of worker tasks (`lib/asm/asm.lisp`)
  across the 16 VP nodes. The herd starts as a tenth of the number of source
  files, 13 for the 135 `.vp` files, and has one more for each node other
  than the one that asked, so 28 worker tasks on 16 nodes.

* **Task-Local `within-compile-env` Environments:** While worker tasks share the
  root environment of their host node, the compilation environment is scoped per
  task. Each of the 28 worker tasks enters its own `(within-compile-env ...)`
  block, independently synthesizing its own private compiler environment from
  scratch in parallel.

* **Zero-State Bootstrapping:** In the first fraction of a millisecond, each
  worker task evaluates `lib/asm/`, macro generators (`def-class`, `def-method`,
  `assign`), register allocation tables, and CScript transpilers.

* **RAM-Native Toolchains:** Within microseconds, 28 complete, fully
  functional native assembly engines are live in RAM across the 16-node
  cluster.

### 2. Phase 2: Distributed Parallel Execution Across 28 Worker Tasks

Once the 28 worker tasks across the 16 nodes have independently synthesized
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
  macro tables, CScript variable scopes, and code-generation environments.

* **Immediate Heap Reclamation:** All ASTs, intermediate strings, and parser
  structures are dereferenced, and memory cells are returned to the allocator
  via `:sys_mem :collect`.

* **No Lingering State:** The 28 toolchain instances **do not remain cached in
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

* `obj/vp64/VP64/sys/boot_image`: **160,764 bytes**

* `obj/x86_64/AMD64/sys/boot_image`: **218,524 bytes**

* `obj/x86_64/WIN64/sys/boot_image`: **219,076 bytes**

* `obj/arm64/ARM64/sys/boot_image`: **233,164 bytes**

* `obj/riscv64/RISCV64/sys/boot_image`: **270,540 bytes**

* `obj/la64/LA64/sys/boot_image`: **270,108 bytes**

These are the sizes on 2026-10-06, about 5% up on those of 2026-10-03.

The three link register targets include call fusion, see `lib/trans/vp.inc`.
It was measured when it went in, on 2026-10-03. The images were then 221,900,
256,900 and 256,532 bytes, and without it 227,196, 267,900 and 267,516, so it
saves 2.4% on ARM64 and 4.2% on RISCV64 and LA64. What it costs was measured
too. On ARM64, as one sweep with the existing prepass, `make test` on 19
nodes went from a mean of 0.0714 to 0.0723 seconds over 8 interleaved runs
each, and the bootstrap install from 1.79 to 1.82 seconds, as they were that
day. Both differences are inside the run to run noise.

Because these complete system images are from 160 to 270 KB, they fit entirely
inside the L1/L2 instruction and data caches of modern CPU cores. The CPU rarely
stalls on main memory access during core execution, resulting in near-zero memory
bus latency.

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

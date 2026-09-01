# Evidence, Not Faith: Benchmarking ChrysaLisp

ChrysaLisp makes a series of claims that can seem extraordinary to those
accustomed to traditional operating systems: sub-second full system rebuilds,
seamless cross-platform compilation, and extreme efficiency on hardware ranging
from high-end laptops to low-power single-board computers.

These are not aspirations; they are the measured results of a system designed
from first principles. This document presents the concrete evidence for these
claims, derived directly from the system's own build and diagnostic tools.

## The Anatomy of a 66ms Build: What Actually Happens

When a benchmark reports an entire operating system rebuild in **0.0667 seconds
(66.7 ms)** on an Apple Silicon M4 processor booted with 20 VP nodes, it is
natural to assume it is merely compiling a few differential modules using a
pre-warmed, monolithic compiler cached in memory.

In ChrysaLisp, that is not what happens. Every build cycle executes a complete,
cold, and hermetic lifecycle across a shared-nothing cluster:

```
[Zero State: No Compiler/Build Tools in RAM]
       |
       V (Phase 1: Genesis in microseconds)
[Synthesize ~40 Independent Toolchains Across 20 Nodes]
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

### 1. Phase 1: Synthesizing ~40 Independent Toolchains Across 20 Nodes

Before the build command executes, **no assembler, compiler, or code generator
exists in memory on any node**. Because ChrysaLisp uses a strictly isolated,
task-centric memory model across its nodes:

* **~40 Task-Isolated Syntheses:** The make pipeline (`lib/asm/asm.inc`) uses
  `lib/task/local.inc` to spawn a dynamic herd of worker tasks
  (`lib/asm/asm.lisp`) across the 20 VP nodes—typically scaling to approximately
  40 worker tasks (~2 per node).

* **Task-Local `within-compile-env` Environments:** While worker tasks share the
  root environment of their host node, the compilation environment is scoped per
  task. Each of the ~40 worker tasks enters its own `(within-compile-env ...)`
  block, independently synthesizing its own private compiler environment from
  scratch in parallel.

* **Zero-State Bootstrapping:** In the first fraction of a millisecond, each
  worker task evaluates `lib/asm/`, macro generators (`def-class`, `def-method`,
  `assign`), register allocation tables, and CScript transpilers.

* **RAM-Native Toolchains:** Within microseconds, approximately 40 complete,
  fully functional native assembly engines are live in RAM across the 20-node
  cluster.

### 2. Phase 2: Distributed Parallel Execution Across ~40 Worker Tasks

Once the ~40 worker tasks across the 20 nodes have independently synthesized
their toolchains, the build workload is dynamically distributed:

* **20 Host OS Processes:** The host kernel (macOS) actively schedules and
  context-switches 20 separate host processes across its performance and
  efficiency cores, hosting the cooperative task scheduler within each VP node.

* **Dynamic Herd Dispatch:** The master build coordinator dispatches jobs to the
  worker task herd using mailbox messages, dynamically load-balancing work units
  as worker tasks finish chunks and report back.

* **Inter-Node Shared Memory Links (`sys_link`):** Dual-channel circular ring
  buffers (`lk_shmem`) coordinate communication, negotiate channel ownership,
  and synchronize status words (`lk_chan_status_frag`, `ping`, `skip`).

* **Decentralized Load Balancing (`+kn_call_child`):** Child task creation
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

* **No Lingering State:** The ~40 toolchain instances **do not remain cached in
  memory** between build cycles. The next run begins again from absolute zero.

## The Benchmarks: Multi-Platform Build Analysis

The following benchmarks were captured on an Apple MacBook Pro equipped with an
Apple M4 processor.

### Test 1: Native Compilation & Distributed Lifecycle (The Baseline)

This test measures the complete, cold lifecycle: synthesizing 20 independent
toolchains, compiling all source modules, linking the complete OS, and tearing
down all compiler environments.

* **Command:** `make test`

* **Action:** The Lisp application `cmd/make.lisp` executes repeated cold
  rebuild cycles, reporting live statistical metrics inside ChrysaLisp's native
  GUI benchmark window.

* **Mean Time:** **66,705 us (0.0667 seconds)**.

* **Best Time:** **~60,000–61,000 us (~0.060 seconds)** (observed at the far
  left bound of the distribution, well below the 65.3ms grid mark).

* **Worst Time:** **73,988 us (0.0739 seconds)**.

* **Jitter / Spread:** **~8.6 ms total variance**.

* **Evidence:** The extremely tight spread between best and worst runs proves
  the total absence of GC pause spikes, allocator fragmentation, or JIT
  de-optimization penalties. At ~66.7ms, the system can execute this complete
  birth-to-death compilation cycle **15 times per second**.

### Test 2: Multi-Platform Simultaneous Cross-Compilation (Throughput)

This test measures the time to simultaneously compile all system sources for six
different target architectures from scratch.

* **Command:** `make all platforms | time`

* **Action:** Invokes `make-all-platforms`, cross-compiling the operating system
  for `x86_64/AMD64`, `x86_64/WIN64`, `arm64/ARM64`, `riscv64/RISCV64`,
  `la64/LA64`, and `vp64/VP64`.

* **Result:** **0.42 seconds**.

* **Evidence:** The entire operating system is compiled from source six times
  over (once for each architecture) in roughly four-tenths of a second,
  demonstrating the massive throughput of the lightweight JIT assembler.

### Test 3: The Bootstrap Install (The Portability Test)

This test measures the system's ability to bootstrap a native build from scratch
while running entirely inside the portable C++ software emulator.

* **Command:** `make install`

* **Action:** Launches the **emulated VP64** environment and invokes `make all
  boot` to construct a fully native **ARM64** boot image from source.

* **Result (Apple M4):** **1.68 seconds**.

* **Result (Raspberry Pi 4):** **~10.0 seconds**.

* **Evidence:** Even when executing inside a single-threaded portable C++
  software emulator, ChrysaLisp can compile and link its entire native
  environment in under 2 seconds on an M4 laptop, and in 10 seconds on a
  low-power Raspberry Pi 4.

## Compact Boot Images: L1 Cache Residence

ChrysaLisp's "linkerless" direct-offset architecture produces self-contained,
minimal `boot_image` binaries:

* `obj/x86_64/AMD64/sys/boot_image`: **197,228 bytes**

* `obj/x86_64/WIN64/sys/boot_image`: **197,540 bytes**

* `obj/arm64/ARM64/sys/boot_image`: **215,156 bytes**

* `obj/riscv64/RISCV64/sys/boot_image`: **251,220 bytes**

* `obj/la64/LA64/sys/boot_image`: **252,132 bytes**

* `obj/vp64/VP64/sys/boot_image`: **144,036 bytes**

Because these complete system images are around ~200 KB, they fit entirely
inside the L1 instruction and data caches of modern CPU cores. The CPU rarely
stalls on main memory access during core execution, resulting in near-zero
memory bus latency.

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

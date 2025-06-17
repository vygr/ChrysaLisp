# Evidence, Not Faith: Benchmarking ChrysaLisp

ChrysaLisp makes a series of claims that can seem extraordinary to those
accustomed to traditional operating systems: sub-second full system rebuilds,
seamless cross-platform compilation, and extreme efficiency on hardware ranging
from high-end laptops to low-power single-board computers.

These are not aspirations; they are the measured results of a system designed
from first principles. This document presents the concrete evidence for these
claims, derived directly from the system's own build and test tools.

## The Foundation of Speed: A Radically Different Build Process

Before presenting the numbers, it's critical to understand *why* the system is
so fast. ChrysaLisp's performance is not a brute-force optimization but an
architectural feature.

1.  **A "Linkerless" System:** Traditional builds spend a significant amount of
    time in a separate, complex linking stage. ChrysaLisp eliminates this. The
    `boot-image` tool performs a trivial, near-instantaneous concatenation of
    compiled function binaries. Symbol resolution is handled by converting
    symbolic paths to simple relative memory offsets, with a final fix-up to
    absolute addresses occurring at load time. This removes the single largest
    bottleneck in most build pipelines.

2.  **Self-Hosted, JIT-Compiled Tooling:** The `make` and `asm` tools are not
    external shell scripts or slow interpreters. They are native ChrysaLisp
    applications (`cmd/make.lisp`, `lib/asm/asm.inc`). When executed, they are
    themselves JIT-compiled into the host's native machine code, running with
    the full performance of the system they are building.

## The Benchmarks: A Multi-Platform Build Analysis

The following benchmarks were captured on a MacBook Pro with an M4 processor.
They demonstrate the system's performance across different build scenarios, from
native compilation to full cross-platform bootstrapping.

### Test 1: Native Compilation Speed (The Performance Ceiling)

This test measures the time to recompile the entire OS and its standard
libraries for a single, native architecture.

*   **Command:** `make test`

*   **Action:** The Lisp application `cmd/make.lisp` is invoked. Its `make-test`
    function recompiles all `.vp` source files for the host architecture (ARM64)
    ten times and calculates the average.

*   **Result:** **~0.084 seconds** (mean time per rebuild).

*   **Evidence:** This demonstrates the absolute speed of the ChrysaLisp
    compiler running natively. A complete OS rebuild in 84 milliseconds sets the
    baseline for all other benchmarks.

### Test 2: Multi-Platform Compilation Speed (The Throughput)

This test measures the time to cross-compile all source files for five different
target architectures simultaneously.

*   **Command:** `make all platforms | time`

*   **Action:** `make all platforms` invokes the `make-all-platforms` function,
    which compiles all sources for x86_64/AMD64, x86_64/WIN64, arm64/ARM64,
    riscv64/RISCV64, and vp64/VP64. The output is piped to the `cmd/time.lisp`
    application, which measures the total duration.

*   **Result:** **0.444356 seconds**.

*   **Evidence:** This is the total time to compile the *entire operating system
    five times*, once for each target architecture. This showcases the extreme
    throughput of the JIT-enabled build tools.

### Test 3: The Bootstrap Install (The Portability Test)

This is the most comprehensive test. It demonstrates the system's ability to
build a native version of itself while running on the portable C++ emulator.

*   **Command:** `make install`

*   **Action:** This command launches the **emulated VP64** version of
    ChrysaLisp. Inside this emulated environment, it then invokes
    `make all boot` to build a **native ARM64** boot image from source.

*   **Result (M4):** **2.535553 seconds**.

*   **Result (Raspberry Pi 4):** **~10 seconds**.

*   **Evidence:** The 2.5-second time on the M4 is not a native build; it's the
    time for the portable software emulator to perform a full native compile and
    link. The fact that a low-power Raspberry Pi 4 can perform this same complex
    bootstrap in only ~10 seconds is the ultimate proof of the architecture's
    efficiency. The performance scales predictably, even when running on an
    entirely different class of hardware via emulation.

## The Result: Compact and Efficient Boot Images

The "linkerless" build process results in remarkably small, self-contained
`boot_image` files, validating the minimal footprint claim.

*   `obj/x86_64/AMD64/sys/boot_image` **(173788 bytes)**

*   `obj/x86_64/WIN64/sys/boot_image` **(174092 bytes)**

*   `obj/arm64/ARM64/sys/boot_image` **(203620 bytes)**

*   `obj/riscv64/RISCV64/sys/boot_image` **(228676 bytes)**

*   `obj/vp64/VP64/sys/boot_image` **(131948 bytes)**

These sizes, typically under 200 KB, are small enough to fit within the L1 cache
of a modern CPU core, drastically reducing memory latency and contributing to
the system's overall responsiveness.

## The VP64 Target: A First-Class Citizen

It is critical to understand that `VP64` is not merely an "emulation mode." It
is a fully-specified, first-class target architecture with its own clean,
orthogonal, RISC-like instruction set.

*   **The Universal Installer:** As demonstrated by the `make install`
    benchmark, the `vp64` `boot_image` serves as the portable "golden master."
    It can be used to bootstrap a native ChrysaLisp environment on any platform
    that can compile its simple C++ host application. The installer itself is
    built in release mode (`*debug_mode* 0`), with all debug checks compiled out
    to minimize installation time.

*   **The Hardware Hypothetical:** The simplicity of the VP64 instruction set
    makes it an ideal target for a hardware implementation. A `VP64` CPU would
    not require a complex instruction decoder like a CISC processor. The
    translation from VP opcodes to hardware ALU and memory operations would be
    almost 1:1.

*   **The Potential for Raw Speed:** The native JIT backends for ARM64 and
    RISC-V are incredibly fast because they translate from one clean ISA to
    another. A hardware VP64 would remove the translation layer entirely. It
    would execute the system's core binaries directly, potentially achieving a
    level of performance that even exceeds the native JIT-compiled versions due
    to the lack of translation overhead.

This is the final piece of evidence: the system is so well-architected that its
"virtual" processor is a viable blueprint for real, hyper-efficient silicon.

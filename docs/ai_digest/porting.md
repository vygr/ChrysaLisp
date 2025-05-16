# ChrysaLisp Porting Guide

**Version 1.0**

**Date: May 14, 2025**

## 1. Introduction

This guide provides a comprehensive overview of the process for porting
ChrysaLisp to new host operating systems, CPU architectures, and adapting to
different C/C++ compiler ABIs. ChrysaLisp's design, centered around a Virtual
Processor (VP) and a Platform Interface Implementation (PII), facilitates
portability.

**Scope:**

* Porting the host bootstrap environment (OS interaction, GUI/Audio abstraction).

* Creating a new VP-to-native code translator for a new CPU target.

* Considerations for different C/C++ compiler ABIs on the host.

**Porting Philosophy:**

ChrysaLisp aims for a high degree of hardware independence through its Virtual
Processor. The core of the system is written in VP assembly or Lisp that
compiles to VP. Porting primarily involves:

1. Implementing a PII layer that maps ChrysaLisp's required host OS interactions
   to the new target's system calls.

2. If targeting a new native CPU, creating a new VP instruction translator.

**Phased Approach:**

This guide recommends a phased approach for bringing up ChrysaLisp on a new,
unsupported platform:

1. **Phase 1: Host OS & ABI Porting for VP64 Emulator:**

    * Focus on getting the C++ bootstrap (`src/host/main.cpp`) to compile and
      run on the new host OS and C/C++ ABI.

    * Implement the PII layer for core OS functionalities.

    * Initially, target the existing VP64 emulator (`src/host/vp64.cpp`). This
      allows testing the PII, host build system, and basic ChrysaLisp
      functionality without needing a full native code translator.

    * Optionally, implement PII for GUI and Audio if these are desired and will
      use different underlying libraries on the new host.

2. **Phase 2: Native CPU Target Porting (if applicable):**

    * If the new platform has a CPU architecture not yet supported by a native
      translator, this phase involves creating a new VP instruction translator
      in `lib/trans/`.

This document will detail the steps and considerations for each phase.

**Prerequisites:**

* Familiarity with the ChrysaLisp architecture (refer to `memory_architecture.md`,
  `vp_translation.md`, `host_os.md`, `host_gui.md`, `host_audio.md`).

* Proficiency in C/C++ for the bootstrap and PII layer.

* Proficiency in Lisp for the translator and build system.

* A suitable C/C++ cross-compiler and development environment for the target
  platform.

* Access to the target hardware or a reliable emulator for testing.

---

## 2. Phase 1: Host OS & ABI Porting (for VP64 Emulator)

The goal of this phase is to run the ChrysaLisp system using the VP64 emulator on
the new host OS. This means the ChrysaLisp Lisp code and VP assembly will be
interpreted by the emulator, but the emulator itself and its interactions with
the OS (file system, memory mapping, console I/O, GUI, audio) must be native to
the new host.

### 2.1. Understanding the Host Bootstrap and PII

* **`src/host/main.cpp`:** This is the main entry point when ChrysaLisp starts.
  It's a C++ program responsible for:

    * Loading the ChrysaLisp `boot_image` (which contains pre-compiled VP code).

    * Setting up the initial environment for the VP.

    * Passing tables of host function pointers (PII) to the ChrysaLisp system.
      These allow the VP code (and Lisp running on the VP) to interact with the
      host OS.

* **Platform Interface Implementation (PII):**

    * Located in `src/host/pii_*.cpp` files (e.g., `pii_linux.cpp`,
      `pii_darwin.cpp`).

    * Defines functions that implement the `host_os_funcs`, `host_gui_funcs`,
      and `host_audio_funcs` arrays.

    * `host_os_funcs`: Core OS services like file I/O, memory mapping, time,
      process exit (see `host_os.md`, `sys_pii.md`).

    * `host_gui_funcs`: GUI operations like window creation, drawing primitives,
      event polling (see `host_gui.md`, `service/gui/class.inc`).

    * `host_audio_funcs`: Audio operations like init, play, stop (see
      `host_audio.md`, `service/audio/class.inc`).

### 2.2. Makefile Adaptations (`src/host/Makefile`)

The `src/host/Makefile` needs to be modified:

1. **OS and CPU Detection:**

    * Update the sections that detect `OS` (e.g., `uname`) and `CPU` (`uname -m`)
      to recognize your new target.

    * Define a new `ABI` symbol if necessary (e.g., `WIN64ARM` or use existing
      `ARM64` if the C/C++ ABI aligns with an existing ARM64 target like
      Linux/Arm64, though specific Windows calling conventions will still need
      care).

2. **Compiler and Linker Flags:**

    * Add new `CFLAGS` and `CPPFLAGS` specific to your target's cross-compiler.
      This includes:

        * `-D_HOST_GUI=<value>`: Set to the appropriate value if you're
          porting/using a specific GUI backend (0 for SDL, 1 for FB, 2 for RAW as
          per `gui_fb.c` and `gui_raw.cpp`). If no GUI, this might be undefined
          or a new value.

        * `-D_HOST_AUDIO=<value>`: Similar to `_HOST_GUI` for audio.

        * Include paths for system headers (e.g., Windows SDK).

        * Library paths and libraries to link against (e.g., core Windows
          libraries, GUI/audio libraries if not SDL).

3. **Object and Executable Paths:**

    * Define rules to place compiled objects in a new target-specific directory,
      e.g., `obj/$(CPU)/$(ABI)/$(OS)/`.

    * Ensure the final `main_tui` (and `main_gui` if applicable) executables are
      placed correctly.

4. **PII Source File Selection:**

    * Ensure the Makefile selects your new PII C++ files (e.g.,
      `pii_myos_myarch.cpp`) for compilation when building for the new target.

### 2.3. Implementing PII Functions

This is the most substantial part of Phase 1. You need to create new `.cpp`
files in `src/host/` for your target OS/architecture, or adapt existing ones.

1. **Core OS Functions (`pii_myos_myarch.cpp`):**

    * Implement all functions declared in `sys/pii.h` and exposed via the
      `host_os_funcs` array (see `host_os.md`, `sys_pii.md`). Examples:

        * `pii_open`, `pii_close`, `pii_read`, `pii_write`, `pii_seek`,
          `pii_stat`, `pii_remove`, `pii_unlink`, `pii_dirlist`: Use your host
          OS's native file system APIs.

        * `pii_mmap`, `pii_munmap`, `pii_mprotect`: Use host OS memory mapping
          and protection APIs. This is critical for loading and executing VP
          code.

        * `pii_gettime`: Use host OS time functions.

        * `pii_exit`: Use host OS process exit.

        * `pii_open_shared`, `pii_close_shared`: For inter-process communication
          via shared memory, if this mechanism is used by link drivers on the
          target (`sys/link/class.inc`).

        * `pii_flush_icache`: Essential for JIT-compiled code; use appropriate
          cache control primitives for the target CPU/OS.

        * `pii_random`, `pii_sleep`.

    * Populate the `host_os_funcs` array with pointers to these implementations.

2. **GUI Functions (`gui_myos_myarch.cpp` - Optional):**

    * If you intend to have a native GUI (not TUI-only):

        * Implement functions declared in `service/gui/class.inc` and exposed via
          `host_gui_funcs` (see `host_gui.md`).

        * This may involve:

            * Initializing the native graphics system.

            * Window creation and management.

            * Drawing primitives (box, filled_box, blit).

            * Texture management.

            * Event polling and translation into ChrysaLisp's `SDL_Event`-like
              structure (even if not using SDL, ChrysaLisp's GUI event system
              internally uses these structures as seen in
              `service/gui/lisp.inc`).

            * Clip rectangle management.

        * You might use a cross-platform library like SDL (compiled for your
          target) or directly use native APIs (e.g., Windows GDI/DirectX, Cocoa
          on macOS, X11 on Linux). The `gui_sdl.cpp`, `gui_fb.c`, and
          `gui_raw.cpp` provide examples of different backend approaches.

    * Populate `host_gui_funcs`. If no GUI, this array might be null or point to
      stub functions.

3. **Audio Functions (`audio_myos_myarch.cpp` - Optional):**

    * Similar to GUI, implement functions for `host_audio_funcs` (see
      `host_audio.md`, `service/audio/class.inc`) using native audio APIs or a
      library.

    * Populate `host_audio_funcs`.

### 2.4. Host C/C++ Compiler ABI Considerations

* The C++ bootstrap (`main.cpp`) and the PII functions you write will be
  compiled by your chosen host C/C++ compiler and will adhere to that compiler's
  ABI for the target platform (e.g., Windows ARM64 AAPCS).

* When VP code calls into the PII (via `vp-call-abi` which eventually resolves
  to a call through the `host_os_funcs` / `host_gui_funcs` / `host_audio_funcs`
  tables), the VP translator (or in this phase, the VP64 emulator) must set up
  the call according to this *host C/C++ ABI*.

* The `sys/pii/abi.inc` file (implied by its usage in VP code, e.g.
  `sys/kernel/class.vp` using `abi-args`) is likely where Lisp macros like
  `abi-args` and `abi-trashed` are defined. These macros would be used by the VP
  code that *declares* the PII functions it expects to call, to ensure VP
  registers are correctly mapped to the argument-passing registers and stack
  layout of the target host C ABI.

    * You may need to define a new section in `sys/pii/abi.inc` for your new
      OS/ABI combination, specifying which VP registers correspond to the first,
      second, etc., arguments of a C function, and which registers are
      caller/callee-saved according to the target ABI.

    * The VP64 emulator (`src/host/vp64.cpp`) in its `VP64_CALL_ABI` opcode
      handler performs the actual marshalling of arguments from VP registers to
      what a C function would expect, based on a generic understanding of C
      calling conventions (likely assuming a System V AMD64-like style for its
      existing targets if not explicitly checking `*abi*`). This part of the
      emulator might need to be made ABI-aware if the new target's C ABI is
      significantly different (e.g., regarding stack alignment, shadow space, or
      many floating-point/vector arguments). However, for basic integer/pointer
      arguments, common ABIs often have similarities for the first few arguments
      passed in registers.

### 2.5. Building and Testing the VP64 Emulator

1. **Compile `src/host/main.cpp` and PII files:** Use your cross-compiler and
   the updated Makefile.

2. **VP64 Emulator:** The `src/host/vp64.cpp` provides the VP64 emulator. The
   `main.cpp` will, based on command-line arguments or configuration (e.g., the
   `-e` flag in `run_tui.sh`), decide whether to JIT compile or emulate. For
   Phase 1, you'll force it to use the emulator.

    * The Makefile rule for `main_tui` or `main_gui` might already have a way to
      build an emulator-only version, or you might need to add a compile-time
      flag (like the `run_emu` global in `main.cpp`).

3. **Obtain a `vp64` Boot Image:**

    * You'll need a `boot_image` compiled for the `vp64` target (i.e.,
      containing VP64 bytecode, not native code for another architecture). This
      is typically generated by `make CPU=vp64 ABI=VP64 boot` using a working
      ChrysaLisp environment on another machine.

4. **Run:**

    * Transfer `main_tui` (or `main_gui`), the `vp64` boot image, and the
      necessary ChrysaLisp directory structure (`cmd/`, `lib/`, `apps/` etc.) to
      your target environment.

    * Attempt to run it: `./main_tui obj/vp64/VP64/sys/boot_image -e ... [other
      args]`

5. **Test Basic Functionality:**

    * **Console I/O:** Can the TUI terminal start? Can you type commands? (Tests
      `pii_read`, `pii_write` for console).

    * **File System:** Can commands like `ls` (if ported or a simple `files`
      command) work? Can Lisp load files? (Tests PII file operations).

    * **Lisp REPL:** Start `lisp` and try simple expressions.

    * **GUI/Audio (if PII implemented):** Test basic GUI apps or audio playback.

At the end of Phase 1, you should have a ChrysaLisp system running on the new
host OS via the VP64 emulator. This validates your PII layer and host bootstrap
environment.

---

## 3. Phase 2: Native CPU Target Porting

If your target platform's CPU architecture is not one for which ChrysaLisp
already has a native code translator (e.g., x86-64, Arm64, RISC-V 64 as implied
by `vp_translation.md`), you will need to create one.

This involves writing Lisp code that translates VP assembly instructions into
the native machine code for your target CPU.

### 3.1. Understanding VP-to-Native Translation

* **`vp_translation.md`:** This document is key. It describes the VP
  architecture (registers, instruction set) and the multi-pass translation
  pipeline.

* **Translator Location:** Translators reside in `lib/trans/` as Lisp include
  files (e.g., `lib/trans/x86_64.inc`, `lib/trans/arm64.inc`).

* **Core Components of a Translator File:**

    * **`emit-native-reg?` function:** Maps symbolic VP register names (e.g.,
      `:r0`) to the target CPU's numerical register encodings.

    * **`emit-xxx` Lisp functions:** A set of Lisp functions, one for each VP
      instruction (e.g., `emit-add-rr`, `emit-cpy-cr`). These functions take
      mapped native register numbers and immediate values as arguments and
      generate the corresponding sequence of native machine code bytes. Helper
      macros (like `x64-rr` for x86-64) are often used to construct instruction
      encodings. These bytes are written to an output stream (`*stream*`) and a
      program counter (`*pc*`) is updated.

* **`emit-call-abi` function:** This specific emitter is responsible for
  generating the native instruction sequence to call external C functions
  according to the target CPU's native C ABI (e.g., Windows on Arm64 AAPCS64).
  This is different from `vp-call` which uses ChrysaLisp's internal VP calling
  convention.

* **Multi-Pass Translation:** `lib/trans/vp.inc` (function `emit-translate`)
  handles this. It's needed to resolve forward branches and jumps. The process
  repeats, re-emitting code and updating label addresses, until the generated
  byte stream stabilizes. Your `emit-xxx` functions for branches must correctly
  calculate offsets, potentially supporting short/long forms, and interact with
  the `*offsets*` list if your architecture requires it (like x86-64).

### 3.2. Creating `lib/trans/mycpu_myabi.inc`

1. **Copy an Existing Translator:** Start by copying an existing translator file
   (e.g., `lib/trans/arm64.inc` if your target is ARM-based, or `riscv64.inc` if
   RISC-V based) to `lib/trans/mycpu_myabi.inc`.

2. **Define `emit-native-reg?`:**

    * Adapt this function to map VP registers (`:r0` - `:r14`, `:rsp`) to the
      register numbers/encodings used by your target CPU and its specific ABI's
      register usage (e.g., argument registers, callee-saved, caller-saved,
      stack pointer, link register).

3. **Implement `emit-xxx` Emitter Functions:**

    * This is the most intensive part. For *each* VP instruction listed in
      `vp_vm.md` (or inferred from existing translators), you must write a Lisp
      function (e.g., `(defun emit-add-rr (s d) ...)`).

    * This function will receive native register numbers (as mapped by
      `emit-native-reg?`) and immediate values as its arguments.

    * It must use Lisp macros or functions (which you might also need to write,
      e.g., `mycpu-rrr` for R-type instructions) to construct the exact byte
      sequence for the corresponding native instruction on `mycpu`. This
      requires deep knowledge of `mycpu`'s instruction set architecture (ISA)
      and encoding.

    * Use `(emitm-byte ...)` / `(emitm-short ...)` / `(emitm-int ...)` /
      `(emitm-long ...)` (defined in `lib/trans/vp.inc`) to write the generated
      machine code bytes to the `*stream*` and thereby advance `*pc*`.

    * Pay close attention to:

        * Instruction variants (e.g., register-register, register-immediate).

        * Operand sizes (byte, short, int, long/qword).

        * Sign extension for loads.

        * Addressing modes.

        * Conditional branch logic and offset calculation.

        * Stack operations (`vp-push`, `vp-pop`, `vp-alloc`, `vp-free`) must
          correctly manipulate the native stack pointer according to the ABI.

4. **Implement `emit-call-abi`:**

    * This function receives the VP registers holding arguments for an external
      C call, the target function address (usually in a register), and
      information about the number of arguments.

    * It must generate native code to:

        * Move arguments from VP registers/stack to the locations specified by
          the target's C ABI (e.g., specific registers for the first N
          arguments, then the stack).

        * Align the stack if required by the ABI before the call.

        * Execute the native call instruction (e.g., `BL` on ARM, `CALL` on
          x86).

        * Handle the return value, moving it from the ABI-defined location back
          to the appropriate VP register (usually `:r0`).

        * Restore the stack if necessary.

5. **Stack Frame Setup (`emit-stack-init`):**

    * The `emit-stack-init` function is called by the bootstrap code to set up
      the initial stack for a new task. It typically receives the stack pointer
      (`s`), the function entry point (`f`), and an argument (`x` usually 0). It
      needs to arrange the stack so that when `vp-ret` is eventually executed
      from the task's main function, it returns to a known `exit` routine. The
      `arm64.inc` and `x86_64.inc` examples show setting up a "return" to the
      `tk-load-state` which then `vp-ret`s to the real caller, after pushing the
      `x` argument and the address of an `arm_exit` or `x64_exit` label. The
      details are architecture-specific.

6. **Constants and Helper Macros:**

    * Define any CPU-specific constants or helper Lisp macros (like `riscv64-r`
      or `x64-rr` in existing translators) to simplify instruction encoding.

    * Define `stack_align` and `stack_state` (list of registers saved/restored
      by `tk-save-state`/`tk-load-state`) as per your target ABI's requirements.

### 3.3. Boot Image Generation and Testing

1. **Integrate with `cmd/make.lisp`:**

    * Add your new CPU and ABI symbols to the lists of supported platforms in
      `cmd/make.lisp` or `lib/asm/asm.inc` where these are defined (e.g.,
      `+supported_cpu`, `+supported_abi`).

    * Ensure `make` selects `lib/trans/mycpu_myabi.inc` when compiling for this
      target.

2. **Compile VP Code:** Use `make CPU=mycpu ABI=myabi` to compile some simple
   `.vp` files. Examine the generated object files (`obj/mycpu/myabi/...`) to
   ensure the machine code is correct.

3. **Generate Boot Image:** Run `make CPU=mycpu ABI=myabi boot`.

4. **Test on Target:**

    * Load the native `boot_image` using your ported `main.cpp` (which should
      now directly execute the translated native code, not the emulator).

    * Start with the TUI environment (`run_tui.sh` equivalent).

    * Test extensively, from basic Lisp operations to complex applications.

    * Use a native debugger for `mycpu` to step through generated code if issues
      arise.

---

## 4. Cross-Cutting Concerns

### 4.1. Lisp-Level Configuration

* **`class/lisp/root.inc` (or similar startup file):**

    * The Lisp environment often has global variables like `*os*`, `*cpu*`, and
      `*abi*`. Ensure these are set correctly when ChrysaLisp starts on the new
      platform. This might involve the bootstrap passing this information or
      Lisp querying it via PII.

* **Conditional Code:** Review the Lisp codebase for any sections that are
  conditional on OS, CPU, or ABI, and add cases for your new platform if
  necessary. This is rare in most application-level Lisp code due to the VP
  abstraction but can occur in system libraries.

### 4.2. ChrysaLisp Build System (`cmd/make.lisp`)

As mentioned in 3.3.1, `cmd/make.lisp` needs to be aware of the new platform:

* It uses the `*cpu*` and `*abi*` Lisp variables to determine output paths and
  select the correct translator file from `lib/trans/`.

* Ensure the `compile` function in `lib/asm/asm.inc` and the various `make-*`
  functions in `cmd/make.lisp` correctly handle the new platform identifiers
  when constructing paths and invoking the translator.

---

## 5. Testing and Debugging Strategies

* **Incremental Testing:**

    * **Phase 1:** Start by testing individual PII functions with small C/C++
      test programs on the target host before integrating with the full VP64
      emulator. Test console I/O, file I/O, memory mapping. Then test the VP64
      emulator with a very simple "hello world" ChrysaLisp program.

    * **Phase 2:** Start by translating and testing very simple VP functions
      (e.g., return a constant). Gradually add more complex instructions:
      arithmetic, memory access, branches, then calls.

* **Disassembler:** Have a reliable disassembler for your target CPU to verify
  the machine code generated by your `emit-xxx` functions.

* **Native Debugger:** Use GDB, LLDB, WinDbg, or your target's native debugger
  to:

    * Step through the C++ bootstrap (`main.cpp`) and PII functions.

    * Step through the generated native code from your translator.

* **ChrysaLisp Debug Tools:**

    * Use `(print)` extensively in your Lisp translator code.

    * The `*debug_emit*` and `*debug_inst*` flags (see `vp_assignment.md`) can
      show the VP instructions before and after optimization, and the final Lisp
      forms passed to `emit-translate`.

* **VP64 Emulator as a Reference:** If a native translation behaves
  unexpectedly, compare its register states and memory effects with the VP64
  emulator running the same VP code (requires the PII layer to be solid).

* **Small Test Cases:** Create minimal `.vp` files that exercise specific VP
  instructions or sequences to isolate issues in the translator.

---

## 6. Conclusion

Porting ChrysaLisp is a complex task that requires a methodical approach. By
first focusing on the host OS PII layer with the VP64 emulator, potential issues
with basic OS interaction and C/C++ ABI can be resolved. Subsequently,
developing a new native CPU translator involves careful implementation of
instruction emitters and rigorous testing.

Adherence to ChrysaLisp's internal structures (PII function tables, VP
instruction set, translator framework) and thorough testing are crucial for a
successful port. Referencing existing PII implementations and translator files
for supported platforms will provide valuable guidance.
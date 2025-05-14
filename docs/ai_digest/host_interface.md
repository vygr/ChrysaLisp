# Host Interface

The ChrysaLisp host interface is the bridge between the ChrysaLisp Virtual
Processor (VP) environment and the underlying native operating system and
hardware. It provides access to essential services like file I/O, memory
management, time, GUI rendering, and audio playback. This interface is primarily
defined by a set of C/C++ functions whose addresses are passed to the ChrysaLisp
`sys/load/init` function when a boot image is started.

## Host ABI Vtables

At boot time, `main.cpp` passes pointers to three main "vtables" (arrays of
function pointers) to the ChrysaLisp environment. These tables define the Host
Application Binary Interface (ABI) that ChrysaLisp code uses to interact with
the host.

1. **`host_os_funcs` (Platform Interface Implementation - PII - OS Layer):**

    * Defined in `src/host/pii_*.cpp` (platform-specific: `pii_darwin.cpp`,
      `pii_linux.cpp`, `pii_windows.cpp`).

    * This table provides core operating system services.

    * Functions include:

        * `exit`: Terminate the process.

        * `pii_stat`: Get file status (modification time, size, mode).

        * `pii_open`: Open a file.

        * `pii_close`: Close a file descriptor.

        * `pii_unlink`: Delete a file (used by `rm`).

        * `pii_read`: Read from a file descriptor.

        * `pii_write`: Write to a file descriptor.

        * `pii_mmap`: Map files or anonymous memory into address space.

        * `pii_munmap`: Unmap memory.

        * `pii_mprotect`: Change memory protection (e.g., to make code
          executable).

        * `pii_gettime`: Get current time in microseconds.

        * `pii_open_shared`: Open/create a shared memory object (POSIX
          `shm_open` or Windows `CreateFileMapping`). Used for inter-process
          links.

        * `pii_close_shared`: Close/unlink a shared memory object.

        * `pii_flush_icache` (or `pii_clear_icache`): Ensure instruction cache
          coherency after writing/modifying code in memory.

        * `pii_dirlist`: List directory contents.

        * `pii_remove`: Remove a file or recursively a directory.

        * `pii_seek`: Seek within a file.

        * `pii_random`: Get cryptographically secure random bytes.

        * `pii_sleep`: Sleep for a specified number of microseconds.

2. **`host_gui_funcs` (GUI Layer):**

    * The specific implementation is chosen at compile time based on the `GUI`
      make variable, which sets the `_HOST_GUI` preprocessor define.

    * `_HOST_GUI = 0` (Default or `GUI=sdl`): Uses `src/host/gui_sdl.cpp`. This
      relies on the SDL2 library for windowing, event handling, and 2D
      rendering.

    * `_HOST_GUI = 1` (`GUI=fb`): Uses `src/host/gui_fb.c`. This is for direct
      Linux Framebuffer access, along with direct `/dev/tty` for keyboard and
      `/dev/input/mice` for mouse.

    * `_HOST_GUI = 2` (`GUI=raw`): Uses `src/host/gui_raw.cpp`. This is a
      minimal, raw pixel buffer implementation, likely for testing or
      environments without SDL/framebuffer, where an external mechanism might
      display the buffer.

    * Common functions provided by these drivers (interfacing with
      `service/gui/composite.vp` and `gui/ctx/*`):

        * `host_gui_init`: Initialize the GUI system (e.g., create window,
          renderer). Takes desired dimensions and flags.

        * `host_gui_deinit`: Shut down the GUI system.

        * `host_gui_box`: Draw an outline rectangle.

        * `host_gui_filled_box`: Draw a filled rectangle.

        * `host_gui_blit`: Copy/blend a texture (source drawable) to the
          screen/backbuffer. Handles transparency and color modulation based on
          texture mode.

        * `host_gui_set_clip`: Set the clipping rectangle for drawing
          operations.

        * `host_gui_set_color`: Set the current drawing color (RGBA).

        * `host_gui_set_texture_color`: Set the color modulation for a texture
          (used in glyph rendering).

        * `host_gui_destroy_texture`: Free a texture.

        * `host_gui_create_texture`: Create a texture from pixel data. Handles
          different modes (e.g., alpha-only for glyphs vs. full ARGB).

        * `host_gui_begin_composite`: (SDL) Sets render target to backbuffer.
          (FB/Raw) Might be a no-op.

        * `host_gui_end_composite`: (SDL) Resets render target. (FB/Raw) Might
          be a no-op.

        * `host_gui_flush`: Copies the relevant portion of the backbuffer to the
          visible screen.

        * `host_gui_resize`: Handles window resize events.

        * `host_gui_poll_event`: Polls for host system events (keyboard, mouse,
          window) and translates them into an `SDL_Event`-like structure.

3. **`host_audio_funcs` (Audio Layer):**

    * The implementation is chosen by `_HOST_AUDIO`. Currently, `_HOST_AUDIO =
      0` uses `src/host/audio_sdl.cpp`.

    * Relies on SDL2_mixer library.

    * Functions include:

        * `host_audio_init`: Initializes the audio system
          (SDL_Init(SDL_INIT_AUDIO), Mix_OpenAudio).

        * `host_audio_deinit`: Shuts down the audio system.

        * `host_audio_add_sfx`: Loads a sound effect (currently only `.wav`)
          and returns a handle.

        * `host_audio_play_sfx`: Plays a loaded sound effect by its handle.

        * `host_audio_change_sfx`: Pauses, resumes, or stops a playing sound
          effect.

        * `host_audio_remove_sfx`: Frees a loaded sound effect.

## Bootstrapping, Emulation, and the Install Process

ChrysaLisp has a clever bootstrapping mechanism that can involve a VP64 bytecode
interpreter.

**`main.cpp` (The Host Program Entry Point):**

1. **Argument Parsing:**

    * It expects the path to a ChrysaLisp boot image as `argv[1]`.

    * It checks for an `-e` flag among the arguments.

2. **`-e` Flag (Emulator Mode):**

    * If `-e` is present, `run_emu` is set to `true`.

    * Crucially, `argv[1]` (the boot image path) is **overridden** to
      `obj/vp64/VP64/sys/boot_image`. This specific boot image is compiled for
      the `vp64` target (i.e., it's VP64 bytecode).

3. **Loading the Boot Image:**

    * `pii_open` opens the boot image file.

    * `pii_mmap` maps the file's content into memory.

4. **Execution Path Decision:**

    * **If `run_emu` is `true`:**

        * A new stack for the VP64 interpreter is allocated (`pii_mmap`).

        * The `vp64()` function (defined in `src/host/vp64.cpp`) is called. It's
          passed:

            * The memory address of the loaded VP64 boot image (`data`).

            * The newly allocated stack.

            * The original `argv` (with the potentially modified boot image
              path).

            * Pointers to `host_os_funcs`, `host_gui_funcs`, and
              `host_audio_funcs`.

        * `vp64()` then interprets the VP64 bytecode instructions from the boot
          image. When the bytecode executes a `VP64_CALL_ABI` instruction, the
          interpreter uses the passed-in host function tables to call the
          appropriate native C/C++ function.

    * **If `run_emu` is `false` (Native Mode):**

        * The loaded boot image (which must be native code for the host CPU/ABI)
          is made executable using `pii_mprotect(..., mmap_exec)` and
          `pii_flush_icache`.

        * The `main.cpp` then calls directly into the ChrysaLisp `sys/load/init`
          function within the boot image. The entry point address is hardcoded
          or read from a fixed offset in the boot image header (specifically,
          `data[5]` which is `fn_header_entry` if `data` is `uint16_t*`).

        * The `sys/load/init` function receives `argv` and the host function
          tables as arguments.

**The `vp64()` Interpreter (`src/host/vp64.cpp`):**

* This is a C++ function that implements a simple interpreter loop for VP64
  bytecode.

* It maintains a set of 16 virtual registers (`regs[16]`) and a program counter
  (`pc`).

* It fetches opcodes (which are `int16_t`), decodes them, and executes the
  corresponding operation on the virtual registers or memory (which is the
  host's memory space).

* The `VP64_CALL_ABI` opcode is special: it looks up a function pointer in the
  `host_os_funcs`, `host_gui_funcs`, or `host_audio_funcs` (the table is likely
  implied by the specific ABI call number) and calls it, marshalling arguments
  from the virtual registers and placing the return value back into a virtual
  register.

**Install Process (`make install`):**

* The `Makefile`'s `install` target is: `clean hostenv tui gui inst`.

* The `inst` target rule is: `./run_tui.sh -n 8 -i -e -f`.

* Let's break down the `run_tui.sh -n 8 -i -e -f` command:

    * `run_tui.sh`: This script (and its PowerShell equivalent `run_tui.ps1`) is
      designed to launch multiple ChrysaLisp nodes connected in a default
      fully-connected mesh topology.

    * `-n 8`: Specifies 8 nodes.

    * `-i`: This tells `run_tui.sh` to set the initial script for the primary
      node (CPU 0) to `apps/tui/install.lisp` instead of the default
      `apps/tui/tui.lisp`.

    * `-e`: This is the crucial part for installation. It tells `main.cpp` (via
      `run_tui.sh` which passes arguments through) to use the emulator mode.

    * `-f`: Runs the primary node in the foreground.

* **Bootstrapping Sequence for Installation:**

    * `make install` triggers `run_tui.sh -e ... -i ...`.

    * `run_tui.sh` launches 8 instances of `main_tui` (or `main_tui.exe`).

    * Each `main_tui` instance, due to the `-e` flag, loads
    `obj/vp64/VP64/sys/boot_image` and starts the `vp64()` interpreter.

    * The primary node's `vp64()` interpreter, after initializing, receives the
    `-run apps/tui/install.lisp` argument.

    * The Lisp environment within the emulated primary node then executes
    `apps/tui/install.lisp`.

    * `apps/tui/install.lisp` likely contains commands such as `(make all
    platforms boot)` or similar, which compiles all ChrysaLisp source code into
    *native* object files (`obj/$(CPU)/$(ABI)/...`) and creates native boot
    images.

* **Outcome:** The initial build and "installation" (compilation of the entire
system into native code) of ChrysaLisp is performed by running the Lisp `make`
command *inside the VP64 emulated environment*. After this, subsequent runs
(without `-e`) can use the newly built native boot images.

## Run Scripts and Network Topologies

The `run*.sh` (for Linux/macOS) and `run*.ps1` (for Windows PowerShell) scripts
are used to launch multiple ChrysaLisp VP nodes and connect them in various
network topologies. They share common helper functions from `funcs.sh` or
`funcs.ps1`.

**Helper Functions (e.g., in `funcs.sh`):**

* `zero_pad <num>`: Pads a number with leading zeros to ensure 3-digit link
  names (e.g., `001`, `008`, `012`).

* `add_link <src_cpu> <dst_cpu>`:

    * Takes two CPU numbers (relative to the `base_cpu` for the script).

    * Pads them using `zero_pad`.

    * Constructs a link string like `-l 001-002`.

    * Ensures that links are specified consistently (e.g., always
      `smaller-larger`) to avoid duplicates in the `links` variable for a node.

    * Appends the link string to a global `links` variable if it's a new link
      for the current node being configured.

* `wrap <cpu_num> <num_total_cpus>`: Calculates `$cpu_num % $num_total_cpus`
  for circular topologies.

* `boot_cpu_gui <cpu_idx_in_script> "<link_args_string>"`, `boot_cpu_tui
  <cpu_idx_in_script> "<link_args_string>"`:

    * These are the core functions for launching a single ChrysaLisp node.

    * They construct the command line: `./obj/$CPU/$ABI/$OS/main_gui
      obj/$CPU/$ABI/sys/boot_image <link_args_string> $emu -run
      <initial_script>`.

    * The `$emu` variable will be "-e" if emulator mode is active for the
      script.

    * The `<initial_script>` is typically `service/gui/app.lisp` for
      `boot_cpu_gui` and `apps/tui/tui.lisp` for `boot_cpu_tui` for the primary
      node (node 0 in the script's context). Other nodes usually don't get an
      initial `-run` script and just start their link drivers.

    * They handle backgrounding (`&`) for all but the primary foreground node
      (if `-f` is used).

**Topologies:**

* **`run.sh`, `run.ps1` (Default - Fully Connected Mesh):**

    * Typically launches 10 nodes (`num_cpu=10`).

    * Each node `cpu` is linked to every other node `lcpu` (`for lcpu=0;
      lcpu<$num_cpu; lcpu++`). This creates a fully connected mesh where every
      node has a direct link to every other node.

* **`run_ring.sh`, `run_ring.ps1`:**

    * Launches `num_cpu` nodes (default 64).

    * Each node `cpu` is linked to `cpu-1` (wrapped) and `cpu+1` (wrapped),
      forming a ring.

* **`run_mesh.sh`, `run_mesh.ps1`:**

    * Launches `num_cpu * num_cpu` nodes (default 8x8 = 64).

    * Nodes are arranged in a 2D grid. Each node `(cpu_x, cpu_y)` is linked to
      its neighbors: `(cpu_x-1, cpu_y)`, `(cpu_x+1, cpu_y)`, `(cpu_x, cpu_y-1)`,
      `(cpu_x, cpu_y+1)`, with wrapping at the edges (toroidal mesh).

* **`run_cube.sh` (No `.ps1` directly provided, but logic is similar):**

    * Launches `num_cpu * num_cpu * num_cpu` nodes (default 4x4x4 = 64).

    * Nodes in a 3D grid. Each node `(x,y,z)` is linked to its 6 Cartesian
      neighbors `(x±1,y,z)`, `(x,y±1,z)`, `(x,y,z±1)`, with wrapping (toroidal
      cube/3D torus).

* **`run_star.sh`, `run_star.ps1`:**

    * Launches `num_cpu` nodes (default 64).

    * Node 0 is the central hub. All other nodes `cpu > 0` are linked only to
      node 0.

* **`run_tree.sh`, `run_tree.ps1`:**

    * Launches `num_cpu` nodes (default 64).

    * Connects nodes in a binary tree structure:

        * Node `cpu` is linked to its parent `(cpu-1)/2`.

        * Node `cpu` is linked to its left child `(cpu*2)+1` (if it exists).

        * Node `cpu` is linked to its right child `(cpu*2)+2` (if it exists).

**Passing Arguments:**

The run scripts accept common arguments:

* `-n <count>`: Number of nodes (or side length for mesh/cube).

* `-b <base_offset>`: Base number for CPU IDs, useful for running multiple
  topologies side-by-side without link name clashes.

* `-e`: Run in emulator mode (passes `-e` to `main_gui`/`main_tui`).

* `-f`: Run the primary node in the foreground.

* `-i`: (for `run_tui.sh`) Run `apps/tui/install.lisp` on the primary node.

## Stop Scripts

* **`stop.sh`:** Uses `killall main_gui -KILL` and `killall main_tui -KILL` to
  forcefully terminate all ChrysaLisp node processes. It also removes link files
  from `/tmp/` (e.g., `rm -f /tmp/???-???`).

* **`stop.ps1`:** Uses `Stop-Process -Name main_gui -Force` and `Stop-Process
  -Name main_tui -Force`.

* **`stop.bat`:** Uses `taskkill /IM main_tui.exe /F` and `taskkill /IM
  main_gui.exe /F`.

These scripts provide a simple way to clean up all running ChrysaLisp processes.

## Makefile Overview (`Makefile`)

* **Variables:**

    * `OS`, `CPU`, `ABI`: Determined by `uname` (or hardcoded for Windows in
      scripts).

    * `GUI`: Can be set externally (e.g., `make GUI=fb`). Defaults likely to
      SDL.

    * `HOST_GUI`: Preprocessor define set based on `$(GUI)` (0 for SDL, 1 for
      FB, 2 for Raw).

    * `HOST_AUDIO`: Preprocessor define (currently fixed at 0 for SDL_mixer).

* **Targets:**

    * `all`: Default, builds `hostenv`, `tui`, and `gui`.

    * `gui`: Builds the GUI-enabled main executable
      (`obj/$(CPU)/$(ABI)/$(OS)/main_gui`).

    * `tui`: Builds the TUI-only main executable
      (`obj/$(CPU)/$(ABI)/$(OS)/main_tui`).

    * `hostenv`: Creates `cpu`, `os`, `abi` files in the current directory,
      which are likely read by Lisp `make` scripts to determine the current
      build environment. It also creates necessary object directories.

    * `install`: Runs `clean`, `hostenv`, `tui`, `gui`, then `inst`. The `inst`
      rule executes `./run_tui.sh -n 8 -i -e -f`, performing the initial system
      compilation using the emulator as described above.

    * `snapshot`: Creates `snapshot.zip` containing the VP64 boot image and
      pre-built Windows executables, for distribution.

    * `clean`: Removes all `obj` directories and unpacks `snapshot.zip` (to
      restore pre-built binaries and the VP64 boot image).

* **Compilation & Linking:**

    * Separate compilation rules for `.cpp` and `.c` files into GUI-specific
      object files (in `$(OBJ_DIR_GUI)`) and TUI-specific object files (in
      `$(OBJ_DIR_TUI)`).

    * GUI builds conditionally include SDL cflags (`sdl2-config --cflags`) and
      link against SDL and SDL_mixer libs (`sdl2-config --libs -lSDL2_mixer`),
      unless `GUI=fb` is specified.

    * The `_HOST_GUI` and `_HOST_AUDIO` defines are passed to the compiler to
      select the correct host driver code.

* **Dependency Management:** `-include $(OBJ_FILES:.o=.d)` includes
  auto-generated dependency files.

This detailed host setup allows ChrysaLisp to be both bootstrapped on new
platforms using its own VP64 interpreter and then run natively for performance,
while providing a flexible way to interface with diverse host system
capabilities.
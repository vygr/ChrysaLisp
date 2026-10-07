# Host Interface

The ChrysaLisp host interface is the bridge between the ChrysaLisp Virtual
Processor (VP) environment and the underlying native operating system and
hardware. It provides access to essential services like file I/O, memory
management, time, GUI rendering, and audio playback. This interface is
primarily defined by a set of C/C++ functions whose addresses are passed to the
ChrysaLisp `sys/load/init` function when a boot image is started.

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

        * `pii_open_shared`: Open/create the shared memory of a link, a file
          in `/tmp` on macOS and Linux, Windows `CreateFileMapping`. It
          waits for the other end to make it. Used for inter-process links.

        * `pii_close_shared`: Close the shared memory of a link. The file is
          removed by the launch scripts.

        * `pii_shm_open`: Shared memory that is only ever memory, POSIX
          `shm_open` or Windows `CreateFileMapping` with no file, so nothing
          is written to disk. One node makes it under a key, a 64 bit
          number, others on the machine find it by the key, and it never
          waits, one that is not there to be found is an error, and so is
          one that is there to be made. Its name is `clpx-` and the key in
          hex. Used for the pixels of a pixmap that several nodes draw on.

        * `pii_shm_close`: Close it. The node that made it lets go of the
          key as well, and the memory lasts till the last node has unmapped
          it. A node that is killed lets go of nothing, so on macOS and
          Linux each one is noted in `/tmp/chrysalisp_shm_<name>` with the
          pid of its maker, and the host program run as `main_tui
          -shm_sweep` lets go of those of the dead. `stop.sh` runs it.

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

    * With no `GUI=` given the driver is `sdl3` if SDL3 is on the machine,
      else `sdl`.

    * `_HOST_GUI = 0` (`GUI=sdl`): Uses `src/host/gui_sdl.cpp`.
      This relies on the SDL2 library for windowing, event handling, and 2D
      rendering.

    * `_HOST_GUI = 1` (`GUI=fb`): Uses `src/host/gui_fb.c`. This is for direct
      Linux Framebuffer access, along with direct `/dev/tty` for keyboard and
      `/dev/input/mice` for mouse.

    * `_HOST_GUI = 2` (`GUI=raw`): Uses `src/host/gui_raw.cpp`. The drawing is
      done by the driver itself into a pixel buffer, and SDL2 is used only for
      the window, the events, and to show the buffer.

    * `_HOST_GUI = 3` (`GUI=sdl3`): Uses `src/host/gui_sdl3.cpp`. The GUI on
      SDL3, with SDL's GPU renderer for the 2D drawing. It is the one driver
      that can draw a shader on the GPU, see the shader calls below, for
      which it needs SDL 3.4 or later, on an older SDL3 it is built without.
      It runs on a desktop or, with no desktop, on the bare display of a
      Linux machine.

    * `_HOST_GUI = 4` (`GUI=raw3`): The raw driver, `src/host/gui_raw.cpp`,
      with SDL3 for the window and events in place of SDL2.

    * SDL2 and SDL3 give their calls the same names, so one program can not
      link both. `GUI=sdl` and `GUI=raw` are SDL2 programs, `GUI=sdl3` and
      `GUI=raw3` are SDL3 programs, each with the audio driver that goes with
      it.

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

        * `host_gui_flush`: Copies the relevant portion of the backbuffer to
          the visible screen.

        * `host_gui_resize`: Handles window resize events.

        * `host_gui_poll_event`: Polls for host system events (keyboard,
          mouse, window) and gives each as a `host_gui_event`, the one
          record every driver fills, see `src/host/gui_event.h` and
          `sys/pii/lisp.inc`.

        * `host_gui_clip_put`, `host_gui_clip_get`, `host_gui_clip_free`: Put
          text on the host clipboard, get the text that is on it, and free
          what a get returned.

    * The shader calls, at the end of the table. A driver that can not draw
      a shader has them all the same, and answers format 0. Only the sdl3
      driver can. See `docs/ai_digest/shader_language.md`.

        * `host_gui_shader_format`: The shading language the driver takes, 0
          none, 1 Metal Shading Language text, 2 a SPIR-V module.

        * `host_gui_shader_create`: A shader from a vertex and a fragment
          shader in that language, each given as bytes and a length. Returns
          a handle, or 0. The entry points are `vertex_main` and
          `fragment_main`. The handle is given at once and the shader is
          built on a thread, a driver can take many seconds over it.

        * `host_gui_shader_destroy`: Free a shader.

        * `host_gui_shader_texture`: A texture of a width and height that a
          shader can draw into, and that `host_gui_blit` can then draw.

        * `host_gui_shader_draw`: Draw a shader into such a texture, with a
          block of bytes as its inputs, and a `host_gui_rect`, the part of
          the texture to draw, or 0 for all of it. Returns 1 if it drew, and
          -1 if the shader did not build. One draw is on the go at a time,
          while the GPU has not finished the last, or the shader is still
          being built, this draws nothing and returns 0, and the caller tries
          again later. That is so a GPU that takes long over a frame can be given
          it a strip at a time, with the GUI drawn in between.

        * `host_gui_read_texture`: Read a texture back as 32 bit ARGB pixels,
          premultiplied, into memory of the width, height and stride given.
          Returns 1 if it could.

3. **`host_audio_funcs` (Audio Layer):**

    * The implementation is chosen by `_HOST_AUDIO`, which follows the GUI
      driver. `_HOST_AUDIO = 0` uses `src/host/audio_sdl.cpp`, on SDL2 and
      the SDL2_mixer library. `_HOST_AUDIO = 1` uses
      `src/host/audio_sdl3.cpp`, on SDL3 alone, there is no mixer library to
      depend on. `_HOST_AUDIO = 2` uses `src/host/audio_alsa.cpp`, on ALSA,
      for the frame buffer GUI, which has no SDL under it. A build with no
      audio driver has no table, and the audio service does not start.

    * The mixing is in `src/host/mixer.h`, which has no SDL in it, nor any
      other library. Sounds are held as float stereo at 44.1kHz, 32 play at
      once, each with its pan, and the mix is limited, not clipped. A driver
      on top of it does three things. It opens a device and, when the device
      wants more, calls `mixer_mix`. It reads a sound file into float stereo
      for `mixer_add`. And it holds a lock of its own round every call to
      the mixer. The SDL3 driver is those three and little else.

    * A wav file is read by `src/host/wav.h`, which has no library in it
      either. PCM of 8, 16, 24 and 32 bits and 32 bit float, any number of
      channels, any rate, into the float stereo the mixer wants. The ALSA
      driver is `mixer.h`, `wav.h`, a thread that feeds the device, and a
      lock, 173 lines. It is what a driver for a machine with no host under
      it would look like.

    * The mixing is on a thread of the host, the one the device calls on,
      and not a ChrysaLisp task. Tasks are co-operative, a task runs when
      the ones before it let go, and a sound device that is not fed in time
      is heard. A task that asked to be woken every 5ms on a node that was
      also shading tiles of the raymarch shader was up to 44ms late on an
      Apple M4 Max and 171ms late on a Raspberry Pi 4. A mixer that was a
      task would have to keep that much sound queued ahead, which is that
      much delay on every sound effect, and a task that held its node for a
      second would still break it.

    * Functions include:

        * `host_audio_init`: Initializes the audio system
          and opens the device.

        * `host_audio_deinit`: Shuts down the audio system.

        * `host_audio_add_sfx`: Loads a sound effect (currently only `.wav`)
          and returns a handle.

        * `host_audio_play_sfx`: Plays a loaded sound effect by its handle,
          with a stereo pan from -255 (left) to 255 (right).

        * `host_audio_change_sfx`: Pauses, resumes, or stops a playing sound
          effect.

        * `host_audio_remove_sfx`: Frees a loaded sound effect.

## Bootstrapping, Emulation, and the Install Process

ChrysaLisp has a clever bootstrapping mechanism that can involve a VP64
bytecode interpreter.

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

        * `vp64()` then interprets the VP64 bytecode instructions from the
          boot image. When the bytecode executes a `VP64_CALL_ABI`
          instruction, the interpreter uses the passed-in host function
          tables to call the appropriate native C/C++ function.

    * **If `run_emu` is `false` (Native Mode):**

        * The loaded boot image (which must be native code for the host
          CPU/ABI) is made executable using `pii_mprotect(..., mmap_exec)`
          and `pii_flush_icache`.

        * The `main.cpp` then calls directly into the ChrysaLisp
          `sys/load/init` function within the boot image. The entry point
          address is hardcoded or read from a fixed offset in the boot image
          header (specifically, `data[5]` which is `fn_header_entry` if `data`
          is `uint16_t*`).

        * The `sys/load/init` function receives `argv` and the host function
          tables as arguments.

**The `vp64()` Interpreter (`src/host/vp64.cpp`):**

* This is a C++ function that implements a simple interpreter loop for VP64
  bytecode.

* It maintains a set of 16 virtual registers (`regs[16]`) and a program
  counter (`pc`).

* It fetches opcodes (which are `int16_t`), decodes them, and executes the
  corresponding operation on the virtual registers or memory (which is the
  host's memory space).

* The `VP64_CALL_ABI` opcode is special: it looks up a function pointer in the
  `host_os_funcs`, `host_gui_funcs`, or `host_audio_funcs` (the table is
  likely implied by the specific ABI call number) and calls it, marshalling
  arguments from the virtual registers and placing the return value back into
  a virtual register.

**Install Process (`make install`):**

* The `Makefile`'s `install` target is: `clean hostenv tui gui inst`.

* The `inst` target rule is: `./run_tui.sh -i -e -f`.

* Let's break down the `run_tui.sh -i -e -f` command:

    * `run_tui.sh`: This script (and its PowerShell equivalent `run_tui.ps1`)
      is designed to launch multiple ChrysaLisp nodes connected in a default
      fully-connected mesh topology.

    * `-i`: This tells `run_tui.sh` to set the initial script for the primary
      node (CPU 0) to `apps/tui/install.lisp` instead of the default
      `apps/tui/tui.lisp`.

    * `-e`: This is the crucial part for installation. It tells `main.cpp`
      (via `run_tui.sh` which passes arguments through) to use the emulator
      mode.

    * `-f`: Runs the primary node in the foreground.

* **Bootstrapping Sequence for Installation:**

    * `make install` triggers `run_tui.sh -e ... -i ...`.

    * `run_tui.sh` launches 8 instances of `main_tui` (or `main_tui.exe`).

    * Each `main_tui` instance, due to the `-e` flag, loads
      `obj/vp64/VP64/sys/boot_image` and starts the `vp64()` interpreter.

    * The primary node's `vp64()` interpreter, after initializing, receives
      the `-run apps/tui/install.lisp` argument.

    * The Lisp environment within the emulated primary node then executes
      `apps/tui/install.lisp`.

    * `apps/tui/install.lisp` likely contains commands such as `(make all
      platforms boot)` or similar, which compiles all ChrysaLisp source code
      into *native* object files (`obj/$(CPU)/$(ABI)/...`) and creates
      native boot images.

* **Outcome:** The initial build and "installation" (compilation of the entire
  system into native code) of ChrysaLisp is performed by running the Lisp
  `make` command *inside the VP64 emulated environment*. After this,
  subsequent runs (without `-e`) can use the newly built native boot images.

## Run Scripts and Network Topologies

The `run*.sh` (for Linux/macOS) and `run*.ps1` (for Windows PowerShell)
scripts are used to launch multiple ChrysaLisp VP nodes and connect them in
various network topologies. They share common helper functions from
`funcs.sh` or `funcs.ps1`.

**Helper Functions (e.g., in `funcs.sh`):**

* `zero_pad <num>`: Pads a number with leading zeros to ensure 3-digit link
  names (e.g., `001`, `008`, `012`).

* `add_link <src_cpu> <dst_cpu>`:

    * Takes two CPU numbers (relative to the `base_cpu` for the script).

    * Pads them using `zero_pad`.

    * Constructs a link string like `-l 001-002`.

    * Ensures that links are specified consistently (e.g., always
      `smaller-larger`) to avoid duplicates in the `links` variable for a
      node.

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
      `boot_cpu_gui` and `apps/tui/tui.lisp` for `boot_cpu_tui` for the
      primary node (node 0 in the script's context). Other nodes usually
      don't get an initial `-run` script and just start their link drivers.

    * They handle backgrounding (`&`) for all but the primary foreground
      node (if `-f` is used).

**Topologies:**

* **`run.sh`, `run.ps1` (Default - Fully Connected Mesh):**

    * Typically launches 10 nodes (`num_cpu=10`).

    * Each node `cpu` is linked to every other node `lcpu` (`for lcpu=0;
      lcpu<$num_cpu; lcpu++`). This creates a fully connected mesh where
      every node has a direct link to every other node.

* **`run_ring.sh`, `run_ring.ps1`:**

    * Launches `num_cpu` nodes (default 64).

    * Each node `cpu` is linked to `cpu-1` (wrapped) and `cpu+1` (wrapped),
      forming a ring.

* **`run_mesh.sh`, `run_mesh.ps1`:**

    * Launches `num_cpu * num_cpu` nodes (default 8x8 = 64).

    * Nodes are arranged in a 2D grid. Each node `(cpu_x, cpu_y)` is linked to
      its neighbors: `(cpu_x-1, cpu_y)`, `(cpu_x+1, cpu_y)`, `(cpu_x,
      cpu_y-1)`, `(cpu_x, cpu_y+1)`, with wrapping at the edges (toroidal
      mesh).

* **`run_cube.sh` (No `.ps1` directly provided, but logic is similar):**

    * Launches `num_cpu * num_cpu * num_cpu` nodes (default 4x4x4 = 64).

    * Nodes in a 3D grid. Each node `(x,y,z)` is linked to its 6 Cartesian
      neighbors `(x±1,y,z)`, `(x,y±1,z)`, `(x,y,z±1)`, with wrapping
      (toroidal cube/3D torus).

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

* `-e`: Run in emulator mode (passes `-e` to `main_gui`/`main_tui`).

* `-f`: Run the primary node in the foreground.

* `-i`: (for `run_tui.sh`) Run `apps/tui/install.lisp` on the primary node.

## Stop Scripts

* **A session stops itself.** The shell launch scripts keep the pid of each
  node they start, and give the links of a launch names of its own, six
  characters of base 36 from a random number. A node that starts more nodes,
  `(node-spawn)`, leaves their pids and link names in
  `/tmp/chrysalisp_<pid>.session`. A session lives while it has a front, a
  way in, a terminal or a desktop. When the last front has gone the script,
  or a watch it leaves behind, stops the rest of the nodes and removes their
  link and session files. So two sessions on one machine do not meet, and
  stopping one leaves the other alone.

* **A desktop is a node.** `(node-spawn num kind script)` can start either
  host program, `:gui` or `:tui`, whichever this node is, and give the new
  node a script to run. `nodes -g 1` starts a GUI node that runs the GUI
  service, a desktop, on a TUI network as well, and `nodes -t 1` adds a node
  on the lighter TUI host to a GUI network. A node exits when its GUI quits.

* **`stop.sh`:** Uses `killall main_gui -KILL` and `killall main_tui -KILL`
  to forcefully terminate all ChrysaLisp node processes, whoever started
  them. It also removes link and session files from `/tmp/`. It is for when
  a session has not stopped itself.

* **`stop.ps1`:** Uses `Stop-Process -Name main_gui -Force` and `Stop-Process
  -Name main_tui -Force`.

* **`stop.bat`:** Uses `taskkill /IM main_tui.exe /F` and `taskkill /IM
  main_gui.exe /F`.

These scripts provide a simple way to clean up all running ChrysaLisp
processes.

## Makefile Overview (`Makefile`)

* **Variables:**

    * `OS`, `CPU`, `ABI`: Determined by `uname` (or hardcoded for Windows in
      scripts).

    * `GUI`: Set externally, `make GUI=sdl` say. One of `sdl3`, `sdl`,
      `raw3`, `raw` and `fb`. Not given, it is `sdl3` if `pkg-config` knows
      of SDL3 or there is an `sdl3_prefix` file, else `sdl`.

    * `HOST_GUI`: Preprocessor define set from `$(GUI)`, 0 for `sdl`, 1 for
      `fb`, 2 for `raw`, 3 for `sdl3`, 4 for `raw3`.

    * `HOST_AUDIO`: Preprocessor define, 0 for the SDL2_mixer driver, 1 for
      the SDL3 driver, 2 for the ALSA driver. It follows the GUI driver,
      `GUI=fb` has the ALSA driver if `pkg-config` knows of ALSA, and none if
      it does not.

    * `SDL3_PREFIX`: The folder SDL3 is installed in, for a machine where
      `pkg-config` does not know of it, one built from source say. It is
      kept in the file `sdl3_prefix`, so it need be given only once.

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
      rule executes `./run_tui.sh -n 8 -i -e -f`, performing the initial
      system compilation using the emulator as described above.

    * `snapshot`: Creates `snapshot.zip` containing the VP64 boot image and
      pre-built Windows executables, for distribution.

    * `clean`: Removes all `obj` directories and unpacks `snapshot.zip` (to
      restore pre-built binaries and the VP64 boot image).

* **Compilation & Linking:**

    * Separate compilation rules for `.cpp` and `.c` files into GUI-specific
      object files (in `$(OBJ_DIR_GUI)`) and TUI-specific object files (in
      `$(OBJ_DIR_TUI)`).

    * Each GUI driver has an object folder of its own, so a change of `GUI=`
      builds that driver and links `main_gui` again, and a change back does
      not build it all a second time.

    * SDL2 builds take their flags from `sdl2-config`, and link SDL2 and
      SDL2_mixer. SDL3 builds take theirs from `pkg-config sdl3`, or from
      `SDL3_PREFIX`. `GUI=fb` links no SDL.

    * The `_HOST_GUI` and `_HOST_AUDIO` defines are passed to the compiler
      to select the correct host driver code.

* **Dependency Management:** `-include $(OBJ_FILES:.o=.d)` includes
  auto-generated dependency files.

This detailed host setup allows ChrysaLisp to be both bootstrapped on new
platforms using its own VP64 interpreter and then run natively for
performance, while providing a flexible way to interface with diverse host
system capabilities.

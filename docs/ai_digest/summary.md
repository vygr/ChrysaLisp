# ChrysaLisp System Summary

ChrysaLisp is a distinct Lisp operating system and environment with a
microkernel-like architecture, a rich GUI system, and a suite of applications.
It features its own object system, task management, inter-task communication
via message passing (mail), and a platform abstraction layer (PII). The system
includes a self-hosting build system and supports compilation to native code
for various architectures (x86_64, ARM64, RISC-V64) as well as a custom Virtual
Processor (VP64).

## 1. Core System and Kernel (`sys/` directory)

* **Kernel (`sys/kernel/kernel.lisp`):** The central component responsible for
bootstrapping the system. It initializes static system resources, task
management, mail system, and memory management. It then processes command-line
arguments, starts link drivers for inter-node communication, and enters a main
loop to service kernel mail and manage task scheduling.

* **Task Management (`sys/task/`):** Provides preemptive multitasking. Tasks
have priorities, private stacks, mailboxes, and can be started
(`sys_task:start`), stopped (`sys_task:stop`), suspended/resumed, and put to
sleep. The kernel manages task lists and a timer list for scheduling.

* **Mail System (`sys/mail/`):** The primary mechanism for inter-task
communication. Tasks can allocate mailboxes (`mail-alloc-mbox`), declare
services with named mailboxes (`mail-declare`), send messages (`mail-send`),
read messages (`mail-read`), and poll/select on multiple mailboxes
(`mail-poll`, `mail-select`). The system supports parceling large messages and
routing messages across nodes via link drivers.

* **Memory Management (`sys/mem/`):** Implements a heap-based memory allocator
(`sys_mem_heaps`) with cell-based allocation for different size ranges.
Provides functions for `alloc`, `calloc`, `free`, `realloc`, and `collect`
(garbage collection of unused blocks).

* **Loading and Linking (`sys/load/`):** Handles loading of the boot image and
dynamically loading/linking compiled function modules (`.vp` files translated
to native or VP64 code). It resolves inter-function references. `load-path`
provides the path to object files for the current CPU/ABI.

* **Link Drivers (`sys/link/`):** Facilitate inter-node communication over
shared memory (`lk_shmem` structure). Each link has an `in` and `out` task to
manage message transfer.

* **Platform Interface Invocation (PII) (`sys/pii/`):** An abstraction layer
for host operating system functionalities. This includes file I/O, time, memory
mapping, random number generation, and process control. Platform-specific
implementations exist for Linux, Windows, and Darwin.

* **Statics (`sys/statics/class.inc`):** A global structure
(`@sys/statics/statics`) holding pointers to system-wide resources like the
kernel TCB, task lists, mail system structures, memory heaps, GUI screen, and
the Lisp root environment.

## 2. ChrysaLisp Language and Runtime (`class/` directory)

* **Object System:** ChrysaLisp features a class-based object system. Classes
are defined with `def-class`, methods with `def-method`. It supports
inheritance (e.g., `View` is a base for many GUI widgets), virtual methods, and
a vtable mechanism. Reference counting (`obj_count`) is used for object
lifetime management.

* **Core Data Types:**

    * **Numbers:** `Num` (integers), `Fixed` (fixed-point, `+fp_shift` for
    precision), `Real` (higher-precision numbers).

    * **Sequences:** `Seq` (base class), `Str` (strings), `List` (dynamic
    lists/arrays of objects), `Array` (base for numeric arrays), `Nums` (arrays
    of numbers), `Fixeds`, `Reals`, `Path` (for 2D graphics).

    * **Symbols:** `Sym` (interned symbols).

    * **Functions & Macros:** `Func` (can be raw-arg or eval-arg), `(lambda
    ...)`, `(macro ...)`.

    * **Collections:** `Hmap` (hash map, used for environments), `Hset` (hash
    set). `Lmap`, `Fmap`, `Emap`, `Xmap`, `Xset`, `Fset` provide various
    map/set implementations.

    * **Streams:** `Stream` (base), `Fstream` (file), `Sstream` (string),
    `In`/`Out` (for IPC).

    * **Other:** `Error`, `Dim` (multi-dimensional arrays).

* **FFI (Foreign Function Interface):** `(ffi name "path" flags)` allows
binding native C/C++ functions compiled into the system.

* **Environments and Scoping:** Environments are `Hmap` instances. `(env)`,
`(penv)`, `(env-push)`, `(env-pop)` manage lexical scopes.

* **REPL:** The `(repl stream name)` function drives the read-eval-print loop.

* **VP (Virtual Processor):** ChrysaLisp has its own assembly-like language
(`.vp` files) and associated tools in `lib/asm/` for compiling it. The
`vp64.cpp` file suggests a VP64 interpreter or target.

## 3. GUI System (`gui/` and `service/gui/`)

* **GUI Service (`service/gui/app_impl.lisp`):** Manages the overall graphical
user interface, event dispatching, and windowing. It initializes the host GUI
(e.g., SDL or framebuffer via PII), handles mouse and keyboard input, and
updates the screen.

* **View Hierarchy:** GUI elements inherit from a base `View` class. `View` objects form a tree structure.

    * **Window (`gui/window/lisp.inc`):** Top-level containers for
    applications.

    * **Layouts:** `Flow` (`gui/flow/lisp.inc`) and `Grid`
    (`gui/grid/lisp.inc`) are used for arranging child views. `Stack`
    (`gui/stack/lisp.inc`) provides tabbed views.

    * **Widgets:** A rich set of widgets including `Label`, `Text`, `Button`,
    `Textfield`, `Slider`, `Scroll`, `Progress`, `Vdu` (virtual display unit),
    `Canvas`, `Tree`, `Spinner`, `Hchart`, `Stroke` (for drawing input),
    `Radiobar`, `Backdrop`, `Title`.

* **UI Definition:** `ui-*` macros (e.g., `ui-window`, `ui-button`) provide a
declarative way to build UI trees.

* **Event System:** Events (mouse, key, action, etc.) are represented by
structures (`+ev_msg_*`). The GUI service dispatches these events to the
appropriate views or their owner tasks. `+ev_type_*` defines event types.

* **Drawing:** Views have `:draw` methods. The `ctx` (context) API provides
drawing primitives (`:ctx_set_color`, `:ctx_box`, `:ctx_filled_box`,
`:ctx_blit`). Canvases support path-based drawing with antialiasing.

* **Graphics Primitives:** `Path` objects define 2D shapes. Functions exist for
generating arcs, cubics, quadratics, and transforming paths. `Pixmap` and
`Texture` classes handle image data and rendering.

* **Keyboard Mapping:** `*normal_map*` and `*shift_map*` (from
`lib/keys/*.inc`) translate scancodes to character codes based on the
`*env_keyboard_map*` setting.

## 4. Standard Libraries (`lib/`)

* **Collections (`lib/collections/`):** Provides various map and set
implementations (`Fmap`, `Lmap`, `Emap`, `Fset`, `Xset`, etc.) and tree
serialization/deserialization (`tree-load`, `tree-save`).

* **Text Processing (`lib/text/`):** Includes `Buffer` for text manipulation,
`Syntax` for syntax highlighting, `Dictionary` for spell-checking/completions,
and `Search` classes (`Substr`, `Regexp`) for pattern matching. `charclass.inc`
defines character classes.

* **File Utilities (`lib/files/`):** Functions for listing files and
directories (`files-all`, `files-dirs`), managing dependencies
(`files-depends`), and URL/path manipulation (`url-ext`).

* **Task Utilities (`lib/task/`):**

    * `Pipe`: For creating and managing command pipelines.

    * `Farm`, `Local`, `Global`: Classes for managing farms of worker tasks for
    parallel processing.

* **Debugging and Profiling (`lib/debug/`):**

    * `debug.inc`: A system for sending debug messages to the Debug
    application.

    * `frames.inc`: Provides stack frame information.

    * `profile.inc`: For profiling function execution times.

* **Options Parsing (`lib/options/options.inc`):** A utility for parsing
command-line arguments for applications.

* **Date and Time (`lib/date/`):** Functions for date and time calculations,
timezone handling (using location-based timezones from `timezones.inc`).

* **Math (`lib/math/`):** Vector math (`vector.inc`), matrix math
(`matrix.inc`), 3D mesh (`mesh.inc`, `surface.inc`), and scene graph
(`scene.inc`) libraries.

* **XML/SVG Parsing (`lib/xml/`):** Basic XML/SVG parsing capabilities
(`XML-parse`, `SVG-Canvas`, `SVG-info`).

* **Streams (`lib/streams/diff.inc`):** Text stream diffing and patching
utilities.

* **Constants (`lib/consts/`):** Defines constants for colors, characters, and
keyboard scancodes.

* **Anaphoric Macros (`lib/anaphoric/anaphoric.inc`):** Provides anaphoric
macros like `aif`, `awhen`.

## 5. Applications (`apps/`)

ChrysaLisp comes with a variety of applications, demonstrating its
capabilities:

* **Core Desktop/Session:**

    * `login`: Manages user login and session startup.

    * `wallpaper`: Sets the desktop wallpaper and launches auto-start
    applications.

    * `launcher`: Provides a graphical application launcher.

    * `logout`: Allows users to log out or quit the GUI session.

* **Development & System Tools:**

    * `debug`: A graphical debugger that receives and displays messages from
    instrumented code.

    * `profile`: A profiler application that displays reports from
    `profile-report`.

    * `terminal`: A command-line terminal application.

    * `edit`: A text editor with syntax highlighting, find/replace, macros,
    etc.

    * `viewer`: A file viewer, sharing some code with the editor but focused on
    read-only operations.

    * `files`: A file picker dialog.

    * `services`: Lists currently running system services.

    * `netmon`: Network monitor displaying task counts and memory usage per
    node.

    * `netspeed`: Network speed tester, measures VOPs (Vector Operations Per
    Second) for registers, memory, and reals across nodes.

    * `fonts`: Displays available fonts and their glyphs.

    * `docs`: A documentation viewer for Markdown-like files with custom
    content handlers.

* **Graphics & Multimedia:**

    * `images`: An image viewer supporting CPM, TGA, and SVG formats.

    * `films`: A viewer for `.flm` (film/animation) files.

    * `canvas`: A demo application showcasing 2D path drawing and
    transformations.

    * `raymarch`: A 3D raymarching renderer using a task farm.

    * `mandelbrot`: A Mandelbrot set renderer, also using a task farm.

    * `molecule`: A 3D molecule viewer for SDF files.

    * `pcb`: A PCB (Printed Circuit Board) viewer and autorouter.

* **Games & Demos:**

    * `boing`: The classic Amiga Boing Ball demo.

    * `freeball`: A bouncing ball demo.

    * `bubbles`: A 3D bubbles screensaver-like application.

    * `chess`: A chess game with an AI.

    * `minefield`: A Minesweeper-like game.

* **Utilities:**

    * `calculator`: A simple graphical calculator.

    * `chat`: A basic network chat application.

    * `whiteboard`: A collaborative drawing application.

* **TUI (Text User Interface):**

    * `apps/tui/tui.lisp`: The main TUI application, providing a command-line
    interface that interacts with `pii-read-char` and `pii-write-char`.

    * `apps/tui/install.lisp`: An installer script that can build a boot image
    for the system.

* **Template (`apps/template/`):** A skeleton application for developers.

## 6. Build System and Compilation (`cmd/make.lisp`, `lib/asm/`)

* ChrysaLisp includes a `make` command (`cmd/make.lisp`) written in ChrysaLisp
itself.

* This build system compiles `.vp` (Virtual Processor assembly) files.

* It uses translation modules in `lib/trans/` (e.g., `vp64.inc`, `x86_64.inc`,
`arm64.inc`, `riscv64.inc`) to convert VP assembly into native machine code for
different targets or into VP64 bytecode.

* The build system handles dependencies between files and can create boot
images (`lib/boot/image.inc`).

* Optimizers for VP assembly (`vpopt.inc`) and C-Script (`csopt.inc`) are
present.

* `lib/asm/scopes.inc` helps manage scopes during the compilation of `.vp`
files.

## 7. Environment Configuration (`apps/login/`)

* User-specific settings are managed through `env.inc` files located in user
directories (e.g., `apps/login/Guest/env.inc`).

* The main `apps/login/env.inc` loads the environment for the `*env_user*`
(defaulting to "Guest" if `apps/login/current` doesn't specify another user).

* Environment variables (`*env_*`) control aspects like:

    * Launcher applications (`*env_launcher_apps*`, `*env_launcher_auto_apps*`)

    * Wallpaper images (`*env_wallpaper_images*`)

    * Terminal prompt and line count (`*env_terminal_prompt*`,
    `*env_terminal_lines*`)

    * Clock settings (`*env_clock_*`)

    * Default UI properties (colors, fonts, borders - `*env_window_col*`,
    `*env_title_font*`, etc.)

    * Keyboard map (`*env_keyboard_map*`)

In summary, ChrysaLisp is a comprehensive Lisp-based system that provides a
graphical environment, a set of applications, and tools for its own development
and compilation. It emphasizes a service-oriented architecture with inter-task
communication and offers a unique approach to system design and implementation.
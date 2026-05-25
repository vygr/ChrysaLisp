# Editor

The `Editor` application is a multi buffer programers editor tailored for the
ChrysaLisp environment, language and file types present within the ChrysaLisp
file tree.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

The editor can open multiple files at a time, but you can only have a single
instance of the editor open at once, as there is user persistent state stored.
This state is saved and loaded to maintain a consistent project work
environment between sessions in your user folder under the name
`editor.tre`.

## UI

```widget
apps/tools/edit/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Editor (found in `apps/tools/edit/`) is a highly integrated,
multi-buffer programmer's text editor designed specifically for the ChrysaLisp
parallel processing operating system. It serves as a prime demonstration of
ChrysaLisp's graphical user interface, declarative widget framework, native
Virtual Processor (VP) assembly speed, and distributed parallel capabilities.

### 1. Core Editor Characteristics

The ChrysaLisp Editor utilizes several key architectural elements of the OS:

* **Multi-Cursor / Multi-Selection Engine:** Built directly into the core
  `Buffer` and `Document` classes, the editor supports editing at arbitrary
  multiple locations simultaneously. Cursors are tracked as tuples of
  coordinates: `(cx cy ax ay sx)` where `cx/cy` represent the cursor position,
  `ax/ay` represent the selection anchor, and `sx` is a "sticky X" column index
  used to preserve vertical alignment during vertical movements.

* **Distributed Parallel Grep:** Global search (`action-find-global` in
  `search.inc`) maps file paths across active network nodes, executing parallel
  search tasks via a `pipe-farm` command pipeline. It filters out common
  dictionaries to optimize performance across the cluster.

* **Intelligent Word Autocompletion:** Integrates a global `Dictionary` object
  that indexes words from loaded files and standard text databases. The
  `show-matches` system captures context words in a 20-line radius around the
  cursor, sorting auto-completion matches by local frequency and alphabetical
  order inside a floating overlay window (`match_window`).

* **Multi-File Unified Undo/Redo:** Rather than restricting undo actions to the
  current buffer, the editor maintains `*global_undo_stack*` and
  `*global_redo_stack*` transaction groups. This tracks historical states across
  all active buffers, allowing a single grouped edit (like a global macro
  playback or replace-all) to be reverted cleanly across multiple files.

* **RPC-Driven Inter-Process Communication:** Registers an "Edit" service using
  `mail-declare` to handle remote requests like `+edit_rpc_type_jump`, enabling
  external debugging tools (such as the ChrysaLisp Debugger) to trigger file
  opening and cursor jump-to-line operations.

### 2. Application Architecture

The editor uses a Model-View-Controller (MVC) design pattern that is tightly
coupled with ChrysaLisp's cooperative internal scheduling.

```code
          +------------------+     (RPC/Service)
          |   Edit Service   | <-----------------+
          +------------------+                   |
                   |                             |
                   v                             |
          +------------------+                   |
          |  app_impl.lisp   | <-------+         |
          |   (Controller)   |         |         |
          +------------------+         |         |
             |            |            |         |
             v            v            |         |
      +------------+  +------------+   |         |
      | widgets.inc|  |   ui.inc   |   |         |
      |   (View)   |  | (Editor-   |   |         |
      +------------+  |   edit)    |   |         |
                      +------------+   |         |
                            |          |         |
                            v          |         |
                      +------------+   |         |
                      |  Edit/VDU  |   |         |
                      |   Layers   |   |         |
                      +------------+   |         |
                            |          |         |
                            v          |         |
                      +------------+   |         |
                      |  Document  | --+ (Undo)  |
                      |   Model    | ------------+
                      +------------+
```

#### 2.1 Entry Point and Event Loop (`app_impl.lisp`)

The `main` function initializes the application, starting up the UI window,
loading saved state, and listening on a select list of mailboxes created via
`(task-mboxes +select_size)`. The select index mappings are managed via the
`+select` enum:

* `+select_main`: Handles GUI window events, routing keyboard and mouse inputs.

* `+select_tip`: Manages tooltips.

* `+select_remote`: Manages incoming RPC requests (e.g., jump-to-line from the
  debugger).

#### 2.2 Dual-Tree Workspace Layout

The UI layout (`widgets.inc`) features a sidebar on the left containing two
hierarchical trees: `*open_files_selector*` (listing currently active buffers)
and `*file_selector*` (for browsing the project directory). The right side
places line numbers (`*vdu_lines*`) next to the main `*edit*` area with its
scroll sliders (`*xslider*`, `*yslider*`).

### 3. Core Editing Engine

#### 3.1 The Controller-View Coordinate (`Edit` in `gui/edit/lisp.inc`)

Inheriting from `View`, the `Edit` class manages the low-level `Vdu` (Video
Display Unit) widget, tracks scrolling offsets (`scroll_x`, `scroll_y`), and
maps physical coordinates to character cells. It acts as a proxy, delegating
editing methods (such as `:insert`, `:delete`, `:backspace`, `:undo`, `:redo`)
to the underlying `Buffer` model.

#### 3.2 Selection Mask Overlays (`underlay` in `app_impl.lisp`)

To render selections, cursor carets, and matching bracket highlights, the editor
does not redraw the text. Instead, a custom `underlay` function calculates
sub-regions and configures `Mask` overlay widgets (`mask_ink`, `mask_selected`,
`mask_found`, `mask_region`) placed behind the translucent text layers. This
allows the VDU to draw text as purely static layers while the GPU handles
selection compositing.

#### 3.3 The Data Model (`Buffer` in `lib/text/buffer.inc`)

`Buffer` represents the data model. It manages raw text lines (`buffer_lines`)
and provides low-level, coordinate-safe mutation methods like `:iinsert`
(internal insert), `:idelete` (internal delete), and `:icopy` (internal copy).

* **Smart Cursor Mapping:** When text is modified, other cursors on the same or
  subsequent lines must be shifted to remain accurate. This is handled by
  FFI-bound native functions `csr_map_insert` and `csr_map_delete` (compiled
  from `lisp.vp`), preventing cursors from becoming misaligned during edits.

* **The Document Subclass (`Document` in `lib/text/document.inc`):** Inheriting
  from `Buffer`, this subclass enriches the raw text model with high-level
  block, word, paragraph, and form-level selection actions, alongside code
  formatting tools such as `:reflow`, `:split`, `:sort`, `:unique`, `:comment`,
  and `:trim`.

#### 3.4 Specialized Subclassing (`Editor-edit` in `ui.inc`)

The Editor app inherits from the generic `Edit` class to define `Editor-edit`.
It overrides mouse and scroll wheel interactions, updates scroll sliders, and
processes multi-clicks:

* **Double-click:** Triggers `action-select-word`.

* **Triple-click:** Triggers `action-select-line`.

* **Quadruple-click:** Triggers `action-select-paragraph`.

### 4. Key Subsystems

#### 4.1 Search and Replace (`search.inc`)

Local search compiles the search query (`query` in `searching.inc`) and utilizes
the `:find` method on the `Buffer`. Replacement uses `replace-compile` to build
an optimized replacement template, patching capture group indices during edits
via `:iinsert` and `:idelete`. Global searches are delegated to the distributed
network via `pipe-farm`.

#### 4.2 The Macro Subsystem (`macros.inc`)

When `*macro_record*` is active, user interactions matching `*recorded_actions*`
are logged to a record buffer in `+macro_map`.

* **Playback Slots:** Supports saving macros to 10 independent slots (`0` to
  `9`).

* **Execution Modes:** Playback can run once, to the end of the file
  (`action-macro-to-eof`), or globally across all open buffers
  (`action-macro-global`).

* **Recursive Safety:** Playback is protected against infinite recursion at
  compile time by analyzing slot calls.

#### 4.3 Undo/Redo Subsystem (`undo.inc` / `buffer.inc`)

When editing, the `undoable` macro groups atomic edits inside a transaction
boundaries by pushing a `:mark` and snapshotting active cursors.

* **Global Undo Coordination:** The global stack tracks buffer keys (the file
  path string or `:nil` for the scratchpad) along with transaction marks.

* **Group Reversals:** Undoing or redoing pops the global transaction, switching
  active buffers dynamically and calling `:rewind` or `:forwardwind` on each
  affected buffer to sync their historical state.

#### 4.4 File Buffers and State Persistence (`state.inc`)

Session state is saved to `editor.tre` in the user's home directory. It
preserves:

* Window geometry (`x`, `y`, `width`, `height`).

* Search strings, regex, and whole-word flags.

* The open file list, including each buffer's cursors, selections, and scroll
  positions.

* Serialized/encoded macros across all 10 slots.

### 5. User Interface and Event Dispatching

#### 5.1 Declarative UI Construction

The UI is built using declarative macros (e.g., `ui-window`, `ui-flow`,
`ui-scroll`, `ui-vdu`), enabling readable, non-backtracking layout
configurations. The layout recalculates its metrics on window resize through a
two-pass constraint and layout cycle.

#### 5.2 Central Action Dispatcher (`app_impl.lisp` / `actions.inc`)

Events are routed through the `dispatch-action` function, which maps key inputs
and UI events to their corresponding routines.

```code
              +--------------------+
              |  Keyboard / Mouse  |
              +--------------------+
                        |
                        v
              +--------------------+
              |    app_impl.lisp   |
              |  (dispatch-action) |
              +--------------------+
                        |
                  +----+----+
                  |         |
                  v         v
              +----------+ +-----------+
              |  Macro   | | Undo/Redo |
              | Recorder | |  Tracker  |
              +----------+ +-----------+
                  |         |
                  +----+----+
                        |
                        v
              +--------------------+
              |  Action Execution  | (e.g., action-insert)
              +--------------------+
```

Keyboard events are categorized into three distinct maps to handle modifiers
cleanly:

* `*key_map*`: Maps raw scancodes (e.g., arrows, backspace, enter, tab).

* `*key_map_shift*`: Handles shift modifiers (e.g., selecting text, outdenting).

* `*key_map_control*`: Processes control commands (e.g., copy, paste, find,
  macros, font scaling).

### 6. Remote Procedure Calls (RPC)

By registering with the local name server as the `"Edit"` service, the editor
can receive cross-task messages. The `edit-jump-rpc` mechanism in `rpc.inc`
allows external processes (such as compiling or debugging tasks) to transmit
jump requests to the editor. The editor parses the `+edit_rpc_jump` message
payload, opens the target file, and positions the primary cursor on the
designated line.

### Conclusion

The ChrysaLisp Editor is a robust implementation of the operating system's
architectural principles. By combining Model-View-Controller separation with
low-overhead message-passing primitives, it achieves high performance within a
small footprint. The native, FFI-bound cursor mapping and distributed parallel
search engine demonstrate the speed and scalability of ChrysaLisp's Virtual
Processor architecture.
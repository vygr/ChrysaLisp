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

The ChrysaLisp Editor, found in `apps/tools/edit/`, is a sophisticated,
multi-buffer programmer's text editor tailored for the ChrysaLisp environment.
It showcases many of the GUI system's strengths, including its event handling,
widget composition, and the Lisp-centric property system. This document delves
into its architecture, key features, and implementation details based on the
provided source code.

Its defining characteristics include:

* **Multi-Cursor/Multi-Selection Editing:** Native support for simultaneous
  editing at multiple locations.

* **Distributed Architecture:** Capable of offloading search operations (`grep`)
  to other nodes in the network via `pipe-farm`.

* **Persistent State:** Automatically saves session state (open files, cursor
  positions, macros) to disk.

* **RPC Interface:** Acts as a service, allowing debuggers and external tools to
  trigger file opens and line jumps.

## 2. Application Architecture

The application follows a Model-View-Controller (MVC) pattern, distributed
across several files and classes.

### 2.1 Entry Point and Event Loop (`app_impl.lisp`)

The `main` function initializes the application. It sets up the UI window, loads
the previous session state, and enters the message handling loop.

The event loop monitors a `select` list containing:

1. **Main Mailbox:** Receives UI events (clicks, key presses) and Window Manager
   messages.

2. **Tip Mailbox:** Handles tooltips.

3. **Remote Mailbox:** Listens for RPC calls (e.g., from the Debugger app) to
   jump to specific code locations.

### 2.2 Component Hierarchy

1. **The Window (`ui-window` in `widgets.inc`):** The top-level container.

2. **The Edit Widget (`Editor-edit` in `ui.inc`):** A custom subclass of the
   core `Edit` class. It handles input routing and display logic.

3. **The Buffer (`Buffer` in `lib/text/buffer.inc`):** The data model
   representing the text, cursors, and undo history.

4. **The VDU (`ui-vdu`):** The low-level rendering component used by the Edit
   widget to draw text grids.

## 3. The Core Editing Engine

The text editing capabilities are split between the visual management (`Edit`
class) and the logical manipulation (`Buffer` class).

### 3.1 The `Edit` Class (`gui/edit/lisp.inc`)

This class inherits from `View`. It acts as the **Controller** and **View**
coordinator.

* **Responsibility:** It manages the `Vdu` (Video Display Unit) widget, handles
  scrolling offsets (`scroll_x`, `scroll_y`), and translates raw mouse
  coordinates into text indices.

* **Delegation:** It defines proxy methods (e.g., `defproxymethod :insert`) that
  forward high-level actions directly to the `Buffer`.

* **Rendering:** It implements `:draw` to render selection masks (colorizing
  selected text regions) and overlays before asking the VDU to render the actual
  characters.

### 3.2 The `Buffer` Class (`lib/text/buffer.inc`)

This is the **Model**. It contains the actual string data (`buffer_lines`) and
the logic for manipulating it.

* **Multi-Cursor Engine:** The buffer does not track a single cursor. It tracks
  a list of cursors, where each cursor is a tuple: `(current_index anchor_index
  sticky_x)`.

* **State Management:** It manages the `undo_stack` and `redo_stack`.

* **Syntax Highlighting:** It holds a reference to a `Syntax` engine instance,
  which parses lines on demand to generate color maps for rendering.

### 3.3 Subclassing for Specialization (`ui.inc`)

The Editor app defines a specific subclass `Editor-edit` that inherits from
`Edit`. This allows the application to override input behaviors, specifically
mouse interaction, to integrate with the application-specific `refresh` logic
and status bar updates.

```vdu
(defclass Editor-edit () (Edit)
    ...
    (defmethod :mouse_wheel (event)
        (.super this :mouse_wheel event)
        ; App-specific logic to update sliders and refresh display
        (refresh-sliders) (refresh-display) this)
)
```

## 4. Key Logic Subsystems

### 4.1 Search and Replace (`search.inc`)

The editor implements a robust search system supporting:

* **Local Search:** Uses the `Buffer` methods `:find` to locate text within the
  open file.

* **Global Search:** Uses the `grep` command via `pipe-farm`. This spawns search
  tasks across the distributed network (available CPU nodes) to scan files in
  parallel.

* **Matches UI:** The application can spawn a floating window (`match_window`)
  listing search results (like auto-complete suggestions or dictionary lookups).

### 4.2 Undo/Redo System (`utils.inc`, `undo.inc`)

The editor utilizes an `undoable` macro that wraps editing operations. This
macro performs a "snapshot" logic:

1. Push a `:mark` to the undo stack.

2. Save the current cursor positions.

3. Execute the editing code (which pushes specific line changes to the undo
   stack).

4. Push a closing `:mark`.

This groups atomic actions (like a global replace) into a single undoable block.

### 4.3 Macros (`macros.inc`)

The editor features a recording system.

1. **Recording:** When enabled (`*macro_record*`), specific actions defined in
   `*recorded_actions*` are intercepted in `dispatch-action`.

2. **Storage:** Actions are stored in `+macro_map`.

3. **Playback:** The `macro-playback` function iterates through the stored list,
   executing the functions.

4. **Persistence:** Macros are serialized into the `editor.tre` state file,
   preserving them between reboots.

### 4.4 File I/O (`file.inc`)

* **Loading:** Uses `populate-buffer` to create a `Buffer` object for a file. It
  performs lazy loading where possible but ensures the `Syntax` engine scans
  lines for coloring.

* **Saving:** Checks the `:modified` flag on the buffer. It uses `file-stream`
  to write the `buffer_lines` list back to disk.

## 5. User Interface Implementation

### 5.1 Widget Hierarchy (`widgets.inc`)

The layout is defined using a declarative UI macro syntax.

* **Main Container:** A `ui-flow` that organizes toolbars and the main editing
  area.

* **Navigation:** A file tree (`ui-files`) is placed on the left or right
  (depending on configuration) to browse the project directory.

* **Status Bar:** Located at the bottom, updating dynamically based on cursor
  position (`*cx*`, `*cy*`) and selection status.

### 5.2 Event Dispatch (`actions.inc`)

The application uses a `scatter` map (hash map) to bind event IDs to functions.

* **`*event_map*`:** Maps UI button clicks (e.g., `+event_save`) to functions
  (`action-save`).

* **`*key_map*`:** Maps keyboard scancodes to movement or editing functions.

* **Dispatch Logic:** The `dispatch-action` function acts as the central router.
  It checks modifiers (Shift/Ctrl), handles macro recording if active, and then
  invokes the bound function via `eval`.

## 6. State Persistence (`state.inc`)

The editor maintains continuity by saving its state to `editor.tre` in the
user's home directory.

* **Data Saved:** Window dimensions, currently open files, cursor positions for
  every open file, find/replace history, and recorded macros.

* **Mechanism:** It uses `tree-save` to serialize the `*meta_map*` (an `Emap`
  containing configuration) to a structured text format. On startup,
  `state-load` restores this environment, reopening files and restoring cursors
  to their previous lines.

## 7. Remote Procedure Calls (RPC)

The editor registers itself as a service named "Edit".

* **Service Registration:** `(mail-declare (elem-get select +select_remote)
  "Edit" ...)`

* **Handling:** In the main loop, messages arriving at `+select_remote` are
  parsed.

* **Jump Command:** The `edit-jump-rpc` logic parses a payload string (e.g.,
  "id|filename|line") and calls `action-breakpoint` to open the file and move
  the cursor to the specific line, facilitating debugging integration.

## Conclusion

In conclusion, the ChrysaLisp Editor serves as a definitive showcase of the
operating system's architectural philosophy, combining a rigorous MVC separation
with powerful, intrinsic features. Its reliance on the Buffer class enables
complex multi-cursor editing natively, while its seamless integration with the
distributed network allows for parallelized global search operations across
available nodes. By incorporating self-persisting state management, a recordable
macro system, and a service-oriented RPC interface for inter-process
communication, the Editor demonstrates how high-level functionality can be built
efficiently upon ChrysaLisp's low-level message-passing primitives.

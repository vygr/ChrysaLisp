# Viewer

The `Viewer` application is a single buffer programers file viewer tailored for
the ChrysaLisp environment, language and file types present within the
ChrysaLisp file tree.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

The viewer can only open a single file at a time, and you can't change any file
contents. But you can copy content to the clipboard and search for things same
as in the full `Editor` application.

You can have multiple instances of the viewer open at the same time, as there
is no user persistent state stored.

## UI

```widget
apps/tools/viewer/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Viewer, located in `apps/tools/viewer/`, is a lightweight,
read-only document and code browser.

It is designed to complement the Editor, leveraging ChrysaLisp’s modular GUI
architecture to reuse model, view, and utility libraries while providing a
streamlined interface dedicated to navigation, searching, and file system
exploration.

### 1. Core Architecture and Components

The Viewer's architecture showcases the highly reusable, component-based design
of the ChrysaLisp GUI system.

It adapts the Model and View layers used by the Editor, implementing a focused
Controller layer to enforce read-only execution while retaining rich navigation
features.

* **Model (Text Management - Reused from Editor):**

    * **`Buffer` Class (`lib/text/buffer.inc`):** The Viewer utilizes the exact
      same `Buffer` class for text management. Each opened file is loaded into a
      `Buffer` instance. The Viewer accesses only the non-destructive, read-only
      methods of the buffer (such as `:get_text_line`, `:get_size`, and
      `:find`), preventing text modification.

    * **`Document` Class (`lib/text/document.inc`):** Files are loaded as
      `Document` objects (which inherit from `Buffer`) inside `populate-buffer`.
      This provides high-level text selection features (word, line, form, and
      paragraph) without editing capabilities.

    * **`Syntax` Class (`lib/text/syntax.inc`):** Code highlighting is fully
      integrated. A global instance of the `Syntax` class (`*syntax*`) is passed
      to each `Document` buffer during initialization, rendering the same color
      schemes as those in the Editor.

* **View (User Interface):**

    * **`*window*` (`apps/tools/viewer/widgets.inc`):** The top-level `Window`
      container that serves as the root of the UI tree.

    * **`Viewer-edit` Class (`apps/tools/viewer/ui.inc`):** A custom class that
      inherits from `Edit` (defined in `gui/edit/lisp.inc`). The Viewer reuses
      the `Edit` widget but overrides its mouse event handlers to implement
      read-only selection and copy actions, bypassing text modifications. The
      primary editor controller instance is named `*edit*`.

    * **`Vdu` Widget (`gui/vdu/lisp.inc`):** Low-level rendering is handled by
      `Vdu` instances, drawing text characters, selections, and search
      highlights onto the screen on top of the underlying mask elements.

    * **Workspace Navigation (`*file_selector*`):** A hierarchical directory
      tree (`ui-files` widget) is pinned to the left to browse and load files
      within the workspace.

* **Controller (Application Logic and Event Handling):**

    * **`apps/tools/viewer/app.lisp`:** The main application entry point. It
      manages the `main` event loop and coordinates buffer state transitions.

    * **`apps/tools/viewer/actions.inc`:** Defines the event map (`*event_map*`)
      and key maps (`*key_map*`, `*key_map_shift*`, `*key_map_control*`) that
      bind keyboard shortcuts and GUI button events to navigation and viewing
      routines, omitting all mutation functions.

    * **`dispatch-action`:** Evaluates actions retrieved from the event maps,
      managing search states and view updates during user interactions.

### 2. UI Structure (`apps/tools/viewer/widgets.inc`)

The Viewer's UI is optimized for read-only navigation, file browsing, and search
feedback.

* **Main Workspace Layout:** A `ui-flow` with a `:flow_right_fill` flag splits
  the main area into two vertical panes: the file browser (`*file_selector*`) on
  the left and the viewport area on the right.

* **Toolbars:**

    * `*main_toolbar*`: Provides essential viewing actions like "Copy", "Select
      Paragraph", "Select Form", and form boundary markers.

    * `*find_toolbar*`: Hosts find and select-region controls, including "Select
      Region", "Whole Words", "Regexp", and directional searches.

* **Content Viewport:**

    * `*scale_flow*`: Wraps the coordinate view. It places line numbers
      (`*vdu_lines*`) next to the main `*edit*` viewing widget.

    * `*xslider*` and `*yslider*`: Sliders for scrolling the viewport
      horizontally and vertically.

* **Status Panel:** A `ui-flow` at the bottom displaying the cursor position
  (`*cx*`, `*cy*`), selection bounds (`*sw*`, `*sh*`), and the total number of
  found matches (`*fc*`) matching the active search query.

### 3. Event Handling and Dispatch

The event loop in `apps/tools/viewer/app.lisp` monitors a select list of
mailboxes created via `(task-mboxes +select_size)`, where `+select_size` is 2:

* `+select_main`: Handles GUI window events, routing keyboard and mouse inputs.

* `+select_tip`: Manages tooltips.

Because the Viewer is a read-only browser, it does not require a remote RPC
mailbox to receive compiler or debugger jumps.

The event-routing logic matches the Editor's implementation, directing events to
viewing-specific routines:

* **Navigation:** Keyboard events (arrows, home, end, and page bounds) map to
  selection-aware movement actions (e.g. `action-left`, `action-right`).

* **Search:** Links the find-text input and arrow buttons to directional matches
  (`action-find-down`, `action-find-up`).

* **Selections:** Leverages block, form, and paragraph-level selection logic.

* **Clipboard:** Binds to `action-copy` (via `service/clipboard/app.inc`). All
  text mutation actions (such as cut, paste, delete, and backspace) are
  excluded.

### 4. File Management and State

The Viewer manages document buffers dynamically through the global `*meta_map*`
in a simpler manner than the Editor.

* **`*meta_map*`:** An `Emap` that stores active metadata for opened files.

* **Buffer Lifecycle:** The Viewer does not maintain an explicit, open tab-bar
  list of files. Instead, it relies on the `*file_selector*` sidebar. A file's
  buffer is lazily loaded into `*meta_map*` the first time it is selected.

* **`*current_file*`:** Tracks the path of the currently active file.

* **`populate-vdu` (`app.lisp`):** Orchestrates buffer switching. It retrieves
  the file's `Buffer` from `*meta_map*`, binds it to the `*edit*` widget,
  restores the last saved cursor, selection, and scroll positions, and triggers
  a full screen `(refresh)`.

### 5. Specialized Read-Only View: The `Viewer-edit` Class

The Viewer enforces its read-only nature not by using a distinct rendering
widget, but by subclassing the generic `Edit` view and overriding its
interaction handlers inside `apps/tools/viewer/ui.inc`.

* **`Viewer-edit` (`ui.inc`):** Inherits from `Edit`.

* **Overridden Mouse Handlers:**

    * `(:mouse_down event)`: Overrides the cursor placement behavior. It sets
      the cursor and anchor to the clicked coordinates, clearing any previous
      selections without preparing for text input.

    * `(:mouse_move event)`: If the mouse is dragged, it updates the selection
      coordinates to highlight the text.

    * `(:mouse_up event)`: Handles multi-click selection events (double-click to
      select a word, triple-click for a line, etc.).

    * `(:mouse_wheel event)`: Adjusts scrolling offsets, stores coordinates to
      metadata, and updates the scrollbar positions.

* This demonstrates a powerful object-oriented pattern: inheriting a complex
  rendering component (`Edit`) and overriding only its interactive handlers to
  enforce read-only constraints while preserving all selection-compositing
  masks.

### 6. Deep Utility Reuse

A major design strength of the Viewer is its direct reuse of the Editor's core
utility modules.

Rather than duplicating code, `apps/tools/viewer/actions.inc` imports the
following modules directly:

* `apps/tools/edit/utils.inc`: For coordinate sorting and selection-aware
  viewport tracking.

* `apps/tools/edit/cursor.inc`: For cursor stack push/pop history, allowing
  users to jump to previous positions via `*cursor_stack*`.

* `apps/tools/edit/select.inc`: For structural selections.

* `apps/tools/edit/clipboard.inc`: For copying text.

* `apps/tools/edit/search.inc`: For local buffer searches and parallelized,
  cluster-wide global searches (`action-find-global`) using `pipe-farm`.

This level of reuse ensures that enhancements made to the core search and
selection utilities automatically benefit both the Editor and the Viewer.

## Conclusion

The ChrysaLisp Viewer stands as a prime example of software reuse and
component-based design. By leveraging the same `Buffer` model, `Document`
structures, and `Edit` rendering logic as the Editor, it avoids duplicating
complex text representation, syntax highlighting, and selection rendering code.

It accomplishes its specialized read-only purpose simply by providing an
intuitive split-pane layout and a lightweight `Viewer-edit` subclass that
overrides interactive inputs.

Its direct integration with the Editor's searching utilities enables powerful
search capabilities, combining local buffer indexing with cluster-wide parallel
searches to deliver a fast, responsive browsing experience.
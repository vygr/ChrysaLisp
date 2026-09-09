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
architecture to directly reuse model, view, and utility libraries while providing a
streamlined, non-destructive interface dedicated to fast navigation, searching, and
file system exploration.

### 1. Core Architecture and Components

The Viewer's architecture showcases the highly reusable, component-based design
of the ChrysaLisp GUI system. It directly imports the Model, View, and utility
layers from `apps/tools/edit/`, implementing a focused Controller layer to enforce
read-only execution while retaining rich navigation and multi-cursor inspection
features.

*	**Model (Text Management - Reused from Editor):**

	*	**`Buffer` Class (`lib/text/buffer.inc`):** The Viewer utilizes the exact
		same `Buffer` class for text management. The Viewer accesses only the
		non-destructive, read-only methods of the buffer (such as `:get_text_line`,
		`:get_size`, and `:find`), preventing text modification.

	*	**`Document` Class (`lib/text/document.inc`):** Files are loaded as
		`Document` objects inside `populate-buffer`. Crucially, `Document` instances
		in Viewer are created without the `+buffer_flag_undo` flag, eliminating undo
		stack allocation and keeping memory usage minimal.

	*	**`Syntax` Class (`lib/text/syntax.inc`):** Code highlighting is fully
		integrated. A global instance of the `Syntax` class (`*syntax*`) is passed
		to each `Document` buffer during initialization (for non-prose file types),
		rendering identical color schemes to those in the Editor.

*	**View (User Interface):**

	*	**`*window*` (`apps/tools/viewer/widgets.inc`):** The top-level `Window`
		container that serves as the root of the UI tree. The title dynamically
		updates to show the active file path: `Viewer -> path/to/file`.

	*	**`Viewer-edit` Class (`apps/tools/viewer/ui.inc`):** A custom class that
		inherits from `Edit` (defined in `gui/edit/lisp.inc`). The Viewer reuses
		the `Edit` widget but overrides its mouse event handlers to implement
		read-only selection and copy actions, bypassing text modifications. The
		primary viewer controller instance is named `*edit*`.

	*	**`Vdu` Widget (`gui/vdu/lisp.inc`):** Low-level rendering is handled by
		`Vdu` instances, drawing text characters, selections, and search
		highlights onto the screen on top of the underlying mask elements
		(`mask_ink`, `mask_selected`, `mask_found`, `mask_region`).

	*	**Workspace Navigation (`*file_selector*`):** A hierarchical directory
		tree (`ui-files` widget) pinned to the left pane to browse and load files
		within the workspace.

*	**Controller (Application Logic and Event Handling):**

	*	**`apps/tools/viewer/app.lisp`:** The main application entry point. It
		manages the `main` event loop and coordinates buffer state transitions.

	*	**`apps/tools/viewer/actions.inc`:** Defines the event map (`*event_map*`)
		and key maps (`*key_map*`, `*key_map_shift*`, `*key_map_control*`) that
		bind keyboard shortcuts and GUI button events to navigation and viewing
		routines, omitting all mutation functions.

	*	**`dispatch-action`:** Evaluates actions retrieved from the event maps,
		automatically passing search flags (`*whole_words*`, `*regexp*`, `*ignore_case*`,
		and query text) to search actions.

### 2. UI Structure (`apps/tools/viewer/widgets.inc`)

The Viewer's UI is optimized for read-only navigation, file browsing, and search
feedback.

*	**Main Workspace Layout:** A `ui-flow` with a `:flow_right_fill` flag splits
	the main area into two vertical panes: the project file browser
	(`*file_selector*`) on the left and the viewport area on the right.

*	**Toolbars:**

	*	`*main_toolbar*`: Provides essential viewing and copying actions: "Copy",
		"Select Paragraph", "Select Form", "Start Form", and "End Form".

	*	`*find_toolbar*`: Hosts search controls: "Select Region", "Whole Words",
		"Regexp", "Ignore Case", "Find Down", and "Find Up", paired with the
		`*find_text*` textfield.

*	**Content Viewport:**

	*	`*scale_flow*`: Wraps the coordinate view. It places line numbers
		(`*vdu_lines*`) next to the main `*edit*` viewing widget.

	*	`*xslider*` and `*yslider*`: Sliders for scrolling the viewport
		horizontally and vertically.

*	**Status Bar:** A `ui-flow` at the bottom displaying five real-time metrics:
	*	`cx`: Cursor column (1-indexed).
	*	`cy`: Cursor line (1-indexed).
	*	`sw`: Selection width in characters (`|cx - ax|`).
	*	`sh`: Selection height in lines (`|cy - ay|`).
	*	`fc`: Search match count (`find-count`) matching the active query in the buffer.

### 3. Event Handling and Dispatch

The event loop in `apps/tools/viewer/app.lisp` monitors a select list of
mailboxes created via `(task-mboxes +select_size)`, where `+select_size` is 2:

*	`+select_main`: Handles GUI window events, routing keyboard and mouse inputs.

*	`+select_tip`: Manages tooltips.

Because the Viewer is a standalone read-only browser, it does not require an RPC
mailbox for debugger jumps or state saving.

The event-routing logic directs inputs to viewing-specific routines:

*	**Navigation:** Keyboard events (arrows, home, end, top, bottom) map to
	selection-aware movement actions (e.g. `action-left`, `action-right`,
	`action-home`, `action-end`).

*	**Navigation History Stack (`*cursor_stack*`):** `action-push` (`Ctrl+D`) and
	`action-pop` (`Ctrl+Shift+D`) record and restore cursor positions, allowing
	seamless back-and-forth traversal across multiple files.

*	**Search:** Links the find-text input and buttons to directional matches
	(`action-find-down`, `action-find-up`). Search operations can be scoped to a
	selected vertical range using `action-region` (`+event_region`).

*	**Multi-Cursor Inspection:** Supports multi-cursor search inspection using
	`action-add-next` (`Ctrl+Shift+F`), `action-set-cursors` (`Ctrl+G`), and
	`action-add-cursors` (`Ctrl+Shift+G`).

*	**Structural Selections:** Supports form selection (`action-select-form`, `Ctrl+/`),
	bracket navigation (`action-left-bracket`, `action-right-bracket`, `Ctrl+[`, `Ctrl+]`,
	`Ctrl+9`, `Ctrl+0`), block selection (`action-select-block`, `Ctrl+B`), word selection
	(`action-select-word`, `Ctrl+W`), and paragraph selection (`action-select-paragraph`, `Ctrl+P`).

*	**Non-Destructive Copying:** Allows copying text selections (`action-copy`, `Ctrl+C`)
	as well as direct structural copies without altering the selection:
	`action-copy-form` (`Ctrl+?`), `action-copy-block` (`Ctrl+Shift+B`), `action-copy-word`
	(`Ctrl+Shift+W`), `action-copy-line` (`Ctrl+Shift+L`), and `action-copy-paragraph`
	(`Ctrl+Shift+P`). All mutation actions (cut, paste, delete, backspace) are excluded.

*	**Dynamic Font Scaling:** `action-scale-up` (`Ctrl+}`) and `action-scale-down`
	(`Ctrl+{`) dynamically rescale fonts and line numbers between 0.75x and 1.5x.

### 4. File Management and State

The Viewer manages document buffers dynamically through the global `*meta_map*`:

*	**`*meta_map*`:** An `Fmap` that stores active metadata for opened files, keyed
	by path string.

*	**Lazy Buffer Loading:** When a file is selected in `*file_selector*`,
	`populate-vdu` lazily creates and caches its `Document` buffer along with cursor,
	selection, and scroll coordinates in `*meta_map*`.

*	**`*current_file*`:** Tracks the path of the currently active file.

*	**Stateless Multi-Instance Operation:** Unlike the Editor, the Viewer does not
	persist state to an `editor.tre` file on disk, allowing multiple independent
	Viewer windows to run concurrently without lock conflicts.

### 5. Specialized Read-Only View: The `Viewer-edit` Class

The Viewer enforces its read-only nature by subclassing the generic `Edit` view
and overriding its interaction handlers inside `apps/tools/viewer/ui.inc`:

*	**`Viewer-edit` (`ui.inc`):** Inherits from `Edit`.

*	**Overridden Mouse Handlers:**

	*	`(:mouse_down event)`: Positions the cursor and anchor to clicked coordinates,
		clearing previous selections without entering text-entry state.

	*	`(:mouse_move event)`: Clears autocomplete matches and updates selection coordinates
		when dragging.

	*	`(:mouse_up event)`: Dispatches multi-click selection events: double-click for
		`action-select-word`, triple-click for `action-select-line`, and quadruple-click
		for `action-select-paragraph`.

	*	`(:mouse_wheel event)`: Adjusts vertical scroll offsets, stores coordinates to
		file metadata, and updates scrollbar positions.

### 6. Deep Utility Reuse from `apps/tools/edit/`

A major design strength of the Viewer is its direct reuse of the Editor's core
utility modules. Rather than maintaining duplicated viewing and navigation logic,
`apps/tools/viewer/actions.inc` directly imports from `apps/tools/edit/`:

*	`apps/tools/edit/utils.inc`: Coordinate sorting, cursor clipping, and selection-aware
	viewport metrics.

*	`apps/tools/edit/cursor.inc`: Cursor movement actions and the `*cursor_stack*`
	history mechanism.

*	`apps/tools/edit/block.inc`: Structural block, line, paragraph, and form selections.

*	`apps/tools/edit/select.inc`: Selection helper routines.

*	`apps/tools/edit/clipboard.inc`: System clipboard copy operations.

*	`apps/tools/edit/search.inc`: Query compilation, local buffer search, region
	scoping, and multi-cursor generation.

*	`apps/tools/edit/ui.inc`: Base UI actions, window resizing, and slider syncing.

## Conclusion

The ChrysaLisp Viewer stands as a prime example of software reuse and
component-based design. By directly leveraging the `Buffer` model, `Document`
structures, `Edit` rendering logic, and utility modules from the Editor, it avoids
duplicating complex text representation, syntax highlighting, and selection compositing.

It accomplishes its specialized read-only purpose simply by providing an
intuitive split-pane layout, lightweight `Document` buffers without undo overhead,
and a `Viewer-edit` subclass that restricts interactive inputs to navigation,
searching, and clipboard copying.

# Whiteboard

The `Whiteboard` application is a vector graphics editor.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/media/whiteboard/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Whiteboard, located in `apps/media/whiteboard/`, is a
vector-based drawing and object manipulation application. It serves as an
informative case study for developers transitioning from text-based utilities to
interactive graphical environments, illustrating how to handle multi-touch
inputs, manage complex transactional object states, perform spatial selection
queries, and serialize vector data to disk.

### 1. Core Architecture and Components

The Whiteboard application is structured around a Model-View-Controller design
tailored for ChrysaLisp's parallel, cooperative multitasking environment.

* **Model (The Drawing State):**

    The core data represents the state of the vector canvas. This is stored in
    `app.lisp` through several state variables:

    * **`*committed_groups*`**: A list of groups. Each group is structured as
      `(group_bbox strokes flags)`, where `strokes` contains individual paths
      with their respective color, polygon data, and local bounding box. It
      represents the permanent vector state of the drawing.

    * **`*staging_paths*`**: A list of temporary, in-progress paths currently
      being drawn or previewed on the canvas (e.g., pen lines, circle bounds,
      selection boxes).

    * **`*grabbed_groups*` and `*moving_groups*`**: Lists used to track and
      render groups currently selected and actively translated across the screen
      by the user.

    * **`*undo_stack*` and `*redo_stack*`**: Stacks storing snapshots of
      `*committed_groups*` to facilitate transactional state reversal.

* **View (User Interface):**

    Defined in `apps/media/whiteboard/widgets.inc`, the drawing area utilizes a
    `ui-flow` layout named `*strokes_stack*` with the `+flow_stack_fill` flag to
    overlay several components:

    1. **`*backdrop*` (bottom layer):** A `Backdrop` widget rendering background
       styles (plain, grid, lines, or axis) and handling canvas snapping.

    2. **`*committed_canvas*` (middle layer):** A `Canvas` widget dedicated to
       rendering finalized, static groups from `*committed_groups*`.

    3. **`*staging_canvas*` (top layer):** An independent `Canvas` widget
       rendering active `*staging_paths*` and `*moving_groups*` dynamically
       during drag operations.

    4. **`*strokes*` (input layer):** A transparent `ui-stroke` widget at the
       very top of the stack, designed solely to capture mouse/touch events and
       emit raw coordinates.

* **Controller (Event Handling and Logic):**

    * **`apps/media/whiteboard/app.lisp`**: Runs the main event loop, receiving
      messages from the UI and timeouts. It implements the primary input
      processing logic like `action-stroke`.

    * **`apps/media/whiteboard/ui.inc`**: Implements standard action handlers
      mapped by `*event_map*` in `actions.inc`, such as toolbar selections,
      undo/redo, file operations, grouping, and alignment.

### 2. The Drawing Lifecycle: Input to Vector Geometry

The drawing lifecycle handles coordinate conversion, path smoothing, and
rendering across separated canvas layers:

1. **Input Capture**: When the user drags across the canvas, `*strokes*`
   (`gui/stroke/lisp.inc`) records the coordinate streams.

2. **Event Dispatch**: The `*strokes*` widget emits a `+event_stroke` message
   containing the accumulated paths and touch states.

3. **Active Staging**: The event loop dispatches the message to `action-stroke`
   in `app.lisp`.

    * If the selected tool is `+event_pen`, the coordinate stream is smoothed
      using `path-smooth`.

    * The points are converted to fixed-point format via `n2f` and filtered
      using `path-filter` with a tolerance threshold `+tol`.

    * The path is passed to `flatten_path` to generate output shapes (polylines
      with rounded caps for the pen, bevel caps for arrows, or calculated
      polygons for boxes/circles).

    * The temporary geometry is pushed to `*staging_paths*`. `redraw-layers`
      flags `+layer_staging` to refresh the `*staging_canvas*` without redrawing
      the static committed canvas beneath it.

4. **Committing the Stroke**: Upon releasing the mouse (`commits` evaluates to
   true):

    * A `(snapshot)` of the `*committed_groups*` is pushed onto `*undo_stack*`.

    * The path is committed to `*committed_groups*` via `(commit p front)`.

    * If consecutive strokes occur within the `*group_timeout*` threshold, the
      new stroke is added directly to the existing group (using `cat` to create
      a new list, preventing accidental mutation of shared undo stack entries).
      Otherwise, a new group is initialized via `create-group`.

    * The temporary `*staging_paths*` list is cleared, and `+layer_all` is
      flagged to trigger a complete redraw.

### 3. Selection, Grouping, and Alignment

The Whiteboard provides advanced vector manipulation utilities beyond basic path
drawing:

* **Selection Mode (`+event_select`)**: The user drags a selection box. The
  application performs a bounding box intersection check or an exact polygon
  point-in-polygon test via `vector-point-in-polygon`. Matching groups are
  tagged with `+group_selected` in their flags field.

* **Move Mode (`+event_move`)**: When dragging selected items, `action-stroke`
  uses `*grabbed_groups*` to track selected elements and offset their path
  coordinates. It draws the displaced shapes onto `*staging_canvas*` as
  `*moving_groups*` until committed.

* **Grouping & Ungrouping**:

    * `action-group` filters selected groups, concatenates their internal stroke
      arrays, and generates a new merged group utilizing `create-group` with the
      `+group_selected` flag set.

    * `action-ungroup` splits composite groups back into individual stroke
      elements, keeping each resulting group selected.

* **Alignment**: `align-selected` calculates the collective bounding box of all
  selected groups, determines the target alignment edge (`:left`, `:vcenter`,
  `:right`, `:top`, `:hcenter`, `:bottom`), and applies a translation offset
  vector to each polygon coordinate.

### 4. Transactional State Snapshots (Undo / Redo)

The undo/redo engine leverages Lisp's immutable list sharing characteristics to
optimize memory usage:

* **Shallow Copying**: The `(snapshot)` function performs `(push *undo_stack*
  (cat *committed_groups*))`. By copying only the top-level list of group
  references, it creates a fast, low-overhead snapshot.

* **Preventing Mutability Leakage**: When appending a new stroke to an existing
  group, `(commit)` creates a new list using `cat` to modify the group structure
  rather than mutating it in-place. This ensures that historical snapshots
  stored in the `*undo_stack*` remain preserved.

### 5. File I/O and Service Integration

The Whiteboard application demonstrates ChrysaLisp’s service-oriented design and
inter-process messaging patterns:

* **Leveraging External Services**: Instead of maintaining a complex, custom
  file dialog within the whiteboard, the application requests the system's file
  browser (`apps/system/files/child.lisp`) as an independent child task via
  `(open-child ...)` with the `+kn_call_open` flag.

* **Asynchronous Message Exchange**: The Whiteboard passes a temporary mailbox
  (`*picker_mbox*`) to the file browser. The main loop listens on this port.
  When the user selects a file, the file browser sends the path back as a
  string, and the temporary mailbox is closed.

* **Structured Tree Serialization**:

    * **Saving**: `action-save` serializes the vector model via `tree-save` into
      a `.cwb` (ChrysaLisp Whiteboard) formatted text file. It strips out
      selection flags from the exported group structures to ensure clean file
      state on load.

    * **Loading**: `tree-load` deserializes the file. The loader contains
      backward-compatibility parsing logic for older versions (Version 2, which
      used a flat list of `polygons`) and normalized validation for the current
      Version 3 groups.
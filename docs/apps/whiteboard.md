# Whiteboard

The `Whiteboard` application is a vector graphics editor.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/whiteboard/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Whiteboard, found in `apps/whiteboard/`, is a simple vector
drawing application. It serves as an excellent case study for developers moving
beyond text-based applications, demonstrating how to handle graphical input,
manage complex object state, and implement features like undo/redo and file
serialization. It showcases a different set of GUI components than the Editor,
notably the `Canvas` and a custom `Stroke` capture widget.

### 1. Core Architecture and Components

The Whiteboard application is a perfect illustration of the
Model-View-Controller pattern in ChrysaLisp.

*   **Model (The Drawing State):**

    * The core data of the application is the drawing itself. This is stored in
        `app.lisp` in a few key Lisp `list`s:

        * **`*committed_polygons*`**: A list where each element represents a
            completed, flattened shape on the main canvas. This is the
            "permanent" state of the drawing.

        * **`overlay_paths`**: A temporary list that holds the path(s) currently
            being drawn by the user. It's cleared after each stroke is
            finalized.

        * **`*undo_stack*` and `*redo_stack*`**: Lisp `list`s that store
            previous states of `*committed_polygons*`, enabling the undo/redo
            functionality.

    * The current tool settings are also part of the model's state:
        `*stroke_col*`, `*stroke_radius*`, and `*stroke_mode*` store the
        selected color, line width, and drawing tool (pen, line, box, etc.).

*   **View (User Interface):**

    * **`apps/whiteboard/widgets.inc`:** This file defines the UI. The central
        element is a `ui-stack` widget named `*strokes_stack*`. This stack
        cleverly overlays three different widgets to create the drawing
        experience:

        1. **`*backdrop*` (bottom layer):** A `Backdrop` widget that provides
            the background grid, axis, or plain color.

        2. **`*committed_canvas*` (middle layer):** A `Canvas` widget that
            displays the finalized drawing by rendering the polygons from
            `*committed_polygons*`.

        3. **`*overlay_canvas*` (top layer):** A `Canvas` widget used to render
            the stroke currently being drawn (`overlay_paths`) in real-time,
            providing immediate visual feedback.

        4. **`*strokes*` (input layer):** A `Stroke` widget, which is a
            specialized, transparent view placed on top of everything else. Its
            sole purpose is to capture mouse drag events and report them as a
            series of points.

    * The rest of the UI consists of toolbars (`*main_toolbar*`,
        `*mode_toolbar*`, etc.) populated with `Button`s and `Radiobar`s for
        selecting tools and actions.

*   **Controller (Event Handling and Logic):**

    * **`apps/whiteboard/app.lisp`:** The `main` function runs the core event
        loop, receiving messages from the UI and timers.

    * **`apps/whiteboard/actions.inc`:** Defines the `*event_map*` which maps
        user interactions (like clicking a tool button) to handler functions.

    * **`apps/whiteboard/ui.inc`:** Implements the handler functions. These
        functions are responsible for modifying the Model (e.g., changing
        `*stroke_mode*` or adding a polygon to `*committed_polygons*`) and
        triggering redraws of the View.

### 2. The Drawing Lifecycle: From Mouse Click to Committed Shape

Understanding the sequence of events when a user draws a line is key to
understanding the app's architecture.

1.  **Input Capture:** The user clicks and drags the mouse over the main drawing
    area. The transparent `*strokes*` widget (`gui/stroke/lisp.inc`) captures
    these events. It does not draw anything itself; it simply collects the
    coordinates of the mouse path into an internal list.

2.  **Event Emission:** As the user moves the mouse (or releases the button),
    the `*strokes*` widget emits an `+event_stroke` event. This event's message
    contains the list of points for the current stroke(s).

3.  **Action Dispatch:** The main event loop receives this message and, via
    `*event_map*`, calls the `action-stroke` function in `ui.inc`.

4.  **Overlay Drawing:**

    * `action-stroke` gets the raw point data from the `*strokes*` widget.

    * It calls `flatten_path` to convert this raw data into a renderable
        polygon. The shape of the polygon depends on the currently selected
        `*stroke_mode*` (e.g., a simple polyline for the pen, or a rectangle for
        the box tool).

    * This flattened path is stored in the temporary `overlay_paths` list.

    * `redraw-layers` is called with a mask that only includes the
        `+layer_overlay`. This causes the `*overlay_canvas*` to be redrawn,
        showing the user the line they are currently drawing in real-time.

5.  **Committing the Stroke:** When the user releases the mouse button, the
    `*strokes*` widget signals that the stroke is complete.

    * The `action-stroke` function calls `(snapshot)` to save the current state
        of `*committed_polygons*` to the `*undo_stack*`.

    * It then calls `(commit p)` to take the final path from `overlay_paths` and
        append it to the main `*committed_polygons*` list.

    * `overlay_paths` is cleared.

6.  **Final Redraw:**

    * `redraw-layers` is called with the `+layer_all` mask.

    * This triggers `redraw` (`app.lisp`), which redraws both canvases. The
        `*overlay_canvas*` is cleared, and the `*committed_canvas*` is redrawn
        with the newly added shape.

    * Finally, the `*strokes*` widget's internal buffer is cleared, ready for
        the next input.

### 3. State Management and Tool Selection

The various toolbars allow the user to change the drawing parameters. This is
handled by a simple state-change pattern.

*   **Tool Selection:** Widgets like `*ink_toolbar*`, `*radius_toolbar*`, and
    `*mode_toolbar*` are `Radiobar`s. When a button in a `Radiobar` is clicked,
    it emits its associated event (`+event_ink`, `+event_radius`, `+event_pen`).

*   **Action Handlers:** The corresponding action handlers in `ui.inc` are
    extremely simple. For example:

    ```vdu
    (defun action-radius ()
        (setq *stroke_radius* (elem-get *radiuss* (. *radius_toolbar* :get_selected))))
    ```

    This function simply updates the global `*stroke_radius*` variable. It
    doesn't trigger any drawing itself. The new radius value will be used the
    *next* time `action-stroke` is called. The same pattern applies to color and
    drawing mode.

### 4. Undo and Redo System (`apps/whiteboard/undo.inc`)

The undo system is a classic example of a state-snapshot implementation.

*   **`*undo_stack*` & `*redo_stack*`:** Two lists that hold previous versions
    of the `*committed_polygons*` list.

*   **`snapshot()`:** This function is the core of the system. It is called
    *before* any change is made to `*committed_polygons*` (e.g., in
    `action-stroke` or `action-clear`). It pushes a *full copy* of the current
    `*committed_polygons*` list onto the `*undo_stack*` and clears the
    `*redo_stack*`.

*   **`action-undo()`:**

    1. Pushes the *current* `*committed_polygons*` state onto the `*redo_stack*`.

    2. Pops the *previous* state from the `*undo_stack*` and makes it the new
        `*committed_polygons*`.

    3. Calls `(redraw-layers +layer_committed)` to update the view.

*   **`action-redo()`:** Reverses the process, popping from redo and pushing to
    undo.

While simple, this method is robust and easy to implement. For an application
with a more complex state, this could become memory-intensive, but for a list of
polygons, it is perfectly adequate.

### 5. File I/O and Inter-App Communication

The Whiteboard demonstrates how a ChrysaLisp application can use another
application as a service—in this case, using the File Browser app as a "file
dialog."

*   **`action-save` & `action-load`:** These functions do not implement their
    own file browser UI. Instead, they launch the `apps/files/child.lisp`
    application as a new task using `(open-child ...)`.

*   **Communication:** They pass a newly created mailbox (`*picker_mbox*`) to
    the file browser task. The main application then waits for a message on this
    mailbox, which will contain the file path selected by the user.

*   **Serialization (`tree-save`, `tree-load`):** Once a filename is received,
    `action-save` uses `(tree-save ...)` from `lib/collections/tree.inc` to
    serialize the application's state (specifically, the `*committed_polygons*`
    list) into a text file with a `.cwb` extension. `action-load` uses
    `(tree-load ...)` to parse this file and restore the state. This
    demonstrates a simple, human-readable persistence format.

## Conclusion

The Whiteboard app is an excellent "next step" after the Template. It introduces
fundamental concepts for graphical applications, including:

*   A layered `Canvas` approach for separating background, committed, and
    in-progress drawing.

*   The use of a dedicated, non-rendering `Stroke` widget for capturing user
    input.

*   A clear, state-driven drawing pipeline where tool selection modifies global
    state, and drawing actions use that state to produce graphical output.

*   A straightforward and effective snapshot-based undo/redo system.

*   A powerful example of inter-application communication, leveraging the File
    Browser app to act as a system service.

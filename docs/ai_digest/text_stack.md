# The ChrysaLisp Text Stack: A Comprehensive Analysis

This document analyzes the architecture of text handling in ChrysaLisp, tracing
the hierarchy from low-level data management to high-level visual rendering and
application logic. The architecture demonstrates a strict separation of
concerns: **Logic/Data** (`Buffer`, `Document`) is separated from
**Presentation** (`Edit`, `Mask`, `Vdu`), bridged by a unique delegation
mechanism.

## 1. The Foundation: Data & Logic

The core text manipulation logic resides in the library layer, specifically
within `lib/text/`. These classes have no knowledge of pixels, fonts, or
windows; they operate purely on coordinate tuples and strings.

### 1.1. `Buffer` (`lib/text/buffer.inc`)

The `Buffer` class is the base unit of text storage and atomic manipulation. It
manages the raw state of the text and the "cursors" navigating it.

* **State:**

    * `buffer_lines`: A list of strings representing the file content.

    * `cursors`: A list of cursor tuples. ChrysaLisp supports **multi-cursor
      editing** natively at this low level.

    * `undo_stack` / `redo_stack`: Built-in history management.

* **The Cursor Tuple:** A cursor is not just an X/Y pair. It is defined as `(cx
  cy ax ay sx)`.

    * `cx, cy`: The active cursor position (the "head").

    * `ax, ay`: The anchor position (the "tail"). If distinct from `cx, cy`, a
      selection exists.

    * `sx` ("Sticky X"): Remembers the desired horizontal column when moving
      vertically through lines of varying lengths.

*   **Key Responsibilities:**

    * **Atomic Mutation:** Methods like `:iinsert` (internal insert) and
      `:idelete` perform the actual string manipulation and record state for
      Undo/Redo.

    * **Navigation:** Implements logic for `:left`, `:right`, `:up`, `:down`,
      `:home`, and `:end`. It handles wrapping across line boundaries.

    * **Cursor Management:** `:merge_cursors` ensures that multiple cursors
      overlapping or touching are combined into single selection ranges,
      preventing logic errors during bulk edits.

    * **Clipping:** `:clip_cursor` ensures coordinates remain within the bounds
      of the text content.

### 1.2. `Document` (`lib/text/document.inc`)

`Document` inherits from `Buffer`. It expands the low-level character
manipulations into higher-level semantic text operations. It represents a "file"
in memory.

* **Semantic Selection:**

    * Uses character class logic (from `lib/text/charclass.inc`) to implement
      `:select_word`.

    * Implements `:select_paragraph` by scanning for blank lines.

    * `:select_block` handles bracket matching logic.

* **Text Processing:**

    * Implements functional text transformations like `:to_upper`, `:to_lower`,
      `:sort` (lines), and `:unique`.

    * Handles Indentation (`:tab`, `:left_tab`) and reformatting (`:reflow`,
      `:split`).

* **I/O:**

    * `:file_load` and `:file_save` manage reading from and writing to `stream`
      objects.

    * Integrates with the `Syntax` engine to generate highlighting data
      (`buffer_syntax`) during load or mutation.

## 2. The Presentation Layer: Composition & Rendering

The GUI layer (`gui/`) is responsible for visualizing the `Document`. ChrysaLisp
uses a composition approach where the editor widget aggregates several
specialized sub-views.

### 2.1. `Edit` (`gui/edit/lisp.inc`)

The `Edit` class is the main visual component. It inherits from `View`.
Crucially, it **contains** a `Document` instance (stored in the `:buffer`
property) rather than inheriting from it.

* **Composition:** An `Edit` view is a container for specific visual layers
  added as children:

    1. **`Vdu`:** Renders the actual text glyphs.

    2. **`Mask` (Ink):** Renders bracket matching highlights.

    3. **`Mask` (Selected):** Renders selection backgrounds (e.g., grey blocks).

    4. **`Mask` (Found):** Renders search result highlights.

    5. **`Mask` (Region):** Renders region locks/focus.

* **The `:underlay` Method:**

    This is the rendering heart of the `Edit` class. Every frame (or upon
    refresh), it calculates the intersection of the `buffer` state with the
    visual viewport.

    1. It calculates pixel coordinates based on the `vdu_text` char size.

    2. It translates text coordinates (lines/cols) into visual regions
       (rectangles).

    3. It calls `:add_opaque` on the various `Mask` children to define where
       colors should be drawn.

    4. It loads the visible slice of text into the `Vdu`.

*   **Proxy Delegation:**

    Because `Edit` *has-a* `Document` but needs to expose `Document` methods
    (like `:insert` or `:left`), it uses `defproxymethod`.
    
    ```vdu
    (defproxymethod :left () :buffer)
    (defproxymethod :insert (text) :buffer)
    ```
    
    This macro creates a method on `Edit` that looks up the object in
    `this->buffer` and calls the corresponding method on it. This effectively
    flattens the API, allowing the application to treat the `Edit` widget as if
    it were the text buffer itself.

### 2.2. `Mask` (`gui/mask/lisp.inc`)

A lightweight `View`. Its `:draw` method simply sets a color and fills the
`+view_opaque_region`. This relies on the efficient region arithmetic in the GUI
kernel to draw complex, non-contiguous selection shapes without manual geometry
management.

### 2.3. `Vdu` (`gui/vdu/`)

A specialized view for monospaced text rendering. It manages a texture cache of
glyphs and a grid of characters (`vdu_chars`). The `Edit` class loads text into
it via `:vdu_load`.

## 3. The Application Layer: Interaction & Orchestration

The applications (`apps/tools/edit` and `apps/tools/viewer`) wire the `Edit`
widget to user inputs. They do not implement text logic; they implement
*command* logic.

### 3.1. Controller Logic (`app.lisp` & `actions.inc`)

The main loop consumes events from the kernel/GUI service.

1. **Event Mapping:** Key presses and GUI events are mapped to Action Functions
   via `*key_map*` and `*event_map*`.

    * *Example:* `Ctrl+C` -> `action-copy`. `ArrowUp` -> `action-up`.

2. **Action Functions:** These functions (defined in `edit.inc`, `cursor.inc`,
   etc.) act as the controller.

    * They perform logic (e.g., calculating scroll offset).

    * They invoke methods on the `*edit*` object (which proxies them to the
      `buffer`).

    * **Example (`action-insert`):**

        ```vdu
        (defun action-insert (text)
            (. *edit* :insert text) ; Proxies to Document:iinsert
            (refresh))              ; Triggers UI redraw/slider updates
        ```

3. **Macro Recording:**

    Because all user interaction flows through these `action-*` functions, the
    `Edit` app implements macro recording by simply logging the sequence of
    action calls and their arguments into a list (`*macro_record*`), allowing
    for complex replays.

### 3.2. `Editor-edit` vs `Viewer-edit`

The separation of logic (`Document`) and view (`Edit`) allows for easy
specialization via inheritance.

* **`Editor-edit` (`apps/tools/edit/ui.inc`):** Inherits `Edit`. Handles mouse
  interactions for full editing (setting cursor position, dragging to select).
    
* **`Viewer-edit` (`apps/tools/viewer/ui.inc`):** Inherits `Edit`. Overrides
  mouse interactions to support read-only behaviors, such as clicking to follow
  links (file navigation) rather than placing a cursor for typing.

## Summary Flow

When a user types a character in the **Edit** application:

1. **Input:** The kernel sends a Key Event to the App's mailbox.

2. **Dispatch:** `app.lisp` looks up the key in `*key_map*` or `*normal_map*`.
   It resolves to `action-insert`.

3. **Action:** `action-insert` calls `(. *edit* :insert char)`.

4. **Proxy:** The `Edit` class proxy forwards the call to `(. buffer :insert
   char)`.

5. **Logic:** `Document` (the buffer) calculates the new string state, updates
   the undo stack, and moves the cursor `cx`.

6. **Refresh:** The `action-insert` function calls `(refresh)`.

7. **Render Prep:** `refresh` calls `(. *edit* :underlay)`.

8. **Masking:** `:underlay` calculates where the cursor is (pixels) and adds a
   rect to `mask_ink`'s opaque region.

9. **VDU Load:** `:underlay` pushes the relevant slice of text lines into the
   `Vdu` widget.

10. **Draw:** The GUI compositor traverses the view tree. `Mask` fills the
    cursor rectangle. `Vdu` blits the glyph textures.

This architecture ensures that the heavy lifting of text manipulation is
isolated from the rendering, while the rendering leverages the OS's native
region management for high performance.

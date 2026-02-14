# The ChrysaLisp Text Stack: A Unified Architecture

This document analyzes the architecture of text handling in ChrysaLisp, tracing
the hierarchy from low-level data management to high-level visual rendering and
application logic.

The architecture demonstrates a strict separation of concerns, utilizing a
**Model-View-Controller** pattern adapted for ChrysaLisp's object capabilities.
A shared abstraction layer ensures that text manipulation logic is identical
whether driven by a graphical user interface or a headless batch script.

## 1. The Foundation: Data & Logic (The Model)

The core text manipulation logic resides in the library layer (`lib/text/`).
These classes have no knowledge of pixels, fonts, or windows; they operate
purely on coordinate tuples and strings.

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

* **Key Responsibilities:**

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

    * `:stream_load` and `:stream_save` manage reading from and writing to
      `stream` objects.

    * Integrates with the `Syntax` engine to generate highlighting data
      (`buffer_syntax`) during load or mutation.

## 2. The Abstraction Layer (The Interface)

To allow the same code to drive both a GUI editor and a headless script,
ChrysaLisp utilizes a polymorphic abstraction layer.

### 2.1. `lib/text/edit.inc`

This file defines a suite of global functions (e.g., `edit-down`, `edit-insert`,
`edit-copy`) that act as the public API for text manipulation. These functions
do not contain logic themselves; they operate on a dynamic variable `*edit*`.

```vdu
(defmacro gen-edit (n m) `(defun ,(sym (str "edit-" n)) () (. *edit* ,m)))
(gen-edit top :top)
(defun edit-insert (txt) (. *edit* :insert txt))
```

* **Polymorphism:**

    * In a **GUI context**, `*edit*` is bound to an `Edit` Widget.

    * In a **CLI context**, `*edit*` is bound directly to a `Document`.

    * Because the `Edit` widget proxies methods to its internal `Document`, the
      API signature is identical in both contexts.

## 3. The Presentation Layer: Composition & Rendering (The View)

The GUI layer (`gui/`) is responsible for visualizing the `Document`. ChrysaLisp
uses a composition approach where the editor widget aggregates several
specialized sub-views.

### 3.1. `Edit` (`gui/edit/lisp.inc`)

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

* **Proxy Delegation:**

    Because `Edit` *has-a* `Document` but needs to conform to the API expected
    by `lib/text/edit.inc`, it uses `defproxymethod`.

    ```vdu
    (defproxymethod :left () :buffer)
    (defproxymethod :insert (text) :buffer)
    ```

    This creates a method on `Edit` that looks up the object in `this->buffer`
    and calls the corresponding method on it.

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

### 3.2. Sub-Components

* **`Mask` (`gui/mask/lisp.inc`):** A lightweight `View`. Its `:draw` method
  simply sets a color and fills the `+view_opaque_region`. This relies on the
  efficient region arithmetic in the GUI kernel to draw complex, non-contiguous
  selection shapes without manual geometry management.

* **`Vdu` (`gui/vdu/`):** A specialized view for monospaced text rendering. It
  manages a texture cache of glyphs and a grid of characters.

## 4. The Application Layer (The Controllers)

Applications wire the model and view to user inputs or scripts.

### 4.1. GUI Editor (`apps/tools/edit/`)

This is the interactive controller.

* **Initialization:** Creates an `Editor-edit` widget (subclass of `Edit`) and a
  `Document`.

* **Event Handling:** `actions.inc` maps key presses (via `*key_map*`) to the
  functions in `lib/text/edit.inc` (e.g., `Ctrl+C` -> `action-copy` ->
  `edit-copy`).

* **Feedback Loop:** After an action changes the buffer, `(refresh)` is called.
  This triggers the `Edit` widget's `:underlay` method to update the visual
  masks and VDU text.

### 4.2. GUI Viewer (`apps/tools/viewer/`)

Inherits `Edit` functionality via `Viewer-edit` but overrides mouse interactions
to support read-only behaviors (clicking to follow links) rather than placing
cursors for typing. It reuses the exact same display logic as the Editor.

### 4.3. CLI Editor (`cmd/edit.lisp`)

This is the headless controller, used for batch processing or scripting.

* **Architecture:** It instantiates a `Document` but **not** an `Edit` widget.

* **Binding:** It binds the `Document` instance to the `*edit*` variable.

* **Execution:**

    * It accepts a script via `-c` (command string) or `-s` (script file).

    * The script executes in an environment where functions like `(edit-down)`
      or `(edit-replace "foo")` are available.

    * These functions operate directly on the `Document` via the `*edit*`
      binding.

* **Result:** Complex editing operations (multi-cursor find/replace, block
  moves) can be performed programmatically with zero GUI overhead, using the
  exact same logic code as the interactive editor.

## Summary of Flow

When a command like `(edit-insert "A")` is executed:

1. **If in GUI:**

    * `lib/text/edit.inc` calls `(. *edit* :insert "A")`.

    * `*edit*` is an `Edit` widget. It proxies the call to its internal
      `:buffer`.

    * `Buffer` updates text and cursors.

    * The App calls `(refresh)`, causing `Edit` to redraw masks and glyphs.

2. **If in CLI:**

    * `lib/text/edit.inc` calls `(. *edit* :insert "A")`.

    * `*edit*` is a `Document` object. It executes the update directly.

    * No refresh or rendering occurs.

This architecture ensures zero duplication of effort, high performance, and
total consistency between graphical and command-line text manipulation tools.
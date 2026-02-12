# The Text Buffer and Edit Architecture

In ChrysaLisp, the relationship between text manipulation and the user interface
is architected around a strict separation of concerns. The `Buffer` class
(`lib/text/buffer.inc`) serves as the intelligent, headless engine for text
operations, while the `Edit` class (`gui/edit/lisp.inc`) acts as the visual
controller.

Crucially, **multi-cursor support is not a UI feature; it is a fundamental
property of the Buffer class itself.** This means any application, whether a
graphical text editor, a command-line refactoring tool, or a batch processing
script, inherently supports simultaneous multi-location editing, selection, and
navigation.

## 1. The Buffer Class (`lib/text/buffer.inc`)

The `Buffer` is a headless object responsible for storing text, managing history
(undo/redo), and, most importantly, manipulating cursors. It treats the cursor
not as a singleton coordinate pair, but as a list of state tuples.

### 1.1 The Multi-Cursor Primitive

In `Buffer`, the state of user interaction is stored in a list field named
`:cursors`. A single cursor is defined by a list of 5 values:

```vdu
(cx cy ax ay sx)
```

* **cx, cy**: The current position of the caret (Cursor X, Cursor Y).

* **ax, ay**: The anchor position (Anchor X, Anchor Y). If `cx/cy` differs from
  `ax/ay`, text is selected between them.

* **sx**: Sticky X. Used when moving up or down lines to remember the original
  horizontal column preference, even if passing through shorter lines.

### 1.2 The Mutation Pipeline

The `Buffer` class implements a sophisticated pipeline to ensure data
consistency when multiple cursors operate simultaneously.

1. **Operation Request:** A method like `:insert` or `:delete` is called on the
   Buffer.

2. **Cursor Sorting:** The buffer sorts cursors (usually bottom-to-top or
   top-to-bottom depending on the operation) to prevent operations from
   invalidating pending cursor positions.

3. **Map and Merge:**

    * The buffer iterates over the cursors.

    * For every mutation (e.g., inserting text at cursor A), the buffer
      calculates the offset and automatically adjusts the coordinates of all
      other cursors (B, C, etc.) via helper functions like `csr-map-insert` and
      `csr-map-delete`.

    * Finally, `Buffer` calls `:merge_cursors`. If two cursors collide or
      overlap due to movement or typing, they are mathematically merged into a
      single cursor/selection.

This logic is entirely encapsulated within `lib/text/buffer.inc`. The UI does
not calculate where cursors go; the Buffer tells the UI where they are.

### 1.3 Key Methods (The "Workhorse" API)

The `Buffer` class provides a rich API that can be driven programmatically:

* **Navigation:** `:left`, `:right`, `:up`, `:down`, `:home`, `:end`. These
  update *all* active cursors simultaneously.

* **Selection:** `:select_word`, `:select_line`, `:select_all`.

* **Editing:** `:insert`, `:delete`, `:backspace`, `:tab`, `:indent`.

* **Search:** `:find` (returns a list of match coordinates which can be
  immediately converted into cursors).

* **IO:** `:file_load`, `:file_save`, `:cut`, `:copy`, `:paste`.

* **Undo/Redo:** `:undo`, `:redo`.

## 2. The Edit Class (`gui/edit/lisp.inc`)

The `Edit` class is a visual widget (inheriting from `View`) that wraps a
`Buffer`. It does not contain text processing logic. Instead, it acts as a
projection of the Buffer's state onto the screen and a relay for input events.

### 2.1 Composition, Not Inheritance

The `Edit` class contains a `Buffer` instance in its `:buffer` field. It
delegates almost all logic to this instance.

### 2.2 The Proxy Pattern

ChrysaLisp uses the `defproxymethod` macro to wire UI actions directly to the
Buffer. For example, in `gui/edit/lisp.inc`:

```vdu
(defproxymethod :left () :buffer)
(defproxymethod :insert (text) :buffer)
```

When the user presses the Left Arrow key, the `Edit` widget receives the event,
but simply calls `(. buffer :left)`. The Buffer calculates the new positions of
all 50 active cursors, handles line wrapping and merging, and updates its state.

### 2.3 Visualization

The `Edit` widget is responsible for:

1. **Rendering Text:** Using the `:vdu_text` (Visual Display Unit) component.

2. **Rendering Overlays:** It asks the Buffer for cursor positions (`. buffer
   :get_cursors`) and selection ranges (`. buffer :get_selected`). It then draws
   these as `Mask` layers (`:mask_ink`, `:mask_selected`, `:mask_found`) over
   the text.

3. **Input Translation:** Converting mouse clicks (pixel coordinates) into text
   coordinates (`:char_pos`) to tell the Buffer where to place cursors.

## 3. Headless Usage: The Buffer as an Intelligent Workhorse

Because `Buffer` depends only on `sys/str`, `sys/list`, and `sys/files` (not
`gui/*`), it can be instantiated in command-line tools or background services to
perform complex text processing tasks using the same logic as the interactive
editor.

### Scenario: Batch Refactoring via Command Line

Imagine a command-line tool that needs to find every occurrence of "foo" in a
file, replace it with "bar", and wrap the line in parenthesis.

In a traditional language, this requires regex string splicing loops. In
ChrysaLisp, we simply drive the `Buffer` class:

```vdu
(import "lib/text/buffer.inc")

(defun batch-process (filename)
    ; 1. Instantiate a headless buffer
    (defq buf (Buffer))

    ; 2. Load content
    (. buf :stream_load (file-stream filename))

    ; 3. Find all occurrences of "foo". 
    ; :find returns a list of coordinate tuples, which :set_cursors accepts directly.
    (defq matches (. buf :find "foo" :nil :nil))
    
    (if (nempty? matches)
        (progn
            ; 4. Set a cursor at every "foo" simultaneously
            (. buf :set_cursors matches)

            ; 5. Delete "foo" (delete selection at all cursors)
            (. buf :delete)

            ; 6. Type "bar" at all locations
            (. buf :insert "bar")

            ; 7. Select the lines we are currently on
            (. buf :select_line)

            ; 8. Move to start of lines and type "("
            (. buf :left) 
            (. buf :insert "(")

            ; 9. Move to end of lines and type ")"
            (. buf :end)
            (. buf :insert ")")

            ; 10. Save the result
            (. buf :file_save filename)
            (print "Processed " (length matches) " instances.")))
    )
```

### Advantages of this Approach

1. **Consistency:** The batch script behaves exactly like the text editor. There
   are no discrepancies between how regex works in the GUI vs the CLI because
   they use the exact same engine.

2. **Safety:** The `Buffer` handles overlapping edits automatically. If two
   "foo"s are on the same line or adjacent, the multi-cursor merge logic ensures
   the operations don't corrupt the buffer structure.

3. **Undo/Redo:** Even in a script, you can perform operations and then call `(.
   buf :undo)` if a heuristic check fails before saving.

4. **Power:** Operations that are hard to express as regex (like "move down 3
   lines and indent") are trivial when driving the cursor API.

## Summary

The relationship between `Edit` and `Buffer` is a classic Model-View separation,
but optimized for the unique capabilities of the ChrysaLisp virtual machine.

* **`lib/text/buffer.inc` (The Brains):** A powerful text engine with native,
  deep support for multiple cursors, selection management, and history. It is
  UI-agnostic.

* **`gui/edit/lisp.inc` (The Face):** A thin view layer that proxies input to
  the buffer and renders the resulting state.

This architecture ensures that "Multi-Cursor" is not just a feature of the text
editor application, but a pervasive capability of the operating system's text
processing libraries, available to any script or tool.
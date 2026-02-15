# The Text Buffer and Edit Architecture

In ChrysaLisp, the relationship between text manipulation and the user interface
is architected around a strict separation of concerns. The text stack is
composed of three primary layers: the **Buffer** (raw data), the **Document**
(semantic logic), and the **Edit** widget (visual projection).

Crucially, **multi-cursor support is not a UI feature; it is a fundamental
property of the Buffer class itself.** This means any application—whether a
graphical text editor, a command-line refactoring tool, or a batch processing
script—inherently supports simultaneous multi-location editing, selection, and
navigation.

## 1. The Model Layer (`lib/text/`)

### 1.1 The Buffer Class (`lib/text/buffer.inc`)

The `Buffer` is a headless engine responsible for storing text (as a list of
strings), managing history (undo/redo), and manipulating cursors. It treats the
cursor not as a singleton coordinate pair, but as a list of state tuples.

#### The Multi-Cursor Primitive

In `Buffer`, active cursors are stored in the `:cursors` field. A single cursor
is defined by a vector of 5 values:

```vdu
(cx cy ax ay sx)
```

* **cx, cy**: The current position of the caret (Cursor X, Cursor Y).

* **ax, ay**: The anchor position (Anchor X, Anchor Y). If `cx/cy` differs from
  `ax/ay`, text is selected between them.

* **sx**: Sticky X. Used when moving up or down lines to remember the original
  horizontal column preference when passing through shorter lines. A value of
  `-1` indicates it hasn't been set yet.

#### The Mutation Pipeline

The `Buffer` class ensures data consistency when multiple cursors operate
simultaneously:

1. **Operation Request:** A method like `:insert` or `:delete` is called.

2. **Cursor Sorting:** The buffer sorts cursors (using `csr-cmp`) to ensure
   deterministic behavior.

3. **Map and Merge:**

    * For every mutation (e.g., `:iinsert` at cursor A), the buffer calculates
      the offset and automatically adjusts the coordinates of all other cursors
      via helper functions like `csr-map-insert` and `csr-map-delete`.

    * Finally, `:merge_cursors` is called. If two cursors collide or overlap,
      they are mathematically merged into a single cursor/selection.

### 1.2 The Document Class (`lib/text/document.inc`)

`Document` inherits from `Buffer` and adds high-level semantic awareness. While
`Buffer` handles raw character-level operations, `Document` understands words,
paragraphs, and structural blocks.

**Key Semantic Methods:**

* **Selection:** `:select_word`, `:select_line`, `:select_paragraph`,
  `:select_block`, `:select_form`.

* **Transformations:** `:to_upper`, `:to_lower`, `:sort`, `:unique`, `:invert`,
  `:comment`.

* **Formatting:** `:reflow`, `:split`, `:tab`, `:left_tab` (outdent),
  `:right_tab` (indent).

* **Persistence:** Uses `Buffer`'s `:stream_load` and `:stream_save`.

## 2. The View Layer (`gui/edit/lisp.inc`)

The `Edit` class is a visual widget (inheriting from `View`) that wraps a
`Document`. It acts as a projection of the Document's state onto the screen and
a relay for input events.

### 2.1 Composition, Not Inheritance

The `Edit` class contains a `Document` instance in its `:buffer` field. It
delegates almost all logic to this instance.

### 2.2 The Proxy Pattern

ChrysaLisp uses the `defproxymethod` macro to wire UI actions directly to the
Document.

```vdu
(defproxymethod :left () :buffer)
(defproxymethod :insert (text) :buffer)
(defproxymethod :select_word () :buffer)
```

When a user interacts with the `Edit` widget, it simply calls the corresponding
method on the internal `:buffer`.

### 2.3 Visualization

The `Edit` widget is responsible for:

1. **Rendering Text:** Utilizing the `Vdu` (Visual Display Unit) component.

2. **Rendering Overlays:** It asks the Document for cursor positions and
   selection ranges, then draws these as `Mask` layers (`:mask_ink`,
   `:mask_selected`, `:mask_found`, `:mask_region`) over the text.

3. **Input Translation:** Converting mouse clicks (pixel coordinates) into text
   coordinates (`:char_pos`) to tell the Document where to place cursors.

## 3. Headless Usage: The Document as a Scripting Workhorse

Because `Document` (and `Buffer`) are decoupled from the GUI, they can be used
in command-line tools or background services for complex text processing using
the exact same logic as the interactive editor.

### Scenario: Batch Refactoring via Command Line

Imagine a script that finds every occurrence of "foo" in a file, replaces it
with "bar", and wraps the containing line in parentheses.

```vdu
(import "lib/text/document.inc")

(defun batch-process (filename)
    ; 1. Instantiate a headless document (which is a Buffer)
    (defq doc (Document))

    ; 2. Load content from a file stream
    (. doc :stream_load (file-stream filename))

    ; 3. Find all occurrences of "foo".
    ; :find returns a buffer_found structure (list of matches per line).
    (defq matches (. doc :find "foo" :nil :nil))
    
    (if matches
        (progn
            ; 4. Set cursors to ALL matches simultaneously
            (. doc :set_found_cursors matches)

            ; 5. Replace "foo" with "bar" at all locations
            ; . doc :insert handles deleting selections and inserting new text
            (. doc :insert "bar")

            ; 6. Select the lines we just modified
            (. doc :select_line)

            ; 7. Copy the lines, wrap them, and paste back
            (defq lines (split (. doc :copy) "\f"))
            (defq wrapped (join (map (# (str "(" %0 ")")) lines) "\f"))
            (. doc :paste wrapped)

            ; 8. Save the result back to the stream
            (. doc :stream_save (file-stream filename :write))
            (print "Processed matches.")))
    )
```

### Advantages of this Approach

1. **Consistency:** The batch script behaves exactly like the text editor.

2. **Safety:** The `Buffer` handles overlapping edits and coordinate shifting
   automatically.

3. **Undo/Redo:** Scripts can perform operations and call `(. doc :undo)` if
   validation fails.

4. **Malleability:** Operations that are difficult to express with standard
   regex (like "find X, move up 2 lines, and indent") are trivial using the
   multi-cursor API.

## Summary

* **`lib/text/buffer.inc`**: The raw engine. Handles text storage, multi-cursor
  math, and undo history.

* **`lib/text/document.inc`**: The semantic layer. Handles word/block selection,
  text transformations, and I/O.

* **`gui/edit/lisp.inc`**: The visual layer. Proxies input to the `Document` and
  renders the result using `Vdu` and `Masks`.

* **`lib/text/edit.inc`**: The global API. Provides a set of `edit-*` functions
  that operate on a dynamic `*edit*` variable, allowing the same code to drive
  either an `Edit` widget or a headless `Document`.

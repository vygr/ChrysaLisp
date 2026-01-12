# The Buffer Class (`lib/text/buffer.inc`)

The `Buffer` class is the fundamental text document model within the ChrysaLisp
GUI subsystem. It is responsible for storing text data, managing line states,
handling syntax highlighting, managing the undo/redo history, and—most
critically—executing complex **multi-cursor** editing logic.

While the `Edit` class (`gui/edit/lisp.inc`) provides the visual component (the
View and Controller), the `Buffer` class acts as the Model. The `Edit` class
relies heavily on the `Buffer` class to perform the actual "grunt work" of text
manipulation, often delegating user inputs directly to the buffer via proxy
methods.

## Architecture and Relationship with `Edit`

In ChrysaLisp's text editing architecture, there is a strict separation of
concerns:

1. **`Edit` (The Frontend):** Handles user input events (keyboard, mouse),
   rendering (VDU), scrolling, and screen coordinate mapping.

2. **`Buffer` (The Backend):** Handles string manipulation, file I/O, cursor
   logic, history, and syntax state.

The `Edit` class contains a `:buffer` property holding an instance of this
class. Many operations in `Edit` are simply pass-throughs. For example, in
`gui/edit/lisp.inc`:

```vdu
(defproxymethod :insert (text) :buffer)
(defproxymethod :delete () :buffer)
(defproxymethod :backspace () :buffer)
```

When a user types a character in the `Edit` widget, it calls `.buffer :insert`.
The `Buffer` determines *where* that text goes (based on potentially multiple
cursors) and updates the internal state.

## Internal State

The `Buffer` maintains the state of the document using the following key
properties:

* **:buffer_lines:** A list of strings representing the rows of text.

* **:cursors:** A list of cursor definitions. Each cursor is a list
  `(current_index anchor_index preferred_x)`.

* **:undo_stack / :redo_stack:** Lists containing snapshots of the buffer state
  for history navigation.

* **:syntax_engine:** An instance of the `Syntax` class used to colorize text.

* **:buffer_syntax:** Cached color data for the lines.

* **:buffer_index:** A cached index allowing fast conversion between *(x, y)*
  coordinates and linear integer indices.

## Multi-Cursor Editing Engine

The most complex feature of the `Buffer` class is its native support for
multi-cursor editing. Operations are rarely performed on a single point;
instead, they are broadcast to all active cursors.

### Cursor Definition

A cursor is defined as a tuple of indices relative to the linear start of the
file, not *(x, y)* coordinates.

* **ci (Current Index):** The location of the caret.

* **ai (Anchor Index):** The location where selection began. If `ci == ai`,
  there is no selection.

* **sx (Sticky X):** The preferred visual X coordinate (used when moving up/down
  through lines of varying lengths).

### Cursor Merging

The `Buffer` class ensures that cursors never overlap or cross in invalid ways.
The method `:merge_cursors` is called after navigation or selection changes. It
sorts cursors by position and merges overlapping selection ranges into single,
continuous selections. This prevents logic errors where two cursors might try to
delete the same character twice.

### Batch Execution

Mutation methods (like `:insert` or `:delete`) typically use `reach`
(reverse-each) iteration over the cursor list. By processing cursors from the
bottom of the document to the top, changes made by one cursor do not invalidate
the indices of cursors appearing earlier in the document.

## Coordinate Systems

The `Buffer` abstracts text as a linear stream of characters, but the UI renders
them as a 2D grid. The class provides methods to translate between these
domains:

* **:index_to_cursor (idx):** Converts a linear character index into an `(x, y)`
  tuple. It uses binary search on the `:buffer_index` cache for performance.

* **:cursor_to_index (x, y):** Converts 2D coordinates into a linear index.

## Method Reference

### Lifecycle and I/O

* **`:init (mode syntax)`** Initializes the buffer. `mode` determines if syntax
  highlighting is active. `syntax` is an optional specific syntax engine
  instance.

* **`:file_load (filepath)`** Loads a file from disk, normalizes line endings,
  expands tabs based on `:tab_width`, and initializes the undo stack.

* **`:file_save (filepath)`** Writes the current `:buffer_lines` to disk.

* **`:file_load_hex (filepath width)`** Loads a binary file and formats it into
  a hex dump layout for editing.

### Cursor Management

* **`:get_cursors ()`** Returns the list of active cursors.

* **`:set_cursors (list)`** Sets the active cursors.

* **`:add_cursor (cx cy [ax ay])`** Adds a new cursor at the specific *(x, y)*
  coordinates. Optional anchor coordinates define a selection.

* **`:get_cursor ()`** Returns the primary (last active) cursor as `(cx cy ax
  ay)`.

### Navigation

These methods update the positions of *all* active cursors simultaneously.

* **`:left ()` / `:right ()`** Moves cursors horizontally. If a selection
  exists, it collapses the cursor to the start/end of the selection.

* **`:up ()` / `:down ()`** Moves cursors vertically, respecting the "Sticky X"
  (`sx`) position to maintain column alignment across lines of different
  lengths.

* **`:home ()` / `:end ()`** Moves cursors to the start or end of the current
  line.

* **`:top ()` / `:bottom ()`** Moves all cursors to the absolute start or end of
  the document.

### Selection

These methods behave like navigation but update the Current Index (`ci`) while
leaving the Anchor Index (`ai`) in place, creating or modifying text selections.

* **`:left_select ()` / `:right_select ()`**

* **`:up_select ()` / `:down_select ()`**

* **`:home_select ()` / `:end_select ()`**

* **`:select_all ()`** Selects the entire document.

### Content Mutation

These methods are wrapped in the `undoable` macro (provided in `edit.inc`
context usually) to ensure history is saved before modification.

* **`:insert (text)`** Inserts `text` at every cursor position. If cursors have
  active selections, the selected text is replaced.

* **`:delete ()`** Deletes the character after the cursor (Delete key behavior),
  or deletes the current selection.

* **`:backspace ()`** Deletes the character before the cursor, or the current
  selection.

* **`:cut ()`** Removes selected text from all cursors and returns it (typically
  joined by newlines for the clipboard).

* **`:paste (text)`** Intelligent paste. If the number of lines in the clipboard
  matches the number of cursors, it performs a "vertical paste" (one line per
  cursor). Otherwise, it inserts the full text at every cursor.

* **`:tab ()`** Inserts spaces to align to the next tab stop.

* **`:comment ()`** Toggles comment prefixes on lines covered by cursors.

### Undo/Redo

* **`:push_undo (record)`** Pushes a state snapshot onto the stack.

* **`:undo ()`** Reverts the buffer to the previous state.

* **`:redo ()`** Re-applies a reverted state.

### Metadata & Highlighting

* **`:build_syntax (this [end_state])`** Runs the syntax engine over dirty lines
  to update `:buffer_syntax` (colors) and `:buffer_states` (tokenizer state).

* **`:build_brackets (this)`** Scans for matching parentheses/brackets relative
  to cursor positions to highlight matching pairs.

* **`:build_index (this)`** Rebuilds the linear index cache map used for
  coordinate conversion. This is flagged dirty whenever text changes.

## Usage Example (Conceptual)

While `Buffer` is rarely instantiated manually outside of an `Edit` widget, here
is how the relationship works conceptually:

```vdu
; Within the Edit class initialization
(def this :buffer (Buffer))

; User presses "Right Arrow"
; Edit class catches event:
(defmethod :key_down (event)
    (if (eql key 0x4000004f) ; Right arrow scancode
        (. buffer :right))) ; Proxy to buffer

; Buffer implementation of :right
(defmethod :right ()
    ; Logic to update every cursor in :cursors list
    ; adjusting for line length wrapping
    (lower :cursors (. this :merge_cursors
        (map (lambda ((ci ai ignore))
             (list (inc ci) (inc ci) :nil))
             (get :cursors this))))
    this)
```

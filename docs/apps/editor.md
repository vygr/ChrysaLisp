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
apps/edit/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Editor, found in `apps/edit/`, is a sophisticated, multi-buffer programmer's text editor tailored for the ChrysaLisp environment. It showcases many of the GUI system's strengths, including its event handling, widget composition, and the Lisp-centric property system. This document delves into its architecture, key features, and implementation details based on the provided source code.

### 1. Core Architecture and Components

The Editor follows a structure that loosely resembles Model-View-Controller, with clear separation of concerns:

* **Model (Text Management):**

    * **`Buffer` Class (`lib/text/buffer.inc`):** This is the heart of text data management. Each open file or scratchpad is represented by a `Buffer` instance. It handles:

        * Storing lines of text (`:buffer_line`).

        * Undo/redo stack (`:undo_stack`, `:redo_stack`).

        * Cursor position (`:cursor_x`, `:cursor_y`).

        * Syntax highlighting data (`:buffer_syntax` when a `:syntax_engine` is
          provided).

        * Bracket matching information (`:buffer_brackets`).

        * Find results (`:buffer_found`).

        * File loading/saving (`:file_load`, `:file_save`).

        * Core text manipulations like `:insert`, `:delete`, `:backspace`,
          `:cut`, `:copy`, `:paste`.

    * **`Syntax` Class (`lib/text/syntax.inc`):** An instance of this class
      (`*syntax*` in `app_impl.lisp`) is passed to each `Buffer` to provide
      syntax highlighting.

    * **`Dictionary` Class (`lib/text/dictionary.inc`):** Used for type-ahead
      suggestions. An instance (`dictionary` in `app_impl.lisp`) is populated
      with keywords and words from open files.

* **View (User Interface):**

    * **`*window*` (`apps/edit/widgets.inc`):** The main application `Window`,
      serving as the root of the UI tree.

    * **`Editor-edit` Class (`gui/edit/lisp.inc`):** A specialized class
      inheriting from `Edit`. The primary instance is `*edit*`. This widget is
      responsible for visually rendering the text from its active `Buffer` and
      handling user interactions.

    * **`Edit` Widget (`gui/edit/lisp.inc`):** The underlying graphical text
      editing component. It uses child `Vdu` widgets (`:vdu_text`,
      `:vdu_paper`, `:vdu_ink`) for rendering different visual layers (text,
      selections, bracket highlights).

    * **`Vdu` Widget (`gui/vdu/lisp.inc`):** Renders text on a character grid.

    * **Toolbars, Sliders, Trees (`apps/edit/widgets.inc`):** Standard widgets for find/replace, file navigation, scrollbars, etc. (e.g., `*main_toolbar*`, `*find_toolbar*`, `*open_tree*`, `*file_tree*`, `*xslider*`, `*yslider*`).

* **Controller (Application Logic and Event Handling):**

    * **`apps/edit/app_impl.lisp`:** Contains the main application loop (`main` function) and orchestrates the interaction between the model and view.

    * **`apps/edit/actions.inc` (and its includes like `cursor.inc`, `file.inc`, etc.):** Defines `Fmap`s (`*event_map*`, `*key_map*`, `*key_map_shift*`, `*key_map_control*`) that map event IDs and key codes to handler functions. These handler functions implement the editor's commands and features.

    * **`dispatch-action` helper (`app_impl.lisp`):** A central function to `(eval)` actions retrieved from the event/key maps, also handling macro recording.

### 2. UI Structure (`apps/edit/widgets.inc`)

The Editor's UI is built using ChrysaLisp's UI builder macros:

* The root is `*window*` (a `ui-window`).

* It contains a main `Flow` layout (`*edit_flow*`) that holds the primary editing area and status bar.

* **Toolbars:** Several `ui-tool-bar` widgets (`*main_toolbar*`, `*macro_toolbar*`, `*buffer_toolbar*`, `*find_toolbar*`, `*replace_toolbar*`) house `ui-buttons` for various actions.

* **File Navigation:** A `ui-flow` contains a `ui-stack` named `*tab_flow*`. This stack has two main tabs:

    * "Open Files": A `ui-scroll` (`*open_tree_scroll*`) containing a `ui-tree` (`*open_tree*`) to display currently open files.

    * "Project": A `ui-scroll` (`*file_tree_scroll*`) containing a `ui-tree` (`*file_tree*`) for browsing the file system.

* **Editing Area:** The `*edit_flow*` contains:

    * `*vdu_lines*`: A `Vdu` widget to display line numbers.

    * `*edit*`: An instance of `Editor-edit` (which is an `Edit` widget), the main text editing canvas. This itself is wrapped in a `Flow` to accommodate scrollbars.

    * `*yslider*` and `*xslider*`: `ui-slider` widgets for vertical and horizontal scrolling of the `*edit*` widget.

* **Status Bar:** A `ui-flow` at the bottom displays information like cursor position (`*cx*`, `*cy*`), selection size (`*sw*`, `*sh*`), and find count (`*fc*`) using `ui-text` widgets.

* **Find/Replace:** `ui-textfield` widgets (`*find_text*`, `*replace_text*`) for inputting search and replacement strings.

The UI definition extensively uses the `(:connect event_id)` property on buttons to link them to specific events handled in `actions.inc`.

### 3. Event Handling and Dispatch

The event loop in `apps/edit/app_impl.lisp` (`main` function) uses `(mail-select)` on a list of mailboxes (`*select*`).

* **Main GUI Events (`+select_main`):**

    * If an event's `+ev_msg_target_id` maps to an action in `*event_map*`, `(dispatch-action)` is called.

    * Keyboard events (`+ev_type_key_down`) not targeting a `Textfield` are processed against `*key_map*`, `*key_map_shift*`, or `*key_map_control*` based on modifier keys. The corresponding action is then dispatched.

    * Unhandled events are passed to `(. *window* :event msg)`.

* **Tooltip Events (`+select_tip`):** Handled by calling `(. view :show_tip)` on the target view.

* **`dispatch-action`:** This helper function in `app_impl.lisp` first checks if the action should be recorded (if `*macro_record*` is true and the function is in `*recorded_actions*`). Then, it `(eval)`s the action list (e.g., `(action-save-all)` or `(action-insert "text")`). It includes a `(catch ...)` to prevent errors in actions from crashing the editor.

### 4. Multi-Buffer Management

The Editor manages multiple open files and scratch buffers:

* **`*meta_map*` (`app_impl.lisp`):** An `Emap` storing metadata for all known files (open or not). The top-level keys are `:files` (another `Fmap` mapping file paths to their specific metadata) and other global editor settings.

* **Per-File Metadata:** For each file path in `*meta_map*` -> `:files`, another `Emap` stores:

    * `:buffer`: The actual `Buffer` instance for the file's content.

    * `:cx`, `:cy`: Last cursor position.

    * `:ax`, `:ay`: Last anchor position.

    * `:sx`, `:sy`: Last scroll position.

* **`*open_files*` (`app_impl.lisp`):** A Lisp `list` containing the paths of files currently "open" (i.e., having a buffer loaded and potentially displayed in the "Open Files" tree).

* **`*current_file*` (`app_impl.lisp`):** The path of the file currently active in the `*edit*` widget. Can be `:nil` for a scratch buffer.

* **`populate-buffer` function (`app_impl.lisp`):**

    * Takes a `file` path. If a metadata entry for this file doesn't exist in `*meta_map*`, it creates one.

    * If the metadata entry doesn't yet have a `:buffer` associated, it creates a new `Buffer` instance (passing `*syntax*` engine), loads the file content using `(. buffer :file_load file)`, and populates the `dictionary`.

* **`populate-vdu` function (`app_impl.lisp`):**

    * Switches the `*edit*` widget to display the `Buffer` associated with the given `file`.

    * Updates `*current_file*`.

    * Restores cursor, anchor, and scroll positions from the file's metadata.

    * Calls `(refresh)` to update the display.

* **Tree Views:** `*open_tree*` and `*file_tree*` are populated based on `*open_files*` and file system scans, respectively. Clicking items in these trees calls `action-open-leaf-action` or `action-file-leaf-action` which use `populate-vdu` or `open-file` (which itself calls `populate-vdu`).

### 5. Text Editing Core

* **`Editor-edit` (`gui/edit/lisp.inc`):** This class inherits from the generic `Edit` widget. It primarily sets default font and color properties.

* **`Edit` Widget (`gui/edit/lisp.inc`):**

    * It doesn't directly store text but holds a reference to a `Buffer` object via its `:buffer` property (set by `(:set_buffer ...)`).

    * Most text manipulation methods on `Edit` (like `:insert`, `:backspace`, `:cut`, `:copy`, `:paste`, `:break`, `:tab`, etc.) are wrappers that call the corresponding methods on its current `Buffer`.

    * `(:underlay_paper)` and `(:underlay_ink)` methods are responsible for preparing the visual overlays for selections and bracket matching, respectively, by loading appropriate character attributes into its child `Vdu` widgets (`:vdu_paper`, `:vdu_ink`).

* **`Buffer` Class (`lib/text/buffer.inc`):**

    * Manages text as a list of strings (`:buffer_line`).

    * Tracks modified status (`:modified`).

    * **Undo/Redo:**

        * `(:push_undo record ...)`: Pushes records onto `:undo_stack`. Records are typically lists like `(:cursor x y)`, `(:insert x y "text")`, `(:delete x y "text")`, or `(:mark mark_id)`.

        * `(:undo)` and `(:redo)` methods pop records from `:undo_stack` or `:redo_stack` and reverse/reapply the operations. The `(undoable ...)` macro (`apps/edit/utils.inc`) is used to wrap editing actions, automatically pushing cursor state and a unique mark before and after the action, allowing a set of operations to be undone/redone as a single logical step.

    * Cursor and selection logic is handled by methods like `(:constrain x y)`, `(:left)`, `(:right)`, etc.

### 6. Syntax Highlighting

* A global `*syntax*` object (instance of `Syntax`) is created in `app_impl.lisp`.

* When a `Buffer` is created by `populate-buffer`, it's initialized with this `*syntax*` engine.

* **`Syntax` Class (`lib/text/syntax.inc`):**

    * `(:colorise str)`: This method takes a line of text. It tokenizes the line based on character types (symbol, number, string, comment, etc.) and its current internal state (e.g., inside a multi-line comment or string). It then consults its internal keyword maps and color properties (e.g., `:ink_keyword1`, `:ink_strings`) to return an `array` of character attributes. Each element in this array is a `long` where the lower bytes are the character code and higher bytes encode the color.

    * The `Vdu` widget can directly render such an array of attributed characters.

* **`Buffer` Integration:**

    * `Buffer` stores the syntax-highlighted version of lines in `:buffer_syntax`.

    * `(:vdu_load vdu ...)` method in `Buffer`: If a `syntax_engine` is present (as it is for the Editor's buffers), it calls `(build-syntax this end_state)` which iterates through lines, calls `:colorise` on its `syntax_engine`, and stores the result. This colored data is then passed to the `vdu`'s `:load` method.

    * `build-syntax` in `gui/text/buffer.inc` manages caching and re-colorizing lines only when necessary (based on `:dirty_flags`).

### 7. Macro Recording System (`apps/edit/macros.inc`)

* **`*macro_record*` flag (`app_impl.lisp`):** Toggled by `action-macro-record`.

* **`+macro_map*` (`macros.inc`):** An `Lmap` storing recorded macros. Slot `0` is for the current recording, slots `1-9` for saved macros.

* **`macro-record` function (`macros.inc`):**

    * Called by `dispatch-action` if `*macro_record*` is true.

    * It checks if the dispatched function is in `*recorded_actions*` (a list of editor commands suitable for recording).

    * If so, it appends the `action` (which is a list like `(action-insert "text")`) to the list in `+macro_map` at slot `0`. It attempts to coalesce consecutive `:insert` actions.

* **`macro-playback` function (`macros.inc`):**

    * Takes a list of `actions`.

    * Iterates through the actions, `(eval)`ing each one. Special handling exists for find actions to stop playback if a find fails.

* **`action-macro-save-N`, `action-macro-playback-N` (`macros.inc`):**

    * Save the current recording (slot `0`) to slot `N`.

    * Playback the macro from slot `N`.

    * `macro-encode` and `macro-decode` are used to convert action lists to/from a more compact storable format by replacing function objects with their index in `*recorded_actions*`. This is used when saving macros to the project state.

* `action-macro-to-eof` and `action-macro-global` provide ways to repeatedly apply a macro.

### 8. Type-Ahead/Auto-Completion

* **`dictionary` object (`app_impl.lisp`):** An instance of `Dictionary` (`lib/text/dictionary.inc`).

* **Population:**

    * Initialized with keywords from the `*syntax*` engine's keyword map.

    * Words from files listed in `+dictionaries` (e.g., "lib/text/english.txt") are added.

    * When a file is loaded into a buffer (`populate-buffer`), its words (longer than `+min_word_size`) are added to the `dictionary` via `populate-dictionary`.

* **`action-tab` (`apps/edit/edit.inc`):**

    * When Tab is pressed and not in a selection, it attempts auto-completion.

    * It calls `url-ext`, which internally calls `(. dictionary :find_matches_case prefix)` to get a list of matching words.

    * If matches are found, it tries to complete the current word or, if ambiguous, potentially shows a list of matches (though `show-matches` functionality seems to be more for find highlighting than direct autocompletion popup in the provided snippets). The primary effect is completing to the longest common prefix or a single match.

* **`show-matches` (`apps/edit/search.inc`, but further detailed in `app_impl.lisp`'s context):**

    * This function seems geared towards displaying multiple suggestions in a popup `Window` (`match_window`). While triggered by `action-insert` after typing, it uses the `Dictionary` for suggestions if the current word fragment is long enough.

    * The `select-match` function allows navigating these suggestions.

### 9. Project State Management (`apps/edit/state.inc`)

* **`+state_filename`:** `"editor.tre"` stored in `*env_home*`.

* **`state-save` function:**

    * Saves current find/replace text, `*whole_words*`, `*regexp*` settings into `*meta_map*`.

    * Saves recorded macros (slots 0-9) from `+macro_map` into `*meta_map*` after encoding them using `macro-encode`.

    * Saves `*meta_map*` to the state file using `tree-save`. `tree-save` serializes Lisp collections (like `Emap`, `Lmap`, `list`) into a readable text format. The `:buffer` itself is filtered out by `key_filters` to avoid saving entire file contents in the state file.

* **`state-load` function:**

    * Loads the `*meta_map*` from the state file using `tree-load`.

    * Restores settings and macros (using `macro-decode`).

    * Iterates through the `:files` entry in `*meta_map*`:

        * Checks if each file still exists using `(age file)`.

        * If it exists, `populate-buffer` is called to load/recreate its buffer and associate it with the editor, restoring cursor/scroll positions.

        * `*open_files*` is reconstructed.

    * The last `*current_file*` is returned to be made active.

### 10. Extendability

* **Action Handlers:** New editor commands can be added by defining new Lisp functions and mapping them to event IDs or key codes in `actions.inc` or its included files.

* **Key Bindings:** Easily modified by changing the `Fmap`s in `actions.inc`.

* **Syntax Highlighting:** The `Syntax` class could be subclassed or its keyword maps extended to support new languages or refine existing ones.

* **UI:** The UI can be modified by changing `apps/edit/widgets.inc`. New toolbars, buttons, or views can be integrated.

* **Programmatic Control:** The `Buffer` and `Editor-edit` classes expose a comprehensive set of methods, allowing other Lisp code (e.g., plugins, if a system for them were developed) to interact with and control the editor.

## Conclusion

The ChrysaLisp Editor is a feature-rich application that effectively demonstrates the capabilities of the ChrysaLisp GUI system and its Lisp-centric design. Its use of `Buffer` objects for text management, the `Syntax` engine for highlighting, a flexible event dispatch system using `Fmap`s, and a Lisp-native project state persistence mechanism make it a powerful tool within the ChrysaLisp ecosystem. The modularity of its action handlers and the data-driven nature of its UI and behavior provide significant avenues for customization and extension.

## Appendix: Editor Actions and Key Bindings

This appendix provides a comprehensive reference for all actions available
within the ChrysaLisp Editor. Each entry lists the action's purpose, its
corresponding UI event name (useful for custom UI development), and its default
keyboard shortcut(s).

**Note on Key Bindings:**

*   `Ctrl + Key`: Refers to holding the Control key (or Command key on macOS)
    and pressing the specified key.

*   `Shift + Key`: Refers to holding the Shift key while pressing another key.

---

### Window & File Management

**Close Window**

Closes the editor window and terminates the application.

*   **UI Event:** `+event_close`

*   **Key Binding:** None (Typically bound to the window's close button)

**Minimize Window**

Minimizes the editor window.

*   **UI Event:** `+event_min`

*   **Key Binding:** `Ctrl + -`

**Maximize Window**

Maximizes or resizes the editor window to a larger preset size.

*   **UI Event:** `+event_max`

*   **Key Binding:** `Ctrl + =`

**New File**

Creates a new file with the name entered in the "new file" text field.

*   **UI Event:** `+event_new`

*   **Key Binding:** None

**Save Current File**

Saves the currently active buffer to its associated file.

*   **UI Event:** `+event_save`

*   **Key Binding:** `Ctrl + s`

**Save All Files**

Saves all open and modified buffers to their respective files.

*   **UI Event:** `+event_save_all`

*   **Key Binding:** `Ctrl + S` (Shift + s)

**Load Selected Text as Files**

Takes the selected text in the current buffer, interprets each line as a file
path, and opens all valid files.

*   **UI Event:** `+event_load_selected`

*   **Key Binding:** None

**Next Buffer**

Switches the view to the next file in the list of open files.

*   **UI Event:** `+event_next`

*   **Key Binding:** `Ctrl + n`

**Previous Buffer**

Switches the view to the previous file in the list of open files.

*   **UI Event:** `+event_prev`

*   **Key Binding:** `Ctrl + N` (Shift + n)

**Close Current Buffer**

Saves and closes the current file, removing it from the open files list.

*   **UI Event:** `+event_close_buffer`

*   **Key Binding:** None

**Close All Buffers**

Saves and closes all open files.

*   **UI Event:** `+event_close_all`

*   **Key Binding:** None

**Open Scratchpad**

Switches to a temporary, unnamed buffer that will not be saved.

*   **UI Event:** `+event_scratch`

*   **Key Binding:** None

**Load Dependencies**

Analyzes the current file for `(import)` and `(include)` forms and loads all
directly referenced files.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + e`

**Load All Dependencies**

Recursively analyzes the current file and all its dependencies, loading the
entire dependency tree.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + E` (Shift + e)

### Cursor Movement

**Move Cursor Left/Right/Up/Down**

Moves the cursor one position. If a selection is active, this collapses the
selection to the start (Left/Up) or end (Right/Down) of the selection range
before moving.

*   **UI Event:** None

*   **Key Binding:** `Left Arrow`, `Right Arrow`, `Up Arrow`, `Down Arrow`

**Move to Beginning of Line**

Moves the cursor to the beginning of the current line (or to the end of the
prompt in the terminal).

*   **UI Event:** None

*   **Key Binding:** `Home`

**Move to End of Line**

Moves the cursor to the end of the current line.

*   **UI Event:** None

*   **Key Binding:** `End`

**Move to Top of File**

Moves the cursor to the beginning of the first line of the file.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + Up Arrow`

**Move to Bottom of File**

Moves the cursor to the end of the last line of the file.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + Down Arrow`

**Move to Matching Bracket (Left)**

Finds the matching opening bracket `(` for the bracket `)` under or near the
cursor and moves the cursor to it.

*   **UI Event:** `+event_bracket_left`

*   **Key Binding:** `Ctrl + [` or `Ctrl + 9`

**Move to Matching Bracket (Right)**

Finds the matching closing bracket `)` for the bracket `(` under or near the
cursor and moves the cursor to it.

*   **UI Event:** `+event_bracket_right`

*   **Key Binding:** `Ctrl + ]` or `Ctrl + 0`

**Push Cursor Position to Stack**

Saves the current file and cursor position onto a navigation stack.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + d`

**Pop Cursor Position from Stack**

Loads the most recently saved file and cursor position from the navigation
stack.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + D` (Shift + d) or `Ctrl + J` (Shift + j)

### Selection

**Select with Arrow Keys**

Extends the current selection in the direction of the arrow key.

*   **UI Event:** None

*   **Key Binding:** `Shift + Left/Right/Up/Down Arrow`

**Select to Beginning/End of Line**

Extends the selection from the current cursor position to the beginning or end
of the line.

*   **UI Event:** None

*   **Key Binding:** `Shift + Home`, `Shift + End`

**Select All**

Selects the entire content of the current buffer.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + a`

**Select Word**

Selects the word currently under the cursor.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + w`

**Select Line**

Selects the entire current line, including the newline character.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + l`

**Select Paragraph**

Selects the current paragraph (a block of text surrounded by blank lines).

*   **UI Event:** `+event_paragraph`

*   **Key Binding:** `Ctrl + p`

**Select Block/Form**

Selects the S-expression (or "form") under the cursor, from its opening to its
closing parenthesis.

*   **UI Event:** `+event_block`

*   **Key Binding:** `Ctrl + b`

### Editing & Clipboard

**Insert Character**

Inserts a typed character at the cursor position, replacing any active
selection.

*   **UI Event:** None

*   **Key Binding:** Any printable character key.

**Delete Character Before Cursor**

Deletes the character to the left of the cursor, or deletes the current
selection.

*   **UI Event:** None

*   **Key Binding:** `Backspace`

**Delete Character After Cursor**

Deletes the character to the right of the cursor, or deletes the current
selection.

*   **UI Event:** None

*   **Key Binding:** `Delete`

**Insert Newline**

Inserts a newline character. If the line has leading indentation, the new line
will be auto-indented to match.

*   **UI Event:** None

*   **Key Binding:** `Enter` or `Return`

**Insert Tab / Auto-complete**

In the terminal, attempts to auto-complete the current word as a file path. In
the editor, indents the current line or selection.

*   **UI Event:** None

*   **Key Binding:** `Tab`

**Cut**

Cuts the current selection to the system clipboard.

*   **UI Event:** `+event_cut`

*   **Key Binding:** `Ctrl + x`

**Copy**

Copies the current selection to the system clipboard.

*   **UI Event:** `+event_copy`

*   **Key Binding:** `Ctrl + c`

**Paste**

Pastes the content of the system clipboard at the cursor.

*   **UI Event:** `+event_paste`

*   **Key Binding:** `Ctrl + v`

**Cut Word / Line / Paragraph / Block**

Selects and then cuts the respective text element.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + W`, `Ctrl + L`, `Ctrl + P`, `Ctrl + B`

**Copy Word / Line / Paragraph / Block**

Selects and then copies the respective text element.

*   **UI Event:** None

*   **Key Binding:** (None for word), (None for line), (None for paragraph),
    (None for block)

*   *Note: These actions exist but are not bound by default in `actions.inc`.
    Their `cut` equivalents are.*

### Block & Text Manipulation

**Outdent Selection (Left Tab)**

Removes one level of indentation from the selected lines.

*   **UI Event:** `+event_tab_left`

*   **Key Binding:** `Shift + Tab`

**Indent Selection (Right Tab)**

Adds one level of indentation to the selected lines.

*   **UI Event:** `+event_tab_right`

*   **Key Binding:** (Editor) `Tab` when multiple lines are selected.

**Comment/Uncomment Selection**

Toggles Lisp-style `;; ` block comments on the selected lines.

*   **UI Event:** `+event_comment`

*   **Key Binding:** `Ctrl + /`

**Convert to Uppercase**

Converts the selected text to uppercase.

*   **UI Event:** `+event_toupper`

*   **Key Binding:** `Ctrl + u`

**Convert to Lowercase**

Converts the selected text to lowercase.

*   **UI Event:** `+event_tolower`

*   **Key Binding:** `Ctrl + U` (Shift + u)

**Sort Lines**

Sorts the selected lines alphabetically.

*   **UI Event:** `+event_sort`

*   **Key Binding:** `Ctrl + o`

**Unique Lines**

Removes duplicate lines from the current selection.

*   **UI Event:** `+event_unique`

*   **Key Binding:** `Ctrl + O` (Shift + o)

**Invert/Reverse Lines**

Reverses the order of the selected lines.

*   **UI Event:** `+event_invert`

*   **Key Binding:** `Ctrl + i`

**Reflow Paragraph**

Re-wraps the selected lines of text to fit within the editor's configured line
width.

*   **UI Event:** `+event_reflow`

*   **Key Binding:** `Ctrl + q`

**Split into Words**

Takes the selected text and puts each space-separated word onto its own new
line.

*   **UI Event:** `+event_split`

*   **Key Binding:** `Ctrl + Q` (Shift + q)

### Search and Replace

**Set Find Text from Selection**

Copies the current selection into the "find" text field.

*   **UI Event:** None

*   **Key Binding:** `Ctrl + f`

**Find Next Occurrence**

Finds the next occurrence of the text in the "find" field, starting from the
cursor's current position.

*   **UI Event:** `+event_find_down`

*   **Key Binding:** `Enter` (when focus is in the "find" text field)

**Find Previous Occurrence**

Finds the previous occurrence of the text in the "find" field.

*   **UI Event:** `+event_find_up`

*   **Key Binding:** None

**Find in All Open Files (Global Find)**

Searches for the pattern in all open files and displays a list of matching
files.

*   **UI Event:** `+event_global`

*   **Key Binding:** None

**Toggle Whole Words Search**

Toggles whether the search matches only whole words.

*   **UI Event:** `+event_whole_words`

*   **Key Binding:** None

**Toggle Regular Expression Search**

Toggles whether the search pattern is treated as a regular expression.

*   **UI Event:** `+event_regexp`

*   **Key Binding:** None

**Toggle Region Search**

Toggles whether the search is confined to the currently selected lines.

*   **UI Event:** `+event_region`

*   **Key Binding:** `Ctrl + A` (Shift + a)

**Replace**

Replaces the current selection (if it matches the find text) with the text in
the "replace" field, then finds the next occurrence.

*   **UI Event:** `+event_replace`

*   **Key Binding:** `Enter` (when focus is in the "replace" text field)

**Replace All**

Replaces all occurrences of the find text with the replace text within the
current buffer (or selection if region is active).

*   **UI Event:** `+event_replace_all`

*   **Key Binding:** None

**Replace in All Open Files (Global Replace)**

Performs a replace-all operation across all currently open files.

*   **UI Event:** `+event_replace_global`

*   **Key Binding:** None

### Undo & Redo

**Undo**

Reverts the last editing action.

*   **UI Event:** `+event_undo`

*   **Key Binding:** `Ctrl + z`

**Redo**

Re-applies the last undone action.

*   **UI Event:** `+event_redo`

*   **Key Binding:** `Ctrl + Z` (Shift + z)

**Rewind to Last Save**

Reverts all changes in the buffer back to the state it was in when it was last saved.

*   **UI Event:** `+event_rewind`

*   **Key Binding:** None

**Global Undo/Redo**

Performs an undo or redo action, but steps across all open files chronologically
instead of just the current one.

*   **UI Event:** +event_gundo, +event_gredo

*   **Key Binding:** None

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
apps/viewer/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Viewer, located in `apps/viewer/`, is a powerful read-only text
and code viewer. It serves as a complementary tool to the Editor, sharing many
of its core components while specializing in navigation, search, and file system
browsing. This study examines its architecture and implementation, highlighting
how it reuses and adapts core libraries for a different purpose.

### 1. Core Architecture and Components

The Viewer's architecture is a testament to the component-based design of the
ChrysaLisp GUI system. It intentionally reuses the same core **Model** and
**View** components as the Editor, but provides a distinct **Controller** layer
to enforce read-only behavior and offer specialized features.

*   **Model (Text Management - Reused from Editor):**

    * **`Buffer` Class (`lib/text/buffer.inc`):** The Viewer uses the exact same
        `Buffer` class as the Editor for all text management. Each file is
        loaded into a `Buffer` instance. Crucially, the Viewer's application
        logic only calls the *read* methods of the buffer (e.g.,
        `:get_text_line`, `:get_size`, `:find`) and never the destructive ones
        (like `:insert`, `:delete`).

    * **`Syntax` Class (`lib/text/syntax.inc`):** Syntax highlighting is fully
        supported. An instance of `Syntax` (`*syntax*` in `app.lisp`) is passed
        to each `Buffer` upon creation, providing the same rich code coloring as
        the Editor.

*   **View (User Interface):**

    * **`*window*` (`apps/viewer/widgets.inc`):** The main `Window` widget that
        serves as the root of the UI tree.

    * **`Viewer-edit` Class (`apps/viewer/ui.inc`):** This is a custom class
        that **inherits from `Edit`**. This is a key design choice. The Viewer
        doesn't need a new rendering widget; it uses the powerful `Edit` widget
        but overrides its mouse event handlers to implement read-only selection
        behavior instead of text modification. The primary instance is named
        `*edit*`.

    * **`Edit` / `Vdu` Widgets (`gui/edit/lisp.inc`, `gui/vdu/lisp.inc`):** The
        underlying rendering is handled by the same components as the Editor.
        The `*edit*` widget uses child `Vdu` instances for displaying text,
        selections, and highlights.

    * **File Browser (`*file_tree*`):** A prominent `Tree` widget for navigating
        the project's file system, which is a central feature of the Viewer.

*   **Controller (Application Logic and Event Handling):**

    * **`apps/viewer/app.lisp`:** This is the main application file, containing
        the `main` event loop and the functions that orchestrate the Viewer's
        behavior.

    * **`apps/viewer/actions.inc` (and includes):** Defines the `Fmap`s
        (`*event_map*`, `*key_map*`, etc.) that map GUI events and keyboard
        shortcuts to viewer-specific handler functions. The set of actions is
        smaller and focused on navigation and search, omitting all editing
        functions.

    * **`dispatch-action` helper (`app_impl.lisp`):** Just like the Editor, this
        function evaluates actions retrieved from the event maps.

### 2. UI Structure (`apps/viewer/widgets.inc`)

The Viewer's UI is simpler and more focused on browsing than the Editor's.

*   **Main Layout:** A `ui-flow` with a `:flow_right_fill` flag splits the main
    area into two panes: the file browser on the left and the content viewer on
    the right. This is in contrast to the Editor's tab-like system for open
    files.

*   **Toolbars:**

    * `*main_toolbar*`: Provides essential read-only actions like "Copy",
        "Select Paragraph", "Select Form".

    * `*find_toolbar*`: Contains widgets for search functionality, including
        buttons for "Whole Words", "Regexp", and "Find Down/Up".

*   **File Browser:** A `ui-scroll` (`*file_tree_scroll*`) contains a `ui-tree` (`*file_tree*`). This is populated by scanning the "docs" and project root directories for files with supported extensions (`+file_types`).

*   **Content Viewer:**

    * `*edit_flow*`: The main viewing area, which contains the `*edit*` widget
        (the instance of `Viewer-edit`).

    * `*vdu_lines*`: Displays line numbers, similar to the Editor.

    * `*xslider*`, `*yslider*`: Standard sliders for scrolling the content.

*   **Status Bar:** A `ui-flow` at the bottom displays cursor position (`*cx*`,
    `*cy*`), selection size (`*sw*`, `*sh*`), and the number of matches found
    (`*fc*`).

### 3. Event Handling and Dispatch

The event handling pattern in `apps/viewer/app.lisp` mirrors the Editor's. The
`main` function's event loop waits on `(mail-select)` and dispatches actions
based on the source of the message (GUI, timer, etc.).

*   GUI events are mapped via `*event_map*` and the various `*key_map*`s to
    their handler functions (e.g., `action-find-down`, `action-select-word`).

*   The Viewer defines its own set of actions in `apps/viewer/actions.inc` and
    its includes. These actions are tailored for viewing:

    * **Navigation:** `action-left`, `action-right`, `action-home`, `action-end`
        all map to the corresponding selection-aware methods in the
        `Viewer-edit` widget.

    * **Search:** `action-find-down`, `action-find-up`.

    * **Selection:** `action-select-word`, `action-select-line`.

    * **Clipboard:** `action-copy`. Note the conspicuous absence of `action-cut`
        and `action-paste`.

### 4. File Management and State

The Viewer manages files and state in a way that is similar to, but simpler
than, the Editor.

*   **`*meta_map*` (`app.lisp`):** It uses the same `Emap`-based structure to
    store metadata. The `state-load` and `state-save` functions persist this map
    to `"viewer_state.tre"`.

*   **No Explicit `*open_files*` List:** Unlike the Editor, the Viewer does not
    maintain a distinct list of "open" files. Its file navigation is based
    directly on the `*file_tree*` which reflects the filesystem. A file's buffer
    and state are loaded into the `*meta_map*` the first time it is opened via
    `populate-buffer`.

*   **`*current_file*`:** Tracks the currently displayed file.

*   **`populate-vdu` (`app.lisp`):** This function is the core of switching
    between files. It retrieves the appropriate `Buffer` from the `*meta_map*`,
    assigns it to the `*edit*` widget, restores scroll/cursor positions, and
    calls `(refresh)`.

### 5. Read-Only Interaction: The `Viewer-edit` Class

This is the most significant point of specialization for the Viewer. It achieves
its read-only nature not by using a different rendering widget, but by
subclassing `Edit` and overriding its behavior.

*   **`Viewer-edit` (`apps/viewer/ui.inc`):** Inherits from the `Edit` class.

*   **Overridden Mouse Handlers:**

    * `(:mouse_down event)`: Instead of preparing for text insertion, this
        method simply sets the cursor position and clears any existing selection
        by setting the anchor to the same spot.

    * `(:mouse_move event)`: If the mouse button is held down, this method
        updates the cursor position, creating a text selection.

    * `(:mouse_up event)`: Handles multi-click selections (double-click for
        word, triple for line, etc.) by calling helper functions like
        `action-select-word`.

*   This approach demonstrates a powerful object-oriented pattern: inheriting a
    component with rich rendering capabilities (`Edit`) and overriding only the
    interaction logic to suit a different use case, maximizing code reuse.

### 6. Search and Integration (`apps/viewer/search.inc`)

The Viewer's search functionality is particularly powerful and showcases
ChrysaLisp's system integration.

*   **Local Find:** `action-find-down` and `action-find-up` search within the
    currently displayed buffer. This uses the `(. buffer :find ...)` method,
    which is the same as the Editor's.

*   **Global Find and File Tree Highlighting:** The `refresh-matches` function
    provides a "find-as-you-type" global search.

    * It constructs a command-line string using `grep`.

    * It executes this command on all relevant files
        (`files-all "docs" '(".md")`) using `(pipe-farm ...)` from
        `lib/task/farm.inc`.

    * `pipe-farm` distributes the `grep` tasks across available nodes in the
        cluster.

    * The list of files returned by `grep` is then used to highlight entries in
        the `*file_tree*` widget using `(. *file_tree* :highlight file)`.

    * `action-next-doc` and `action-prev-doc` then allow the user to cycle
        through *only the highlighted files*, effectively navigating the search
        results across the entire project.

## Conclusion

The ChrysaLisp Viewer is an excellent example of software reuse and
component-based design. By leveraging the same `Buffer` model and `Edit`
rendering widget as the Editor, it avoids reinventing the wheel for complex
tasks like text storage, syntax highlighting, and selection rendering. It
achieves its distinct, read-only purpose by providing a simplified UI and a
specialized `Viewer-edit` subclass that overrides interactive behaviors. Its
standout feature is the `grep`-based global search, which integrates seamlessly
with the file tree to provide a fast and powerful code and documentation
browsing experience that leverages the distributed nature of the OS.
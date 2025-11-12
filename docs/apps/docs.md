# Docs

The `Docs` application is the Chrysalisp documentation viewer. It is used to
display tutorials as well as reference material auto generated from the source
code files.

Section handlers are loaded dynamically as required and given responsibility
for the embedding of content. Content ranges from wrapped text to images and
live Lisp code snippets, including embedding the entire UI of applications. The
mechanization for the section handlers is explained in the `event_dispatch.md`
document.

The Terminal command app `make docs` is used to scan the source files and
create the reference documentation files.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/docs/widgets.inc *window* 512 512
```

## Implementation Study

### 1. Introduction

The ChrysaLisp Docs application serves as the primary interface for users to
browse and read documentation, including tutorials and auto-generated reference
material. A key architectural feature of the Docs application is its ability to
dynamically load "section handlers" to render diverse content types embedded
within documentation files. This study examines the implementation of the Docs
application, with a particular focus on this dynamic section handling mechanism.

### 2. Core Functionality

The Docs application presents a user interface typically consisting of:

* A file tree (`*file_selector*`) for navigating documentation files (usually
  `.md` files within the `docs/` directory).

* A content display area (`*page_scroll*`) where the selected document's content
  is rendered.

* A search/navigation bar for finding text within documents and moving between
  documents.

When a user selects a document, the Docs application parses its content line by
line. It identifies special markup tags that denote different types of content
sections (e.g., plain text, images, code blocks, embedded UI widgets). For each
section type, it invokes a corresponding handler responsible for rendering that
content.

### 3. Key Components

* **Main Application File:** `apps/docs/app.lisp`

* **UI Definition:** `apps/docs/widgets.inc`

* **Section Handler Modules:** Located in `apps/docs/`, e.g., `text.inc`,
  `image.inc`, `code.inc`, `vdu.inc`, `widget.inc`.

### 4. Dynamic Section Handling

This is the most distinctive feature of the Docs application's implementation.
It allows for extensible content rendering without modifying the core
application logic.

#### 4.1. Markup Convention

Documentation files use a simple markup convention to define special sections. A
section of a specific `<type>` is typically denoted by:

```
;   ```<type>
;   ... content for this type ...
;   ```
```

For example:

```
;   ```image
;   apps/images/data/logo.cpm
;   ```
```

Or:

```
;   ```widget
;   apps/my_app/widgets.inc *my_widget_instance* 200 150
;   ```
```

The default state if no tag is active is considered `:text`.

#### 4.2. Handler Discovery and Dynamic Loading

The core mechanism for dynamic section handling resides in the `populate-page`
function within `apps/docs/app.lisp`. This function processes the selected
document line by line, maintaining a `state` variable (a symbol like `:text`,
`:image`, `:code`, etc.) that indicates the current section type being
processed.

* **`handlers` Emap:** An `Emap` instance, named `handlers`, is used as a cache
  for loaded section handler functions. It maps the state symbol (e.g., `:image`)
  to the actual Lisp handler function.

* **`handler-func` Helper:** A local helper function, `handler-func`, is
  responsible for retrieving or loading the appropriate handler:

    1. It first checks if a handler for the current `state` already exists in
       the `handlers` `Emap`.

    2. If not found, it dynamically constructs a module path: `(cat "apps/docs/"
       (rest state) ".inc")`. For example, if `state` is `:image`, the path
       becomes `"apps/docs/image.inc"`.

    3. It then uses `(repl (file-stream module) module)` to load and evaluate
       this module. The `repl` function, in this context, effectively imports
       the module's definitions into a dedicated environment, `*handler_env*`,
       which is a child of the Docs application's main environment.

    4. The loaded module is expected to define and export a function named
       `handler`.

    5. This exported `handler` function is then retrieved from `*handler_env*`
       and stored in the `handlers` `Emap`, keyed by the `state` symbol.

* **Invocation:** Once the handler function is obtained (either from the cache
  or by dynamic loading), it is called with the current `state`, the `page`
  widget (where content should be added), and the `current-line` from the
  document.

```vdu
; Snippet from apps/docs/app.lisp (illustrative)
(defq handlers (Emap)) ; Emap to cache handlers
(defun handler-func (state)
    (unless (defq handler (. handlers :find state))
        (defq module (cat "apps/docs/" (rest state) ".inc"))
        (repl (file-stream module) module) ; Dynamically loads the module
        ; Assuming the module exports 'handler' into *handler_env*
        (. handlers :insert state (setq handler (get 'handler *handler_env*))))
    handler)

(defun populate-page (file)
    ; ...
    (defq state :text)
    (lines! (lambda (line)
            ; ... logic to update 'state' based on "```<type>" tags ...
            (catch (setq state ((handler-func state) state page line))
                (progn (prin _) (print) (setq state :text) :t)))
        (file-stream file))
    ; ...
)
```

#### 4.3. Handler Interface and Contract

Each section handler module (e.g., `apps/docs/text.inc`, `apps/docs/image.inc`)
must define and export a function named `handler`.

* **Signature:** `(handler current-state page-widget current-line) -> new-state`

* **Responsibilities:**

    * `page-widget`: The parent UI widget (typically a `Flow` layout named
      `page` in `populate-page`) to which the handler should add its rendered
      content.

    * `current-line`: The current line of text from the document being
      processed.

    * The handler parses `current-line`.

    * If `current-line` is the closing tag for its section (e.g., "```"), it
      should finalize any UI elements it has been constructing, add them to
      `page-widget`, and return the new state, which is typically `:text`.

    * If `current-line` is content for its section type, it processes it (e.g.,
      accumulates text, loads an image) and updates its UI elements. It then
      returns the `current-state` to indicate it's still handling this section
      type.

    * Handlers are responsible for creating and managing their own UI widgets
      (e.g., `Text`, `Vdu`, `Canvas`, or even custom application widgets).

#### 4.4. Example Handlers

* **`apps/docs/text.inc` (Handler for `:text` state):**

    * Parses the `line` for inline styling like `*italic*`, `**bold**`, or
      ``code` ``.

    * Creates `Text` widgets for each segment, applying appropriate fonts (e.g.,
      `font_italic`, `font_bold`, `font_term` obtained from the `page` widget's
      properties).

    * Adds these `Text` widgets to a new `Flow` widget representing the current
      line, which is then added to the main `page` widget.

    * Collects created `Text` widgets into `*search_widgets*` for find
      functionality.

* **`apps/docs/image.inc` (Handler for `:image` state):**

    * If `line` is not "```", it interprets `line` as an image file path.

    * Loads the image using `(canvas-load line +load_flag_shared)`.

    * Adds the resulting `canvas` widget to the `page` widget.

    * Returns `:text` when "```" is encountered.

* **`apps/docs/vdu.inc` (Handler for `:vdu` state, also used by `:code`):**

    * If `line` is not "```", it accumulates the line (after processing tabs)
      into a local `lines` list.

    * When "```" is encountered:

        * Creates a `Vdu` widget.

        * Sets its font (e.g., `font_term_small` from `page` properties).

        * Calculates `vdu_width` and `vdu_height` based on the accumulated
          lines.

        * Loads the lines into the `Vdu` widget using `(. vdu :load lines ...)`.

        * Adds the `Vdu` widget to a `Backdrop` (for background color) and then
          to the `page`.

    * Returns `:text`.

* **`apps/docs/widget.inc` (Handler for `:widget` state):**

    * This is a particularly powerful handler demonstrating ChrysaLisp's dynamic
      capabilities.

    * It parses `line` to extract a Lisp file path, a widget symbol name, and
      optional dimensions.

    * It uses `(import-from file (list widget-symbol))` to load only the
      specified widget's definition from the Lisp file into the current
      environment.

    * It then `(eval widget-symbol)` to create an instance of the widget.

    * It sets `:tip_mbox` and `:owner` properties on the embedded widget to
      integrate it with the Docs app's event system (allowing tooltips and
      routing events to the special `+select_embedded` mailbox if the widget
      needs to communicate).

    * The live widget instance is added to the `page`.

    * Returns `:text` when "```" is encountered.

### 5. Event Loop and UI Updates

* The main event loop in `apps/docs/app.lisp` handles standard UI events
  (navigation, search, window management).

* When a document is selected (`action-file-leaf-action`), `populate-page` is
  called.

* `populate-page` creates a new `page_flow` widget. Section handlers add their
  content to this `page_flow`.

* After `populate-page` completes, the `page_flow` is added as a child to
  `*page_scroll*`.

* The layout of the main document flow (`*doc_flow*`) is then updated using
  `(.-> *doc_flow* :layout :dirty_all)`, which triggers redrawing of the new
  content.

* The `*search_widgets*` list, populated by the `:text` handler, is used by
  search actions (`action-find-down`, `action-find-up` in `apps/docs/search.inc`)
  to iterate over displayable text elements and highlight matches.

### 6. Strengths and Observations

* **High Extensibility:** New content types can be supported by simply creating
  a new section handler module (e.g., `apps/docs/newtype.inc`) and using the
  "```newtype" tag in documentation. The core Docs application logic does not
  need to be changed.

* **Decoupling:** The Docs app is decoupled from the specifics of how each
  content type is rendered.

* **Lisp-centric Design:** The system heavily leverages Lisp's dynamic features,
  including `repl`/`(import)` for code loading and the environment system for
  managing handlers.

* **Powerful Embedding:** The `:widget` handler demonstrates the ability to
  embed fully functional, live ChrysaLisp UI components directly within
  documentation, which is a testament to the GUI's flexibility.

* **Performance:** Handler modules are loaded dynamically only once per type per
  session and then cached in the `handlers` `Emap`. Subsequent encounters of the
  same section type use the cached handler function directly.

* **Handler State:** Each handler is a function. If a handler needs to maintain
  state across multiple lines of its section (e.g., the `:vdu` handler
  accumulating lines), it must do so using local variables within its lexical
  scope that persist across calls *if the handler itself is a closure that
  captures this state*. In the current `apps/docs/*.inc` implementations,
  handlers like `:vdu` use `(push lines ...)` where `lines` is a local variable
  within the `handler` function defined in the module; this `lines` variable is
  reinitialized on each call to `handler-func` when it fetches/re-imports the
  handler, so state across lines for a single section block is managed by the
  `populate-page` loop passing the current `state` to the handler, and the
  handler deciding if it's done or needs more lines for that same `state`.

## Conclusion

The ChrysaLisp Docs application effectively utilizes a dynamic, module-based
section handling system. This architecture makes the application highly
extensible and showcases several of ChrysaLisp's core strengths, including its
powerful Lisp environment, dynamic code loading, and flexible GUI system. The
ability to embed live widgets directly into documentation is a particularly
notable feature.
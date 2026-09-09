# Docs

The `Docs` application is the Chrysalisp documentation viewer. It is used to
display tutorials as well as reference material auto generated from the source
code files.

Section handlers are loaded dynamically as required and given responsibility for
the embedding of content. Content ranges from wrapped text to images and live
Lisp code snippets, including embedding the entire UI of applications. The
mechanization for the section handlers is explained in the `event_dispatch.md`
document.

The Terminal command app `make docs` is used to scan the source files and create
the reference documentation files.

If you hover the mouse over the embedded UI below you can see the kind of
features available. There are more features available through the key bindings
which can be found in the `keys.md` documentation.

## UI

```widget
apps/desktop/docs/widgets.inc *window* 512 512
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

* **Main Application File:** `apps/desktop/docs/app.lisp`

* **UI Definition:** `apps/desktop/docs/widgets.inc`

* **Actions and Event Routing:** `apps/desktop/docs/actions.inc` and `apps/desktop/docs/ui.inc`

* **Distributed Search Engine:** `apps/desktop/docs/search.inc`

* **Markdown Layout Engine:** `gui/md/lisp.inc` (the `Md` widget)

* **Section Handler Modules:** Located in `apps/desktop/docs/handlers/`:
	`text.inc`, `code.inc`, `vdu.inc`, `file.inc`, `widget.inc`, `lisp.inc`,
	`lispq.inc`, `info.inc`, `image.inc`.

### 4. Dynamic Section Handling

This is the most distinctive feature of the Docs application's implementation.
It allows for extensible content rendering without modifying the core
application logic.

#### 4.1. Markup Convention

Documentation files use a simple markup convention to define special sections. A
section of a specific `<type>` is denoted by a fenced block:

```
;   ```<type>
;   ... content for this type ...
;   ```
```

For example:

```
;   ```image
;   apps/media/images/data/logo.cpm
;   ```
```

Or an embedded live widget:

```
;   ```widget
;   apps/desktop/docs/widgets.inc *window* 512 512
;   ```
```

The default state when no fenced tag is active is `:text`.

#### 4.2. Handler Discovery and Dynamic Loading

The core mechanism for dynamic section handling resides in the `populate-page`
function within `apps/desktop/docs/app.lisp`. This function processes the
selected document line by line, maintaining a `state` variable (a symbol like
`:text`, `:image`, `:code`, `:vdu`, `:file`, etc.) that indicates the current section
type being processed.

* **`handlers` Emap:** An `Emap` instance, named `handlers`, is used as a cache
	for loaded section handler functions. It maps the state symbol (e.g.,
	`:image`) to the actual Lisp handler function.

* **`handler-func` Helper:** A local helper function, `handler-func`, is
	responsible for retrieving or dynamically loading the appropriate handler:

	1. It first checks if a handler for the current `state` already exists in
		 the `handlers` `Emap`.

	2. If not found, it dynamically constructs a module path: `(cat
		 (const (cat *app_root* "handlers/")) (rest state) ".inc")`. For example, if
		 `state` is `:image`, the path becomes
		 `"apps/desktop/docs/handlers/image.inc"`.

	3. It then uses `(repl (file-stream module) module)` to evaluate the
		 module. The module evaluates within an `(env-push) ... (env-pop)` block and
		 exports its handler via `(export-symbols '(handler))`, binding `handler` in
		 the caller's scope.

	4. This exported `handler` function is stored in the `handlers` `Emap`,
		 keyed by the `state` symbol.

* **Invocation:** Once the handler function is obtained (either from the cache
	or by dynamic loading), it is called with the current `state`, the `page`
	widget (the `Flow` container where content is added), and the `current_line` from
	the document.

```vdu
; Snippet from apps/desktop/docs/app.lisp
(defq handlers (Emap)) ; Emap to cache handlers

(defun handler-func (state)
	(unless (defq handler (. handlers :find state))
		(defq module (cat (const (cat *app_root* "handlers/")) (rest state) ".inc"))
		(repl (file-stream module) module)
		(. handlers :insert state handler))
	handler)

(defun populate-page (file)
	; ...
	(defq state :text)
	(lines! (lambda (line)
			(task-slice)
			(catch (setq state ((handler-func state)
						state page (trim-end line "\r")))
				(progn (prin _) (print) (setq state :text) :t))
			:nil)
		(file-stream file))
	(catch ((handler-func state) state page "```")
		(progn (prin _) (print) (setq state :text) :t))
	; ...
)
```

#### 4.3. Handler Interface and Contract

Each section handler module in `apps/desktop/docs/handlers/` must define and export
a function named `handler`.

* **Signature:** `(handler current_state page_widget current_line) -> new_state`

* **Responsibilities:**

	* `page_widget`: The parent UI widget (a `Flow` layout named `page` within
		`page_flow` in `populate-page`) to which the handler adds its rendered
		content.

	* `current_line`: The current line of text from the document being
		processed.

	* **Tag boundaries (`^\\s*``` `):**
		* For `:text`, encountering a fence tag flushes any accumulated prose lines
			into an `Md` widget, extracts the new section type (e.g., `:vdu`, `:file`,
			or defaulting to `:code` if no tag is specified), and returns that new state.
		* For active block handlers (e.g. `:vdu`, `:code`, `:file`, `:widget`), the
			closing fence tag finalizes any accumulated stream content or pending
			widgets, adds them to `page`, resets working buffers, and returns `:text`.

	* **Section content:**
		* The handler processes or streams the incoming line (for example, into
			`*mem_stream*` or a local list) and returns `current_state` to remain active.

#### 4.4. Section Handlers Reference

The Docs application provides nine dedicated section handlers:

* **`apps/desktop/docs/handlers/text.inc` (Handler for `:text` state):**

	* Handles standard markdown prose between fenced blocks.

	* Accumulates text lines until a block fence tag or EOF is reached.

	* Instantiates an `(Md)` widget (`gui/md/lisp.inc`) configured with current
		window zoom, target page width, and document typography fonts (`+doc_font`,
		`+doc_font_bold`, `+doc_font_italic`, `+doc_font_bold_italic`, `+terminal_font`,
		`+symbol_font`).

	* Invokes `(. md :populate_lines lines)`, which encapsulates markdown parsing
		(prefix stripping, single-pass quote masking, `reduce!` style ladders,
		atomic `splice` formatting, and grid tables) and creates an internal visual tree
		of `Text` word widgets.

	* Adds the populated `Md` widget to `page`.

* **`apps/desktop/docs/handlers/code.inc` (Handler for `:code` state):**

	* Used for plain/unformatted code blocks (bare ```` ``` ```` without a language identifier).

	* Streams incoming lines into `*mem_stream*`, expanding tab stops (4 spaces).

	* Upon encountering the closing tag, loads `*mem_stream*` into an unformatted
		`Document` (`(Document 0 :nil)`), trims whitespace, binds buffer dimensions
		to a `Vdu` terminal widget (`:font_terminal_small`, `:ink_color +argb_blue`,
		`:color 0`), and adds the `Vdu` and a trailing spacer `Text` to `page`.

	* Resets `*mem_stream*` to `:nil`.

* **`apps/desktop/docs/handlers/vdu.inc` (Handler for `:vdu` state):**

	* Used for syntax-highlighted code blocks (e.g. ```` ```vdu ````).

	* Streams incoming lines into `*mem_stream*`.

	* Upon encountering the closing tag, loads `*mem_stream*` into a syntax-highlighted
		`Document` (`(Document +buffer_flag_syntax syntax)`), trims whitespace, and
		inserts framing newlines.

	* Calculates the minimum left indentation margin across all lines and deletes it
		in-place via `(. buffer :idelete ...)` so indented code blocks display flush.

	* Binds the buffer to a `Vdu` widget (`:ink_color +argb_black`, `:color 0`)
		embedded in a `Backdrop` (`+argb_grey1`). If the content width exceeds the terminal
		viewport (`rw > tw`), wraps it inside a horizontal `Scroll` container before
		adding to `page`.

	* Resets `*mem_stream*` to `:nil`.

* **`apps/desktop/docs/handlers/file.inc` (Handler for `:file` state):**

	* Embeds source code snippets directly from repository files without manually
		copy-pasting code into markdown files.

	* Parses `line` as `(file &optional start_exp end_exp)`.

	* Opens the target file via `(file-stream file)` and scans lines until matching
		`start_exp`, streaming lines into `*mem_stream*` until `end_exp` (or empty line / EOF).

	* Formats the extracted snippet with full syntax highlighting, automated margin
		stripping, and a `Backdrop`/`Scroll`-wrapped `Vdu` widget identical to `:vdu`.

* **`apps/desktop/docs/handlers/widget.inc` (Handler for `:widget` state):**

	* Enables embedding live, interactive ChrysaLisp UI components directly inside
		documentation pages.

	* Parses `line` as `(file &optional widget mw mh)`.

	* Uses `(import-from (str file) (list widget))` to load only the requested
		widget definition, and `(eval widget)` to instantiate it.

	* Connects `:tip_mbox` to `+select_tip`, applies optional `:min_width` and
		`:min_height`, and sets `:owner` to `+select_embedded` on the Docs app's
		event multiplexer so embedded user interactions do not trigger host window actions.

	* Adds the live widget to `page` inside a right-aligned `Flow`.

* **`apps/desktop/docs/handlers/lisp.inc` (Handler for `:lisp` state):**

	* A composite handler for interactive executable code tutorials.

	* Upon encountering the closing tag, redirects the accumulated code lines
		to `:vdu` (to display the syntax-highlighted source code) **and** to `:lispq`
		(to evaluate the code and embed the resulting view or output).

* **`apps/desktop/docs/handlers/lispq.inc` (Handler for `:lispq` state):**

	* Evaluates live Lisp expressions within `*handler_env*` without displaying the
		source code text.

	* Evaluates the joined lines via `(repl ss "Lisp handler")` within `*handler_env*`.

	* If the evaluated `*result*` is a `View`, binds its `:tip_mbox` and `:owner`
		(`+select_embedded`) and embeds the live interactive component into `page`.

	* If the result is a data value, outputs the result via a `:vdu` block.

* **`apps/desktop/docs/handlers/info.inc` (Handler for `:info` state):**

	* Provides automated system introspection for reference documentation.

	* Accepts commands `"root-funcs"`, `"root-macros"`, or `"root-lambdas"`.

	* Filters and sorts matching symbols from `*root_env*`, reflows them into
		80-column paragraphs, and prints them in a `:vdu` block.

* **`apps/desktop/docs/handlers/image.inc` (Handler for `:image` state):**

	* Loads bitmap graphics from disk using `(canvas-load line +load_flag_shared)`.

	* Adds the resulting `Canvas` widget directly to `page`, appending a trailing
		spacer `Text` widget on completion.

### 5. Event Loop and UI Architecture

* **Coordinated Event Multiplexing (`task-mboxes`):**
	The main loop in `apps/desktop/docs/app.lisp` listens on three coordinated mailboxes:
	* `+select_main`: Handles window events (resize, minimize, maximize, close), navigation
		toolbar buttons, and file tree selections.
	* `+select_tip`: Dispatches tooltip timers when hovering over views or controls.
	* `+select_embedded`: Dedicated event routing for embedded interactive widgets,
		ensuring embedded UI clicks and actions are processed internally without leaking into
		the Docs window controls.

* **Page Layout and Scroll Restoration:**
	When a document is selected (`action-file-leaf-action`), `populate-page` constructs
	a `page_flow` containing document margins and the central `page` flow. After section
	handlers populate `page`, `page_flow` is measured, attached to `*page_scroll*`, and
	the file's previous vertical scroll position is restored from the `scroll_pos` `Fmap`.

* **Two-Tiered Distributed Search (`search.inc`):**
	* **Global Cluster Document Search:** Entering text in `*find_text*` invokes
		`find-global`, which runs a parallel `pipe-farm` of `cmd/grep.lisp` tasks across all
		`.md` files in `docs/`. Matching documents are highlighted in the `*file_selector*`
		tree in real-time.
	* **In-Page Word Search:** `find-update` searches through `*search_widgets*`
		(the list of `Text` word widgets gathered via `(. md :get_search_widgets)`
		from all `Md` components on the page). It evaluates matches using `query`
		(supporting whole words, regex, and case sensitivity), highlights matching
		words with `*env_highlight_col*`, and smoothly scrolls the match into view.

### 6. Architectural Strengths

* **Component-Based Document Composition:** Rather than treating a document as a
	monolithic rich-text buffer, documents are visual trees composed of `Md` markdown
	blocks, syntax-highlighted `Vdu` terminals, `Canvas` images, and active GUI `Widget` instances.

* **Extensible Micro-Handler Architecture:** New content formats require only a
	new `.inc` file in `apps/desktop/docs/handlers/` exporting `handler`. The core
	application remains decoupled and unchanged.

* **Decoupled Markdown Engine (`Md`):** By encapsulating markdown parsing and layout
	in the reusable `Md` class (`gui/md/lisp.inc`), the Docs app remains lean (around
	130 lines in `app.lisp`) while leveraging ChrysaLisp's hardware-accelerated
	vectorized string operations (`splice`, `reduce!`).

* **Isolated Execution Environment (`*handler_env*`):** A dedicated evaluation
	environment with an overridden `enums` macro allows live code blocks (`:lisp`, `:lispq`,
	`:widget`) to instantiate interactive widgets with isolated event IDs, preventing
	embedded components from interfering with host application navigation.

* **Distributed Compute Integration:** Search queries exploit ChrysaLisp's multi-core
	actor architecture via `pipe-farm`, searching the entire documentation library in
	parallel.

## Conclusion

The ChrysaLisp Docs application demonstrates a modern, component-driven approach
to document viewing. By combining dynamic micro-handler loading with the hardware-accelerated
`Md` markdown engine, stream-based buffer formatting, and cluster-wide `pipe-farm` search,
it provides an extensible, performant viewer capable of displaying everything from rich
prose to live, interactive applications.


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
apps/tools/edit/widgets.inc *window* 512 512
```

## Implementation Study

The ChrysaLisp Editor (found in `apps/tools/edit/`) is a highly integrated,
multi-buffer programmer's text editor designed specifically for the ChrysaLisp
parallel processing operating system. It serves as a prime demonstration of
ChrysaLisp's graphical user interface, declarative widget framework, native
Virtual Processor (VP) assembly speed, and distributed parallel capabilities.

### 1. Core Editor Characteristics

The ChrysaLisp Editor utilizes several key architectural elements of the OS:

*	**Multi-Cursor / Multi-Selection Engine:** Built directly into the core
	`Buffer` and `Document` classes, the editor supports editing at arbitrary
	multiple locations simultaneously. Cursors are tracked as tuples of
	coordinates: `(cx cy ax ay sx)` where `cx/cy` represent the cursor position,
	`ax/ay` represent the selection anchor, and `sx` is a "sticky X" column index
	used to preserve vertical alignment during vertical movements.

*	**Distributed Parallel Grep & Match Harvesting:** Global search
	(`action-find-global` in `search.inc`) maps file paths across active network
	nodes, executing parallel search tasks via a `pipe-farm` command pipeline
	and streaming formatted matches directly into the scratchpad buffer.
	Complementary collection actions (`action-collect`, `action-collect-global`)
	extract matches across the active region, buffer, or all open files into the
	scratchpad.

*	**Integrated Debugger Subsystem:** Features a dedicated `*debug_toolbar*`
	with bidirectional RPC integration with the ChrysaLisp Debugger
	(`apps/system/debug/app.lisp`). It automates target application detection,
	manages inline `(debug-brk "0xHEX")` breakpoints, and handles remote
	breakpoint navigation.

*	**Intelligent Word Autocompletion:** Integrates a global `Dictionary` object
	that indexes words from loaded files and standard text databases. The
	`show-matches` system captures context words in a 20-line radius around the
	cursor, sorting auto-completion matches by local frequency and alphabetical
	order inside a floating overlay window (`match_window`).

*	**Cursor Navigation History Stack:** Maintains a stack of visited locations
	(`*cursor_stack*`), enabling jump-to-definition (`action-find-function`)
	and location pushing/popping (`action-push`, `action-pop`) across multiple
	files and buffers.

*	**Multi-File Unified Undo/Redo:** Rather than restricting undo actions to the
	current buffer, the editor maintains `*global_undo_stack*` and
	`*global_redo_stack*` transaction groups. This tracks historical states across
	all active buffers, allowing a single grouped edit (like a global macro
	playback or replace-all) to be reverted cleanly across multiple files with
	closed-buffer pruning.

*	**RPC-Driven Inter-Process Communication:** Registers an "Edit" service using
	`mail-declare` to handle remote requests like `+edit_rpc_type_jump`, enabling
	external tools (such as the Debugger) to trigger file opening and cursor
	jump-to-line operations.

### 2. Application Architecture

The editor uses a Model-View-Controller (MVC) design pattern that is tightly
coupled with ChrysaLisp's cooperative internal scheduling.

```code
          +------------------+     (RPC/Service)
          |   Edit Service   | <-----------------+
          +------------------+                   |
                   |                             |
                   v                             |
          +------------------+                   |
          |  app_impl.lisp   | <-------+         |
          |   (Controller)   |         |         |
          +------------------+         |         |
             |            |            |         |
             v            v            |         |
      +------------+  +------------+   |         |
      | widgets.inc|  |   ui.inc   |   |         |
      |   (View)   |  | (Editor-   |   |         |
      +------------+  |   edit)    |   |         |
                      +------------+   |         |
                            |          |         |
                            v          |         |
                      +------------+   |         |
                      |  Edit/VDU  |   |         |
                      |   Layers   |   |         |
                      +------------+   |         |
                            |          |         |
                            v          |         |
                      +------------+   |         |
                      |  Document  | --+ (Undo)  |
                      |   Model    | ------------+
                      +------------+
```

#### 2.1 Entry Point and Event Loop (`app_impl.lisp`)

The `main` function initializes the application, starting up the UI window,
loading saved state from `editor.tre`, and listening on a select list of
mailboxes created via `(task-mboxes +select_size)`. The select index mappings
are managed via the `+select` enum:

*	`+select_main`: Handles GUI window events, routing keyboard, mouse, and toolbar inputs.

*	`+select_tip`: Manages tooltips for all toolbars.

*	`+select_remote`: Manages incoming RPC requests (e.g., jump-to-breakpoint from the
	debugger).

#### 2.2 Dual-Tree Workspace Layout

The UI layout (`widgets.inc`) features a sidebar on the left containing two
hierarchical trees: `*open_files_selector*` (listing currently active open buffers)
and `*file_selector*` (for browsing the project directory). Clicking a folder in
the project tree automatically loads all source files in that directory. The right
side places line numbers (`*vdu_lines*`) next to the main `*edit*` area with its
scroll sliders (`*xslider*`, `*yslider*`).

#### 2.3 Real-Time Status Metrics

The status bar at the bottom of the editor pane displays five live metrics
updated by `refresh-display`:

*	`cx`: Current cursor column (1-indexed).

*	`cy`: Current cursor line (1-indexed).

*	`sw`: Selection width in characters (`|cx - ax|`).

*	`sh`: Selection height in lines (`|cy - ay|`).

*	`fc`: Search match count (`find-count`) of the current query within the active buffer.

### 3. Core Editing Engine

#### 3.1 The Controller-View Coordinate (`Edit` in `gui/edit/lisp.inc`)

Inheriting from `View`, the `Edit` class manages the low-level `Vdu` (Video
Display Unit) widget, tracks scrolling offsets (`scroll_x`, `scroll_y`), and
maps physical coordinates to character cells. It acts as a proxy, delegating
editing methods (such as `:insert`, `:delete`, `:backspace`, `:undo`, `:redo`)
to the underlying `Buffer` model.

#### 3.2 Selection Mask Overlays (`underlay` in `app_impl.lisp`)

To render selections, cursor carets, and matching bracket highlights, the editor
does not redraw the text. Instead, a custom `underlay` function calculates
sub-regions and configures `Mask` overlay widgets (`mask_ink`, `mask_selected`,
`mask_found`, `mask_region`) placed behind the translucent text layers. This
allows the VDU to draw text as purely static layers while the GPU handles
selection compositing.

#### 3.3 The Data Model (`Buffer` in `lib/text/buffer.inc`)

`Buffer` represents the data model. It manages raw text lines (`buffer_lines`)
and provides low-level, coordinate-safe mutation methods like `:iinsert`
(internal insert), `:idelete` (internal delete), and `:icopy` (internal copy).

*	**Smart Cursor Mapping:** When text is modified, other cursors on the same or
	subsequent lines must be shifted to remain accurate. This is handled by
	FFI-bound native functions `csr_map_insert` and `csr_map_delete` (compiled
	from `class/lisp/lisp.vp`), preventing cursors from becoming misaligned during edits.

*	**The Document Subclass (`Document` in `lib/text/document.inc`):** Inheriting
	from `Buffer`, this subclass enriches the raw text model with syntax highlighting,
	high-level block, word, paragraph, and form-level selection actions, alongside code
	formatting tools such as `:reflow`, `:split`, `:sort`, `:unique`, `:comment`,
	and `:trim`.

#### 3.4 Specialized Subclassing (`Editor-edit` in `ui.inc`)

The Editor app inherits from the generic `Edit` class to define `Editor-edit`.
It overrides mouse and scroll wheel interactions, updates scroll sliders, and
processes multi-clicks:

*	**Double-click:** Triggers `action-select-word`.

*	**Triple-click:** Triggers `action-select-line`.

*	**Quadruple-click:** Triggers `action-select-paragraph`.

*	**Zoom Scaling:** Font scaling via `action-scale-up` (`Ctrl+}`) and `action-scale-down`
	(`Ctrl+{`) scales the editor font and line number VDU dynamically between 0.75x and 1.5x.

### 4. Key Subsystems

#### 4.1 Search, Replace, and Match Harvesting (`search.inc`)

The editor provides a rich search and transformation engine operating locally and
cluster-wide:

*	**Find Options:** Toggles for whole words (`*whole_words*`), regular expressions
	(`*regexp*`), and case insensitivity (`*ignore_case*`).

*	**Region Scoping (`action-region`):** Restricts find, replace, and collect
	operations to the active cursor selection vertical range (`focus`).

*	**Multi-Cursor Search Integration:**
	*	`action-add-next` (`Ctrl+Shift+F`): Adds the next match occurrence as an active secondary cursor.
	*	`action-set-cursors` (`Ctrl+G`) / `action-add-cursors` (`Ctrl+Shift+G`): Converts all matches
		within the buffer (or selected region) into active multi-cursors.

*	**Jump to Definition (`action-find-function`, `Ctrl+J`):** Selects the word under
	the cursor, searches open files for top-level definitions (`defun`, `defmacro`,
	`redefun`, `redefmacro`, `defclass`, `ffi`), jumps to the definition line, and pushes
	the prior location to `*cursor_stack*`.

*	**Match Harvesting (`action-collect`, `action-collect-global`):** Gathers all
	matching text spans—from the current buffer or across all open buffers—and pastes
	them formatted into the scratchpad buffer (`action-scratch-buffer`).

*	**Compiled Replacement Templates:** Uses `replace-compile` and `replace-edits`
	to apply capture group substitutions in reverse coordinate order to maintain index integrity.

*	**Cluster Distributed Grep (`action-find-global`):** Dispatches parallel `grep`
	tasks across active network nodes using `pipe-farm`, skipping dictionary files and
	pasting aggregated results into the scratchpad.

#### 4.2 Integrated Debugger Subsystem (`debug.inc`)

The editor features deep, bidirectional integration with the ChrysaLisp Debugger:

*	**Launch and Target Detection (`action-debug-launch`):** Saves all open files,
	launches `apps/system/debug/app.lisp`, inspects the paths of open files to identify
	the associated target application (e.g. `apps/demos/boing/app.lisp`), and launches
	the application child task under the debugger.

*	**Breakpoint Management:**
	*	`action-debug-toggle`: Inserts a `(debug-brk "0xHEX")` breakpoint with a randomized ID
		on the current line, or toggles an existing breakpoint on/off by commenting it with `;;; `.
	*	`action-debug-remove`: Deletes the auto-breakpoint on the current line.
	*	`action-debug-enable-all` / `action-debug-disable-all`: Mass-enables or disables breakpoints
		across the entire buffer.
	*	`action-debug-remove-all`: Strips all auto-breakpoints from the active buffer.

*	**Breakpoint Hit Traversal (`action-breakpoint` in `cursor.inc`):** When a breakpoint
	triggers in a running application, the Debugger transmits an `edit-jump-rpc` message.
	The editor opens the file, scans backward up to 256 lines to locate the `(debug-brk "brk_id")`
	token, selects the line, and positions the cursor.

#### 4.3 Buffer, File, and Dependency Automation (`file.inc`)

*	**Dependency Analysis (`action-load-depends`, `action-load-all-depends`):** Uses
	`files-depends` to parse `(import ...)` statements in the active file and load all
	direct dependencies, or `files-all-depends` to recursively load the entire transitive
	import tree across the project.

*	**Selection Loading (`action-load-selected`):** Scans selected text for file paths
	and opens all matching files.

*	**Dedicated Scratchpad (`action-scratch-buffer`):** A transient buffer (represented
	by `:nil` file path) used for temporary notes, scratch code, and search/collect output.

*	**Inline File Creation:** The `*name_text*` textfield on the buffer toolbar allows
	typing a new file path and creating it instantly on `Enter`.

#### 4.4 The Macro Subsystem (`macros.inc`)

When `*macro_record*` is active, user interactions matching `*recorded_actions*`
are logged to a record buffer in `+macro_map`.

*	**Playback Slots:** Supports saving macros to 10 independent slots (`0` to `9`).

*	**Execution Modes:** Playback can run once, to the end of the file
	(`action-macro-to-eof`), or globally across all open buffers (`action-macro-global`).

*	**Recursive Safety:** Playback is protected against infinite recursion at compile
	time by analyzing slot calls.

#### 4.5 Undo/Redo Subsystem (`undo.inc` / `buffer.inc`)

When editing, the `undoable` macro groups atomic edits inside transaction
boundaries by pushing a `:mark` and snapshotting active cursors.

*	**Global Undo Coordination:** The global stack tracks buffer keys along with
	transaction marks.

*	**Group Reversals:** Undoing (`action-undo`) or redoing (`action-redo`) pops the
	global transaction, switching active buffers dynamically and calling `:rewind` or
	`:forwardwind` on each affected buffer to sync their historical state.

*	**Closed-Buffer Pruning (`clean-stack`):** When a file is closed, `clean-stack`
	purges references to that file from both `*global_undo_stack*` and `*global_redo_stack*`,
	ensuring multi-buffer undos never attempt to switch to a closed buffer.

*	**Redraw Suppression:** During multi-buffer transaction rewinds, `*refresh_mode*` is
	pushed to `+refresh_mode_hidden`, suppressing screen flicker until all buffers are synchronized.

#### 4.6 S-Expression and Code Transformations (`actions.inc`)

Tailored for Lisp and assembly programming, the editor provides extensive structural
code editing actions:

*	**Form Selection and Cutting:** `action-select-form` (`Ctrl+/`), `action-cut-form`
	(`Ctrl+?`), and `action-copy-form` select or cut balanced S-expressions.

*	**Bracket Balancing:** `action-left-bracket` (`Ctrl+[`) and `action-right-bracket`
	(`Ctrl+]`) navigate to matching parentheses/brackets.

*	**Line and Block Manipulation:** `action-reflow` (`Ctrl+Q`), `action-split` (`Ctrl+Shift+Q`),
	`action-sort` (`Ctrl+O`), `action-unique` (`Ctrl+Shift+O`), `action-invert` (`Ctrl+I`, reverses line order),
	and `action-comment` (`Ctrl+/`, toggles `;` line comments).

*	**Whitespace Trimming:** `action-trim` (`Ctrl+T`) strips trailing whitespace.

#### 4.7 Session State Persistence (`state.inc`)

Session state is saved to `editor.tre` in the user's home directory (`usr/User/editor.tre`).
It preserves:

*	Window geometry (`x`, `y`, `width`, `height`).

*	Search strings, regex, and whole-word flags.

*	The complete open file list, including each buffer's cursors, selections, and scroll positions.

*	Serialized/encoded macros across all 10 slots.

### 5. User Interface and Event Dispatching

#### 5.1 Six Specialized Toolbars

The top header organizes actions into six functional toolbars:

1.	`*main_toolbar*`: Edit actions (undo, redo, rewind, cut, copy, paste, split, reflow,
	paragraph, indent/outdent, forms, case, sort, unique, reverse, comment).
2.	`*macro_toolbar*`: Macro playback, playback to EOF, global playback, and record toggle.
3.	`*debug_toolbar*`: Debugger launch with target auto-detection, toggle breakpoint,
	remove breakpoint, enable all, disable all, and remove all breakpoints.
4.	`*buffer_toolbar*`: Previous/next buffer, scratchpad, close, close all, save, save all,
	load selected, load all dependencies, and new file textfield (`*name_text*`).
5.	`*find_toolbar*`: Global search, region scope, whole words, regex, ignore case,
	find down, find up, and find textfield (`*find_text*`).
6.	`*replace_toolbar*`: Collect, collect global, replace, replace all, replace global,
	and replace textfield (`*replace_text*`).

#### 5.2 Central Action Dispatcher (`actions.inc`)

Events are routed through the `dispatch-action` function, which maps key inputs
and UI events to their corresponding routines.

```code
              +--------------------+
              |  Keyboard / Mouse  |
              +--------------------+
                        |
                        v
              +--------------------+
              |    app_impl.lisp   |
              |  (dispatch-action) |
              +--------------------+
                        |
            +-----------+-----------+
            |           |           |
            v           v           v
      +----------+ +-----------+ +---------+
      |  Macro   | | Undo/Redo | | Cursor  |
      | Recorder | |  Tracker  | |  Stack  |
      +----------+ +-----------+ +---------+
            |           |           |
            +-----------+-----------+
                        |
                        v
              +--------------------+
              |  Action Execution  | (e.g., action-insert)
              +--------------------+
```

Keyboard events are categorized into three distinct maps to handle modifiers cleanly:

*	`*key_map*`: Maps raw scancodes (e.g., arrows, backspace, enter, tab).

*	`*key_map_shift*`: Handles shift modifiers (e.g., selecting text, outdenting).

*	`*key_map_control*`: Processes control commands (e.g., copy, paste, find,
	macros, font scaling, navigation, and code refactoring).

### 6. Remote Procedure Calls (RPC)

By registering with the local name server as the `"Edit"` service via `mail-declare`,
the editor receives cross-task RPC requests. The `edit-jump-rpc` mechanism in `rpc.inc`
allows external processes (such as the ChrysaLisp Debugger or compiler error handlers)
to transmit jump requests:

```code
(edit-jump-rpc brk_id file_name line_num)
```

The editor parses the payload, loads the target file, locates the breakpoint or error
line via backward scanning, positions the cursor, highlights the line, and brings the
editor window to the front.

### Conclusion

The ChrysaLisp Editor is a robust implementation of the operating system's
architectural principles. By combining Model-View-Controller separation with
low-overhead message-passing primitives, it achieves high performance within a
small footprint. The native, FFI-bound cursor mapping, multi-cursor search harvesting,
distributed cluster grep, and tight debugger integration demonstrate the speed,
cohesion, and scalability of ChrysaLisp's Virtual Processor architecture.

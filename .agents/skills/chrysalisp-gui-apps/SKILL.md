---
name: chrysalisp-gui-apps
display-name: ChrysaLisp GUI Apps
description: Use when writing or modifying ChrysaLisp GUI applications — windows, widgets, toolbars, event loops, and action handlers.
---

# ChrysaLisp GUI App Skill

A ChrysaLisp GUI app is a long-running task that owns a window, renders
a widget tree, and dispatches events from a mailbox select loop. The
general ChrysaLisp disciplines (see the `chrysalisp` skill, `LLM.md`, and
`docs/ai_digest/`) apply on top of these app-specific patterns.

## Canonical File Structure

Copy `apps/template/` as the starting point for a new app:

*	`app.lisp`: Entry point — imports, mailbox setup, the main event
	loop.

*	`widgets.inc`: Declarative UI definition — the widget tree, event
	enums, and tool tips.

*	`actions.inc`: The switchboard — maps event IDs to action functions
	and key codes to actions.

*	`ui.inc`: The action handler implementations (application logic).

*	`utils.inc`, `clipboard.inc`, `undo.inc`: Helper, clipboard, and
	undo/redo logic.

Reference apps: `apps/desktop/docs/` (good but simple),
`apps/tools/edit/` (complex, with services and RPC), and
`apps/demos/boing/` (timer-driven animation).

## app.lisp — The Event Loop

	;debug options
	(case 2
	(0 (import "lib/debug/frames.inc"))
	(1 (import "lib/debug/profile.inc"))
	(2 (import "lib/debug/debug.inc")))

	(import "usr/env.inc")
	(import "gui/lisp.inc")
	(import "service/clipboard/app.inc")

	;our UI widgets
	(import "./widgets.inc")

	(enums +select 0
		(enum main tip timer))

	(defq +rate (/ 1000000 1))

	;import actions and bindings
	(import "./actions.inc")

	(defun dispatch-action (&rest action)
		(catch (eval action) (progn (prin _) (print) :t)))

	(defun main ()
		(defq select (task-mboxes +select_size) *running* :t)
		(def *window* :tip_mbox (elem-get select +select_tip))
		(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
		(gui-add-front-rpc (. *window* :change x y w h))
		(mail-timeout (elem-get select +select_timer) +rate 0)
		(while *running*
			(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
			(cond
				((= idx +select_tip)
					;tip event
					(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
						(. view :show_tip)))
				((= idx +select_timer)
					;timer event, re-arm and do periodic work here
					(mail-timeout (elem-get select +select_timer) +rate 0))
				;must be +select_main
				((. *window* :dispatch *msg*))
				((. *window* :event *msg*))))
		(gui-sub-rpc *window*)
		(profile-report "Template"))

Key points:

*	The `+select` enum names the mailbox slots. Slot 0 (`main`) receives
	window events; add extra slots as needed (e.g., `timer`, `remote`)
	and handle them in the `cond` before dispatch.

*	The current message is held in the global `*msg*`, so action
	handlers can inspect it, e.g. `(getf *msg* +ev_msg_action_source_id)`.

*	`(. *window* :dispatch *msg*)` looks up the event's target widget
	and its `:connect` event in `*event_map*`, then calls
	`dispatch-action`. The final clause, `(. *window* :event *msg*)`,
	handles everything else at the window level.

*	`tip_mbox` receives hover-timeout messages from the GUI; look up
	the hovered widget with `find_id` and call `:show_tip`.

*	The window is registered with the GUI compositor via
	`gui-add-front-rpc` and unregistered on exit with `gui-sub-rpc`.

*	Window placement: `view-locate` centers a window of its preferred
	size on screen; `view-fit` clamps an explicit position and size to
	the screen.

*	The `case 2` debug header selects stack frames, profiling, or
	debugger support. `(debug-brk "name")` and `profile-report` are
	no-ops unless the matching debug import is active.

## widgets.inc — The Widget Tree

	(enums +event 0
		(enum close max min)
		(enum undo redo rewind cut copy paste)
		(enum button_1 button_2))

	(ui-window *window* ()
		(ui-title-bar *title* "Template" (0xea19 0xea1b 0xea1a) +event_close)
		(ui-tool-bar *main_toolbar* ()
			(ui-buttons (0xe9fe 0xe99d 0xe9ff 0xea08 0xe9ca 0xe9c9) +event_undo))
		(ui-stack *stack_flow* '("main" "settings") :nil
			(ui-grid *main_widget* (:grid_width 2 :color +argb_orange)
				(. (ui-button *b1* (:text "1")) :connect +event_button_1)
				(. (ui-button *b2* (:text "2")) :connect +event_button_2))
			(ui-backdrop *settings_widget* (:min_width 512 :min_height 256
					:color +argb_black :ink_color +argb_red :spacing 16
					:style :lines))))

	(ui-tool-tips *main_toolbar*
		'("undo" "redo" "rewind" "cut" "copy" "paste"))

Key points:

*	The `+event` enum groups related events. Every interactive widget
	gets a unique event symbol via its `:connect` property; that is the
	key used in `*event_map*`.

*	The first event passed to `ui-buttons` is the radio group: it
	highlights which button in that toolbar is active.

*	Named widgets become globals (`*b1*`); anonymous ones use `_`.

*	Containers: `ui-flow` (with `:flow_flags`), `ui-grid`, `ui-stack`
	(tabbed views from a list of strings), and `ui-scroll`. Flow flags:
	`+flow_right_fill`, `+flow_left_fill`, `+flow_up_fill`,
	`+flow_down_fill`, `+flow_stack_fill`.

*	Other common widgets: `ui-textfield` (`:hint_text`,
	`:clear_text`), `ui-files` (file tree selector), `ui-slider`,
	`ui-label`, `ui-text`, `ui-backdrop`, `ui-canvas`, and `ui-vdu`.
	See `gui/lisp.inc` for the full set.

*	Use `(const ...)` for compile-time values and reference other
	widgets' properties directly, e.g. `(:color (get :color *other*))`.

*	`ui-tool-tips` attaches a list of hover tips to a toolbar or stack.

## Two-Pass GUI Layout & Constraint Primitives

The GUI framework renders views using a strict, non-backtracking two-pass
cycle:

1.	**Constraint Pass (`:constraint`):** Traverses top-down to compute the
	minimum required dimensions (`w`, `h`) of each widget based on content
	(e.g., text bounds or child count).

2.	**Layout Pass (`:layout`):** Traverses bottom-up to assign final
	coordinates and bounds to each widget.

*	**Greedy Sizing & Space Absorption:** Flow flags such as `+flow_stack_fill`
	and `+flow_down_fill` use `lastw` and `lasth` properties to absorb
	remaining container space.

*	In custom layout containers, always ensure child bounds respect `lastw`
	and `lasth` to prevent clipping or improper overflow.

## actions.inc — The Switchboard

	;module
	(env-push)

	(import "./ui.inc")

	(defq
	*event_map* (scatter (Fmap)
		+event_close action-close
		+event_min action-minimise
		+event_max action-maximise
		+event_undo action-undo
		+event_button_1 action-button-1)

	*key_map* (scatter (Fmap)
		(ascii-code "1") action-button-1)

	*key_map_shift* (scatter (Fmap))

	*key_map_control* (scatter (Fmap)
		(ascii-code "z") action-undo))

	;module
	(export-symbols
		'(*event_map* *key_map* *key_map_shift* *key_map_control*))
	(env-pop)

Key points:

*	The module pattern — `env-push` ... `export-symbols` + `env-pop` —
	keeps the maps local to this file's environment.

*	Map values are action function *names*, not calls:
	`dispatch-action` evaluates the list.

*	Key maps split by modifier state: plain, shift, control. Use
	`(ascii-code "x")` for characters and `0x4000xxxx` hex codes for
	special keys (arrows, home/end).

## ui.inc — The Action Handlers

	(defun action-close ()
		(setq *running* :nil))

	(defun action-button-1 ()
	(debug-brk "button1")
		(def (. *b1* :dirty) :color +argb_red))

	(defun action-minimise ()
	(debug-brk "minimize")
		(bind '(x y w h) (apply view-fit
			(cat (. *window* :get_pos) (. *window* :pref_size))))
		(. *window* :change_dirty x y w h))

Key points:

*	`(def (. *widget* :dirty) :prop val)` — the `:dirty` marker tells
	the GUI to redraw that widget on the next frame.

*	Window resizes go through `view-fit`/`view-locate` then
	`:change_dirty`.

*	`(debug-brk "name")` goes on its own line at column 0 — that is how
	the debugger wants to see them.

## Advanced Patterns

*	**Single instance per node.** Guard the import in `app.lisp` (see
	`apps/tools/edit/app.lisp`):

	(if (= 0 (length (mail-enquire "@Edit,")))
		(import "./app_impl.lisp"))

*	**Services and RPC.** Declare a service in `main` with
	`(mail-declare mbox "Name" "info")`, add a `remote` select slot for
	its mailbox, handle remote messages in the loop, and `(mail-forget
	key)` on exit (see `apps/tools/edit/app_impl.lisp`).

*	**Zoom.** Scale font sizes with
	`(n2i (* (n2f size) (n2f (get :zoom *window*))))`, set
	`(def *window* :zoom new_size)`, and rebuild the affected views
	(see `apps/desktop/docs/ui.inc` action-scale-up).

*	**State persistence (`config-load` / `config-save`):** Save app state to a `.tre` file in `*env_home*` with `tree-save`/`tree-load` on a file stream.
	*	**Canonical `config-load`:**
		```lisp
		(defun config-load ()
			(defq old_config :nil)
			(if (defq stream (file-stream *config_file*))
				(setq old_config (tree-load stream) stream :nil))
			(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
				(setq *config* (config-default))
				(setq *config* old_config))
			(setq *selected_id* (. *config* :find :selected_id)
				*selected_cat* (. *config* :find :selected_cat)
				*search_query* (. *config* :find :search_query)))
		```
	*	**Single `setq` Statement (Zero Type Checks):** Always extract all configuration fields in a single multi-pair `setq` statement. Never write separate individual `setq` statements per field, and never write `(str? ...)`, `(sym? ...)`, or `(num? ...)` tests on loaded fields!
	*	**`gather` with `bind` vs `setq`:**
		To extract multiple values from a map into local variables, you can use `(gather map :k1 :k2 ...)` direct into `(bind '(v1 v2 ...) (gather map :k1 :k2 ...))`.
		However, remember that **`(bind)` is doing `(def (env) ...)`, NOT `(set (env) ...)`**. It establishes *new* bindings in the local frame and will NOT update outer/global `*...*` variables. Thus, in `config-load` when updating global app variables, use a single multi-pair `setq`.
	*	**No Redundant Guarding:** Never write paranoid checks like `(if (not (str? *selected_cat*)) (setq *selected_cat* "All"))`. The `*config_version*` check at the boundary guarantees schema integrity. If missing or invalid, `(config-default)` already provided valid defaults. Do not waste performance guarding against states that cannot happen!
	*	**Lifecycle Placement:** Never call `(config-load)` at the top level of the file. `(config-load)` belongs strictly inside `(main)` at app startup, and `(config-save)` at app shutdown (and/or on user actions). Top-level execution runs prematurely before GUI initialization and can lead to duplicate loads or state corruption.

*	**Sensible Wrapping for `(def)` and `(set)` Widget Configuration:**
	When instantiating or configuring widgets with multiple properties, avoid the vertical ladder anti-pattern where every single property and value occupies its own indented line:
	```lisp
	;; ANTI-PATTERN: Excessive vertical sprawl (1 pair per line):
	(def (defq vdu (Vdu))
		:font +font_code
		:vdu_width 80
		:vdu_height h
		:color 0
		:ink_color +argb_black)

	;; IDIOMATIC: Sensible wrapping (group 2-3 pairs per line within ~80-100 columns):
	(def (defq vdu (Vdu))
		:font +font_code :vdu_width 80 :vdu_height h
		:color 0 :ink_color +argb_black)

	;; IDIOMATIC: Compact forms on a single line:
	(def (defq backdrop (Backdrop)) :color +argb_grey1 :min_width (max pad_w page_w) :min_height rh)
	(def (defq scroll (Scroll +scroll_flag_horizontal)) :min_width page_w :min_height rh)
	```

*	**Widget Borders (`:border`):**
	In ChrysaLisp, `:border` defines the 3D bevel/panel border thickness in pixels, *not* padding or margin!
	*	**Buttons & TextFields:** Leave `:border` as the default (or use `1`), unless specifically needing `:border 0` for a flat look (e.g., flat list row items or link-style clickable buttons).
	*	**Labels & Headers:** Labels default to `*env_label_border*` (0 = flat text). Never assign arbitrary large values like `:border 4`, `:border 8`, or `:border 12` to labels or section headers—doing so draws an unsightly, heavy 3D beveled box and dark drop shadow around the text. Leave `:border` at default (or `:border 0`).

*	**Typography & Font Hierarchy (`*env_` and Parent Property Lookups):**
	Apps should **never** hardcode `(create-font "fonts/..." size)`. Instead, apps must respect user-configured environment fonts from `usr/env.inc` and leverage ChrysaLisp's dynamic parent property lookup:
	*	**Parent Property Lookup (`raise`):** When a child widget (Button, Label, Textfield) does not have an explicit `:font` property defined, ChrysaLisp's `(raise :font ...)` mechanism dynamically climbs up the widget tree to find the parent container's font. Therefore:
		- Containers (`ui-flow`, `ui-grid`, `ui-tool-bar`) establish the font context for their children. For example, `(ui-flow _ (:font *env_button_font*) (ui-textfield ...) (ui-button ...))` allows text fields and buttons to inherit without individual font properties.
		- Avoid setting hardcoded properties on raw class constructors (`Button`, `Textfield`), as that blocks `raise` from looking up parent properties.
	*	**Standard Environment Font Definitions:**
		- `*env_window_font*` (18 Regular): Default window font inherited by views.
		- `*env_title_font*` (20 Bold): Window title bar text.
		- `*env_sub_title_font*` (16 Bold): Section headers, hero title labels.
		- `*env_bold_font*` (14 Bold): Emphasized labels, card badges.
		- `*env_body_font*` (14 Regular): Primary text, markdown documents.
		- `*env_button_font*` (13 Regular): Interactive text buttons, inputs, category selectors.
		- `*env_small_font*` (12 Regular): Secondary info, timestamps, status bars, metadata.
		- `*env_tiny_font*` (10 Regular): Fine print, sub-labels, telemetry, attribution.
		- `*env_display_font*` (32 Bold): Hero metrics (temperatures, asset prices).
		- `*env_terminal_font*` / `*env_medium_terminal_font*` / `*env_small_terminal_font*`: Monospace code, terminals, hashes, hex values.

*	**Overloading.** Import another app's `ui.inc` and `redefun` the
	actions you need to change (see `apps/desktop/docs/ui.inc`).

*	**Scroll Panes and Subtrees:** Children of `(Scroll)` widgets are marked with `+view_flag_subtree`. ChrysaLisp's view layout and flattening system does **not** recurse into subtrees. Therefore, content inside a scroll pane must be sized directly:
	```lisp
	(bind '(w h) (. child :pref_size))
	(. child :change_dirty 0 0 w h :t)
	(.-> scroll :layout :dirty_all)
	```
	Without explicitly calling `:change` or `:change_dirty` with `:t` directly on the scroll child, the subtree will remain `(0 0)` in size and will not render.

*	**Bottom-Pinned Bars & Expanding Panes (`+flow_up_fill`):** In windows with fixed bottom bars (such as an input textfield or a footer status bar) and an expanding body/scroll area, wrap them in `(ui-flow _ (:flow_flags +flow_up_fill))` inside the window:
	```lisp
	(ui-flow _ (:flow_flags +flow_up_fill)
		; 1. Bottom-pinned bar listed FIRST (natural height, pinned to bottom):
		(ui-flow input_bar (:flow_flags +flow_right_fill ...) ...)
		; 2. Main stretchable pane listed LAST (receives +flow_flag_lasth to fill all remaining height):
		(ui-scroll main_scroll +scroll_flag_vertical ...))
	```
	Because standard `ui-window` flows downward (`+flow_down_fill`), placing an input bar at the bottom without `+flow_up_fill` causes the input bar to receive `lasth` and stretch vertically.

*	**Non-Blocking Child Spawning (`open-task` & Trash Mailbox):**
	`open-child` internally allocates a temporary mailbox and performs a **synchronous, blocking** `(mail-read mbox)` waiting for the kernel to return the child's `net_id`. In GUI apps, calling `open-child` in `trigger-fetch` functions stalls the event loop and freezes the UI!
	When the spawn confirmation `net_id` is not needed:
	1. Allocate a `trash` mailbox in `+select`:
	   ```lisp
	   (enums +select 0
	       (enum main tip timer worker trash))
	   ```
	2. Spawn the child asynchronously using `open-task` with the trash mailbox:
	   ```lisp
	   (open-task task_code (task-nodeid) +kn_call_run 0 (elem-get select +select_trash))
	   ```
	   This sends the launch request to the kernel and returns **immediately** without waiting.
	3. In the event loop `case`, because the message is already read by `(mail-read (elem-get select (defq idx (mail-select select))))`, you don't even need a clause for `+select_trash` if all other mailboxes are explicitly handled! It simply matches nothing and is automatically discarded:
	   ```lisp
	   (case idx
	       (+select_main ...)
	       (+select_tip ...)
	       (+select_timer ...)
	       (+select_worker ...))
	   ;; (+select_trash is dequeued and discarded with zero extra code!)
	   ```

*	**Idiomatic Conditionals & Implicit Progn in Else Clauses:**
	*	Never write `(if (not cond) ...)`. Always use `(ifn cond then [else ...])` or `(unless cond body ...)`.
	*	Both `(if test then else_1 else_2 ...)` and `(ifn test then else_1 else_2 ...)` evaluate all forms after `then` as an **implicit `progn`**.
	*	**NEVER wrap the `else` clause in `(progn ...)`** — it is completely redundant.
	*	For UI switching (e.g. empty/fallback state vs populated detail pane), use `ifn`:
		```lisp
		(ifn entry
			(progn
				(def (defq md (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
				(. *right_container* :add_child md)
				(. md :populate_lines '("# None" "" "*No item selected.*")))
			;; ELSE clause has implicit progn - no (progn ...) wrapper!
			(def (defq md_top (Md)) :page_width page_w :zoom 1.0 :base_font_size 14)
			(. *right_container* :add_child md_top)
			(. md_top :populate_lines (catalog-overview-markdown entry))
			(defq code_vdu (create-code-vdu (elem-get entry 7) page_w))
			(. *right_container* :add_child code_vdu))
		```

*	**Timer-driven animation.** Re-arm `(mail-timeout ...)` on each
	timer tick, use `+rate (/ 1000000 fps)` for the period, mark
	changed regions with `:add_dirty` and widgets with `:dirty` (see
	`apps/demos/boing/app.lisp`).

## GUI App Debugging & Validation Disciplines

*	**GUI App Debug Prints (`(print "xyz")(print)`):**
	Because GUI apps cannot be launched directly in headless automated test / agent environments, pair with the user for execution. Use top-level trace checkpoints:
	```lisp
	(print "CHECKPOINT 1: before import")(print)
	```
	Note the trailing `(print)` to flush the output stream. The user runs the GUI app interactively to observe which checkpoint is reached before a crash or freeze. Clean up all debug prints before final commit.

*	**Mandatory Elementary Bracket Match Counting:**
	ALWAYS perform strict parenthesis / bracket counting on any modified forms or definitions before testing or declaring an edit complete. Lisp compilers and readers will report cryptic errors or fail silently during child task execution / file loading if an extra or missing paren alters function boundaries or swallows subsequent expressions.



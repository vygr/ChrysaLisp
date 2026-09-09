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

	(if (= 0 (length (mail-enquire "Edit,")))
		(import "./app_impl.lisp"))

*	**Services and RPC.** Declare a service in `main` with
	`(mail-declare mbox "Name" "info")`, add a `remote` select slot for
	its mailbox, handle remote messages in the loop, and `(mail-forget
	key)` on exit (see `apps/tools/edit/app_impl.lisp`).

*	**Zoom.** Scale font sizes with
	`(n2i (* (n2f size) (n2f (get :zoom *window*))))`, set
	`(def *window* :zoom new_size)`, and rebuild the affected views
	(see `apps/desktop/docs/ui.inc` action-scale-up).

*	**State persistence.** Save app state to a `.tre` file with
	`tree-save`/`tree-load` on a file stream (see
	`apps/tools/edit/state.inc`).

*	**Overloading.** Import another app's `ui.inc` and `redefun` the
	actions you need to change (see `apps/desktop/docs/ui.inc`).

*	**Timer-driven animation.** Re-arm `(mail-timeout ...)` on each
	timer tick, use `+rate (/ 1000000 fps)` for the period, mark
	changed regions with `:add_dirty` and widgets with `:dirty` (see
	`apps/demos/boing/app.lisp`).

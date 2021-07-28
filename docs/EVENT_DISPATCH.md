# Event handling and dispatch

This document covers how events sent to the event loop of an application are
handled, or dispatched, so that the correct action is performed for each event
received.

When a GUI application receives an event message in its `main` mailbox these
come from the GUI process. They consist of various types, mouse events,
keyboard presses and UI widget notifications when objects like buttons are
clicked on etc.

In the `EVENT_LOOPS` document we talked about applications allocating multiple
mailboxes to differentiate between type of messages. But not covered techniques
for handling the events that come to the `main` mailbox from the GUI.

Each GUI event message has a common header format which contains a `type` field
so your code can tell what to do with it. But there are various ways you can
handle how you to pick what to do.

Some ways are more flexible than others and can help with code reuse and
maintenance, others while not so flexible, may be fine for applications with
not many events to handle.

## GUI events

There are various GUI event types, listed in `gui/gui/lisp.inc`:

```vdu
(enums +ev_type 0
	(enum mouse key action gui wheel enter exit))
```

These event messages follow a standard format, have a common header that they
inherit from, and extebnded fields for each individual event type.

```vdu
(structure +ev_msg 0
	(ulong target_id type))

(structure +ev_msg_mouse +ev_msg_size
	(uint buttons count)
	(int x y rx ry))

(structure +ev_msg_wheel +ev_msg_size
	(uint direction)
	(int x y))

(structure +ev_msg_key +ev_msg_size
	(uint keycode key mod))

(structure +ev_msg_action +ev_msg_size
	(ulong source_id))

(structure +ev_msg_gui +ev_msg_size)

(structure +ev_msg_enter +ev_msg_size)

(structure +ev_msg_exit +ev_msg_size)
```

When an event is received, it will be targeted at a particular target object,
one of the buttons, or a scroll bar, or a textfield etc. The
`+ev_msg_target_id` field contains the id of that target UI widget.

For an action event, this target id, will depend on whatever the user code
decided it should be by use of the View class `:connect` method.

A negative target id, will be used for internal widget events, positive for the
user action events.

## Defining your UI event IDs

Lets look at the `apps/bubbles/app.lisp`, Bubbles demo:

```vdu
(enums +event 0
	(enum close max min)
	(enum reset)
	(enum grid plain axis))
```

We are going to use these target ids when we construct the UI tree for the
application.

* `+event_close, +event_min, +event_max` for our title bar window buttons.

* `+event_reset` for the main toolbar button, that makes the demo reset with a
new set of bubbles.

* `+event_grid, +event_plain, +event_axis` for the background style selection
buttons.

Here is the UI tree for the application. We will cover the UI builder macros in
another document in detail, but just note for now that we use the start of
these three event blocks for each of the 3 button bars in this UI tree.

```vdu
(ui-window *window* ()
	(ui-title-bar _ "Bubbles" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-tool-bar main_toolbar () (ui-buttons (0xe938) +event_reset))
		(ui-tool-bar style_toolbar () (ui-buttons (0xe976 0xe9a3 0xe9f0) +event_grid)))
	(ui-scroll image_scroll (logior +scroll_flag_vertical +scroll_flag_horizontal)
			(:min_width canvas_width :min_height canvas_height)
		(ui-backdrop mybackdrop (:color +argb_black :ink_color +argb_grey8)
			(ui-canvas layer1_canvas canvas_width canvas_height 1))))
```

The UI macros build our widget tree for us and automates calling the View class
`:connect` methods for the buttons on the button bars. When these buttons are
clicked we will receive an `+ev_msg_action` typed event with the
`+ev_msg_target_id` field containing our declared constants.

## The `(cond)` dispatch event loop

The simplest way to deal with the GUI event handling is to have a `(cond)`
statement, each case of which checks the target id to see what the event was.

Here is the Bubbles `(main)` function, stripped down to just the select mailbox
event handling.

```vdu
(defun main ()
	(defq select (alloc-select +select_size))
	...
	(defq id t)
	(mail-timeout (elem +select_timer select) rate 0)
	(while id
		(defq *msg* (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_timer)
				;timer event
				...
				)
			((= idx +select_tip)
				;tip time mail
				...
				)
			;here we know that this msg must be for (= idx +select_main)
			;ie. from the GUI
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id nil))
			((= id +event_min)
				;min button
				...
				)
			((= id +event_max)
				;max button
				...
				)
			((= id +event_reset)
				;reset button
				...
				)
			((<= +event_grid id +event_axis)
				;style buttons
				...
				)
			...
			(t (. *window* :event *msg*))))
	...
	(free-select select))
```

In the default case we pass the event to our windows `:event` method. This
method deals with all internal UI event traffic, mainly the negative target ids
we mentioned earlier.

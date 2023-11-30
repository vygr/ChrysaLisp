(enums +event 0
	(enum undo redo rewind))

(ui-root *ui_buttons* (Flow)
	(:flow_flags +flow_right
	:font *env_toolbar_font*)
	(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo))

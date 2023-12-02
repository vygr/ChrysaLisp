(enums +event 0
	(enum undo redo rewind))

(ui-window *ui_buttons*
	(:min_width 0
	:min_height 0
	:font *env_toolbar_font*)
	(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo))

(enums +event 0
	(enum undo redo rewind))

(ui-window *ui_toolbar*
	(:min_width 0
	:min_height 0)
	(ui-tool-bar view ()
		(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo)))

(ui-tool-tips view '("undo" "redo" "rewind"))

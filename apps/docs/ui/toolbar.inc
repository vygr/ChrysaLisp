(enums +event 0
	(enum undo redo rewind))

(ui-root *ui_toolbar* (Flow)
	(:flow_flags +flow_right)
	(ui-tool-bar view ()
		(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo)))

(ui-tool-tips view '("undo" "redo" "rewind"))

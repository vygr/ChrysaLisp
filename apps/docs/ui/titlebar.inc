(enums +event 0
	(enum close max min))

(ui-root *ui_title_bar* (Flow)
	(:flow_flags +flow_right)
	(ui-title-bar _ "My Title Bar" (0xea19 0xea1b 0xea1a) +event_close))

(enums +event 0
	(enum close max min))

(ui-window *ui_title_bar*
	(:min_width 256
	:min_height 0)
	(ui-title-bar _ "My Title Bar" (0xea19 0xea1b 0xea1a) +event_close))

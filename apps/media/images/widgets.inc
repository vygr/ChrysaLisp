(enums +event +event_user
	(enum open))

;a picture, opened by the file picker, which keeps where pictures were taken from
(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close +sym_max +sym_min) +event_close)
	(ui-tool-bar *tools* ()
		(ui-buttons (+sym_open) +event_open))
	(ui-scroll *image_scroll* +scroll_flag_both))

(ui-tool-tips *tools* '("open"))

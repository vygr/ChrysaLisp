(ui-window *ui_canvas*
	(:min_width 0
	:min_height 0)
	(ui-canvas view 128 128 1))

(.-> view
	(:fill +argb_white)
	(:set_color +argb_red)
	(:fbox 8 8 64 64)
	(:set_color +argb_green)
	(:fbox 32 48 88 64)
	(:set_color +argb_blue)
	(:fbox 16 100 90 16)
	(:swap 0))

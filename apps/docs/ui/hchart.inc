(ui-window *ui_hchart*
	(:min_width 128
	:min_height 0)
	(ui-hchart view "My Chart" 10
		(:color +argb_green
		:units 1024)))

(times 3 (. view :add_bar))

(ui-root *ui_hchart* (Flow)
	(:flow_flags +flow_right)
	(ui-hchart view "My Chart" 10
		(:color +argb_green
		:units 1024)))

(times 3 (. view :add_bar))

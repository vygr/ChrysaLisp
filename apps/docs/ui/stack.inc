(ui-window *ui_stack*
	(:min_width 128
	:min_height 128)
	(ui-stack _ '("Tab 1" "Tab 2" "Tab 3")
		(:min_height 128)
		(ui-button _ (:text "View 1"))
		(ui-button _ (:text "View 2"))
		(ui-button _ (:text "View 3"))))

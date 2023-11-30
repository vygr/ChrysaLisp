(ui-root *ui_stack* (Flow)
	(:flow_flags +flow_right)
	(ui-stack _ '("Tab 1" "Tab 2" "Tab 3")
		(:color *env_window_col*
		:border *env_button_border*
		:min_height 128)
		(ui-button _ (:text "View 1"))
		(ui-button _ (:text "View 2"))
		(ui-button _ (:text "View 3"))))

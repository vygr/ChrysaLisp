(ui-root *ui_grid* (Flow)
	(:flow_flags +flow_right)
	(ui-grid _
		(:grid_width 3
		:grid_height 2
		:color *env_window_col*)
		(ui-button _ (:text "Cell 1"))
		(ui-button _ (:text "Cell 2"))
		(ui-button _ (:text "Cell 3"))
		(ui-button _ (:text "Cell 4"))
		(ui-button _ (:text "Cell 5"))
		(ui-button _ (:text "Cell 6"))))

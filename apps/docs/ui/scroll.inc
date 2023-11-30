(ui-root *ui_scroll* (Flow)
	(:flow_flags +flow_right)
	(ui-scroll _ +scroll_flag_both
		(:color *env_slider_col*
		:min_width 128
		:min_height 128)
		(ui-button view
			(:color +argb_orange
			:min_width 192
			:min_height 192
			:font *env_window_font*
			:text "Scroll View"))))

(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)

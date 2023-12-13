(enums +event 0
	(enum close)
	(enum hvalue)
	(enum clear)
	(enum clear_all))

(defq +width 60 +height 48)

(ui-window *window* (:color +argb_grey1)
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar _ "Profile" (0xea19) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *main_toolbar* ()
				(ui-buttons (0xe960) +event_clear))
			(ui-tool-bar *main_toolbar2* (:color (const *env_toolbar2_col*))
				(ui-buttons (0xe960) +event_clear_all))
			(ui-backdrop _ (:color (const *env_toolbar_col*))))
		(. (ui-slider *hslider* (:value 0)) :connect +event_hvalue)
		(ui-vdu *vdu* (:vdu_width +width :vdu_height +height :ink_color +argb_yellow))))

(ui-tool-tips *main_toolbar*
	'("clear"))

(ui-tool-tips *main_toolbar2*
	'("clear all"))

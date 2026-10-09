(enums +event +event_user
	(enum hvalue)
	(enum clear)
	(enum clear_all))

(defq +width 60 +height 48)

(ui-window *window* (:color +argb_grey1)
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar _ "Profile" (+sym_close) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *main_toolbar* ()
				(ui-buttons (+sym_clear) +event_clear))
			(ui-tool-bar *main_toolbar2* (:color (const *env_toolbar2_col*))
				(ui-buttons (+sym_clear_all) +event_clear_all))
			(ui-backdrop _ (:color (const *env_toolbar_col*))))
		(. (ui-slider *hslider* (:value 0)) :connect +event_hvalue)
		(ui-vdu *vdu* (:vdu_width +width :vdu_height +height :ink_color +argb_yellow))))

(ui-tool-tips *main_toolbar*
	'("clear"))

(ui-tool-tips *main_toolbar2*
	'("clear all"))

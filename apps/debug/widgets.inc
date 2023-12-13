(enums +event 0
	(enum close)
	(enum hvalue)
	(enum play forward pause step clear)
	(enum play_all forward_all pause_all step_all clear_all))

(defq +width 60 +height 48)

(ui-window *window* (:color +argb_grey1)
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar _ "Debug" (0xea19) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *main_toolbar* ()
				(ui-buttons (0xe95e 0xe95a 0xe95d 0xe95c 0xe960) +event_play))
			(ui-tool-bar *main_toolbar2* (:color (const *env_toolbar2_col*))
				(ui-buttons (0xe95e 0xe95a 0xe95d 0xe95c 0xe960) +event_play_all))
			(ui-backdrop _ (:color (const *env_toolbar_col*))))
		(. (ui-slider *hslider* (:value 0)) :connect +event_hvalue)
		(ui-vdu *vdu* (:vdu_width +width :vdu_height +height :ink_color +argb_yellow))))

(ui-tool-tips *main_toolbar*
	'("play" "forward" "pause" "step" "clear"))

(ui-tool-tips *main_toolbar2*
	'("play all" "forward all" "pause all" "step all" "clear all"))

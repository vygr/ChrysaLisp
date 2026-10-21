(enums +event +event_user
	(enum open play pause step start smaller bigger sooner later timeline path))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close +sym_max +sym_min) +event_close)
	;the buttons and what is shown, the timeline under them the width of
	;the film, and the film. A film is opened by the file picker, which
	;keeps where films were taken from
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *player* ()
				(ui-buttons (+sym_open) +event_open)
				(ui-buttons (+sym_media_play +sym_media_pause +sym_media_on +sym_media_start) +event_play)
				(ui-buttons (+sym_zoom_out +sym_zoom_in) +event_smaller)
				(ui-buttons (+sym_go_back +sym_go_forward) +event_sooner))
			(ui-label *status* (:text "" :font :font_body)))
		;a film anywhere, by its path on this machine
		(. (ui-textfield *path* (:paper +argb_white :clear_text "" :hint_text "a film's path, /Users/... or apps/..."))
			:connect +event_path)
		(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_timeline)
		(ui-scroll *image_scroll* +scroll_flag_both)))

(ui-tool-tips *player* '("open" "play" "pause" "step" "start" "smaller" "bigger" "sound sooner" "sound later"))

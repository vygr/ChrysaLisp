(enums +event +event_user
	(enum open reset play pause smaller bigger quiet))

(ui-window *window* ()
	(ui-title-bar *window_title* "Spectrum" (+sym_close +sym_max +sym_min) +event_close)
	;the buttons, the screen, and what is shown in a line at the foot
	(ui-flow _ (:flow_flags +flow_up_fill)
		(ui-label *status* (:text "" :font :font_body))
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-tool-bar *tools* ()
				(ui-buttons (+sym_open +sym_reset) +event_open)
				(ui-buttons (+sym_media_play +sym_media_pause) +event_play)
				(ui-buttons (+sym_zoom_out +sym_zoom_in) +event_smaller)
				(ui-buttons (+sym_mute) +event_quiet))
			(ui-flow *screen* (:flow_flags +flow_down_fill)))))

(ui-tool-tips *tools* '("open" "reset" "play" "pause" "smaller" "bigger" "sound"))

(enums +event +event_user
	(enum open play pause step start smaller bigger sooner later options timeline path choices endings))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close +sym_max +sym_min) +event_close)
	;the buttons, the path of a film filling the rest of the row, the
	;options under them when they are open, the timeline the width of the
	;film, the film, and what is shown in a line at the foot, as Netmon
	;has. A film is opened by the file picker, which keeps where films were
	;taken from
	(ui-flow _ (:flow_flags +flow_up_fill)
		;the line at the foot, empty of it when there is nothing to say
		(ui-flow *status_box* (:flow_flags +flow_down)
			(ui-label *status* (:text "" :font :font_body)))
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar *player* ()
					(ui-buttons (+sym_open) +event_open)
					(ui-buttons (+sym_media_play +sym_media_pause +sym_media_on +sym_media_start) +event_play)
					(ui-buttons (+sym_zoom_out +sym_zoom_in) +event_smaller)
					(ui-buttons (+sym_go_back +sym_go_forward) +event_sooner)
					(ui-buttons (+sym_menu) +event_options))
				;a film anywhere, by its path on this machine
				(. (ui-textfield *path* (:paper +argb_white :clear_text "" :hint_text "a film's path, /Users/... or apps/..."))
					:connect +event_path))
			;the options: the frame counter in the line at the foot, on or off,
			;and what is done when a film ends, one of them: it stops there, is
			;played again from its start, or the next film of its folder is
			;played, or another of them at random. In it only while they are
			;open, (options-open)
			(ui-flow *options_box* (:flow_flags +flow_down)
				(ui-flow *options_row* (:flow_flags +flow_right)
					(. (ui-toggle-bar *choices* ("Frame counter") (:font :font_body)) :connect +event_choices)
					(. (ui-radio-bar *endings* ("Stop" "Loop" "Next" "Random") (:font :font_body)) :connect +event_endings)))
			(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_timeline)
			(ui-scroll *image_scroll* +scroll_flag_both (:min_width 128 :min_height 128 :fit_width 100 :fit_height 100)))))

(ui-tool-tips *player* '("open" "play" "pause" "step" "start" "smaller" "bigger" "sound sooner" "sound later" "options"))
(ui-tool-tips *endings* '("stop at end" "play again" "next film" "random film"))

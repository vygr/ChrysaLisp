(enums +event +event_user
	(enum file_folder_action file_leaf_action play pause step start smaller bigger timeline path))

(ui-window *window* ()
	(ui-title-bar *window_title* "" (+sym_close +sym_max +sym_min) +event_close)
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-files *file_selector* "Films" +event_file_folder_action :nil)
		(ui-flow _ (:flow_flags +flow_down_fill)
			;the buttons and what is shown, the timeline under them the
			;width of the film, and the film
			(ui-flow _ (:flow_flags +flow_right_fill)
				(ui-tool-bar *player* ()
					(ui-buttons (+sym_media_play +sym_media_pause +sym_media_on +sym_media_start) +event_play)
					(ui-buttons (+sym_zoom_out +sym_zoom_in) +event_smaller))
				(ui-label *status* (:text "" :font :font_body)))
			;a film anywhere, by its path on this machine
			(. (ui-textfield *path* (:clear_text "" :hint_text "a film's path, /Users/... or apps/..."))
				:connect +event_path)
			(. (ui-slider *timeline* (:value 0 :maximum 0 :portion 1 :min_height 14)) :connect +event_timeline)
			(ui-scroll *image_scroll* +scroll_flag_both))))

(ui-tool-tips *player* '("play" "pause" "step" "start" "smaller" "bigger"))
